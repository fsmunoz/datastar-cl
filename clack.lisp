;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack specific methods and functions

(in-package #:datastar-cl)

;;; Classes and methods

(defclass clack-sse-generator (sse-generator)
  ((env :initarg :env
        :accessor env
        :documentation "The Clack environment hash-table.")
   (responder :initarg :responder
              :accessor responder
              :documentation "The Clack response callback function."))
  (:documentation "Clack-specific SSE generator implementation."))

(defclass woo-sse-generator (clack-sse-generator)
  ()
  (:documentation "Clack SSE generator specifically for Woo backend.
   Woo requires special socket health checking since it doesn't properly
   propagate socket closure as CL conditions."))

;;; Buffering Writer Stream for Compression Support
;;;
;;; Woo's streaming writer applies HTTP chunked encoding to _every_ callback
;;; invocation. When zstd compresses data, it may call write-sequence multiple
;;; times (frame header, data, checksum), fragmenting the compressed frame
;;; across HTTP chunks and corrupting the protocol.
;;;
;;; This buffering stream collects all compression output and only sends
;;; when explicitly flushed, ensuring one complete compressed frame per chunk.
;;;
;;; This is only used when compression is used (see COMPRESSION.org)

(defclass buffering-writer-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((buffer :initform (make-array 1024 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)
           :accessor buffer)
   (callback :initarg :callback
             :accessor callback
             :documentation "The underlying writer callback function."))
  (:documentation "A buffering stream for compression output.
   Collects all writes into a buffer and only sends when flushed.
   This prevents fragmentation of compressed frames across HTTP chunks."))

(defmethod trivial-gray-streams:stream-write-byte ((stream buffering-writer-stream) byte)
  (vector-push-extend byte (buffer stream))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream buffering-writer-stream) sequence start end &key)
  (loop for i from (or start 0) below (or end (length sequence))
        do (vector-push-extend (aref sequence i) (buffer stream)))
  sequence)

(defmethod trivial-gray-streams:stream-finish-output ((stream buffering-writer-stream))
  "Flush buffer to callback as a single chunk."
  (let ((buf (buffer stream)))
    (when (> (fill-pointer buf) 0)
      ;; Send complete buffer as one callback invocation = one HTTP chunk
      (funcall (callback stream) buf)
      (setf (fill-pointer buf) 0))))

(defmethod trivial-gray-streams:stream-force-output ((stream buffering-writer-stream))
  (trivial-gray-streams:stream-finish-output stream))

(defmethod close ((stream buffering-writer-stream) &key abort)
  (declare (ignore abort))
  ;; Flush any remaining data
  (ignore-errors
    (trivial-gray-streams:stream-finish-output stream))
  ;; Signal close to callback
  (ignore-errors
    (funcall (callback stream) nil :close t)))

(defun make-buffering-writer-stream (callback)
  "Create a buffering writer stream wrapping CALLBACK."
  (make-instance 'buffering-writer-stream :callback callback))

;;; Method implementations for Clack

(defun get-clack-header (env header-name)
  "Extract header value from Clack ENV plist.
   Clack stores headers in a hash-table at :HEADERS with lowercase string keys.
   HEADER-NAME can be a string or keyword like 'accept-encoding' or :accept-encoding."
  (let* ((headers-table (getf env :headers))
         (header-string (string-downcase (string header-name))))
    (when (hash-table-p headers-table)
      (gethash header-string headers-table))))

(defmethod initialize-instance :after ((generator clack-sse-generator)
                                       &key env responder
                                            disable-compression
                                            (compression-level 3)
                                            (compression-priority *default-compression-priority*)
                                       &allow-other-keys)
  "Set up SSE headers and response stream for Clack with optional compression."
  (setf (env generator) env
        (responder generator) responder)

  (multiple-value-bind (algorithm found-p)
      (if disable-compression
          (values nil nil)
          (select-compression-algorithm
           (get-clack-header env :accept-encoding)
           compression-priority))

    (let ((headers (append '(:content-type "text/event-stream; charset=utf-8"
                            :cache-control "no-cache"
                            :connection "keep-alive")
                           (when found-p
                             (list :content-encoding (string-downcase (symbol-name algorithm))
                                   :vary "Accept-Encoding")))))

      (let ((writer-fn (funcall responder `(200 ,headers))))
        (if found-p
            ;; With compression: need flexi-stream for UTF-8 encoding,
            ;; buffering-writer-stream to prevent chunk fragmentation from zstd
            (let* ((base-stream (make-buffering-writer-stream writer-fn))
                   (comp-stream (make-compression-stream algorithm base-stream compression-level))
                   (utf8-stream (flex:make-flexi-stream comp-stream :external-format :utf-8)))
              (setf (response generator) utf8-stream
                    (compressed-stream generator) comp-stream
                    (raw-stream generator) base-stream))
            ;; Without compression: use lack/util/writer-stream directly
            ;; This preserves the original behavior that worked before compression was added.
            ;; lack/util/writer-stream.stream-write-string converts entire strings to bytes
            ;; via babel:string-to-octets in a single callback invocation.
            (setf (response generator)
                  (lack/util/writer-stream:make-writer-stream writer-fn)
                  (compressed-stream generator) nil
                  (raw-stream generator) nil))))))

;; Clack - GET method
(defmethod extract-json-data ((env list) (method (eql :get)))
  "Extract JSON from Clack GET request query string."
  (let* ((query-string (getf env :query-string))
         (datastar-param (when query-string
                          (extract-datastar-from-query query-string))))
    (when datastar-param
      (if (alexandria:emptyp datastar-param)
          (error 'invalid-json-error
                 :json-string ""
                 :message "Empty datastar query parameter")
          (decode-query-parameter datastar-param)))))

;; Clack - POST method
(defmethod extract-json-data ((env list) (method (eql :post)))
  "Extract JSON from Clack POST request body (binary stream)."
  (let ((raw-body (getf env :raw-body))
        (content-length (getf env :content-length)))
    (if (or (null raw-body)
            (null content-length)
            (zerop content-length))
        (error 'invalid-json-error
               :json-string ""
               :message "Empty POST request body")
        ;; Read as binary, decode to string. This avoids jzon:parse
        ;; reading a 0-byte stream in cases where, for whatever
        ;; reason, content length isn't correct.
        (let ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
          (read-sequence buffer raw-body)
          (flexi-streams:octets-to-string buffer :external-format :utf-8)))))

;; Clack doesn't have a built-in way to get the query string.
(defun extract-datastar-from-query (query-string)
  "Extract datastar parameter value from query string."
  (let ((parts (split-sequence:split-sequence #\= query-string)))
    (when (and (>= (length parts) 2)
               (string= (first parts) "datastar"))
      (second parts))))

(defun decode-query-parameter (param)
  "URL-decode a query parameter value using percent-encoding rules."
  (quri:url-decode param))

(defmethod read-signals ((env list) 
                         &key (catch-errors *catch-errors-p*))
  "Read signals from Clack environment."
  (flet ((do-read ()
           (let* ((method (getf env :request-method))
                  (json-data (extract-json-data env method)))
             (when json-data
               (parse-and-validate-json json-data)))))
    (if catch-errors
        (handler-case
            (do-read)
          (datastar-error (condition)
            (format *error-output* "~&[datastar-cl] ~A~%" condition)
            nil))
        (do-read))))

(defmethod ensure-connection-open ((generator clack-sse-generator))
  "Default for Clack backends: rely on natural stream errors.
   Most Clack backends (Hunchentoot, Fcgi, etc.) properly propagate
   stream errors when the client disconnects."
  (values))

(defmethod ensure-connection-open ((generator woo-sse-generator))
  "Proactively check Woo socket state.

   Woo doesn't seem to propagate socket closure as CL conditions,
   so we check the socket state explicitly before I/O operations."
  (let ((socket (getf (env generator) :clack.io)))
    (when socket
      (let ((open-p-slot (find-symbol "OPEN-P" "WOO.EV.SOCKET")))
        (when open-p-slot
          (unless (slot-value socket open-p-slot)
            (error 'sse-connection-lost
                   :stream (response generator)
                   :generator generator)))))))

(defmethod keep-sse-alive :before ((generator clack-sse-generator))
  "Check connection health before sending keep-alive."
  (ensure-connection-open generator))

(defmethod keep-sse-alive ((generator clack-sse-generator))
  "Send keep-alive comment through Clack SSE stream."
  (bt:with-lock-held ((lock generator))
    (let ((stream (response generator))
          (comp-stream (when (slot-boundp generator 'compressed-stream)
                         (compressed-stream generator)))
          (base-stream (when (slot-boundp generator 'raw-stream)
                         (raw-stream generator))))
      (format stream ": keep-alive~%~%")
      ;; Flush through the entire stream stack
      (when comp-stream
        ;; Only call finish-output on response stream when compression is enabled.
        ;; This is important because lack/util/writer-stream's finish-output CLOSES the stream.
        (finish-output stream)
        (finish-output comp-stream))
      ;; Force output on raw stream to ensure data reaches network
      (if base-stream
          (force-output base-stream)
          (force-output stream)))))

;;; Constructor

(defun detect-clack-backend-type (env)
  "Detect the Clack backend type from ENV and return appropriate class symbol.
   Returns 'woo-sse-generator for Woo backend, 'clack-sse-generator otherwise."
  (let ((socket (getf env :clack.io)))
    (if (and socket
             ;; We do not want to depend on Woo so we don't use typep
             (let ((woo-socket-type (find-symbol "SOCKET" "WOO.EV.SOCKET")))
               (and woo-socket-type
                    (typep socket woo-socket-type))))
        'woo-sse-generator
        'clack-sse-generator)))

(defun make-clack-sse-generator (env responder &key disable-compression
                                                     (compression-level 3)
                                                     (compression-priority *default-compression-priority*))
  "Create appropriate Clack SSE generator based on detected backend with optional compression settings.

   Automatically selects woo-sse-generator for Woo, or the generic clack-sse-generator for others.

   Keywords:
     :disable-compression - When T, disable compression even if client supports it
     :compression-level - Compression level (default 3 for zstd, 6 for gzip)
     :compression-priority - List of algorithms in preference order"
  (make-instance (detect-clack-backend-type env)
                 :env env
                 :responder responder
                 :disable-compression disable-compression
                 :compression-level compression-level
                 :compression-priority compression-priority))

