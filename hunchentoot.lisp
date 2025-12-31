;;;; Common Lisp SDK for Datastar
;;;;
;;;; Hunchentoot specific methods and functions

(in-package #:datastar-cl)

;; Classes and Methods

(defclass hunchentoot-sse-generator (sse-generator)
  ()
  (:documentation "Hunchentoot-specific SSE generator implementation."))


;; (defmethod initialize-instance :after ((generator hunchentoot-sse-generator) &key request &allow-other-keys)
;;   "Set up SSE headers and response stream for Hunchentoot."
;;   ;; Set headers using Hunchentoot API
;;   (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8"
;;         (hunchentoot:header-out "Cache-Control") "no-cache")
;;   ;; HTTP/1.1 specific headers
;;   (when (string= (hunchentoot:server-protocol request) "HTTP/1.1")
;;     (setf (hunchentoot:header-out "Connection") "keep-alive"))
;;   ;; Initialize response stream
;;   (let ((raw-stream (hunchentoot:send-headers)))
;;     (setf (response generator)
;;           (flex:make-flexi-stream raw-stream :external-format :utf-8))))


(defmethod initialize-instance :after ((generator hunchentoot-sse-generator)
                                       &key request
                                            disable-compression
                                            (compression-level 3)
                                            (compression-priority *default-compression-priority*)
                                       &allow-other-keys)
  "Set up SSE headers and response stream for Hunchentoot with optional compression.

   Keywords:
     :disable-compression - When T, disable compression even if client supports it
     :compression-level - Compression level (default 3 for zstd)
     :compression-priority - List of algorithms in preference order (default *default-compression-priority*)"
  ;; Set basic SSE headers
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-cache")

  ;; HTTP/1.1 specific headers
  (when (string= (hunchentoot:server-protocol request) "HTTP/1.1")
    (setf (hunchentoot:header-out "Connection") "keep-alive"))

  ;; Select compression algorithm based on Accept-Encoding and priority
  (multiple-value-bind (algorithm found-p)
      (if disable-compression
          (values nil nil)
          (select-compression-algorithm
           (hunchentoot:header-in* :accept-encoding request)
           compression-priority))

    ;; Set Content-Encoding header if compression selected
    (when found-p
      (setf (hunchentoot:header-out "Content-Encoding")
            (string-downcase (symbol-name algorithm))
            (hunchentoot:header-out "Vary") "Accept-Encoding"))

    ;; Create stream stack: raw -> compression (optional) -> UTF-8
    (let* ((base-stream (hunchentoot:send-headers))
           (comp-stream (when found-p
                          (make-compression-stream algorithm base-stream compression-level)))
           (utf8-stream (flex:make-flexi-stream (or comp-stream base-stream)
                                                :external-format :utf-8)))
      (setf (response generator) utf8-stream
            (compressed-stream generator) comp-stream
            (raw-stream generator) base-stream))))

(defmethod read-signals ((request hunchentoot:request) 
                         &key (catch-errors *catch-errors-p*))
  "Read signals from Hunchentoot request."
  (flet ((do-read ()
           (let* ((method (hunchentoot:request-method request))
                  (json-data (extract-json-data request method)))
             (when json-data
               (parse-and-validate-json json-data)))))
    (if catch-errors
        (handler-case
            (do-read)
          (datastar-error (condition)
            (format *error-output* "~&[datastar-cl] ~A~%" condition)
            nil))
        (do-read))))

;; Hunchentoot - GET method
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :get)))
  "Extract JSON from Hunchentoot GET request query string."
  (let ((datastar-param (hunchentoot:get-parameter "datastar" request)))
    (when datastar-param
      (if (alexandria:emptyp datastar-param)
          (error 'invalid-json-error
                 :json-string ""
                 :message "Empty datastar query parameter")
          datastar-param))))

;; Hunchentoot - POST method
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :post)))
  "Extract JSON from Hunchentoot POST request body."
  (let ((raw-data (hunchentoot:raw-post-data :request request :force-text t)))
    (if (or (null raw-data) (alexandria:emptyp raw-data))
        (error 'invalid-json-error
               :json-string ""
               :message "Empty POST request body")
        raw-data)))

(defmethod keep-sse-alive :before ((generator hunchentoot-sse-generator))
  "Check connection health before sending keep-alive."
  (ensure-connection-open generator))

(defmethod keep-sse-alive ((generator hunchentoot-sse-generator))
  "Send keep-alive comment through Hunchentoot SSE stream."
  (bt:with-lock-held ((lock generator))
    (let ((stream (response generator))
          (comp-stream (when (slot-boundp generator 'compressed-stream)
                         (compressed-stream generator)))
          (base-stream (when (slot-boundp generator 'raw-stream)
                         (raw-stream generator))))
      ;; SSE uses ":" as comment. There is no specific keep-alive
      ;; mechanism, so this sends a comment (which is ignored)
      (format stream ": keep-alive~%~%")
      ;; Flush through the entire stream stack
      (finish-output stream)
      (when comp-stream
        (finish-output comp-stream))
      ;; Force output on raw stream to ensure data reaches network
      (if base-stream
          (force-output base-stream)
          (force-output stream)))))

;; Constructor

(defun make-hunchentoot-sse-generator (request &key disable-compression
                                                     (compression-level 3)
                                                     (compression-priority *default-compression-priority*))
  "Create a Hunchentoot SSE generator with REQUEST and optional compression settings.

   Keywords:
     :disable-compression - When T, disable compression even if client supports it
     :compression-level - Compression level (default 3 for zstd, 6 for gzip)
     :compression-priority - List of algorithms in preference order"
  (make-instance 'hunchentoot-sse-generator
                 :request request
                 :response nil
                 :disable-compression disable-compression
                 :compression-level compression-level
                 :compression-priority compression-priority))



