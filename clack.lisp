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

;;; Method implementations for Clack

(defmethod initialize-instance :after ((generator clack-sse-generator) &key env responder &allow-other-keys)
  "Set up SSE headers and response stream for Clack."
  ;; Store the environment and responder
  (setf (env generator) env
        (responder generator) responder)
  ;; Set up SSE headers and get writer function
  (let ((headers '(:content-type "text/event-stream; charset=utf-8"
                   :cache-control "no-cache"
                   :connection "keep-alive")))
    ;; FIXME
    (setf (compressed-stream generator) nil)
    (setf (response generator)
          (lack/util/writer-stream:make-writer-stream
           (funcall responder `(200 ,headers))))))

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
        ;; Read as binary, decode to string. This
        ;; avoids jzon:parse reading a 0-byte
        ;; stream in cases where, for whatever
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

   Woo doesn't properly propagate socket closure as CL conditions,
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
    (let ((stream (response generator)))
      (format stream ": keep-alive~%~%")
      (force-output stream))))

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

(defun make-clack-sse-generator (env responder)
  "Create appropriate Clack SSE generator based on detected backend.
   Automatically selects woo-sse-generator for Woo, or the generic clack-sse-generator for others."
  (make-instance (detect-clack-backend-type env)
                 :env env
                 :responder responder))

