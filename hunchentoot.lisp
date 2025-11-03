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


(defmethod initialize-instance :after ((generator hunchentoot-sse-generator) &key request  &allow-other-keys)
  "Set up SSE headers and response stream for Hunchentoot. Use compression if available"
  ;; Set headers using Hunchentoot API
  (let ((accepts-zstd (search "zstd" (hunchentoot:header-in* :accept-encoding request)
                               :test #'string-equal)))
    (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8"
          (hunchentoot:header-out "Cache-Control") "no-cache")
    ;; HTTP/1.1 specific headers
    
    (when (string= (hunchentoot:server-protocol request) "HTTP/1.1")
      (setf (hunchentoot:header-out "Connection") "keep-alive"))
    (when accepts-zstd
      (setf (hunchentoot:header-out "Content-Encoding") "zstd"
            (hunchentoot:header-out "Vary") "Accept-Encoding"))
    (let* ((raw-stream (hunchentoot:send-headers))
           (compressed-stream (when accepts-zstd
                                (zstd:make-compressing-stream raw-stream :level 3)))
           (utf8-stream (if compressed-stream
                            (flex:make-flexi-stream compressed-stream :external-format :utf-8)
                            (flex:make-flexi-stream raw-stream :external-format :utf-8)
                            )))
    
      (setf (response generator) utf8-stream)
      (setf (compressed-stream generator) compressed-stream))))
    


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
    (let ((stream (response generator)))
      ;; SSE uses ":" as comment. There is no specific keep-alive
      ;; mechanism, so this sends a comment (which is ignored)
      (format stream ": keep-alive~%~%")
      (force-output stream))))

;; Constructor

(defun make-hunchentoot-sse-generator (request)
  "Create a Hunchentoot SSE generator with REQUEST."
  (make-instance 'hunchentoot-sse-generator :request request :response nil))



