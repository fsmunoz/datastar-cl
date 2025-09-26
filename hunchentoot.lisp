;;;; Common Lisp SDK for Datastar
;;;;
;;;; Hunchentoot specific methods and functions

(in-package #:datastar-cl)

;; Classes and Methods

(defclass hunchentoot-sse-generator (sse-generator)
  ()
  (:documentation "Hunchentoot-specific SSE generator implementation."))


(defmethod initialize-instance :after ((generator hunchentoot-sse-generator) &key request &allow-other-keys)
  "Set up SSE headers and response stream for Hunchentoot."
  ;; Set headers using Hunchentoot API
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-cache")
  ;; HTTP/1.1 specific headers
  (when (string= (hunchentoot:server-protocol request) "HTTP/1.1")
    (setf (hunchentoot:header-out "Connection") "keep-alive"))
  ;; Initialize response stream
  (let ((raw-stream (hunchentoot:send-headers)))
    (setf (response generator)
          (flex:make-flexi-stream raw-stream :external-format :utf-8))))


(defmethod read-signals ((request hunchentoot:request))
  "Extract and parse datastar signals from Hunchentoot request. Uses the
jzon library to return an HASH-TABLE with the parsed JSON."
  (let ((json-data (if (eq (hunchentoot:request-method request) :get)
                       (hunchentoot:get-parameter "datastar" request)
                       (hunchentoot:raw-post-data :request request :force-text t))))
    (when json-data
      (jzon:parse json-data))))

;; Constructor

(defun make-hunchentoot-sse-generator (request)
  "Create an Hunchentoot SSE generatorwith REQUEST."
  (make-instance 'hunchentoot-sse-generator :request request :response nil))
