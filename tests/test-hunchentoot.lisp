;;;; Common Lisp SDK for Datastar
;;;;
;;;; Hunchentoot server

(in-package #:datastar-cl-tests)

(defparameter *hunchentoot-http-port* 7331 "Port in which to listen")

(defvar *server*
  "Hunchentoot server instance")

(defun start-hunchentoot-server (&key (port *hunchentoot-http-port*))
  "Start Hunchentoot server"
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port :address "localhost"))
  (hunchentoot:start *server*)
  (format t "~%Hunchentoot server started on port ~a~%" port))

(defun stop-hunchentoot-server ()
  "Stop Hunchentoot server"
  (hunchentoot:stop *server*))


;; Test endpoint using with-sse-response macro
(hunchentoot:define-easy-handler (test :uri "/test") ()
  (let ((signals (datastar-cl:read-signals hunchentoot:*request*)))
    (datastar-cl:with-sse-response (gen hunchentoot:*request*)
      (handle-datastar-signals signals gen))))

(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  "Up.")





