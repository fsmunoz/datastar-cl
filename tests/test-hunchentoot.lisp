;;;; Common Lisp SDK for Datastar
;;;;
;;;; Hunchentoot server

(in-package #:datastar-cl-tests)

(defparameter *hunchentoot-http-port* 7331 "Port in which to listen")

(defvar *server*
  "Hunchentoot server instance")

(defun start-hunchentoot-server (&key (port *hunchentoot-http-port*))
  "Start Hunchentoot server"
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*))

(defun stop-hunchentoot-server ()
  "Stop Hunchentoot server"
  (hunchentoot:stop *server*))

(hunchentoot:define-easy-handler (test :uri "/test") ()
  (let* ((signals (read-signals hunchentoot:*request*))  ;; hash table
         (generator (datastar-cl:make-hunchentoot-sse-generator
                     hunchentoot:*request*)))
    (handle-datastar-signals signals generator)
    nil))


