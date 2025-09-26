;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack server

(in-package #:datastar-cl-tests)


(defparameter *clack-http-port* 7332 "Port in which to listen")

(defun start-clack-server (&key (port *clack-http-port*))
  "Start Clack server"
  (setf *clack-handler* 
        (clack:clackup 
         (datastar-cl::make-clack-datastar-handler #'datastar-cl-tests::handle-datastar-signals )
         :port port
	 :server :woo))
  (format t "Clack server started on port ~a~%" port))

(defun stop-clack-server ()
  "Stop Clack server"
  (when *clack-handler*
    (clack:stop *clack-handler*)
    (setf *clack-handler* nil)))

