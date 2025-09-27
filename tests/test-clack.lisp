;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack server

(in-package #:datastar-cl-tests)


(defparameter *clack-http-port* 7332 "Port in which to listen")


(defun stop-clack-server ()
  "Stop Clack server"
  (when *clack-handler*
    (clack:stop *clack-handler*)
    (setf *clack-handler* nil)))

(defun start-clack-server (&key (port *clack-http-port*))
  "Start Clack server with routing."
  (setf *clack-handler*
        (clack:clackup
         (lambda (env)
           (let ((path (getf env :path-info)))
             (cond
               ;; Handle /test with your datastar handler
               ((string= path "/test")
                (funcall (datastar-cl::make-clack-datastar-handler
                          #'datastar-cl-tests::handle-datastar-signals)
                         env))

               ;; Handle /
               ((string= path "/")
                '(200 (:content-type "text/plain")
                      ("Hello! This is the root path.")))

               ;; Default for other paths
               (t
                '(404 (:content-type "text/plain")
                      ("Not Found"))))))
         :port port
         :server :woo))
  (format t "Clack server started on port ~a~%" port))
