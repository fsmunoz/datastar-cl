;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack server with Hunchentoot backend

(in-package #:datastar-cl-tests)


(defparameter *clack-hunchentoot-http-port* 7333 "Port in which to listen")
(defvar *clack-hunchentoot-handler* nil
  "Clack+Hunchentoot server handler instance")


(defun stop-clack-hunchentoot-server ()
  "Stop Clack+Hunchentoot server"
  (when *clack-hunchentoot-handler*
    (clack:stop *clack-hunchentoot-handler*)
    (setf *clack-hunchentoot-handler* nil)))


(defun start-clack-hunchentoot-server (&key (port *clack-hunchentoot-http-port*))
  "Start Clack server with Hunchentoot backend and routing."
  (setf *clack-hunchentoot-handler*
        (clack:clackup
         (lambda (env)
           (let ((path (getf env :path-info)))
             (cond
               ;; Handle /test with datastar handler using with-sse-response
               ((string= path "/test")
                (lambda (responder)
                  (let ((signals (datastar-cl:read-signals env)))
                    (datastar-cl:with-sse-response (gen (env responder))
                      (handle-datastar-signals signals gen)))))

               ;; SSE macro test endpoint (streaming)
               ((string= path "/test-sse")
                (test-sse-clack-handler env))

               ;; SSE response test endpoint (one-shot)
               ((string= path "/test-sse-response")
                (test-sse-response-clack-handler env))

               ;; SSE response error test endpoint (one-shot with error)
               ((string= path "/test-sse-response-error")
                (test-sse-response-error-clack-handler env))

               ;; Handle /
               ((string= path "/")
                '(200 (:content-type "text/plain")
                      ("Up.")))

               ;; Default for other paths
               (t
                '(404 (:content-type "text/plain")
                      ("Not Found"))))))
         :port port
         :server :hunchentoot))
  (format t "Clack+Hunchentoot server started on port ~a~%" port))
