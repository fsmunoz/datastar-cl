;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack server

(in-package #:datastar-cl-tests)

(defparameter *clack-http-port* 7332 "Port in which to listen")
(defvar *clack-handler* nil
  "Clack server handler instance")

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
         :server :woo))
  (format t "Clack server started on port ~a~%" port))
