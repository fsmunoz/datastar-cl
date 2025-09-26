(in-package #:datastar-cl-tests)

;;; SSE tests, especially for the with-sse-connection macro
;;;
;;; Covers both Hunchentoot and Clack

(defvar *test-connection-count* 0)
(defvar *test-disconnect-count* 0)
(defvar *test-keepalive-count* 0)

(defun reset-test-counters ()
  (setf *test-connection-count* 0
        *test-disconnect-count* 0
        *test-keepalive-count* 0))

;;; Hunchentoot test endpoint
(hunchentoot:define-easy-handler (test-sse-ht :uri "/test-sse") ()
  (datastar-cl:with-sse-connection
      (gen hunchentoot:*request*
       :keep-alive-interval 2  ; Fast for testing
       :body-interval 1        ; Send updates every 2 seconds
       :on-connect (lambda (g)
                     (incf *test-connection-count*)
                     (format t "~&[HT] Client connected. Total: ~a~%"
                             *test-connection-count*))
       :on-disconnect (lambda (g err)
                        (incf *test-disconnect-count*)
                        (format t "~&[HT] Client disconnected: ~a. Total: ~a~%"
                                err *test-disconnect-count*)))
    ;; Send a test message every cycle
    (datastar-cl:patch-signals gen
                              (format nil "{\"time\": \"~a\", \"count\": ~a}"
                                      (get-universal-time)
                                      (incf *test-keepalive-count*)))))

;;; Clack test endpoint

(defun test-sse-clack-handler (env)
  (lambda (responder)
    (datastar-cl:with-sse-connection
        (gen (env responder)
         :keep-alive-interval 2  ; Fast for testing
         :body-interval 1        ; Send updates every 2 seconds
         :on-connect (lambda (g)
                       (incf *test-connection-count*)
                       (format t "~&[Clack] Client connected. Total: ~a~%"
                               *test-connection-count*))
         :on-disconnect (lambda (g err)
                          (incf *test-disconnect-count*)
                          (format t "~&[Clack] Client disconnected: ~a. Total: ~a~%"
                                  err *test-disconnect-count*)))
      (datastar-cl:patch-signals gen
                                (format nil "{\"time\": \"~a\", \"count\": ~a}"
                                        (get-universal-time)
                                        (incf *test-keepalive-count*))))))

;;; Manual test instructions for REPL
(defun print-sse-test-instructions ()
  "Print instructions for manually testing SSE endpoints."
  (format t "~%========================================~%")
  (format t "--- SSE Test Instructions ---~%")
  (format t "========================================~%~%")
  (format t "1. Start servers ~%")
  (format t "2. Reset counters: (reset-test-counters)~%")
  (format t "3. Test Hunchentoot:~%")
  (format t "   curl -N http://localhost:~a/test-sse~%" *hunchentoot-http-port*)
  (format t "~%4. Test Clack:~%")
  (format t "   curl -N http://localhost:~a/test-sse~%" *clack-http-port*)
  (format t "~%5. Watch REPL for connection/disconnect messages~%")
  (format t "6. Press Ctrl+C in curl to disconnect~%")
  (format t "7. Check counters:~%")
  (format t "   *test-connection-count* should increment per connection~%")
  (format t "   *test-disconnect-count* should increment per disconnect~%")
  (format t "   *test-keepalive-count* should increment every 2 seconds~%")
  (format t "~%Expected behavior:~%")
  (format t "  - Connection hooks fire immediately~%")
  (format t "  - Updates sent every 2 seconds~%")
  (format t "  - Disconnect hooks fire on Ctrl+C~%")
  (format t "========================================~%~%"))
