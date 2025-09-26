(in-package #:datastar-cl-tests)

;;; Tests for WITH-SSE-RESPONSE macro (one-shot SSE responses)
;;;
;;; Tests both Hunchentoot and Clack backends
;;; Verifies:
;;;   - One-shot responses work correctly
;;;   - Sequential connections work (cleanup happens)
;;;   - Error handling (cleanup even if body signals)
;;;
;;; The need for sequential testing is due to the behaviour of
;;; Clack+woo, where sockets are not closed when the stream is.

(defvar *test-response-count* 0
  "Counter for successful one-shot responses")

(defvar *test-error-count* 0
  "Counter for errors during one-shot responses")

(defvar *test-cleanup-count* 0
  "Counter to verify cleanup happens (not directly measurable, but helps debugging)")

(defun reset-response-counters ()
  "Reset all test counters for one-shot response tests"
  (setf *test-response-count* 0
        *test-error-count* 0
        *test-cleanup-count* 0))

;;; Hunchentoot test endpoints

(hunchentoot:define-easy-handler (test-sse-response-ht :uri "/test-sse-response") ()
  "Test endpoint for WITH-SSE-RESPONSE macro with Hunchentoot.

   Sends a single SSE event and closes connection immediately.
   This tests the one-shot response pattern."
  (datastar-cl:with-sse-response (gen hunchentoot:*request*)
    (incf *test-response-count*)
    (format t "~&[HT] One-shot response #~a~%" *test-response-count*)
    (datastar-cl:patch-signals gen
                              (format nil "{\"count\": ~a, \"time\": \"~a\", \"backend\": \"hunchentoot\"}"
                                      *test-response-count*
                                      (get-universal-time)))))

(hunchentoot:define-easy-handler (test-sse-response-error-ht :uri "/test-sse-response-error") ()
  "Test endpoint that intentionally signals an error.

   Verifies that WITH-SSE-RESPONSE cleanup (close-sse-generator)
   happens even when body signals an error."
  (handler-case
      (datastar-cl:with-sse-response (gen hunchentoot:*request*)
        (incf *test-response-count*)
        (format t "~&[HT] Response #~a before error~%" *test-response-count*)
        ;; Send partial response
        (datastar-cl:patch-signals gen
                                  (format nil "{\"count\": ~a, \"status\": \"before-error\"}"
                                          *test-response-count*))
        ;; Intentionally signal error
        (error "Intentional test error"))
    (error (e)
      (incf *test-error-count*)
      (format t "~&[HT] Caught error (cleanup should have happened): ~a~%" e)
      ;; Return error response to client
      (setf (hunchentoot:return-code*) 500)
      (format nil "Error: ~a" e))))

;;; Clack test endpoints

(defun test-sse-response-clack-handler (env)
  "Test handler for WITH-SSE-RESPONSE macro with Clack.

   Sends a single SSE event and closes connection immediately."
  (lambda (responder)
    (datastar-cl:with-sse-response (gen (env responder))
      (incf *test-response-count*)
      (format t "~&[Clack] One-shot response #~a~%" *test-response-count*)
      (datastar-cl:patch-signals gen
                                (format nil "{\"count\": ~a, \"time\": \"~a\", \"backend\": \"clack\"}"
                                        *test-response-count*
                                        (get-universal-time))))))

(defun test-sse-response-error-clack-handler (env)
  "Test handler that intentionally signals an error.

   Verifies that WITH-SSE-RESPONSE cleanup happens even with errors."
  (lambda (responder)
    (handler-case
        (datastar-cl:with-sse-response (gen (env responder))
          (incf *test-response-count*)
          (format t "~&[Clack] Response #~a before error~%" *test-response-count*)
          ;; Send partial response
          (datastar-cl:patch-signals gen
                                    (format nil "{\"count\": ~a, \"status\": \"before-error\"}"
                                            *test-response-count*))
          ;; Intentionally signal error
          (error "Intentional test error"))
      (error (e)
        (incf *test-error-count*)
        (format t "~&[Clack] Caught error (cleanup should have happened): ~a~%" e)
        ;; Return error response
        (funcall responder
                 (list 500
                       '(:content-type "text/plain")
                       (list (format nil "Error: ~a" e))))))))

;;; Manual test instructions

(defun print-sse-response-test-instructions ()
  "Print instructions for manually testing WITH-SSE-RESPONSE endpoints."
  (format t "~%========================================~%")
  (format t "--- SSE One-Shot Response Tests ---~%")
  (format t "========================================~%~%")
  (format t "These tests verify WITH-SSE-RESPONSE (one-shot pattern):~%")
  (format t "  - Connection closes immediately after response~%")
  (format t "  - Sequential requests work (cleanup happens)~%")
  (format t "  - Error handling (cleanup even on error)~%~%")

  (format t "1. Start servers (if not running)~%")
  (format t "2. Reset counters: (reset-response-counters)~%~%")

  (format t "3. Test Hunchentoot normal response:~%")
  (format t "   curl http://localhost:~a/test-sse-response~%" *hunchentoot-http-port*)
  (format t "   Expected: Sends single SSE event, connection closes immediately~%")
  (format t "   Expected: *test-response-count* increments~%~%")

  (format t "4. Test sequential Hunchentoot requests:~%")
  (format t "   for i in {1..5}; do curl http://localhost:~a/test-sse-response; done~%"
          *hunchentoot-http-port*)
  (format t "   Expected: Each request succeeds, counter increments to 5~%")
  (format t "   Expected: No connection hanging/leaking~%~%")

  (format t "5. Test Hunchentoot error handling:~%")
  (format t "   curl http://localhost:~a/test-sse-response-error~%" *hunchentoot-http-port*)
  (format t "   Expected: Partial response sent, error caught, HTTP 500~%")
  (format t "   Expected: *test-response-count* and *test-error-count* increment~%")
  (format t "   Expected: Connection still closes (cleanup happened)~%~%")

  (format t "6. Test Clack normal response:~%")
  (format t "   curl http://localhost:~a/test-sse-response~%" *clack-http-port*)
  (format t "   Expected: Same as Hunchentoot test~%~%")

  (format t "7. Test sequential Clack requests:~%")
  (format t "   for i in {1..5}; do curl http://localhost:~a/test-sse-response; done~%"
          *clack-http-port*)
  (format t "   Expected: Same as Hunchentoot sequential test~%~%")

  (format t "8. Test Clack error handling:~%")
  (format t "   curl http://localhost:~a/test-sse-response-error~%" *clack-http-port*)
  (format t "   Expected: Same as Hunchentoot error test~%~%")

  (format t "9. Test Clack+Hunchentoot (third backend):~%")
  (format t "   curl http://localhost:~a/test-sse-response~%" *clack-hunchentoot-http-port*)
  (format t "   Expected: Works like other backends~%~%")

  (format t "SUCCESS CRITERIA:~%")
  (format t "  - All curl commands complete immediately (no hanging)~%")
  (format t "  - Sequential requests succeed (no connection leaks)~%")
  (format t "  - Error requests return 500 but don't leak connections~%")
  (format t "  - *test-response-count* matches total requests~%")
  (format t "  - *test-error-count* matches error requests~%~%")

  (format t "Check counters:~%")
  (format t "  *test-response-count*  => Should match total requests~%")
  (format t "  *test-error-count*     => Should match error endpoint calls~%")
  (format t "========================================~%~%"))

;;; Quick test runner.We're using drakma here, but for the other tests
;;; we are using shell scripts. 

(defun run-sse-response-tests ()
  "Run a series of automated tests for WITH-SSE-RESPONSE.

   NOTE: Requires servers to be running. Start with:
     (start-all-servers)  ; or individual start-*-server functions

   Tests all three backends: Hunchentoot, Clack/Woo, Clack/Hunchentoot"
  (format t "~%Running automated WITH-SSE-RESPONSE tests...~%~%")
  (reset-response-counters)

  (flet ((test-endpoint (name url)
           (format t "Testing ~a... " name)
           (handler-case
               (let ((response (drakma:http-request url :want-stream nil)))
                 (if (search "count" response)
                     (format t "OK - PASS~%")
                     (format t "NOK - FAIL (unexpected response)~%")))
             (error (e)
               (format t "✗ FAIL (~a)~%" e)))))

    ;; Test Hunchentoot
    (format t "~%--- Hunchentoot Backend ---~%")
    (test-endpoint "Normal response"
                   (format nil "http://localhost:~a/test-sse-response" *hunchentoot-http-port*))
    (test-endpoint "Sequential request 1"
                   (format nil "http://localhost:~a/test-sse-response" *hunchentoot-http-port*))
    (test-endpoint "Sequential request 2"
                   (format nil "http://localhost:~a/test-sse-response" *hunchentoot-http-port*))

    ;; Test Clack
    (format t "~%--- Clack/Woo Backend ---~%")
    (test-endpoint "Normal response"
                   (format nil "http://localhost:~a/test-sse-response" *clack-http-port*))
    (test-endpoint "Sequential request 1"
                   (format nil "http://localhost:~a/test-sse-response" *clack-http-port*))
    (test-endpoint "Sequential request 2"
                   (format nil "http://localhost:~a/test-sse-response" *clack-http-port*))

    ;; Test Clack+Hunchentoot
    (format t "~%--- Clack/Hunchentoot Backend ---~%")
    (test-endpoint "Normal response"
                   (format nil "http://localhost:~a/test-sse-response" *clack-hunchentoot-http-port*))
    (test-endpoint "Sequential request 1"
                   (format nil "http://localhost:~a/test-sse-response" *clack-hunchentoot-http-port*))
    (test-endpoint "Sequential request 2"
                   (format nil "http://localhost:~a/test-sse-response" *clack-hunchentoot-http-port*))

    (format t "~%--- Results ---~%")
    (format t "*test-response-count*: ~a (expected: 9)~%" *test-response-count*)
    (format t "*test-error-count*:    ~a (expected: 0)~%" *test-error-count*)

    (if (and (= *test-response-count* 9)
             (= *test-error-count* 0))
        (format t "~%ALL TESTS PASSED~%~%")
        (format t "~%SOME TESTS FAILED~%~%"))))


