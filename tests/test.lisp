;;;; Common Lisp SDK for Datastar
;;;;
;;;; Generic utilities for testing

(in-package #:datastar-cl-tests)

;;; Test server management

(defun start-all-servers ()
  "Start all test servers (Hunchentoot, Clack+Woo, Clack+Hunchentoot)."
  (format t "~&Starting all test servers...~%")
  (start-hunchentoot-server)
  (start-clack-server)
  (start-clack-hunchentoot-server)
  (format t "~&All servers started.~%")
  (values))

(defun stop-all-servers ()
  "Stop all test servers."
  (format t "~&Stopping all test servers...~%")
  (stop-clack-hunchentoot-server)
  (stop-clack-server)
  (stop-hunchentoot-server)
  (format t "~&All servers stopped.~%")
  (values))

;;; Test execution

(defun run-sse-tests ()
  "Run SSE macro tests using the shell script.
   Tests multiple sequential connections to verify worker cleanup."
  (format t "~&~%Running SSE macro tests...~%")
  (let ((result (uiop:run-program "tests/run-sse-tests.sh"
                                  :output :interactive
                                  :error-output :interactive
                                  :ignore-error-status t)))
    (if (zerop result)
        (format t "~&SSE tests PASSED~%")
        (format t "~&SSE tests FAILED (exit code: ~a)~%" result))
    result))

(defun run-sdk-tests (&key (ports "7331 7332 7333"))
  "Run official Datastar SDK compliance tests using the shell script.
   PORTS can be a string of space-separated port numbers (default: all three backends)."
  (format t "~&~%Running SDK compliance tests...~%")
  (let ((result (uiop:run-program "tests/run-sdk-tests.sh"
                                  :output :interactive
                                  :error-output :interactive
                                  :ignore-error-status t
                                  :environment (list (format nil "SDK_TEST_PORTS=~a" ports)))))
    (if (zerop result)
        (format t "~&SDK tests PASSED~%")
        (format t "~&SDK tests FAILED (exit code: ~a)~%" result))
    result))

(defun run-all-tests ()
  "Run both SSE macro tests and SDK compliance tests.
   Returns T if all tests pass, NIL otherwise."
  (format t "~&~%~%")
  (format t "========================================~%")
  (format t "  Running All Datastar SDK Tests~%")
  (format t "========================================~%")

  (let ((sse-result (run-sse-tests))
        (sdk-result (run-sdk-tests)))

    (format t "~&~%")
    (format t "========================================~%")
    (format t "  Test Results~%")
    (format t "========================================~%")
    (format t "SSE Macro Tests:      ~a~%" (if (zerop sse-result) "PASS" "FAIL"))
    (format t "SDK Compliance Tests: ~a~%" (if (zerop sdk-result) "PASS" "FAIL"))
    (format t "========================================~%")

    (if (and (zerop sse-result) (zerop sdk-result))
        (progn
          (format t "~&~%ALL TESTS PASSED~%~%")
          t)
        (progn
          (format t "~&~%SOME TESTS FAILED~%~%")
          nil))))

;;; Convenience function for quick testing

(defun quick-test ()
  "Quick test: start servers (if needed), run all tests.
   Useful for REPL-driven development."
  (run-all-tests))

;;; Print usage instructions

(defun print-test-usage ()
  "Print instructions for running tests from the REPL."
  (format t "~%========================================~%")
  (format t "--- Datastar SDK Test Runner ---~%")
  (format t "========================================~%~%")
  (format t "Start servers:~%")
  (format t "  (datastar-cl-tests::start-all-servers)~%~%")
  (format t "Run individual test suites:~%")
  (format t "  (datastar-cl-tests::run-sse-tests)~%")
  (format t "  (datastar-cl-tests::run-sdk-tests)~%")
  (format t "  (datastar-cl-tests::run-sdk-tests :ports \"7331\")  ; Test one backend~%~%")
  (format t "Run all tests:~%")
  (format t "  (datastar-cl-tests::run-all-tests)~%")
  (format t "  (datastar-cl-tests::quick-test)  ; shorthand~%~%")
  (format t "Stop servers:~%")
  (format t "  (datastar-cl-tests::stop-all-servers)~%~%")
  (format t "Shell scripts (can also run directly):~%")
  (format t "  ./tests/run-sse-tests.sh~%")
  (format t "  ./tests/run-sdk-tests.sh~%")
  (format t "  SDK_TEST_PORTS=\"7331\" ./tests/run-sdk-tests.sh~%")
  (format t "========================================~%~%"))

;;; Handler

(defun handle-datastar-signals (signals generator)
  "Reference implementation showing how to dispatch Datastar events.

   This is a TEST HELPER function, not a required framework component.
   It demonstrates one way to process signals - implement your own based
   on your application needs.

   This function processes a SINGLE batch of signals from ONE request.
   It does NOT maintain connections or loop indefinitely - it iterates
   through the events array in the signals hash-table and exits.

   The 'loop' here is just iteration over events, not connection management.
   For long-lived connections, use WITH-SSE-CONNECTION separately.

   Arguments:
     signals: Hash-table containing parsed JSON from request (or nil)
     generator: SSE generator for sending response events"
  (when signals  
    (let ((events (gethash "events" signals))) ;; vector
      (loop for event across events do
	(let ((type (gethash "type" event))
	      (payload event))
	  (cond
	    ((string= type "patchElements")
	     (datastar-cl:patch-elements generator (gethash "elements" payload)
					 :selector (gethash "selector" payload)
					 :mode (or (gethash "mode" payload)
						   datastar-cl:*default-patch-mode*)
					 :use-view-transition (gethash "useViewTransition" payload)
					 :event-id (gethash "eventId" payload)
					 :retry-duration (gethash "retryDuration" payload)))
	    
	    ((string= type "patchSignals")
	     (datastar-cl:patch-signals generator (or (gethash "signals-raw" payload)
						      (gethash "signals" payload))
					:only-if-missing (gethash "onlyIfMissing" payload)
					:event-id (gethash "eventId" payload)
					:retry-duration (gethash "retryDuration" payload)))
	    
	    ((string= type "executeScript")
	     (datastar-cl:execute-script generator (gethash "script" payload)
					 :auto-remove (if (gethash "autoRemove" payload)
							  (gethash "autoRemove" payload)
							  t)
					 :attributes (gethash "attributes" payload)
					 :event-id (gethash "eventId" payload)
					 :retry-duration (gethash "retryDuration" payload)))
	    
	    (t (warn "Unknown event type: ~a" type))))))))


