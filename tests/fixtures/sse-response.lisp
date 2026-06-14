;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-SSE-RESPONSE.LISP --- SSE one-shot response tests
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

;;; Tests for WITH-SSE (one-shot SSE responses)
;;;
;;; Tests both Hunchentoot and Clack backends
;;; Verifies:
;;;   - One-shot responses work correctly
;;;   - Sequential connections work (cleanup happens)
;;;   - Error handling (cleanup even if body signals)

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
  "Test endpoint for WITH-SSE with Hunchentoot.

   Sends a single SSE event and closes connection immediately.
   This tests the one-shot response pattern."
  (datastar-cl:with-sse (gen hunchentoot:*request*)
    (incf *test-response-count*)
    (format t "~&[HT] One-shot response #~a~%" *test-response-count*)
    (datastar-cl:patch-signals gen
                              (format nil "{\"count\": ~a, \"time\": \"~a\", \"backend\": \"hunchentoot\"}"
                                      *test-response-count*
                                      (get-universal-time)))))

(hunchentoot:define-easy-handler (test-sse-response-error-ht :uri "/test-sse-response-error") ()
  "Test endpoint that intentionally signals an error.

   Verifies that WITH-SSE cleanup (close-sse-generator)
   happens even when body signals an error."
  (handler-case
      (datastar-cl:with-sse (gen hunchentoot:*request*)
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
  "Test handler for WITH-SSE with Clack.

   Sends a single SSE event and closes connection immediately."
  (lambda (responder)
    (datastar-cl:with-sse (gen (env responder))
      (incf *test-response-count*)
      (format t "~&[Clack] One-shot response #~a~%" *test-response-count*)
      (datastar-cl:patch-signals gen
                                (format nil "{\"count\": ~a, \"time\": \"~a\", \"backend\": \"clack\"}"
                                        *test-response-count*
                                        (get-universal-time))))))

(defun test-sse-response-error-clack-handler (env)
  "Test handler that intentionally signals an error.

   Verifies that WITH-SSE cleanup happens even with errors."
  (lambda (responder)
    (handler-case
        (datastar-cl:with-sse (gen (env responder))
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


