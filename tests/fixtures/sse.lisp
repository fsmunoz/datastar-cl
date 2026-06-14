;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-SSE.LISP --- SSE connection tests
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

;;; SSE connection tests
;;;
;;; Covers both Hunchentoot and Clack

(defvar *test-connection-count* 0)
(defvar *test-disconnect-count* 0)

(defun reset-test-counters ()
  "Reset all SSE test counters to zero before a test run."
  (setf *test-connection-count* 0
        *test-disconnect-count* 0))

;;; Hunchentoot test endpoint
;;; Sends one patch-signals event per second; embeds a Unix timestamp in each
;;; event so stress tools can measure delivery delay.
;;; Disconnect is detected at the next write attempt.

(hunchentoot:define-easy-handler (test-sse-ht :uri "/test-sse") ()
  (datastar-cl:with-sse
      (gen hunchentoot:*request*
       :on-connect (lambda (generator)
                     (declare (ignore generator))
                     (incf *test-connection-count*)
                     (format t "~&[HT] Client connected. Total: ~a~%"
                             *test-connection-count*))
       :on-disconnect (lambda (g)
                        (declare (ignore g))
                        (incf *test-disconnect-count*)
                        (format t "~&[HT] Client disconnected. Total: ~a~%"
                                *test-disconnect-count*)))
    (let ((count 0))
      (loop
        (datastar-cl:patch-signals gen
                                   (format nil "{\"time\": ~a, \"count\": ~a}"
                                           (get-universal-time)
                                           (incf count)))
        (sleep 1)))))

;;; Clack test endpoint -- same 1 event/s loop as the Hunchentoot path.
;;; A blocking body blocks the Clack worker; fine for the thread-per-request Hunchentoot backend.

(defun test-sse-clack-handler (env)
  (lambda (responder)
    (datastar-cl:with-sse
        (gen (env responder)
         :on-connect (lambda (generator)
                       (declare (ignore generator))
                       (incf *test-connection-count*)
                       (format t "~&[Clack] Client connected. Total: ~a~%"
                               *test-connection-count*))
         :on-disconnect (lambda (g)
                          (declare (ignore g))
                          (incf *test-disconnect-count*)
                          (format t "~&[Clack] Client disconnected. Total: ~a~%"
                                  *test-disconnect-count*)))
      (let ((count 0))
        (loop
          (datastar-cl:patch-signals gen
                                     (format nil "{\"time\": ~a, \"count\": ~a}"
                                             (get-universal-time)
                                             (incf count)))
          (sleep 1))))))


