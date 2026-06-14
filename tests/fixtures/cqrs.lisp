;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-CQRS.LISP --- CQRS integration test endpoints
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

;;; Tests the CQRS read/write split:
;;;   GET /test-cqrs-stream   - long-lived SSE; body blocks on keep-alive loop
;;;   POST /test-cqrs-command - command; broadcasts probe value directly to all streams
;;;
;;; Uses the plain registry pattern (no Calispel).

;;; Connection registry

(defvar *test-cqrs-lock* (bt:make-lock "test-cqrs-clients"))
(defvar *test-cqrs-clients* '())

(defun test-cqrs-register (gen)
  "Add GEN to the test CQRS client registry."
  (bt:with-lock-held (*test-cqrs-lock*)
    (pushnew gen *test-cqrs-clients* :test #'eq)))

(defun test-cqrs-unregister (gen)
  "Remove GEN from the test CQRS client registry."
  (bt:with-lock-held (*test-cqrs-lock*)
    (setf *test-cqrs-clients*
          (remove gen *test-cqrs-clients* :test #'eq))))

(defun test-cqrs-broadcast (probe)
  "Snapshot the registry, then push patch-signals carrying PROBE to every
   generator directly. Dead generators (broken pipe, closed stream) are
   caught per-entry and unregistered so iteration always continues."
  (let ((payload (format nil "{\"cqrs.probe\": \"~A\"}" probe))
        snapshot)
    (bt:with-lock-held (*test-cqrs-lock*)
      (setq snapshot (copy-list *test-cqrs-clients*)))
    (dolist (gen snapshot)
      (handler-case
          (datastar-cl:patch-signals gen payload)
        (stream-error () (test-cqrs-unregister gen))))))

(defun test-cqrs-parse-probe (query-string)
  "Extract the probe value from a ?probe=<value> query string.
   Returns the value string, or \"no-probe\" if absent."
  (if query-string
      (let ((pos (position #\= query-string)))
        (if pos
            (subseq query-string (1+ pos))
            "no-probe"))
      "no-probe"))

;;; Hunchentoot endpoints

(hunchentoot:define-easy-handler
    (test-cqrs-stream-ht :uri "/test-cqrs-stream") ()
  "Long-lived SSE endpoint. Body blocks on a keep-alive loop; disconnect
   detected at the next heartbeat write."
  (datastar-cl:with-sse (gen hunchentoot:*request*
                          :on-connect    #'test-cqrs-register
                          :on-disconnect #'test-cqrs-unregister)
    (loop
      (sleep 2)
      (datastar-cl:keep-sse-alive gen))))

(hunchentoot:define-easy-handler
    (test-cqrs-command-ht :uri "/test-cqrs-command") (probe)
  "Short-lived command endpoint. Broadcasts the probe value to every
   registered generator and returns immediately."
  (setf (hunchentoot:content-type*) "text/plain")
  (test-cqrs-broadcast (or probe "no-probe"))
  "OK")

;;; Clack handler functions.
;;; Wired into the Clack server's cond in test-clack-hunchentoot.lisp.

(defun test-cqrs-stream-clack-handler (env)
  "Streaming SSE handler for Clack. Returns (lambda (responder) ...)."
  (lambda (responder)
    (datastar-cl:with-sse (gen (env responder)
                           :on-connect    #'test-cqrs-register
                           :on-disconnect #'test-cqrs-unregister)
      (loop
        (sleep 2)
        (datastar-cl:keep-sse-alive gen)))))

(defun test-cqrs-command-clack-handler (env)
  "Command handler for Clack. Reads ?probe=<value> from the query string."
  (let ((probe (test-cqrs-parse-probe (getf env :query-string))))
    (test-cqrs-broadcast probe)
    '(200 (:content-type "text/plain") ("OK"))))
