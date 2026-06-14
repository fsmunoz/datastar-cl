;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-COMPRESSION.LISP --- Compression test endpoints
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

;;; Compression test endpoints.
;;;
;;; /test-compression sends 3 events via with-sse (one-shot); exercises
;;; multiple flush-sse-streams calls through a single still-open compression
;;; stream. Available on both backends (port 7331 HT, port 7333 Clack).
;;;
;;; Long-lived compressed connections are covered by the CQRS burst tests
;;; (compression-cqrs-burst-*), which use /test-cqrs-stream + the shared
;;; *test-cqrs-clients* registry for a more faithful broadcaster push test.
;;;
;;; The endpoints advertise all algorithms that are currently loaded.
;;; Brotli (:br) is included only when the :brotli package is present,
;;; i.e. when datastar-cl/brotli has been loaded. This keeps the core
;;; test system independent of cl-brotli while still exercising brotli
;;; when it is available.

(defun compression-test-priority ()
  "Return compression priority for test endpoints.
   Includes :br when the brotli package is loaded (datastar-cl/brotli)."
  (if (find-package :brotli)
      '(:zstd :br)
      '(:zstd)))

;;; Clack one-shot endpoint (3 events)

(defun test-compression-clack-handler (env)
  "Clack handler mirroring test-compression-ht: sends 3 patch-signals events."
  (lambda (responder)
    (let ((datastar-cl:*default-compression-priority* (compression-test-priority)))
      (datastar-cl:with-sse (gen (env responder))
        (dotimes (i 3)
          (datastar-cl:patch-signals gen
            (format nil "{\"compression-test\": true, \"event-id\": ~d, \"value\": 42}" i)))))))

;;; Hunchentoot one-shot endpoint (3 events)
;;;
;;; Three events exercise flush-sse-streams multiple times against the
;;; same still-open compression stream before close-sse-generator runs.

(hunchentoot:define-easy-handler (test-compression-ht :uri "/test-compression") ()
  (let ((datastar-cl:*default-compression-priority* (compression-test-priority)))
    (datastar-cl:with-sse (gen hunchentoot:*request*)
      (dotimes (i 3)
        (datastar-cl:patch-signals gen
          (format nil "{\"compression-test\": true, \"event-id\": ~d, \"value\": 42}" i))))))

;;; /test-compression-stream was removed: superseded by the CQRS burst tests
;;; (compression-cqrs-burst-zstd / -brotli), which exercise long-lived
;;; compressed connections more faithfully (broadcaster push path, mid-stream
;;; decode of an unterminated frame, back-to-back multi-event). The keep-alive
;;; heartbeat-through-compressor path is covered by KEEPALIVE-IDLE-{ZSTD,BROTLI}.

