;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SMOKE-TESTS.LISP --- Thin seam tests: datastar-cl layer over lc-sse
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/tests/integration)

;;; Two assertions per backend: that the datastar-cl protocol layer and the
;;; lc-sse transport layer compose correctly.  Deep compression/keep-alive/CQRS
;;; coverage lives in lc-sse-tests/integration; these exist only to catch a
;;; bad lc-sse submodule bump.
;;;
;;;   1. compressed one-shot: GET /test-compression with Accept-Encoding: zstd
;;;      decompresses to datastar-patch-signals events carrying the expected value.
;;;   2. registry broadcast: CQRS round-trip via /test-cqrs-stream +
;;;      /test-cqrs-command, verifying a datastar-patch-signals event arrives.

(in-suite :datastar-cl/integration/smoke)

(defun check-compressed-oneshot (url)
  (multiple-value-bind (bytes _status headers)
      (drakma:http-request url
                           :force-binary t
                           :additional-headers '(("Accept-Encoding" . "zstd")))
    (declare (ignore _status))
    (is (string-equal "zstd" (get-header :content-encoding headers))
        "~a: expected Content-Encoding: zstd, got ~a"
        url (get-header :content-encoding headers))
    (let ((text (decompress-zstd-bytes bytes)))
      (is (search "datastar-patch-signals" text)
          "~a: decompressed body missing datastar-patch-signals: ~a" url text)
      (is (search "42" text)
          "~a: decompressed body missing value 42: ~a" url text))))

(defun check-cqrs-smoke (stream-url command-url)
  (let* ((probe  (format nil "smoke-~a-~a" (get-universal-time) (random 99999)))
         (result (cqrs-test stream-url command-url probe 0.5)))
    (is (search "datastar-patch-signals" result)
        "CQRS smoke: expected datastar-patch-signals in stream, got: ~a" result)
    (is (search probe result)
        "CQRS smoke: probe '~a' not found in stream: ~a" probe result)))

;;; 1. Compressed one-shot -- Hunchentoot

(test smoke-ht-compressed-oneshot
  :suite :datastar-cl/integration/smoke
  (ensure-servers)
  (check-compressed-oneshot (ht-url "/test-compression")))

;;; 2. CQRS broadcast -- Hunchentoot

(test smoke-ht-cqrs
  :suite :datastar-cl/integration/smoke
  (ensure-servers)
  (check-cqrs-smoke (ht-url "/test-cqrs-stream")
                    (ht-url "/test-cqrs-command")))

;;; 3. Compressed one-shot -- Clack

(test smoke-clack-compressed-oneshot
  :suite :datastar-cl/integration/smoke
  (ensure-servers)
  (check-compressed-oneshot (clack-url "/test-compression")))

;;; 4. CQRS broadcast -- Clack

(test smoke-clack-cqrs
  :suite :datastar-cl/integration/smoke
  (ensure-servers)
  (check-cqrs-smoke (clack-url "/test-cqrs-stream")
                    (clack-url "/test-cqrs-command")))
