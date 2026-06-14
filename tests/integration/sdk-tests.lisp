;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SDK-TESTS.LISP --- Official Datastar SDK conformance tests
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/tests/integration)

;;; Runs the official Datastar SDK test binary against each backend.
;;; The binary is fetched on demand via `go run`.
;;;
;;; Skipped entirely when `go` is not found in PATH.
;;;
;;; The go binary connects to the test server, runs all ADR-mandated
;;; checks, and exits 0 on success.  A non-zero exit is a test failure.

(in-suite :datastar-cl/integration/sdk)

(defun run-go-sdk-test (port)
  "Run the official Datastar SDK test binary against PORT.
   Returns the process exit code."
  (nth-value 2
    (uiop:run-program
      (list "go" "run"
            "github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest"
            "-server"
            (format nil "http://localhost:~a" port))
      :output :interactive
      :error-output :interactive
      :ignore-error-status t)))

(test sdk-hunchentoot
  :suite :datastar-cl/integration/sdk
  (ensure-servers)
  (if (go-available-p)
      (is (zerop (run-go-sdk-test 7331))
          "Official Datastar SDK tests failed on Hunchentoot (port 7331)")
      (skip "go not found in PATH -- SDK conformance test skipped")))

(test sdk-clack-hunchentoot
  :suite :datastar-cl/integration/sdk
  (ensure-servers)
  (if (go-available-p)
      (is (zerop (run-go-sdk-test 7333))
          "Official Datastar SDK tests failed on Clack+Hunchentoot (port 7333)")
      (skip "go not found in PATH -- SDK conformance test skipped")))
