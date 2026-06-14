;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Unit test package definition
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/tests/unit
  (:use #:cl #:fiveam)
  (:shadow #:run-all-tests)           ; fiveam also exports this name
  (:local-nicknames (:d* :datastar-cl)
                    (:lc :lc-sse)
                    (:a :alexandria))
  (:export #:run-all-tests))

(in-package #:datastar-cl/tests/unit)

(def-suite :datastar-cl
  :description "All datastar-cl unit tests")

(def-suite :datastar-cl/core
  :description "Unit tests for the core subsystem"
  :in :datastar-cl)

(defun run-all-tests ()
  (run! :datastar-cl))
