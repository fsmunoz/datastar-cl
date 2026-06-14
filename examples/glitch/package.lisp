;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Package definition
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/glitch-demo
  (:use #:cl)
  (:local-nicknames (:bt #:bordeaux-threads))
  (:export #:start-server
           #:stop-server
           #:restart-server
           #:run-server
           #:main
           #:start-clack-server
           #:stop-clack-server
           #:run-clack-server
           #:main-clack))
