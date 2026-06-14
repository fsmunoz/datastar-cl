;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Package definition
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/glitch-simple-demo
  (:use #:cl)
  (:local-nicknames (:bt  #:bordeaux-threads)
                    (:reg #:datastar-cl.registry))
  (:export #:start-server
           #:stop-server
           #:restart-server
           #:run-demo))
