;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Package definition
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/rep-detect-demo
  (:use #:cl)
  (:local-nicknames (:sp  #:spinneret)
                    (:d*  #:datastar-cl)
                    (:reg #:datastar-cl.registry)
                    (:bt  #:bordeaux-threads)
                    (:bk  #:bknr.datastore)
                    (:idx #:bknr.indices)
                    (:geo #:cl-maxminddb))
  (:export ;; Hunchentoot entry points
           #:start-server
           #:stop-server
           #:restart-server
           #:run-server
           #:main
           ;; Clack + Woo entry points
           #:start-clack-server
           #:stop-clack-server
           #:run-clack-server
           #:main-clack
           ;; Simulator (shared)
           #:start-simulator
           #:stop-simulator))
