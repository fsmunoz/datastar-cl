;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Package definition
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/horizons-demo
  (:use #:cl  #:snooze)
  (:local-nicknames (:s* :datastar-cl/snooze)
                    (:d* :datastar-cl)                    
                    (:sp :spinneret)
                    (:ps :parenscript))
  
  (:export
   ;; Server management
   #:start-server
   #:restart-server
   #:stop-server
   #:run-server
   #:main

   ;; HTML/CSS generation
   #:generate-index-html
   #:generate-css

   ;; Configuration
   #:*http-port*))
