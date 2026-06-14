;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; REGISTRY.LISP --- datastar-cl.registry compatibility shim
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; The registry implementation lives in lc-sse.registry (src/sse/registry.lisp).
;;;; This shim defines the datastar-cl.registry package as a re-export of
;;;; lc-sse.registry so existing code using datastar-cl.registry:register etc.
;;;; continues to work without modification.

(defpackage #:datastar-cl.registry
  (:use #:cl #:lc-sse.registry)
  (:export
   #:make-sse-registry
   #:make-keyed-sse-registry
   #:register
   #:unregister
   #:notify-subscribers
   #:notify-subscriber
   #:find-by-key
   #:registry-snapshot
   #:registry-count))
