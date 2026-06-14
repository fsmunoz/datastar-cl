;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Package definition and exports
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl
  (:use #:cl #:lc-sse)
  (:local-nicknames (:jzon :com.inuoe.jzon)
                    (:a :alexandria))
  (:export
   ;; Core SDK API
   #:read-signals
   #:read-signal
   #:with-signals
   #:%signal-binding-clauses
   #:patch-elements
   #:remove-element
   #:execute-script
   #:patch-signals
   #:console-log
   #:console-error
   #:redirect
   #:datastar-request-p

   ;; URL action builders
   #:sse-get
   #:sse-post
   #:sse-put
   #:sse-patch
   #:sse-delete
   #:ds-expr
   #:init-signals

   ;; Library inclusion
   #:*datastar-version*
   #:*datastar-cdn-base*
   #:*datastar-source*
   #:datastar-url

   ;; Datastar parameters
   #:*default-patch-mode*
   #:*default-namespace*
   #:*default-auto-remove*
   #:*catch-errors-p*

   ;; Re-exported from lc-sse: generic SSE API
   #:make-sse-generator
   #:send-event
   #:with-sse
   #:with-sse-lock
   #:keep-sse-alive
   #:close-sse-generator
   #:call-with-generator
   #:serve-sse
   #:compression-algorithm

   ;; Re-exported from lc-sse: parameters
   #:+sse-client-default-retry+
   #:*default-retry-duration*
   #:*default-compression-priority*
   #:*default-keep-alive-interval*
   #:*default-compression-parameters*

   ;; Re-exported from lc-sse: generator introspection
   #:request

   ;; Re-exported from lc-sse: backend constructors and helpers
   ;; (available once the respective backend system is loaded)
   #:make-hunchentoot-sse-generator
   #:make-clack-sse-generator
   #:get-clack-header
   #:tcp-nodelay-mixin
   #:tcp-nodelay-easy-acceptor

   ;; Woo backend (available once datastar-cl/woo is loaded)
   #:generator-evloop

   ;; Condition classes
   #:datastar-error
   #:invalid-json-error
   #:compression-error          ; re-exported from lc-sse
   #:client-disconnected        ; re-exported from lc-sse

   ;; Condition accessors
   #:datastar-error-message
   #:invalid-json-error-json-string))
