;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST.LISP --- Test server management and shared request handler
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

;;; Server management

(defun start-all-servers ()
  "Start all test servers (Hunchentoot on 7331, Clack+Hunchentoot on 7333)."
  (format t "~&Starting all test servers...~%")
  (start-hunchentoot-server)
  (start-clack-hunchentoot-server)
  (format t "~&All servers started.~%")
  (values))

(defun stop-all-servers ()
  "Stop all test servers."
  (format t "~&Stopping all test servers...~%")
  (stop-clack-hunchentoot-server)
  (stop-hunchentoot-server)
  (format t "~&All servers stopped.~%")
  (values))

(defun try-load-optional-compression ()
  "Attempt to load optional compression subsystems at test startup.
   Failure is non-fatal: tests skip the unavailable algorithm."
  (handler-case
      (progn
        (asdf:load-system :datastar-cl/brotli)
        (format t "~&[compression] brotli (:br) loaded~%"))
    (error (c)
      (format t "~&[compression] brotli (:br) not available: ~a~%" c))))

;;; Reference implementation for dispatching Datastar events.
;;; Used by the /test endpoint in fixtures/hunchentoot.lisp.

(defun handle-datastar-signals (signals generator)
  "Reference implementation showing how to dispatch Datastar events.

   This is a TEST HELPER function, not a required framework component.
   It demonstrates one way to process signals - implement your own based
   on your application needs."
  (when signals
    (let ((events (gethash "events" signals)))
      (loop for event across events do
        (let ((type (gethash "type" event))
              (payload event))
          (cond
            ((string= type "patchElements")
             (datastar-cl:patch-elements generator (gethash "elements" payload)
                                         :selector (gethash "selector" payload)
                                         :mode (let ((m (gethash "mode" payload)))
                                                 (if m (intern (string-upcase m) :keyword)
                                                     datastar-cl:*default-patch-mode*))
                                         :use-view-transition (gethash "useViewTransition" payload)
                                         :view-transition-selector (gethash "viewTransitionSelector" payload)
                                         :namespace (let ((ns (gethash "namespace" payload)))
                                                      (if ns (intern (string-upcase ns) :keyword)
                                                          datastar-cl:*default-namespace*))
                                         :event-id (gethash "eventId" payload)
                                         :retry-duration (gethash "retryDuration" payload)))

            ((string= type "patchSignals")
             (datastar-cl:patch-signals generator (or (gethash "signals-raw" payload)
                                                       (gethash "signals" payload))
                                        :only-if-missing (gethash "onlyIfMissing" payload)
                                        :event-id (gethash "eventId" payload)
                                        :retry-duration (gethash "retryDuration" payload)))

            ((string= type "executeScript")
             (datastar-cl:execute-script generator (gethash "script" payload)
                                         :auto-remove (multiple-value-bind (v present)
                                                          (gethash "autoRemove" payload)
                                                        (if present v t))
                                         :attributes (gethash "attributes" payload)
                                         :event-id (gethash "eventId" payload)
                                         :retry-duration (gethash "retryDuration" payload)))

            (t (warn "Unknown event type: ~a" type))))))))
