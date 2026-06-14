;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; CONFIG.LISP --- Datastar SDK runtime parameters and constants
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl)

;;; PARAMETERS & CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic SSE parameters (*default-retry-duration*, *default-keep-alive-interval*,
;;; *default-compression-priority*, *default-compression-parameters*) live in
;;; lc-sse (src/sse/config.lisp) and are re-exported from this package.

(defparameter *default-patch-mode* :outer
  "Default element patch mode.")

(defparameter *default-namespace* :html
  "Default element namespace for patch-elements (:html / :svg / :mathml).")

(defparameter *default-auto-remove* t
  "Default auto remove for script execution.")

(a:define-constant +event-types+
  '((:datastar-patch-elements . "datastar-patch-elements")
    (:datastar-patch-signals  . "datastar-patch-signals"))
  :test #'equal
  :documentation "Supported Datastar event types and their wire names.")

;; Inspired by Hunchentoot's approach (but not identical):
;;   [Special variable]
;;   *catch-errors-p*
;;
;;   If the value of this variable is NIL (the default is T), then errors which
;;   happen while a request is handled aren't caught as usual, but instead your
;;   Lisp's debugger is invoked. This variable should obviously always be set to
;;   a true value in a production environment.
;;
;; We do not implement MAYBE-INVOKE-DEBUGGER or anything else: use Hunchentoot's
;; or Clack's debugging variables for dealing with the backends: this
;; *catch-errors-p* is exclusively related with datastar-cl.

(defparameter *catch-errors-p* nil
  "Controls error handling in READ-SIGNALS only.

   When T: Catches DATASTAR-ERROR conditions, logs to *ERROR-OUTPUT*,
                     and returns NIL. Suitable for production.

   When NIL (default): Errors propagate to debugger. Suitable for development.

   NOTE: Does NOT affect SSE sending methods (SEND-EVENT, PATCH-*,
         EXECUTE-SCRIPT). Stream errors from those methods always propagate,
         following CL conventions.")
