;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HANDLERS.LISP --- SSE and command HTTP handlers
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *command-delay* 5
  "Artificial delay (seconds) injected by the retire handler to simulate
   a slow central command round-trip.")

;;; INPUT VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; All /spawn inputs are validated before reaching the store.
;;;
;;; designation: strict alphanumeric+dash allowlist, 1-16 chars.  This charset
;;;   excludes every JS-special character ('  ;  <  >  &  "  `) so a stored
;;;   designation can never break out of a Datastar expression attribute even
;;;   without additional escaping.
;;;
;;; model: exact membership in *allowed-models* (the <select> values).  The
;;;   model list is rendered via (:raw ...) in render-stats, so accepting
;;;   arbitrary strings there would be a stored XSS vector.
;;;
;;; role, incept: capped at 16 chars, printable alphanumeric/space/dot/dash.
;;;   These render only in auto-escaped (:td ...) cells, so length is the
;;;   primary concern here.
;;;
;;; population cap: mirrors the simulator's *sim-max-pop* guard; without it,
;;;   scripted spawning causes O(n) disk/RAM growth and O(n * clients) egress
;;;   amplification on every subsequent broadcast.

(defparameter *max-population* 200
  "Hard cap on total registered replicants (at-large + retired).
   Prevents unbounded store, memory, and broadcast-table growth.")

(defun valid-designation-p (s)
  "T if S is 1-16 characters, each alphanumeric or a hyphen."
  (and (plusp (length s))
       (<= (length s) 16)
       (every (lambda (c) (or (alphanumericp c) (char= c #\-))) s)))

(defun valid-short-field-p (s)
  "T if S is at most 16 characters, each alphanumeric, space, dot, or hyphen."
  (and (<= (length s) 16)
       (every (lambda (c) (or (alphanumericp c) (member c '(#\Space #\. #\-))))
              s)))

;;; SSE HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Uses the "pure-push" CQRS pattern: empty body + :keep-alive.
;;;
;;; on-connect pushes current state immediately and registers the generator.
;;; with-sse own heartbeat loop drives the connection lifetime. a stream error
;;; on the next heartbeat unwinds the macro and fires on-disconnect.

(ht:define-easy-handler (sse-handler :uri "/sse") ()
  (d*:with-signals ((sid "sid" "")) ht:*request*
    (d*:with-sse (gen ht:*request*
                  :on-connect    (lambda (g)
                                   (push-registry g)
                                   (push-radar g)
                                   (push-action-log g)
                                   (push-server-stats g)
                                   (reg:register *clients* g :key sid))
                  :on-disconnect (lambda (g) (reg:unregister *clients* g))
                  :keep-alive    t))))

;;; COMMAND HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; push-radar-event is called on the success path before broadcast so the
;;; same broadcast carries both the updated registry and the new blip.

(ht:define-easy-handler (spawn-handler :uri "/spawn") ()
  (d*:with-signals ((desig  "designation" "")
                    (model  "model"  "Nexus-6")
                    (role   "role"        "") (incept "incept" "")
                    (sid    "sid"         ""))
      ht:*request*
    (cond ((not (valid-designation-p desig))
           (reply-to sid "Invalid designation (1-16 alphanumeric/dash chars)"))
          ((not (member model *models* :test #'string=))
           (reply-to sid "Invalid model"))
          ((not (valid-short-field-p role))
           (reply-to sid "Invalid role (max 16 chars: letters, digits, space, dot, dash)"))
          ((not (valid-short-field-p incept))
           (reply-to sid "Invalid incept date (max 16 chars: letters, digits, space, dot, dash)"))
          ((>= (length (all-replicants)) *max-population*)
           (reply-to sid "Registry at capacity"))
          ((replicant-with-designation desig)
           (reply-to sid "Designation already registered"))
          (t (spawn-replicant desig model role incept)
             (push-radar-event :spawn desig ht:*request* :agent sid)
             (broadcast)
             (reply-to sid))))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

(ht:define-easy-handler (sight-handler :uri "/sight") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) ht:*request*
    (when (plusp (length target))
      (sight-replicant target)
      (push-radar-event :sight target ht:*request* :agent sid)
      (broadcast)
      (reply-to sid)))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

(ht:define-easy-handler (retire-handler :uri "/retire") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) ht:*request*
    (when (plusp (length target))
      (sleep *command-delay*)
      (let ((r (replicant-with-designation target)))
        (cond ((protected-p r)
               (reply-to sid
                         (format nil "~A retirement order failed -- system error code RACH32fa00a139"
                                 target)))
              ((retire-replicant target)
               (push-radar-event :retire target ht:*request* :agent sid)
               (broadcast)
               (reply-to sid))
              (t (reply-to sid
                           (format nil "~A was already retired by another unit -- command void"
                                   target)))))))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")
