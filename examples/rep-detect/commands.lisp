;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; COMMANDS.LISP --- Input validation, domain commands, and SSE lifecycle
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; Backend-neutral core: no hunchentoot, no clack.  Transport files
;;;; (handlers-hunchentoot.lisp, handlers-clack.lisp) extract the IP and
;;;; signal values from their respective request objects, then delegate here.

(in-package #:datastar-cl/rep-detect-demo)

;;; SERVER UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-env-int (name default)
  "Return the integer value of env var NAME, or DEFAULT if absent or unparseable."
  (let ((raw (uiop:getenv name)))
    (if raw
        (handler-case (parse-integer raw)
          (error () default))
        default)))

;;; CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *command-delay* 5
  "Artificial delay (seconds) injected by the retire handler to simulate
   a slow central command round-trip.")

;;; INPUT VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; SSE CONNECTION LIFECYCLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sse-connect (gen sid)
  "Push the current application state to GEN and register it under SID.
   Called from the :on-connect callback in both Hunchentoot and Clack transports."
  (push-registry gen)
  (push-radar gen)
  (push-action-log gen)
  (push-server-stats gen)
  (reg:register *clients* gen :key sid))

;;; DOMAIN COMMANDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Each perform-* function encapsulates the domain logic for one HTTP command
;;; endpoint.  It takes already-parsed signal values and a client IP string (NIL
;;; when unknown), performs side effects (store mutation, broadcast, reply-to),
;;; and returns nothing.  The transport is responsible for setting the HTTP
;;; response status (204 No Content).
;;;
;;; push-radar-event and broadcast are called here; on Woo, broadcast routes
;;; writes through call-with-generator (woo:schedule) automatically.

(defun perform-spawn (desig model role incept sid ip)
  "Validate and execute a SPAWN command.  Broadcasts on success; sends an error
   message to SID on any validation failure."
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
           (push-radar-event :spawn desig ip :agent sid)
           (broadcast)
           (reply-to sid))))

(defun perform-sight (target sid ip)
  "Execute a SIGHT command for TARGET.  No-op when TARGET is empty."
  (when (plusp (length target))
    (sight-replicant target)
    (push-radar-event :sight target ip :agent sid)
    (broadcast)
    (reply-to sid)))

(defun perform-retire (target sid ip)
  "Execute a RETIRE command for TARGET, with *command-delay* to simulate
   a slow central command round-trip.  No-op when TARGET is empty."
  (when (plusp (length target))
    (sleep *command-delay*)
    (let ((r (replicant-with-designation target)))
      (cond ((protected-p r)
             (reply-to sid
                       (format nil "~A retirement order failed -- system error code RACH32fa00a139"
                               target)))
            ((retire-replicant target)
             (push-radar-event :retire target ip :agent sid)
             (broadcast)
             (reply-to sid
                       (format nil "~A retired" target)))
            (t (reply-to sid
                         (format nil "~A was already retired by another unit -- command void"
                                 target)))))))
