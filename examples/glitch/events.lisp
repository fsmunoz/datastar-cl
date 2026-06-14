;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; EVENTS.LISP --- CQRS event hub (Calispel channels + forwarder threads)
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-demo)

;;; Implements the read/write split:
;;;   GET /updates  - long-lived SSE; body blocks on keep-alive loop
;;;   POST /glitch  - command; broadcasts a glitch event to all
;;;                   live subscribers via Calispel channels
;;;
;;; Each subscriber has a Calispel bounded channel and a forwarder thread
;;; that reads from it and writes directly to the SSE generator.
;;; App-level state: *subscribers* under *subs-lock*.

;;; Server/env state (NIL until the Hunchentoot server sets it)

(defvar *taskmaster* nil)

(defun read-env-int (name default)
  "Return the integer value of env var NAME, or DEFAULT if absent or unparseable."
  (let ((raw (uiop:getenv name)))
    (if raw
        (handler-case (parse-integer raw)
          (error () default))
        default)))

;;; Subscriber registry

(defstruct (subscriber (:conc-name sub-))
  gen      ; the SSE generator
  sid      ; per-tab UUID string (from Datastar signal), or NIL
  channel  ; Calispel bounded channel - receives events from broadcast
  thread)  ; forwarder thread that reads channel and writes to gen

(defvar *subscribers* nil
  "List of active SUBSCRIBER structs.")

(defvar *subs-lock* (bt:make-lock "cqrs-subs")
  "Protects *subscribers* list.")

;;; Events are plists: (:glitch t), (:glitch nil), or (:manifesto <string>)

(defvar *stats-fn* nil
  "Nullary function returning server-stats HTML string, or NIL.
   Set by the active transport at server startup; NIL on Woo (stats omitted).")

(defun server-stats-html ()
  "Return a span with current server stats, or NIL if no stats provider is set."
  (when *stats-fn* (funcall *stats-fn*)))

(defun apply-event (gen event)
  "Translate an event plist into an SSE call on GEN."
  (let ((kind (first event))
        (val  (second event)))
    (ecase kind
      (:glitch       (datastar-cl:patch-signals gen (list "glitch.active" val)))
      (:manifesto    (datastar-cl:patch-elements gen val
                                                :selector "#manifesto"
                                                :mode :inner))
      (:server-stats (datastar-cl:patch-elements gen val
                                                :selector "#server-stats")))))

;;; Forwarder: one per subscriber, blocks on Calispel channel,
;;; writes events directly to the generator.
;;;
;;; The generator's internal lock (held by send-event) serialises concurrent
;;; writes; no additional locking needed here.

(defun forwarder-loop (sub)
  (loop
    (let ((event (calispel:? (sub-channel sub))))
      (unless event (return))          ; NIL sentinel = shutdown
      (handler-case
          (datastar-cl:call-with-generator (sub-gen sub)
            (lambda (g) (apply-event g event)))
        (error ()
          (unsubscribe sub)            ; remove zombie subscriber immediately
          (return))))))                ; generator closed; stop forwarding

(defun subscribe (gen sid)
  "Register GEN as an active subscriber with SID (per-tab UUID or NIL).
   Returns the new SUBSCRIBER struct."
  (let* ((ch (make-instance 'calispel:channel
                            :buffer (make-instance 'jpl-queues:bounded-fifo-queue
                                                   :capacity 64)))
         (s (make-subscriber :gen gen :sid sid :channel ch)))
    (setf (sub-thread s)
          (bt:make-thread (lambda () (forwarder-loop s))
                          :name "cqrs-forwarder"))
    (bt:with-lock-held (*subs-lock*)
      (push s *subscribers*))
    s))

(defun unsubscribe (s)
  "Remove subscriber S and shut down its forwarder thread."
  (bt:with-lock-held (*subs-lock*)
    (setf *subscribers* (remove s *subscribers* :test #'eq)))
  (calispel:! (sub-channel s) nil 0))

(defun find-sub (gen)
  "Return the subscriber whose generator is GEN, or NIL."
  (bt:with-lock-held (*subs-lock*)
    (find gen *subscribers* :key #'sub-gen :test #'eq)))

(defun find-sub-by-sid (sid)
  "Return the subscriber whose SID matches, or NIL."
  (when sid
    (bt:with-lock-held (*subs-lock*)
      (find sid *subscribers* :key #'sub-sid :test #'equal))))

(defun unicast (sub event)
  "Send EVENT to just one subscriber's Calispel channel (zero-timeout, drop if full)."
  (when sub
    (calispel:! (sub-channel sub) event 0)))

(defun broadcast (event)
  "Send EVENT to every live subscriber's Calispel channel.
   Uses a zero timeout so a full-buffer subscriber is dropped rather than
   blocking the POST handler."
  (bt:with-lock-held (*subs-lock*)
    (dolist (s *subscribers*)
      (calispel:! (sub-channel s) event 0))))

;;; Routes

;;; QOTD - typed out character-by-character on each fresh connect.

(defparameter *qotd*
  "Computational processes are abstract beings that inhabit computers. \
As they evolve, processes manipulate other abstract things called data. \
The evolution of a process is directed by a pattern of rules called a program. \
People create programs to direct processes. \
In effect, we conjure the spirits of the computer with our spells.")

;;; GET /updates - long-lived SSE (pure-push CQRS, empty-body keep-alive)
;;;
;;; on-connect:    stream QOTD, subscribe (starts forwarder thread)
;;; body:          empty -- WITH-SSE injects the own-thread heartbeat loop;
;;;                the forwarder thread does all the writing to the generator.
;;;                When the client disconnects, the next heartbeat write fails
;;;                (stream-error), unwinding the body and firing on-disconnect.
;;; on-disconnect: unsubscribe (sends nil sentinel to forwarder)

;;; POST /glitch - broadcast

(defun fire-glitch (sid)
  "Broadcast a glitch pulse to all subscribers."
  (broadcast (list :manifesto (format nil "GLITCH by ~A" (or sid "anonymous"))))
  (broadcast '(:glitch t))
  (let ((stats (server-stats-html)))
    (when stats (broadcast (list :server-stats stats))))
  (bt:make-thread
   (lambda ()
     (sleep 0.45)
     (broadcast '(:glitch nil)))
   :name "glitch-reset"))

(defun fire-glitch-me (sid)
  "Pulse glitch on just the subscriber whose SID matches."
  (let ((sub (find-sub-by-sid sid)))
    (when sub
      (unicast sub (list :manifesto (format nil "GLITCH ME by ~A" sid)))
      (unicast sub '(:glitch t))
      (bt:make-thread
       (lambda ()
         (sleep 0.45)
         (unicast sub '(:glitch nil)))
       :name "glitch-me-reset"))))

;;; Shared helpers

(defun read-sid (request-or-env)
  "Return the SID from a Datastar request's signals, or NIL if absent."
  (datastar-cl:read-signal request-or-env "sid"))

(defun make-updates-on-connect (sid)
  "Build the :on-connect callback for /updates, closing over SID."
  (lambda (g)
    (transmit-message *qotd* g "#manifesto" :interval 0.005)
    (let ((stats (server-stats-html)))
      (when stats
        (datastar-cl:patch-elements g stats :selector "#server-stats")))
    (subscribe g sid)
    (datastar-cl:console-log
     g (format nil "CQRS subscribed: sid=~A" (or sid "(none)")))))

(defun updates-on-disconnect (g)
  "The :on-disconnect callback for /updates."
  (let ((s (find-sub g)))
    (when s (unsubscribe s))))

(defun handle-glitch-command (request-or-env)
  "If REQUEST-OR-ENV is a Datastar request, fire a broadcast glitch."
  (when (datastar-cl:datastar-request-p request-or-env)
    (fire-glitch (read-sid request-or-env))))

(defun handle-glitch-me-command (request-or-env)
  "If REQUEST-OR-ENV is a Datastar request, fire a unicast glitch."
  (when (datastar-cl:datastar-request-p request-or-env)
    (fire-glitch-me (read-sid request-or-env))))

(defun transmit-message (message gen selector &key (interval 0.01))
  "Type out MESSAGE character-by-character into the DOM element at SELECTOR."
  (let ((acc (make-array (length message) :element-type 'character
                                          :fill-pointer 0)))
    (loop for c across message
          do (vector-push c acc)
             (datastar-cl:patch-elements gen acc
                                         :selector selector
                                         :mode :inner)
             (sleep interval))))
