;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; EVENTS.LISP --- CQRS event hub using datastar-cl.registry
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-simple-demo)

;;; Implements the read/write split:
;;;   GET /updates     - long-lived SSE; body blocks on keep-alive loop
;;;   POST /glitch     - command; broadcasts a glitch event to all subscribers
;;;   POST /glitch-me  - command; unicasts a glitch event to one subscriber by sid
;;;
;;; BROADCAST vs UNICAST
;;;
;;; *CLIENTS* is a keyed registry: NOTIFY-SUBSCRIBERS broadcasts to all
;;; registered generators; NOTIFY-SUBSCRIBER targets one generator by its
;;; registered key (the session id). Both the generator list and the key->gen
;;; map are updated atomically inside the registry's single lock.
;;;
;;; Multiple registries are first-class: create one per topic, room, or audience
;;; with MAKE-KEYED-SSE-REGISTRY. A generator can be registered in several at once.

;;; Connection registry -- keyed for broadcast + unicast-by-sid

(defvar *clients* (reg:make-keyed-sse-registry "glitch"))

;;; Events are plists: (:glitch t), (:glitch nil), or (:manifesto <string>)

(defun apply-event (gen event)
  (let ((kind (first event))
        (val  (second event)))
    (ecase kind
      (:glitch    (datastar-cl:patch-signals gen (list "glitch.active" val)))
      (:manifesto (datastar-cl:patch-elements gen val
                                              :selector "#manifesto"
                                              :mode :inner)))))

;;; Broadcast: NOTIFY-SUBSCRIBERS fans out to all registered generators.
;;; Dead generators are caught internally and auto-unregistered.

(defun notify-all (event)
  (reg:notify-subscribers *clients*
                          (lambda (gen) (apply-event gen event))))

;;; Unicast: NOTIFY-SUBSCRIBER dispatches via CALL-WITH-GENERATOR on the
;;; generator registered under KEY -- correct on all backends including Woo.

(defun notify-one (sid event)
  (reg:notify-subscriber *clients* sid (lambda (g) (apply-event g event))))

;;; Routes

(defparameter *qotd*
  "Computational processes are abstract beings that inhabit computers. \
As they evolve, processes manipulate other abstract things called data. \
The evolution of a process is directed by a pattern of rules called a program. \
People create programs to direct processes. \
In effect, we conjure the spirits of the computer with our spells.")

;;; GET /updates - long-lived SSE
;;;
;;; on-connect: stream QOTD + register the generator
;;; body:       manual keep-alive loop -- each (keep-sse-alive gen) call both
;;;             heartbeats the proxy and probes for disconnect: a stream-error
;;;             unwinds the body naturally, triggering on-disconnect.
;;;
;;;             This is the NON-EMPTY BODY pattern: the body owns the connection
;;;             lifetime and heartbeats itself explicitly. The equivalent
;;;             empty-body form (see the calispel glitch example) is:
;;;               (with-sse (gen req :keep-alive 30 ...) )   ; empty body
;;;             where WITH-SSE injects the same heartbeat loop automatically.
;;;             Both are correct. Pick the manual loop when the body does other
;;;             work too; use the empty-body :KEEP-ALIVE form for pure-push CQRS
;;;             where ALL writes come from an external broadcaster.
;;; on-disconnect: unregister the generator

(hunchentoot:define-easy-handler (updates-ht :uri "/updates") ()
  (let ((sid (read-sid hunchentoot:*request*)))
    (datastar-cl:with-sse (gen hunchentoot:*request*
                               :on-connect    (make-updates-on-connect sid)
                               :on-disconnect #'updates-on-disconnect)
      (loop
        (sleep datastar-cl:*default-keep-alive-interval*)
        (datastar-cl:keep-sse-alive gen)))))

;;; POST /glitch - broadcast

(defun fire-glitch (sid)
  (notify-all (list :manifesto (format nil "GLITCH by ~A" (or sid "anonymous"))))
  (notify-all '(:glitch t))
  (bt:make-thread
   (lambda ()
     (sleep 0.45)
     (notify-all '(:glitch nil)))
   :name "glitch-reset"))

;;; POST /glitch-me - unicast

(defun fire-glitch-me (sid)
  (when sid
    (notify-one sid (list :manifesto (format nil "GLITCH ME by ~A" sid)))
    (notify-one sid '(:glitch t))
    (bt:make-thread
     (lambda ()
       (sleep 0.45)
       (notify-one sid '(:glitch nil)))
     :name "glitch-me-reset")))

;;; Handlers

(hunchentoot:define-easy-handler (glitch-ht :uri "/glitch") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (handle-glitch-command hunchentoot:*request*)
  "")

(hunchentoot:define-easy-handler (glitch-me-ht :uri "/glitch-me") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (handle-glitch-me-command hunchentoot:*request*)
  "")

;;; Shared helpers

(defun read-sid (request-or-env)
  (datastar-cl:read-signal request-or-env "sid"))

(defun make-updates-on-connect (sid)
  (lambda (g)
    (transmit-message *qotd* g "#manifesto" :interval 0.005)
    (reg:register *clients* g :key sid)
    (datastar-cl:console-log
     g (format nil "CQRS subscribed: sid=~A" (or sid "(none)")))))

(defun updates-on-disconnect (g)
  (reg:unregister *clients* g))

(defun handle-glitch-command (request-or-env)
  (when (datastar-cl:datastar-request-p request-or-env)
    (fire-glitch (read-sid request-or-env))))

(defun handle-glitch-me-command (request-or-env)
  (when (datastar-cl:datastar-request-p request-or-env)
    (fire-glitch-me (read-sid request-or-env))))

(defun transmit-message (message gen selector &key (interval 0.01))
  (let ((acc (make-array (length message) :element-type 'character
                                          :fill-pointer 0)))
    (loop for c across message
          do (vector-push c acc)
             (datastar-cl:patch-elements gen acc
                                         :selector selector
                                         :mode :inner)
             (sleep interval))))
