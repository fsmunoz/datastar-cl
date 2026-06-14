;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PUSH.LISP --- CQRS push layer: subscriber registry and broadcast
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; SUBSCRIBER REGISTRY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *clients* is a keyed registry: broadcast (NOTIFY-SUBSCRIBERS) fans out to
;;; all connected generators; unicast (NOTIFY-SUBSCRIBER + sid key) targets one.
;;; Both the generator list and the key->generator map are protected by a single
;;; lock inside the registry.

(defvar *clients* (reg:make-keyed-sse-registry "rep-detect"))

;;; PUSH PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push-registry (gen)
  "Push the current registry fragment to GEN."
  (d*:patch-elements gen (render-registry) :selector "#registry"))

(defun push-radar (gen)
  "Push the current radar SVG to GEN.
   :namespace :svg tells Datastar to morph/create elements in the SVG XML
   namespace -- without this, morphed children land in the HTML namespace and
   the browser treats them as unknown elements."
  (d*:patch-elements gen (render-radar) :selector "#radar" :namespace :svg))

(defun push-action-log (gen)
  "Push the current action log fragment to GEN."
  (d*:patch-elements gen (render-action-log) :selector "#action-log"))

(defun push-server-stats (gen)
  "Push connected viewer count to GEN."
  (d*:patch-elements gen
                     (sp:with-html-string
                       (:span :id "server-stats"
                              (format nil "viewers ~D" (reg:registry-count *clients*))))
                     :selector "#server-stats"))

;;; BROADCAST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun broadcast ()
  "Push registry + radar + action log + server stats to all current subscribers.
   Writes are routed via CALL-WITH-GENERATOR so each write executes on the
   generator's owning thread -- correct on Hunchentoot (direct funcall) and
   required on Woo (enqueues on the connection's evloop via WOO:SCHEDULE).
   CLIENT-DISCONNECTED on any generator causes it to be closed + unregistered."
  (reg:notify-subscribers *clients*
                          (lambda (gen)
                            (push-registry gen)
                            (push-radar gen)
                            (push-action-log gen)
                            (push-server-stats gen))))

(defun reply-to (sid &optional (msg ""))
  "Send a signal patch with :msg MSG to the generator registered under SID.
   CLIENT-DISCONNECTED causes automatic close + unregister."
  (reg:notify-subscriber *clients* sid
                         (lambda (g) (d*:patch-signals g (list :msg msg)))))
