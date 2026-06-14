;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HANDLERS-HUNCHENTOOT.LISP --- Hunchentoot route registrations for the glitch demo
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-demo)

(hunchentoot:define-easy-handler (root-ht :uri "/") ()
  (hunchentoot:redirect "/index"))

(hunchentoot:define-easy-handler (index-ht :uri "/index") ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-index))

;;; GET /updates - long-lived SSE (pure-push CQRS, empty-body keep-alive)
;;;
;;; on-connect:    stream QOTD, subscribe (starts forwarder thread)
;;; body:          empty -- WITH-SSE injects the own-thread heartbeat loop
;;; on-disconnect: unsubscribe (sends nil sentinel to forwarder)

(hunchentoot:define-easy-handler (updates-ht :uri "/updates") ()
  (let ((sid (read-sid hunchentoot:*request*)))
    (datastar-cl:with-sse (gen hunchentoot:*request*
                           :on-connect    (make-updates-on-connect sid)
                           :on-disconnect #'updates-on-disconnect
                           :keep-alive    30))))

(hunchentoot:define-easy-handler (glitch-ht :uri "/glitch") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (handle-glitch-command hunchentoot:*request*)
  "")

(hunchentoot:define-easy-handler (glitch-me-ht :uri "/glitch-me") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (handle-glitch-me-command hunchentoot:*request*)
  "")
