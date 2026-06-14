;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HANDLERS-CLACK.LISP --- Clack + Woo route handlers for the glitch demo
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-demo)

(defun root-clack (env)
  (declare (ignore env))
  '(302 (:location "/index") ("")))

(defun index-clack (env)
  (declare (ignore env))
  (list 200 '(:content-type "text/html") (list (render-index))))

;;; GET /updates - long-lived SSE (pure-push CQRS)
;;;
;;; Returns a two-arg responder lambda as required by the Clack async protocol.
;;; WITH-SSE's :keep-alive loop blocks the request thread (clack-handler-hunchentoot
;;; is thread-per-request).  The forwarder thread writes via CALL-WITH-GENERATOR,
;;; which on Woo routes through woo:schedule onto the correct evloop thread.

(defun sse-clack (env)
  (let ((sid (read-sid env)))
    (lambda (responder)
      (datastar-cl:with-sse (gen (env responder)
                             :on-connect    (make-updates-on-connect sid)
                             :on-disconnect #'updates-on-disconnect
                             :keep-alive    30)))))

(defun glitch-clack (env)
  (handle-glitch-command env)
  '(204 nil ("")))

(defun glitch-me-clack (env)
  (handle-glitch-me-command env)
  '(204 nil ("")))

(defun router (env)
  (let ((path (getf env :path-info)))
    (cond
      ((string= path "/")          (root-clack env))
      ((string= path "/index")     (index-clack env))
      ((string= path "/updates")   (sse-clack env))
      ((string= path "/glitch")    (glitch-clack env))
      ((string= path "/glitch-me") (glitch-me-clack env))
      (t                           '(404 (:content-type "text/plain") ("Not found"))))))
