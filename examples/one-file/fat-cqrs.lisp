;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; FAT-CQRS.LISP --- Fat morph with CQRS notification pattern
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates two patterns:
;;;
;;; 1) CQRS: commands via POST (button), received through the SSE stream.
;;; 2) Fat morphing: every update patches the entire content area; no
;;;    fine-grained element targeting. The timer loop and button clicks both
;;;    go through a simple subscription model.
;;;
;;; Run with: sbcl --load fat-cqrs.lisp

;;; Single-file loading harness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot :datastar-cl/registry))

(defpackage #:fat-cqrs
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp  :spinneret)
                    (:d*  :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:ht  :hunchentoot)))

(in-package #:fat-cqrs)

;;; Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *clicks* 0)
(defvar *clients* (reg:make-sse-registry "fat-cqrs"))

;;; Broadcast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun broadcast ()
  (let ((content (render-content)))
    (reg:notify-subscribers
     *clients*
     (lambda (g) (d*:patch-elements g content :selector "#content")))))
;;: Content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun render-content ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (sp:with-html-string
        (:div
         :id "content"
         (:button :|data-on:click| (d*:sse-post "/click") "Click me")         
         (:p "Time: " (:strong (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
         (:p "Button clicks: " (:strong *clicks*))
         (:hr)
         (:p "Below is a paragraph with " (:code "data-ignore-morph")
             ". It is never touched by morphing, so you can edit it freely.")
         (:div :data-ignore-morph "true"
               (:p :contenteditable "true"
                   :style "background-color: #ddd; padding:0.5em;"
                   "Edit me: morphing will not reset my content!"))))))

;;; Web server setup
(ht:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
      (:body :data-init (d*:sse-get "/sse")
             (:h1 "Fat CQRS")
             (:p "The time updates every 1s via the SSE loop.  "
                 "Click the button: the count pushes instantly to all clients.")
            (:raw (render-content))))))

(ht:define-easy-handler (sse :uri "/sse") ()
  (d*:with-sse (gen hunchentoot:*request*
                :on-connect    (lambda (g) (reg:register *clients* g))
                :on-disconnect (lambda (g) (reg:unregister *clients* g)))
    (loop
      ;; Note that we're ommitting :selector "#content" ; in "outer" mode (the
      ;; default), Datastar morphs everything (we could pass an entire HTML
      ;; page).
      (d*:patch-elements gen (render-content))
      (sleep 1))))

(ht:define-easy-handler (click :uri "/click") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (incf *clicks*)
  (broadcast)
  "")

(ht:start (make-instance 'ht:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
