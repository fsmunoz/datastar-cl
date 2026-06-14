;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; FAT-MORPH.LISP --- Fat morph pattern demo
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Instead of fine-grained element selection, sends the entire content area's
;;; HTML every tick. Datastar's idiomorph merge applies only actual differences
;;; to the DOM, preserving unchanged elements. Also resilient to SSE
;;; interruptions: after reconnect the next event contains the complete state.

(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot))


(defpackage #:fat-morph
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl) (:ht :hunchentoot)))
(in-package #:fat-morph)

(defun read-file-to-string (pathname)
  (with-open-file (stream pathname :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defparameter *updates* 0)
(defparameter *clicks* 0)

(defun render-content ()
  "Full HTML of the morphed content region -- the 'fat' payload."
  (incf *updates*)
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (sp:with-html-string
      (:div :id "content" :style "border: 1px dotted #faa0a0; padding: 1rem;"
             (:button :|data-on:click| (d*:sse-post "/click") "Click me")            
            (:p "Time: " (:strong (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
            (:p "Updates counter: " (:strong *updates*))
            (:p "Button clicks: " (:strong *clicks*))
            (:hr)
            (:p "Below is a paragraph with " (:code "data-ignore-morph")
                ".  It is never touched by morphing, so you can edit it freely.")
            (:div :data-ignore-morph "true"
                  (:p :contenteditable "true"
                      :style "border:1px solid #ccc;padding:0.25em 0.5em;"
                      "Edit me -- morphing will not reset my content!"))))))

(ht:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url))
            ;;(:style (:raw (read-file-to-string "style.css"))))
            )
      (:body :data-init (d*:sse-get "/sse")
             (:h1 "Fat Morph")
             (:p "The entire #content div (inside the red border) is replaced every second.  "
                 "Only actual changes are applied to the DOM.")
             (:p "The time and counter change each tick: everything else morphs without DOM churn. The button clicks will be updated as part of the backend update loop (1s).")
             (:raw (render-content))))))

(ht:define-easy-handler (sse :uri "/sse") ()
  (d*:with-sse (gen ht:*request*)
    (loop
      (d*:patch-elements gen (render-content) :selector "#content" :mode :outer)
      (sleep 1))))

(ht:define-easy-handler (click :uri "/click") ()
  (d*:with-sse (gen ht:*request*)
    (incf *clicks*)))

(ht:start (make-instance 'ht:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
