;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO-CLACK.LISP --- Minimal Clack example
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; NOTE: A blocking ~with-sse~ body (e.g. loop ... sleep) blocks the Clack
;;; worker thread for the duration of the connection _if using Woo_ -- by
;;; design. Clack+Woo is suitable for the one-shot ~/hi~ endpoint out-of-the box
;;; but for long-lived streaming check the documentation: blocking the event
;;; loop will block Woo. This example uses Hunchentoo as the backend server for
;;; Clack, but Woo is also tested (changeable in :backend below)

(ql:quickload '(:clack :spinneret :datastar-cl/clack))

(defpackage #:clock-clack
  (:use #:cl)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl)))
(in-package #:clock-clack)

(defun render-index ()
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
     (:body :data-signals "{time: '--:--:--', ticks: 0, name: '', message: ''}"
            :data-init (d*:sse-get "/sse")
            (:h1 "Datastar-CL SSE Demo (Clack)")
            (:div :id "clock" :data-text "$time")
            (:div "Ticks: " (:strong :data-text "$ticks"))
            (:p (:input :placeholder "Your name" :data-bind "name")
                (:button :|data-on:click| (d*:sse-get "/hi") "Say hi")
                (:p :data-text "$message"))))))

(defun sse-handler (env)
  (lambda (responder)
    (let ((tick 0))
      (d*:with-sse (gen (env responder))
        (loop
          (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
            (d*:patch-signals gen
                              (list "time"  (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)
                                    "ticks" (incf tick))))
          (sleep 1))))))

(defun hi-handler (env)
  (lambda (responder)
    (let ((name (d*:read-signal env "name")))
      (d*:with-sse (gen (env responder))
        (d*:patch-signals gen
                          (list "message"
                                (format nil "Hello, ~A!"
                                        (if (zerop (length name))
                                            "stranger"
                                          name))))))))

(defun app (env)
  (let ((path (getf env :path-info)))
    (cond
      ((string= path "/")    `(200 (:content-type "text/html; charset=utf-8")
                                   (,(render-index))))
      ((string= path "/sse") (sse-handler env))
      ((string= path "/hi")  (hi-handler env))
      (t                     '(404 (:content-type "text/plain") ("Not Found"))))))

(clack:clackup #'app :server :hunchentoot :port 8989)
(format t "~&Server started on http://localhost:8989~%")
