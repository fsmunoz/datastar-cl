;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO-SIGNALS.LISP --- Signal handling example
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot))

(defpackage #:clock
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl)))
(in-package #:clock)

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
     (:body :data-signals "{time: '--:--:--', ticks: 0}"
            :data-init (d*:sse-get "/sse")
            (:h1 "Datastar-CL SSE Demo")
            (:div :id "clock" :data-text "$time")
            (:p "Ticks: " (:strong :data-text "$ticks"))))))

(hunchentoot:define-easy-handler (sse-handler :uri "/sse") ()
  (let ((tick 0))
    (d*:with-sse (gen hunchentoot:*request*)
      (loop
        (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
          (d*:patch-signals gen
                            (list "time"  (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)
                                  "ticks" (incf tick))))
        (sleep 1)))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
