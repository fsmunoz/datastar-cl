;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO-SCRIPT.LISP --- execute-script / console-log / redirect

;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; The only core SSE method not shown in the other demos. Adds buttons
;;; that trigger one-shot SSE handlers, each calling a different script
;;; variant: console-log, execute-script with auto-remove, and redirect.

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
     (:body :data-init (d*:sse-get "/sse")
            (:h1 "Datastar-CL Script Demo")
            (:div :id "clock" "Waiting...")
            (:button :|data-on:click| (d*:sse-get "/log") "Log")
            (:button :|data-on:click| (d*:sse-get "/alert") "Alert")
            (:button :|data-on:click| (d*:sse-get "/go") "Redirect")))))

(hunchentoot:define-easy-handler (sse-handler :uri "/sse") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (loop
      (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
        (d*:patch-elements gen
                           (format nil "<span>~2,'0d:~2,'0d:~2,'0d</span>" h m s)
                           :selector "#clock"
                           :mode :inner))
      (sleep 1))))

(hunchentoot:define-easy-handler (log-handler :uri "/log") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (d*:console-log gen "Log button clicked at server")))

(hunchentoot:define-easy-handler (alert-handler :uri "/alert") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (d*:execute-script gen "alert('Hello from datastar-cl!')" :auto-remove t)))

(hunchentoot:define-easy-handler (go-handler :uri "/go") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (d*:redirect gen "/")))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
