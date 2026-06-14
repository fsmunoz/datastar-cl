;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO.LISP --- Minimal Hunchentoot example
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot))

(defpackage #:clock
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl) (:ht :hunchentoot)))
(in-package #:clock)

(ht:define-easy-handler (index :uri "/") ()
  (setf (ht:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
      (:body :data-signals (d*:init-signals :time "--:--:--" :ticks 0
                                          :name "" :greeting "Hello" :message "")
             :data-init (d*:sse-get "/sse")
             (:h1 "Datastar-CL SSE Demo")
             (:div :id "clock" :data-text "$time")
             (:div "Ticks: " (:strong :data-text "$ticks"))
             (:p (:select :data-bind "greeting"
                         (:option :value "Hello" "Hello")
                         (:option :value "Hi" "Hi")
                         (:option :value "Hey" "Hey")
                         (:option :value "Greetings" "Greetings"))
                " "
                (:input :placeholder "Your name" :data-bind "name")
                " "
                (:button :|data-on:click| (d*:sse-get "/hi") "Say hi")
                (:p :data-text "$message"))))))

(ht:define-easy-handler (sse-handler :uri "/sse") ()
  (let ((tick 0))
    (d*:with-sse (gen ht:*request*)
      (loop
        (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
          (d*:patch-signals gen
                            (list "time"  (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)
                                  "ticks" (incf tick))))
        (sleep 1)))))

;;; Signal reading progression (simplest ---> most ergonomic):
;;;
;;;   1) READ-SIGNAL: read one signal by key string
;;;        (d*:read-signal ht:*request* "name")
;;;
;;;   2) READ-SIGNALS: parse all signals into a hash-table (once)
;;;        (let ((signals (d*:read-signals ht:*request*)))
;;;          (gethash "name" signals))
;;;
;;;   3) WITH-SIGNALS: bind N variables in one call (below)
;;;        (d*:with-signals ((name "name" "default") ...) req body)
;;;
;;; Below we use WITH-SIGNALS to read greeting + name in one parse, with default
;;; values when either signal is absent.
(ht:define-easy-handler (hi :uri "/hi") ()
  (d*:with-signals ((name     "name"     "stranger")
                    (greeting "greeting" "Hello"))
      ht:*request*
    (d*:with-sse (gen ht:*request*)
      (d*:patch-signals gen
                        (list "message"
                              (format nil "~A, ~A!" greeting name))))))

(ht:start (make-instance 'ht:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
