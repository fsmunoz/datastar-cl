;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; FAT-CQRS-SNOOZE.LISP --- Fat morph with CQRS and Snooze routing
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates four patterns:
;;;
;;; 1) CQRS: commands via POST (button), received through the SSE stream.
;;; 2) Fat morphing: every update patches the entire content area; no
;;;    fine-grained element targeting.
;;; 3) Snooze helpers for datastar-aware routing.
;;; 4) WITH-SIGNALS (Snooze variant): read multiple client signals in one
;;;    call inside a command handler (/set-greeting).
;;;
;;; ... using patch-elements and patch-signals. Some additional non-essentials
;;; are included (CSS loading)
;;;
;;; Run with: sbcl --load fat-cqrs-snooze.lisp

;;; SYSTEM DEFINITION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload '(:spinneret :snooze :datastar-cl/snooze :datastar-cl/registry))

(defpackage #:fat-cqrs-snooze
  (:use #:cl #:snooze)
  (:local-nicknames (:s*  :datastar-cl/snooze)
                    (:d*  :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:sp  :spinneret)
                    (:a   :alexandria))
  (:export #:main))
(in-package #:fat-cqrs-snooze)

;;; PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *clicks* 0)
(defparameter *glow* nil)
(defparameter *greeting* "Hello, World!")  ; updated by /set-greeting
(defvar *clients* (reg:make-sse-registry "fat-cqrs-snooze"))
(defparameter *css* (a:read-file-into-string
                     (merge-pathnames "style.css" *load-truename*)))

;;; BROADCAST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun broadcast (fn)
  (reg:notify-subscribers *clients* fn))

;;; HTML CONTENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun render-content ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (sp:with-html-string
      (:div :id "content" :|data-class| "{glow: $uiGlow}"
            (:p :|data-text| "$uiGlow ? 'Glow is ON' : 'Glow is OFF'")
            (:p "Time: " (:strong (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))
            (:p "Button clicks: " (:strong *clicks*))
            (:p "Greeting: " (:strong *greeting*))
            (:hr)
            (:p "Below is a paragraph with " (:code "data-ignore-morph")
                ". It is never touched by morphing, so you can edit it freely.")
            (:div :data-ignore-morph "true"
                  (:p :contenteditable "true"
                      :style "border:1px solid #ccc;padding:0.25em 0.5em;"
                      "Edit me -- morphing will not reset my content!"))))))

;;; ROUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Index
(defresource index (verb content-type))

(defroute index (:get :text/html)
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url))
            (:style (:raw *css*)))
     (:body :data-signals (d*:init-signals :ui-glow nil
                                          :salutation "Hello"
                                          :recipient "World")
            :data-init (d*:sse-get "/sse")
            (:h1 (:a :href "https://lambda-combine.net" "ΛↃ lambda combine"))
            (:h2 "Datastar CQRS and Fat Morphing")
            (:strong "With Common Lisp and Snooze")

            (:p "The time updates every 1s via the SSE loop.")
            (:p "Click the buttons: updates are pushed instantly to ALL clients via CQRS notification.")
            (:hr)
            (:button :|data-on:click| (d*:sse-post "/click") "INC COUNTER")
            (:button :|data-on:click| (d*:sse-post "/toggle-glow") "TOGGLE GLOW")
            ;; /set-greeting reads TWO signals via S*:WITH-SIGNALS
            " | "
            (:span (:select :data-bind "salutation"
                         (:option :value "Hello" "Hello")
                         (:option :value "Hi" "Hi")
                         (:option :value "Hey" "Hey")
                         (:option :value "Greetings" "Greetings"))
                " "
                (:input :placeholder "Recipient" :data-bind "recipient")
                " "
                (:button :|data-on:click| (d*:sse-post "/set-greeting") "SET GREETING"))
            (:raw (render-content))))))

(setf snooze:*home-resource* #'index)

;; SSE connection, all updates come through it
(s*:defresource sse (verb content-type &key datastar))

(s*:defroute sse (:get :text/html &key datastar)
  (s*:with-sse (gen
                :on-connect (lambda (g)
                              (d*:patch-signals g (list :ui-glow *glow*))
                              (reg:register *clients* g))
                :on-disconnect (lambda (g) (reg:unregister *clients* g)))
    (loop
      (d*:patch-elements gen (render-content) :selector "#content")
      (sleep 1))))

;; Command endpoint for "Increment Counter"
(s*:defresource click (verb content-type &key datastar))

(s*:defroute click (:post :application/json &key datastar)
  (incf *clicks*)
  (broadcast (lambda (g)
               (d*:patch-elements g (render-content) :selector "#content"))))


;; Command endpoint for "Toggle glow"
(s*:defresource toggle-glow (verb content-type &key datastar))

(s*:defroute toggle-glow (:post :application/json &key datastar)
  (setf *glow* (not *glow*))
  (broadcast (lambda (g)
               (d*:patch-signals g (list :ui-glow *glow*)))))

;; Command endpoint for "Set Greeting" -- reads TWO signals with S*:WITH-SIGNALS.
;;
;; This showcases the Snooze variant of WITH-SIGNALS: no request argument is
;; needed because the backend (Hunchentoot or Clack) is resolved automatically
;; from SNOOZE:*BACKEND* -- the same convention used by S*:READ-SIGNALS and
;; S*:WITH-SSE throughout this package.
;;
;; Contrast with the core macro used in demo.lisp:
;;   (d*:with-signals ((x "x") (y "y")) hunchentoot:*request* body)
;; Here the request object is implicit, not passed explicitly.
(s*:defresource set-greeting (verb content-type &key datastar))

(s*:defroute set-greeting (:post :application/json &key datastar)
  (s*:with-signals ((salutation "salutation" "Hello")
                    (recipient  "recipient"  "World"))
    (setf *greeting* (format nil "~A, ~A!" salutation recipient))
    (broadcast (lambda (g)
                 (d*:patch-elements g (render-content) :selector "#content")))))


;;; WEB SERVER STARTUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun main ()
  (push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
  (hunchentoot:start (make-instance 'd*:tcp-nodelay-easy-acceptor :port 8989))
  (format t "~&Server started on http://localhost:8989~%")
  ;; Block so the binary doesn't exit immediately
  (sleep most-positive-fixnum))
(main)
