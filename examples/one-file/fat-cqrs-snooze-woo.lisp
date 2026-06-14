;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; FAT-CQRS-SNOOZE-WOO.LISP --- Fat CQRS + Snooze routing on Clack + Woo
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Companion to fat-cqrs-snooze.lisp (Hunchentoot version).
;;;
;;; This file demonstrates that DATASTAR-CL/SNOOZE already provides a fully
;;; backend-agnostic routing layer.  Compare the two files side-by-side:
;;;
;;;   Identical sections:
;;;     - Package definition
;;;     - All state variables (*clicks*, *glow*, *greeting*, *clients*, ...)
;;;     - broadcast (thin wrapper over reg:notify-subscribers)
;;;     - render-content
;;;     - defresource and defroute for: index, click, toggle-glow, set-greeting
;;;       (s*:with-signals in set-greeting is unchanged -- no request arg needed)
;;;
;;;   Sections that differ and WHY:
;;;
;;;   1) SYSTEM DEFINITION -- Woo replaces Hunchentoot.
;;;
;;;   2) SSE ROUTE -- Pattern 2 (empty body + :KEEP-ALIVE) instead of Pattern 3
;;;      (blocking loop in route body).  On Woo a blocking body stalls the
;;;      event-loop worker.  With S*:WITH-SSE and an empty body, SERVE-SSE
;;;      (datastar-cl/woo specialisation) registers a libev heartbeat timer and
;;;      returns immediately, freeing the worker.  No thread is parked per
;;;      connection.  The S*:WITH-SSE call itself is structurally identical to
;;;      the HT version; only the body and :KEEP-ALIVE differ.
;;;
;;;   3) BACKGROUND TICKER -- a shared bt:make-thread drives 1-second content
;;;      pushes.  In the HT file the (loop (sleep 1) ...) lives inside the SSE
;;;      route body; that blocking loop is unsafe for Woo workers, so we extract
;;;      it into a dedicated thread.
;;;
;;;   4) STARTUP -- snooze:make-clack-app + clack:clackup :server :woo
;;;      instead of snooze:make-hunchentoot-app + hunchentoot:start.
;;;
;;; Run with: sbcl --load fat-cqrs-snooze-woo.lisp
;;;
;;; Prerequisites: clone the patched Woo fork onto your ASDF source registry
;;;   git clone -b cqrs git@github.com:fsmunoz/woo.git
;;; Place it in a directory ASDF searches before Quicklisp (e.g. add an entry
;;; in ~/.config/common-lisp/source-registry.conf.d/).  Then
;;; (ql:quickload :woo) resolves the local clone automatically.

;;; SYSTEM DEFINITION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (differs: Woo stack instead of Hunchentoot)

(ql:quickload '(:clack
                :clack-handler-woo
                :datastar-cl/clack
                :datastar-cl/woo
                :snooze
                :datastar-cl/snooze
                :datastar-cl/registry
                :spinneret
                :bordeaux-threads
                :alexandria))

(defpackage #:fat-cqrs-snooze-woo             ; identical to HT version
  (:use #:cl #:snooze)
  (:local-nicknames (:s*  :datastar-cl/snooze)
                    (:d*  :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:sp  :spinneret)
                    (:bt  :bordeaux-threads)
                    (:a   :alexandria))
  (:export #:main))
(in-package #:fat-cqrs-snooze-woo)

;;; PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (identical)
(defparameter *clicks* 0)
(defparameter *glow* nil)
(defparameter *greeting* "Hello, World!")
(defvar *clients* (reg:make-sse-registry "fat-cqrs-snooze-woo"))
(defparameter *css* (a:read-file-into-string
                     (merge-pathnames "style.css" *load-truename*)))

;;; BROADCAST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (identical)
;;;
;;; REG:NOTIFY-SUBSCRIBERS routes each write through D*:CALL-WITH-GENERATOR
;;; internally -- on Woo this enqueues via WOO:SCHEDULE; on Hunchentoot it is
;;; a transparent funcall.  CLIENT-DISCONNECTED is caught inside the dispatched
;;; closure so dead generators are always pruned, even on async backends.
(defun broadcast (fn)
  (reg:notify-subscribers *clients* fn))

;;; HTML CONTENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (identical)
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

;;; BACKGROUND TICKER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (new)
;;;
;;; In the Hunchentoot version the (loop (sleep 1) ...) lives inside the SSE
;;; route body (Pattern 3).  On Woo that blocking loop would park the evloop
;;; worker.  We extract it here: a single shared thread drives 1-second pushes
;;; to all subscribers.  The worker is never blocked.
(defun start-ticker ()
  (bt:make-thread
   (lambda ()
     (loop
       (sleep 1)
       (broadcast (lambda (g)
                    (d*:patch-elements g (render-content) :selector "#content")))))
   :name "snooze-woo-ticker"))

;;; ROUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Index -- identical to HT version
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
            (:strong "With Common Lisp, Snooze, and Woo")

            (:p "The time updates every 1s via a background ticker thread.")
            (:p "Click the buttons: updates are pushed instantly to ALL clients via CQRS notification.")
            (:hr)
            (:button :|data-on:click| (d*:sse-post "/click") "INC COUNTER")
            (:button :|data-on:click| (d*:sse-post "/toggle-glow") "TOGGLE GLOW")
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

;; SSE connection -- DIFFERS from HT version:
;;
;; Pattern 2 (empty body + :KEEP-ALIVE) instead of Pattern 3 (blocking loop).
;; :ON-CONNECT pushes initial state and registers the generator; the background
;; ticker drives subsequent pushes.  The S*:WITH-SSE call is structurally
;; unchanged from the HT version -- the backend difference (Clack lambda vs HT
;; direct) is handled inside the macro.
(s*:defresource sse (verb content-type &key datastar))

(s*:defroute sse (:get :text/html &key datastar)
  (s*:with-sse (gen
                :keep-alive    1
                :on-connect    (lambda (g)
                                 (d*:patch-signals g (list :ui-glow *glow*))
                                 (d*:patch-elements g (render-content) :selector "#content")
                                 (reg:register *clients* g))
                :on-disconnect (lambda (g) (reg:unregister *clients* g)))))

;; Command endpoint for "Increment Counter" -- identical to HT version
(s*:defresource click (verb content-type &key datastar))

(s*:defroute click (:post :application/json &key datastar)
  (incf *clicks*)
  (broadcast (lambda (g)
               (d*:patch-elements g (render-content) :selector "#content"))))

;; Command endpoint for "Toggle glow" -- identical to HT version
(s*:defresource toggle-glow (verb content-type &key datastar))

(s*:defroute toggle-glow (:post :application/json &key datastar)
  (setf *glow* (not *glow*))
  (broadcast (lambda (g)
               (d*:patch-signals g (list :ui-glow *glow*)))))

;; Command endpoint for "Set Greeting" -- identical to HT version
;;
;; S*:WITH-SIGNALS resolves the request from SNOOZE:*BACKEND* automatically;
;; no request argument is needed.  This is unchanged between backends.
(s*:defresource set-greeting (verb content-type &key datastar))

(s*:defroute set-greeting (:post :application/json &key datastar)
  (s*:with-signals ((salutation "salutation" "Hello")
                    (recipient  "recipient"  "World"))
    (setf *greeting* (format nil "~A, ~A!" salutation recipient))
    (broadcast (lambda (g)
                 (d*:patch-elements g (render-content) :selector "#content")))))

;;; WEB SERVER STARTUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (differs)
;;;
;;; SNOOZE:MAKE-CLACK-APP + CLACK:CLACKUP :SERVER :WOO replaces
;;; SNOOZE:MAKE-HUNCHENTOOT-APP + HUNCHENTOOT:START.
;;; :WORKER-NUM 1 proves the point: with Pattern 2 on Woo, multiple concurrent
;;; SSE connections share one event-loop worker without blocking each other.
(defun main ()
  (start-ticker)
  (clack:clackup (snooze:make-clack-app)
                 :server     :woo
                 :port       8989
                 :worker-num 1
                 :address    "0.0.0.0")
  (format t "~&Server started on http://localhost:8989  (worker-num 1)~%")
  (format t "Open 2+ tabs -- all receive ticks without parking the worker.~%")
  (sleep most-positive-fixnum))
(main)
