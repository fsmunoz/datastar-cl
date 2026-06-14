;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; FAT-CQRS-WOO.LISP --- Fat CQRS demo on Clack + Woo, using WITH-SSE
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates pure-push CQRS (Pattern 2) via WITH-SSE on Clack + Woo.
;;;
;;; The /sse handler is structurally identical to the Hunchentoot pure-push
;;; demos (prevalence.lisp, event-sourcing.lisp): empty body + :KEEP-ALIVE +
;;; :ON-CONNECT/:ON-DISCONNECT.  The only difference is the request argument
;;; shape -- (env responder) instead of HT:*REQUEST*.
;;;
;;; What this demo proves:
;;;
;;;   1) WITH-SSE works on Woo.  On Woo + datastar-cl/woo loaded, empty body +
;;;      :KEEP-ALIVE registers a libev heartbeat timer and returns immediately
;;;      (SERVE-SSE specialisation in src/woo.lisp), freeing the Woo worker
;;;      while the socket stays open.  No worker thread is parked.
;;;
;;;   2) Cross-thread CQRS push.  A shared background broadcaster thread drives
;;;      1 s time ticks; a POST handler increments a counter.  Both broadcast
;;;      via REG:NOTIFY-SUBSCRIBERS, which uses D*:CALL-WITH-GENERATOR
;;;      internally, routing each write through WOO:SCHEDULE onto the
;;;      connection's owning evloop -- thread-safe with any number of workers.
;;;
;;;   3) Clean disconnect.  When a client disconnects, the keep-alive timer
;;;      write fails (STREAM-ERROR) within 1 s, which cancels the timer, runs
;;;      :ON-DISCONNECT (unregister), and closes the generator.
;;;
;;; Validation: run with :WORKER-NUM 1 and open 2+ browser tabs.
;;;   - Both tabs receive a time tick every second (no parked worker).
;;;   - Clicking in one tab pushes the updated count to all tabs instantly.
;;;   - Closing a tab: within ~1 s the connection is torn down cleanly.
;;;
;;; Run with: sbcl --load fat-cqrs-woo.lisp
;;;
;;; Prerequisites: clone the patched Woo fork onto your ASDF source registry
;;;   git clone -b cqrs git@github.com:fsmunoz/woo.git
;;; Place it in a directory ASDF searches before Quicklisp (e.g. add an entry
;;; in ~/.config/common-lisp/source-registry.conf.d/).  Then
;;; (ql:quickload :woo) resolves the local clone automatically.

;;; Single-file loading harness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload '(:clack
                :clack-handler-woo
                :datastar-cl/clack
                :datastar-cl/woo
                :datastar-cl/registry
                :spinneret
                :bordeaux-threads))

(defpackage #:fat-cqrs-woo
  (:use #:cl)
  (:local-nicknames (:sp  :spinneret)
                    (:d*  :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:bt  :bordeaux-threads)))

(in-package #:fat-cqrs-woo)

;;; State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *clicks* 0)
(defvar *clients* (reg:make-sse-registry "fat-cqrs-woo"))

;;; Broadcast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun broadcast ()
  "Push the current rendered content to every registered generator.
   REG:NOTIFY-SUBSCRIBERS uses CALL-WITH-GENERATOR internally, so on Woo each
   write is enqueued via WOO:SCHEDULE onto the connection's owning evloop.
   CLIENT-DISCONNECTED is caught inside the dispatched closure (after the
   evloop fires the write), so dead generators are always pruned correctly."
  (let ((content (render-content)))
    (reg:notify-subscribers
     *clients*
     (lambda (g) (d*:patch-elements g content :selector "#content")))))

;;; Content ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Shared time-tick broadcaster ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A single background thread drives 1-second ticks to all subscribers.
;;; BROADCAST uses NOTIFY-SUBSCRIBERS -> CALL-WITH-GENERATOR, so each write is
;;; routed via WOO:SCHEDULE onto the connection's owning evloop -- no worker parked.

(defun start-tick-broadcaster ()
  (bt:make-thread
   (lambda ()
     (loop
       (sleep 1)
       (broadcast)))
   :name "fat-cqrs-woo-ticker"))

;;; Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun index-handler (env)
  "Serve the page. One-shot Clack response."
  (declare (ignore env))
  (let ((html (sp:with-html-string
                (:doctype)
                (:html
                 (:head (:script :type "module" :src (d*:datastar-url)))
                 (:body :data-init (d*:sse-get "/sse")
                        (:h1 "Fat CQRS (Clack+Woo, WITH-SSE)")
                        (:p "Time updates every 1 s via a shared broadcaster thread. "
                            "The Woo worker is never parked (SERVE-SSE timer path).")
                        (:p "Click the button: count pushes instantly to all tabs "
                            "via cross-thread WOO:SCHEDULE.")
                        (:raw (render-content)))))))
    `(200 (:content-type "text/html") (,html))))

(defun sse-handler (env)
  "Open a long-lived SSE connection using WITH-SSE Pattern 2.
   Empty body + :KEEP-ALIVE 1 + :ON-CONNECT/:ON-DISCONNECT.
   On Woo (datastar-cl/woo loaded) SERVE-SSE registers a libev timer and
   returns immediately, freeing the worker.  Identical in shape to the
   Hunchentoot pure-push demos."
  (lambda (responder)
    (d*:with-sse (gen (env responder)
                  :keep-alive    1
                  :on-connect    (lambda (g)
                                   (d*:patch-elements g (render-content)
                                                      :selector "#content")
                                   (reg:register *clients* g))
                  :on-disconnect (lambda (g) (reg:unregister *clients* g))))))

(defun click-handler (env)
  "Handle a POST /click: increment the counter and broadcast to all clients.
   Runs in an arbitrary thread; writes are routed safely via CALL-WITH-GENERATOR."
  (declare (ignore env))
  (incf *clicks*)
  (broadcast)
  `(200 (:content-type "text/plain") ("ok")))

;;; Routing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun router (env)
  (let ((path (getf env :path-info)))
    (cond ((string= path "/")      (index-handler env))
          ((string= path "/sse")   (sse-handler env))
          ((string= path "/click") (click-handler env))
          (t `(404 (:content-type "text/plain") ("not found"))))))

;;; Start ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(start-tick-broadcaster)
(clack:clackup #'router :server :woo :port 8989 :worker-num 1)
(format t "~&Server started on http://localhost:8989  (worker-num 1)~%")
(format t "Open 2+ tabs, click the button, close a tab -- all should work.~%")
