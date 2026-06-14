;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; EVENT-SOURCING.LISP --- Event-sourced bank account
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates event sourcing combined with CQRS:
;;;
;;; 1) Append-only event log as the source of truth - no mutable balance
;;;    variable exists.
;;; 2) State derived by folding (REDUCE) the event log on every read.
;;;    The REDUCE is not a helper: it IS the state (so, "events are the database").
;;; 3) Commands validate business rules, then produce events, and they never
;;;    mutate state directly.
;;; 4) The "Balance after" column in the event log is like time travel: it shows
;;;    the account state at every point in history, computed by the same
;;;    fold as the current balance but stopped at an earlier event.
;;; 5) CQRS-ES: POST commands append events and broadcast to SSE subscribers, with
;;;    the SSE endpoint being pure-push (empty body + :keep-alive).
;;;
;;; Not everything about "event sourcing" is shown, and what is shown is not
;;; necessarily the best or only way to do it. Part of it is by design (for
;;; simplicity we opt for simple solutions without any other dependency), the
;;; other a result of my partial understanding.

;;; Run with: sbcl --load event-sourcing.lisp

;;; SYSTEM DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload '(:hunchentoot :spinneret :bordeaux-threads
                :datastar-cl/hunchentoot :datastar-cl/registry))

(defpackage #:event-sourcing
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:bt :bordeaux-threads) (:ht :hunchentoot)))

(in-package #:event-sourcing)

;;; EVENT STORE: append-only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct event id type amount timestamp)

(defvar *events* nil) ; newest first (push prepends)
(defvar *events-lock* (bt:make-lock "events"))

(defun append-event (type amount)
  (bt:with-lock-held (*events-lock*)
    (let ((e (make-event :id (1+ (length *events*))
                         :type type :amount amount
                         :timestamp (get-universal-time))))
      (push e *events*) e)))

(defun all-events ()
  (bt:with-lock-held (*events-lock*)
    (reverse *events*)))                       ; chronological order for fold

;;; PROJECTION: state is a reduce, not a field ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; There is no variable holding the current balance.  Every call to
;;; CURRENT-BALANCE replays the full log.  BALANCE-FROM is also used by the
;;; event-log renderer to compute the running balance after each event
;;; (the "time travel" column).
;;;
;;; "In event sourcing, state is a left fold". I checked, and a "left fold" is
;;; what normal people call reduce.

(defun balance-from (events)
  (reduce (lambda (bal e)
            (ecase (event-type e)
              (:deposited (+ bal (event-amount e)))
              (:withdrawn (- bal (event-amount e)))))
          events :initial-value 0))

(defun current-balance () (balance-from (all-events)))

;;; COMMANDS: validate, then produce events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Partially the reason why this comes up when CQRS is mentioned - independent
;;; things that happen to play well together.

(defun cmd-deposit (amount)
  (when (plusp amount)
    (append-event :deposited amount) t))

(defun cmd-withdraw (amount)
  (when (and (plusp amount) (>= (current-balance) amount))
    (append-event :withdrawn amount) t))

;;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fmt-time (ut)
  (multiple-value-bind (s m h) (decode-universal-time ut)
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun render-balance-panel ()
  (sp:with-html-string
    (:div :id "balance-panel"
          (:h2 "Balance: " (:code (format nil "$~D" (current-balance))))
          (:p (:input :type "number" :min "1" :data-bind "amount"
                      :style "width:5em")
              " "
              (:button :|data-on:click| (d*:sse-post "/deposit") "Deposit")
              " "
              (:button :|data-on:click| (d*:sse-post "/withdraw") "Withdraw"))
          (:p :data-show "$error != ''" :style "display:none;color:red"
              :data-text "$error"))))

(defun render-event-log ()
  (let ((events (all-events))
        (running 0))
    (sp:with-html-string
      (:table :id "event-log"
              :style "border-collapse:collapse;width:100%;margin-top:1em"
              (:caption (:em "Event log -- the source of truth"))
              (:thead
               (:tr (:th "#") (:th "Event") (:th "Amount")
                    (:th "Balance after") (:th "Time")))
              (:tbody
               (dolist (e events)
                 (incf running (* (event-amount e)
                                  (if (eq (event-type e) :deposited) 1 -1)))
                 (:tr :style "border-top:1px solid #ccc"
                      (:td (event-id e))
                      (:td (string-downcase (symbol-name (event-type e))))
                      (:td (format nil "~D" (event-amount e)))
                      (:td (:strong (format nil "$~D" running)))
                      (:td (fmt-time (event-timestamp e))))))))))

(defun push-account-state (gen)
  (d*:patch-elements gen (render-balance-panel) :selector "#balance-panel")
  (d*:patch-elements gen (render-event-log) :selector "#event-log"))

;;; SUBSCRIPTION (CQRS push layer) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *CLIENTS* is a keyed registry: NOTIFY-SUBSCRIBERS broadcasts to all
;;; connected generators; NOTIFY-SUBSCRIBER + sid key targets one.
;;; The sid is a UUID assigned by the browser at page load (crypto.randomUUID)
;;; and carried on every request as a Datastar signal, so POST handlers can
;;; target exactly the requester's stream.

(defvar *clients* (reg:make-keyed-sse-registry "event-sourcing"))

(defun broadcast ()
  (reg:notify-subscribers *clients* #'push-account-state))

;;; REPLY-TO delivers the command outcome asynchronously to one client over its
;;; already-open push stream.  NOTIFY-SUBSCRIBER routes via CALL-WITH-GENERATOR
;;; for correct cross-thread dispatch on all backends.
;;; Empty ERROR string clears a previous rejection (signal persists in the store).

(defun reply-to (sid &optional (error ""))
  (reg:notify-subscriber *clients* sid
                         (lambda (g) (d*:patch-signals g (list :error error)))))

;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ht:define-easy-handler (index :uri "/") ()
  (setf (ht:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
     (:body :data-signals (d*:init-signals :amount 10 :error "" :sid "")
            :data-init (format nil "$sid = crypto.randomUUID(); ~A" (d*:sse-get "/sse"))
            (:h1 "Event-Sourced Bank Account")
            (:p "The balance is never stored: it is derived by replaying the "
                "event log on every read (a " (:em "projection") "). "
                "The " (:strong "Balance after") " column shows the account "
                "state at each point in history.")
            (:p "The event IS the data: the succession of events not only produce the final result, they are in themselves important data.")
            (:raw (render-balance-panel))
            (:raw (render-event-log))))))

;;; Pure-push SSE endpoint (empty body, :keep-alive drives heartbeat).
;;; :on-connect pushes current state immediately and registers with the sid so
;;; POST handlers can unicast back to exactly this client.
(ht:define-easy-handler (sse-handler :uri "/sse") ()
  (d*:with-signals ((sid "sid" "")) ht:*request*
    (d*:with-sse (gen ht:*request*
                 :on-connect    (lambda (g) (push-account-state g) (reg:register *clients* g :key sid))
                 :on-disconnect (lambda (g) (reg:unregister *clients* g))
                 :keep-alive    t))))

;;; Command endpoints -- fire-and-forget (204 No Content).
;;;
;;; The POST says "do this if possible" and returns immediately with no body.
;;; The *outcome* arrives asynchronously over the push channel:
;;;   - success  -> broadcast new balance+log to all subscribers (incl. requester);
;;;                unicast empty $error to requester to clear any previous rejection.
;;;   - rejection -> unicast rejection message to requester's stream only.
;;; This decouples "command accepted" from "command outcome": between the click
;;; and the result there may be many intervening transactions.

(ht:define-easy-handler (deposit-handler :uri "/deposit") ()
  (d*:with-signals ((amount "amount" 0) (sid "sid" "")) ht:*request*
    (if (cmd-deposit (round amount))
        (progn (broadcast) (reply-to sid))
        (reply-to sid "Invalid amount")))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

(ht:define-easy-handler (withdraw-handler :uri "/withdraw") ()
  (d*:with-signals ((amount "amount" 0) (sid "sid" "")) ht:*request*
    (if (cmd-withdraw (round amount))
        (progn (broadcast) (reply-to sid))
        (reply-to sid "Insufficient funds")))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

;;; SEED: three initial events so the log is non-empty at startup ;;;;;;;;;;;;;;
(cmd-deposit 1000)
(cmd-deposit 500)
(cmd-withdraw 200)

;;; STARTUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ht:start (make-instance 'ht:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
