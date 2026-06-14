;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HANDLERS-CLACK.LISP --- Clack (+ Woo) HTTP handlers and router
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; Thin transport layer for Clack + Woo: extracts the client IP and signal
;;;; values from the Clack env plist, then delegates to the backend-neutral
;;;; perform-* functions in commands.lisp.
;;;;
;;;; SSE connections use the pure-push CQRS pattern (WITH-SSE empty body +
;;;; :KEEP-ALIVE).  On Woo, SERVE-SSE (datastar-cl/woo) registers a libev
;;;; heartbeat timer and returns immediately, freeing the Woo worker while the
;;;; socket stays open.  Broadcasts from any thread route through
;;;; CALL-WITH-GENERATOR -> WOO:SCHEDULE onto the connection's owning evloop.

(in-package #:datastar-cl/rep-detect-demo)

;;; CLIENT IP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clack stores request headers in a hash-table at (getf env :headers) with
;;; lowercase string keys.  D*:GET-CLACK-HEADER handles the hash-table lookup.
;;; The real IP selection order mirrors the Hunchentoot version: Cloudflare CF
;;; header first, then X-Forwarded-For (first token), then :remote-addr.

(defun client-ip-clack (env)
  "Return the best-effort real client IP string from the Clack env plist, or NIL."
  (flet ((trim (s) (when (and s (plusp (length (string-trim '(#\Space #\Tab) s))))
                     (string-trim '(#\Space #\Tab) s))))
    (or (trim (d*:get-clack-header env "cf-connecting-ip"))
        (let ((xff (trim (d*:get-clack-header env "x-forwarded-for"))))
          (when xff
            (let ((comma (position #\, xff)))
              (trim (if comma (subseq xff 0 comma) xff)))))
        (getf env :remote-addr))))

;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun index-clack (env)
  "Serve the full application page."
  (declare (ignore env))
  `(200 (:content-type "text/html") (,(render-page))))

(defun sse-clack (env)
  "Open a long-lived SSE connection using the pure-push CQRS pattern.
   Empty body + :KEEP-ALIVE + :ON-CONNECT/:ON-DISCONNECT.
   On Woo (datastar-cl/woo loaded) SERVE-SSE registers a libev timer and
   returns immediately, freeing the worker.  Structurally identical to the
   Hunchentoot sse-handler."
  (lambda (responder)
    (d*:with-signals ((sid "sid" "")) env
      (d*:with-sse (gen (env responder)
                    :on-connect    (lambda (g) (sse-connect g sid))
                    :on-disconnect (lambda (g) (reg:unregister *clients* g))
                    :keep-alive    t)))))

(defun spawn-clack (env)
  "Handle a SPAWN command."
  (d*:with-signals ((desig  "designation" "")
                    (model  "model"  "Nexus-6")
                    (role   "role"        "")
                    (incept "incept" "")
                    (sid    "sid"    ""))
      env
    (perform-spawn desig model role incept sid (client-ip-clack env)))
  '(204 nil ("")))

(defun sight-clack (env)
  "Handle a SIGHT command."
  (d*:with-signals ((target "target" "") (sid "sid" "")) env
    (perform-sight target sid (client-ip-clack env)))
  '(204 nil ("")))

(defun retire-clack (env)
  "Handle a RETIRE command."
  (d*:with-signals ((target "target" "") (sid "sid" "")) env
    (perform-retire target sid (client-ip-clack env)))
  '(204 nil ("")))

;;; ROUTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun router (env)
  "Dispatch Clack env to the appropriate handler by path."
  (let ((path (getf env :path-info)))
    (cond ((string= path "/")       (index-clack env))
          ((string= path "/sse")    (sse-clack env))
          ((string= path "/spawn")  (spawn-clack env))
          ((string= path "/sight")  (sight-clack env))
          ((string= path "/retire") (retire-clack env))
          (t `(404 (:content-type "text/plain") ("Not found"))))))
