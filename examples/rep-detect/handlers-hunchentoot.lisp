;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HANDLERS-HUNCHENTOOT.LISP --- Hunchentoot HTTP handlers
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; Thin transport layer for Hunchentoot: extracts the client IP and signal
;;;; values from HUNCHENTOOT:*REQUEST*, then delegates to the backend-neutral
;;;; perform-* functions in commands.lisp.

(in-package #:datastar-cl/rep-detect-demo)

;;; CLIENT IP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; When the server runs behind a Cloudflare tunnel, Cloudflare sets the
;;; CF-Connecting-IP header to the original visitor IP. We check that first,
;;; then X-Forwarded-For (taking the first token), then the socket address.
;;; Hunchentoot header-in* matching is case-insensitive.

(defun client-ip (&optional (request hunchentoot:*request*))
  "Return the best-effort real client IP string for REQUEST, or NIL."
  (unless request (return-from client-ip nil))
  (flet ((trim (s) (when (and s (plusp (length (string-trim '(#\Space #\Tab) s))))
                     (string-trim '(#\Space #\Tab) s))))
    (or (trim (hunchentoot:header-in* :cf-connecting-ip request))
        (let ((xff (trim (hunchentoot:header-in* :x-forwarded-for request))))
          (when xff
            (let ((comma (position #\, xff)))
              (trim (if comma (subseq xff 0 comma) xff)))))
        (hunchentoot:remote-addr* request))))

;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-page))

(hunchentoot:define-easy-handler (sse-handler :uri "/sse") ()
  (d*:with-signals ((sid "sid" "")) hunchentoot:*request*
    (d*:with-sse (gen hunchentoot:*request*
                  :on-connect    (lambda (g) (sse-connect g sid))
                  :on-disconnect (lambda (g) (reg:unregister *clients* g))
                  :keep-alive    t))))

(hunchentoot:define-easy-handler (spawn-handler :uri "/spawn") ()
  (d*:with-signals ((desig  "designation" "")
                    (model  "model"  "Nexus-6")
                    (role   "role"        "")
                    (incept "incept" "")
                    (sid    "sid"    ""))
      hunchentoot:*request*
    (perform-spawn desig model role incept sid (client-ip)))
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")

(hunchentoot:define-easy-handler (sight-handler :uri "/sight") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) hunchentoot:*request*
    (perform-sight target sid (client-ip)))
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")

(hunchentoot:define-easy-handler (retire-handler :uri "/retire") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) hunchentoot:*request*
    (perform-retire target sid (client-ip)))
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  "")
