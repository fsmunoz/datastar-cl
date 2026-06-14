;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SERVER.LISP --- Server startup
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-simple-demo)

;;; Hunchentoot server
;;;
;;; Routes are registered as easy-handlers; Hunchentoot's default dispatch
;;; table finds them automatically. The GET /updates SSE handler parks its
;;; request thread inside with-sse until the client disconnects
;;; (thread-per-request model, so this is free).

(defvar *server* nil)

(defparameter *server-port* 8081)
(asdf:load-system :datastar-cl/brotli)

(defun start-server (&key (port *server-port*))
  "Start the Hunchentoot server."
  (when *server*
    (stop-server))
  (setf *server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor :port port)))
  (format t "~&CQRS Hunchentoot server started on port ~D~%" port))

(defun stop-server ()
  "Stop the Hunchentoot server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&CQRS Hunchentoot server stopped~%")))

(defun restart-server (&key (port *server-port*))
  "Restart the Hunchentoot server."
  (stop-server)
  (start-server :port port))

(defun run-demo ()
  "Start the server and block the main thread."
  (start-server)
  (sleep most-positive-fixnum))
