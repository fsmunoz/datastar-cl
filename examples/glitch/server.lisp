;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SERVER.LISP --- Server startup
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-demo)

;;; Hunchentoot server
;;;
;;; Routes are registered as easy-handlers; Hunchentoot's default dispatch
;;; table finds them automatically. The GET /updates SSE handler parks its
;;; request thread inside with-sse until the client disconnects
;;; (thread-per-request model, so this is free).

(defvar *server* nil)

(defparameter *server-port* 8081)

(defparameter *default-max-threads* 50
  "Fallback max concurrent threads when MAX_THREADS env var is absent.")

(defun start-server (&key (port *server-port*))
  "Start the Hunchentoot server."
  (when *server*
    (stop-server))
  (let* ((max-threads (read-env-int "MAX_THREADS" *default-max-threads*))
         (max-accept  (read-env-int "MAX_ACCEPT"  (+ max-threads 100)))
         (tm (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                            :max-thread-count max-threads
                            :max-accept-count max-accept)))
    (setf *taskmaster* tm
          *stats-fn* (lambda ()
                       (let ((used (hunchentoot:taskmaster-thread-count tm))
                             (max  (hunchentoot:taskmaster-max-thread-count tm)))
                         (spinneret:with-html-string
                           (:span :id "server-stats" (format nil "c ~D/~D" used max)))))
          *server* (hunchentoot:start
                    (make-instance 'hunchentoot:easy-acceptor
                                   :port port
                                   :taskmaster tm)))
    (format t "~&# Threads: ~D~%" max-threads)))

(defun stop-server ()
  "Stop the Hunchentoot server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil
          *taskmaster* nil
          *stats-fn* nil)))

(defun restart-server (&key (port *server-port*))
  "Restart the Hunchentoot server."
  (stop-server)
  (start-server :port port))

(defun run-server ()
  "Start the server and block the current thread indefinitely.
   Intended for use from the Makefile run target."
  (start-server)
  (sleep most-positive-fixnum))

(defun main ()
  "Toplevel for a standalone executable: start server, block until
   Ctrl-C / SIGINT, then cleanly stop."
  (start-server)
  (handler-case (loop (sleep 60))
    (#+sbcl sb-sys:interactive-interrupt
     #-sbcl error ()
      (when *server* (hunchentoot:stop *server*)))))
