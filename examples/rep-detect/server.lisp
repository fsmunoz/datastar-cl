;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SERVER.LISP --- Hunchentoot server startup, teardown, and run entry points
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

(defvar *server*     nil "Active Hunchentoot acceptor instance, or NIL.")
(defvar *taskmaster* nil "Active Hunchentoot taskmaster, or NIL.")

(defparameter *server-port* 8990
  "Default HTTP port for the Hunchentoot REP-DETECT server.")

(defparameter *default-max-threads* 50
  "Fallback max concurrent threads when MAX_THREADS env var is absent.")

(defun start-server (&key (port *server-port*) (simulate t))
  "Start the Hunchentoot server on PORT.
   When SIMULATE is true (default), also starts the background simulator.
   Pass :simulate nil for a quiet server driven only by human input."
  (when *server*
    (stop-server))
  (open-store)
  (open-geo-db)
  (let* ((max-threads (read-env-int "MAX_THREADS" *default-max-threads*))
         (max-accept  (read-env-int "MAX_ACCEPT"  (+ max-threads 100)))
         (tm (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                            :max-thread-count  max-threads
                            :max-accept-count  max-accept)))
    (setf *taskmaster* tm
          *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                     :port port
                                                     :taskmaster tm)))
    (format t "~&# T: ~D~%" max-threads))
  (when simulate
    (start-simulator))
  *server*)

(defun stop-server ()
  "Stop the background simulator (if running) then the Hunchentoot server."
  (stop-simulator)
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&REP-DETECT server stopped~%")))

(defun restart-server (&key (port *server-port*))
  "Restart the server (stop then start)."
  (stop-server)
  (start-server :port port))

(defun run-server ()
  "Start the server and block the current thread indefinitely.
   Intended for use from the Makefile `run` target."
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
