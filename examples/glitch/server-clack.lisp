;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SERVER-CLACK.LISP --- Clack + Woo server startup, teardown, and run entry points
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; Requires:
;;;;   - datastar-cl/woo  (SERVE-SSE specialisation + CALL-WITH-GENERATOR routing)
;;;;   - Woo patched with woo.async (fork: github.com/fsmunoz/woo, branch cqrs)
;;;;   - clack-handler-woo
;;;;
;;;; The Woo event-loop model means SSE connections consume no thread per
;;;; connection -- each is held by a libev timer registered by SERVE-SSE.
;;;; Forwarder threads (Calispel consumers) route writes via CALL-WITH-GENERATOR
;;;; -> woo:schedule so actual stream writes happen on the correct evloop thread.
;;;; WORKER-NUM controls parallelism; 1 is sufficient for the demo.

(in-package #:datastar-cl/glitch-demo)

(defvar *clack-server* nil "Active Clack server handle (returned by CLACK:CLACKUP), or NIL.")

(defparameter *clack-server-port* 8082
  "Default HTTP port for the Clack + Woo glitch server.
   Kept distinct from *server-port* (8081) so both can run side-by-side.
   Override via the PORT env var.")

(defparameter *default-worker-num* 4
  "Default number of Woo event-loop workers when WORKER_NUM env var is absent.")

(defun start-clack-server (&key (port    (read-env-int "PORT"       *clack-server-port*))
                                (workers (read-env-int "WORKER_NUM" *default-worker-num*)))
  "Start the Clack + Woo server on PORT with WORKERS event-loop workers."
  (when *clack-server*
    (stop-clack-server))
  (setf *clack-server*
        (clack:clackup #'router
                       :server     :woo
                       :port       port
                       :worker-num workers
                       :address    "0.0.0.0"))
  (format t "~&GLITCH Clack+Woo server on port ~D (~D worker~:P)~%" port workers)
  *clack-server*)

(defun stop-clack-server ()
  "Stop the Clack + Woo server."
  (when *clack-server*
    (clack:stop *clack-server*)
    (setf *clack-server* nil)
    (format t "~&GLITCH Clack+Woo server stopped~%")))

(defun run-clack-server ()
  "Start the Clack + Woo server and block the current thread indefinitely.
   Intended for use from the Makefile `run-clack` target."
  (start-clack-server)
  (sleep most-positive-fixnum))

(defun main-clack ()
  "Toplevel for a standalone Clack+Woo executable: start server, block until
   Ctrl-C / SIGINT, then cleanly stop."
  (start-clack-server)
  (handler-case (loop (sleep 60))
    (#+sbcl sb-sys:interactive-interrupt
     #-sbcl error ()
      (when *clack-server* (clack:stop *clack-server*)))))
