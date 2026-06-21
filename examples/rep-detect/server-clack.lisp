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
;;;; WORKER-NUM controls parallelism; 1 is sufficient for the demo, 4 or more
;;;; for production.

(in-package #:datastar-cl/rep-detect-demo)

(defvar *clack-server* nil "Active Clack server handle (returned by CLACK:CLACKUP), or NIL.")

(defparameter *clack-server-port* 8991
  "Default HTTP port for the Clack + Woo REP-DETECT server.
   Kept distinct from *server-port* (8990) so both can run side-by-side.
   Override via the PORT env var.")

(defparameter *default-worker-num* 4
  "Default number of Woo event-loop workers when WORKER_NUM env var is absent.")

(defun start-clack-server (&key (port   (read-env-int "PORT"       *clack-server-port*))
                                (workers (read-env-int "WORKER_NUM" *default-worker-num*))
                                (simulate t))
  "Start the Clack + Woo server on PORT with WORKERS event-loop workers.
   When SIMULATE is true (default), also starts the background simulator.
   Pass :simulate nil for a quiet server driven only by human input."
  (when *clack-server*
    (stop-clack-server))
  (open-store)
  (open-geo-db)
  (setf *clack-server*
        (clack:clackup #'router
                       :server     :woo
                       :port       port
                       :worker-num workers
                       :address    "0.0.0.0"))
  ;; Brotli auto-prepends :br to *default-compression-priority* at load time
  ;; (via datastar-cl/brotli); no manual setf needed.
  (format t "~&Compression priority: ~A~%" lc-sse:*default-compression-priority*)
  (format t "~&REP-DETECT Clack+Woo server on port ~D (~D worker~:P)~%" port workers)
  (when simulate
    (start-simulator))
  *clack-server*)

(defun stop-clack-server ()
  "Stop the background simulator (if running) then the Clack server."
  (stop-simulator)
  (when *clack-server*
    (clack:stop *clack-server*)
    (setf *clack-server* nil)
    (format t "~&REP-DETECT Clack+Woo server stopped~%")))

(defun run-clack-server ()
  "Start the Clack + Woo server and block the current thread indefinitely.
   Intended for use from the Makefile `run-clack` target."
  (start-clack-server)
  (sleep most-positive-fixnum))

(defun main-clack ()
  "Toplevel for a standalone Clack+Woo executable: start server, block until
   Ctrl-C / SIGINT or SIGTERM (systemctl stop), then cleanly stop."
  (start-clack-server)
  #+sbcl
  (sb-sys:enable-interrupt
   sb-unix:sigterm
   (lambda (&rest args)
     (declare (ignore args))
     (stop-clack-server)
     (sb-ext:exit 0)))
  (handler-case (loop (sleep 60))
    (#+sbcl sb-sys:interactive-interrupt
     #-sbcl error ()
      (stop-clack-server))))
