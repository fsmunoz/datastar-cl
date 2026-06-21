;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SERVER.LISP --- Server initialization
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/horizons-demo)

;;;; Configuration

(defparameter *server-port* 9080
  "Port for the Hunchentoot server")

(defvar *server* nil
  "Hunchentoot server instance")

(defparameter *static-directory*
  (asdf:system-relative-pathname :horizons-demo "static/")
  "Directory for static files (JS, CSS)")

(defparameter *default-max-threads* 50
  "Fallback max concurrent threads when MAX_THREADS env var is absent.")

(defun read-env-int (name default)
  "Return the integer value of env var NAME, or DEFAULT if absent or unparseable."
  (let ((raw (uiop:getenv name)))
    (if raw
        (handler-case (parse-integer raw)
          (error () default))
        default)))

;;;; HTTP handlers and utilities

(defun start-server (&key (port *server-port*) (address "0.0.0.0"))
  "Start the web server.
   Rebuilds the Hunchentoot dispatch table from scratch so repeated
   reloads can't mangle the order; static prefix first, then the
   Snooze app, then the default fallback."
  (setf hunchentoot:*dispatch-table* '())
  (push (snooze:make-hunchentoot-app)
        hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-static-file-dispatcher-and-handler
         "/planets-canvas.js"
         (asdf:system-relative-pathname :horizons-demo "static/planets-canvas.js"))
        hunchentoot:*dispatch-table*)
  (when *server*
    (stop-server))
  (dsn-updater)
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
    (format t "~&Started Hunchentoot at port ~D - Threads: ~D~%" port max-threads))
  *server*)



(defun stop-server ()
  "Stop the web server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "~&Server stopped~%")))

(defun restart-server (&key (port *server-port*))
  "Restart the server."
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
