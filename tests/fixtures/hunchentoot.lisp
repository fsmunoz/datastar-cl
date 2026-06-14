;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-HUNCHENTOOT.LISP --- Hunchentoot test server
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

(defparameter *hunchentoot-http-port* 7331 "Port in which to listen")

(defvar *server*
  "Hunchentoot server instance")

(defun start-hunchentoot-server (&key (port *hunchentoot-http-port*)
                                      (address "localhost")
                                      max-thread-count
                                      max-accept-count)
  "Start Hunchentoot server.
   :max-thread-count and :max-accept-count configure the taskmaster; both default
   to NIL (Hunchentoot's own defaults). For high-concurrency use, set both higher
   than the expected peak connection count."
  (setf *server*
        (make-instance 'datastar-cl:tcp-nodelay-easy-acceptor
                       :port port
                       :address address
                       :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                                                  :max-thread-count max-thread-count
                                                  :max-accept-count max-accept-count)))
  (hunchentoot:start *server*)
  (format t "~%Hunchentoot server started on port ~a (max-threads: ~a, max-accept: ~a)~%"
          port max-thread-count max-accept-count))

(defun stop-hunchentoot-server ()
  "Stop Hunchentoot server"
  (hunchentoot:stop *server*))


;; Test endpoint using with-sse macro
(hunchentoot:define-easy-handler (test :uri "/test") ()
  (let ((signals (datastar-cl:read-signals hunchentoot:*request*)))
    (datastar-cl:with-sse (gen hunchentoot:*request*)
      (handle-datastar-signals signals gen))))

(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  "Up.")

;;; Plain HTTP endpoint returning the same HTML fragment as the N-event /test SSE workload.
;;; Useful as a baseline comparison for SSE overhead measurements.
(hunchentoot:define-easy-handler (bench-plain :uri "/bench-plain") ()
  (setf (hunchentoot:content-type*) "text/html")
  "<div id=\"message\">Hello, world!</div>")
