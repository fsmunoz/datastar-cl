;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-CLACK-HUNCHENTOOT.LISP --- Clack test server (Hunchentoot backend)
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

(defparameter *clack-hunchentoot-http-port* 7333 "Port in which to listen")
(defvar *clack-hunchentoot-handler* nil
  "Clack+Hunchentoot server handler instance")

(defun stop-clack-hunchentoot-server ()
  "Stop Clack+Hunchentoot server"
  (when *clack-hunchentoot-handler*
    (clack:stop *clack-hunchentoot-handler*)
    (setf *clack-hunchentoot-handler* nil)))

(defun start-clack-hunchentoot-server (&key (port *clack-hunchentoot-http-port*) (address "localhost"))
  "Start Clack server with Hunchentoot backend and routing."
  (setf *clack-hunchentoot-handler*
        (clack:clackup (make-clack-app)
                       :port port
                       :address address
                       :server :hunchentoot))
  (format t "Clack+Hunchentoot server started on port ~a~%" port))
