;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; TEST-CLACK-COMMON.LISP --- Shared Clack routing handler
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl-tests)

(defun make-clack-app ()
  "Return the Clack routing handler shared by all Clack test servers."
  (lambda (env)
    (let ((path (getf env :path-info)))
      (cond
        ((string= path "/test")
         (lambda (responder)
           (let ((signals (datastar-cl:read-signals env)))
             (datastar-cl:with-sse (gen (env responder))
               (handle-datastar-signals signals gen)))))

        ((string= path "/test-sse")
         (test-sse-clack-handler env))

        ((string= path "/test-sse-response")
         (test-sse-response-clack-handler env))

        ((string= path "/test-sse-response-error")
         (test-sse-response-error-clack-handler env))

        ((string= path "/test-compression")
         (test-compression-clack-handler env))

        ((string= path "/test-cqrs-stream")
         (test-cqrs-stream-clack-handler env))
        ((and (string= path "/test-cqrs-command")
              (eq (getf env :request-method) :post))
         (test-cqrs-command-clack-handler env))

        ((string= path "/bench-plain")
         '(200 (:content-type "text/html")
               ("<div id=\"message\">Hello, world!</div>")))

        ((string= path "/")
         '(200 (:content-type "text/plain")
               ("Up.")))

        (t
         '(404 (:content-type "text/plain")
               ("Not Found")))))))
