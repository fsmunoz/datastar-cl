;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; glitch-demo.asd --- CQRS demo system definitions

;;; Core: backend-neutral Calispel CQRS engine + page rendering.
;;; No Hunchentoot or Clack dependency at this layer.

(asdf:defsystem #:glitch-demo
  :description "CQRS demo in Datastar-CL (backend-neutral core)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :maintainer "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:spinneret
               #:lass
               #:parenscript
               #:calispel
               #:jpl-queues
               #:bordeaux-threads
               #:datastar-cl)
  :components ((:file "package")
               (:file "style")
               (:file "index")
               (:file "events")))

;;; Hunchentoot transport: easy-handlers + thread-per-request server lifecycle.

(asdf:defsystem #:glitch-demo/hunchentoot
  :description "Glitch demo - Hunchentoot backend"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:glitch-demo
               #:datastar-cl/hunchentoot
               #:hunchentoot)
  :components ((:file "handlers-hunchentoot")
               (:file "server")))

;;; Clack + Woo transport: router + event-loop server lifecycle.
;;; Requires the patched Woo with woo.async (local clone).

(asdf:defsystem #:glitch-demo/clack
  :description "Glitch demo - Clack + Woo backend"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:glitch-demo
               #:datastar-cl/clack
               #:datastar-cl/woo
               #:clack
               #:clack-handler-woo)
  :components ((:file "handlers-clack")
               (:file "server-clack")))
