;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; glitch-simple-demo.asd --- Simplified CQRS demo standalone system definition

(asdf:defsystem #:glitch-simple-demo
  :description "Simplified CQRS demo in Datastar-CL (uses datastar-cl/registry)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :maintainer "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:hunchentoot
               #:spinneret
               #:lass
               #:parenscript
               #:bordeaux-threads
               #:datastar-cl
               #:datastar-cl/registry)
  :components ((:file "package")
               (:file "style")
               (:file "index")
               (:file "events")
               (:file "server")))
