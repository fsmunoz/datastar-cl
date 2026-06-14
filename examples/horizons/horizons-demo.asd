;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; horizons-demo.asd --- JPL Horizons API demo standalone system definition

(asdf:defsystem #:horizons-demo
  :description "Minimal JPL Horizons API demo with Datastar and Snooze"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl
               #:datastar-cl/snooze
               #:hunchentoot
               #:dexador
               #:spinneret
               #:lass
               #:xmls
               #:snooze
               #:alexandria
               #:parenscript)
  :components ((:file "package")
               (:file "server")
               (:file "details")
               (:file "dsn")
               (:file "style")
               (:file "index")))
