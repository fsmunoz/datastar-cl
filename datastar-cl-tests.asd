;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; datastar-cl-tests.asd

(asdf:defsystem #:datastar-cl-tests
  :description "Test server fixtures for the Common Lisp SDK (Hunchentoot and Clack handlers)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :maintainer "Frederico Muñoz <fsmunoz@gmail.com>"
  :license  "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:hunchentoot
               #:clack
               #:clack-handler-hunchentoot
               #:drakma
               #:bordeaux-threads
               #:datastar-cl)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:module "fixtures"
                  :serial t
                  :components
                  ((:file "server")
                   (:file "hunchentoot")
                   (:file "clack-common")
                   (:file "clack-hunchentoot")
                   (:file "sse")
                   (:file "sse-response")
                   (:file "compression")
                   (:file "cqrs")))))))

(asdf:defsystem #:datastar-cl-tests/unit
  :description "FiveAM unit tests for datastar-cl (no I/O, no servers)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl/core
               #:datastar-cl/hunchentoot
               #:datastar-cl/clack
               #:fiveam)
  :pathname "tests/unit"
  :perform (asdf:test-op (op c) (uiop:symbol-call :fiveam :run! :datastar-cl))
  :components ((:file "package")
               (:file "core-tests")))

(asdf:defsystem #:datastar-cl-tests/integration
  :description "FiveAM integration tests for datastar-cl (require running servers)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl-tests/unit
               #:datastar-cl-tests
               #:fiveam
               #:drakma
               #:bordeaux-threads
               #:zstd)
  :pathname "tests/integration"
  :perform (asdf:test-op (op c)
             (unless (uiop:symbol-call '#:datastar-cl/tests/integration '#:run-all-tests)
               (uiop:quit 1)))
  :components ((:file "package")
               (:file "smoke-tests")
               (:file "sdk-tests")))
