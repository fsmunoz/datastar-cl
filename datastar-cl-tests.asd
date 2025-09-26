;;;; datastar-cl-tests.asd

(asdf:defsystem #:datastar-cl-tests
  :description "Test server for the Common Lisp SDK"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "tests"
  :depends-on (#:hunchentoot
	       #:clack
	       #:woo
	       #:drakma
	       #:datastar-cl)
  :components ((:file "package")
	       (:file "test")
               (:file "test-hunchentoot")
	       (:file "test-clack")
	       (:file "test-clack-hunchentoot")
	       (:file "test-sse")
	       (:file "test-sse-response")
	       ))
