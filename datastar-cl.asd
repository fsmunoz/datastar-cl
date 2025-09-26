;;;; datastar-cl.asd

(asdf:defsystem #:datastar-cl
  :description "Describe datastar-cl here"
  :author "Frederico Muñoz <fsmunoz@gmail.com"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads
	       #:split-sequence
	       #:alexandria
	       #:com.inuoe.jzon
	       #:hunchentoot
	       #:clack
	       #:lack-util-writer-stream
	       )
  :components ((:file "package")
               (:file "datastar-cl")
	       (:file "hunchentoot")	       
	       (:file "clack")))
