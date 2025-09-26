;;;; datastar-cl.asd

(asdf:defsystem #:datastar-cl
  :description "Common Lisp implementation of the Datastar SDK for Server-Sent Events (SSE)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads
               #:split-sequence
               #:alexandria
               #:com.inuoe.jzon
               #:quri
               #:flexi-streams
               #:hunchentoot
               #:clack
               #:lack-util-writer-stream)
  :components ((:file "package")
               (:file "datastar-cl")
               (:file "conditions")
               (:file "hunchentoot")
               (:file "clack")))
