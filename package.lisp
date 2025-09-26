;;;; package.lisp

(defpackage #:datastar-cl
  (:use #:cl)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export
   :read-signals
   :make-sse-generator
   :send-event
   :patch-elements
   :execute-script
   :patch-signals
   :*default-patch-mode*
   :*default-retry-duration*
   :*default-auto-remove*
   :make-hunchentoot-sse-generator
   :make-clack-sse-generator))
