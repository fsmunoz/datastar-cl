;;;; package.lisp

(defpackage #:datastar-cl
  (:use #:cl)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export
   ;; Core SDK API
   :read-signals
   :make-sse-generator
   :send-event
   :patch-elements
   :execute-script
   :patch-signals

   ;; Parameters
   :*default-patch-mode*
   :*default-retry-duration*
   :*default-auto-remove*
   :*catch-errors-p*

   ;; Backend constructors
   :make-hunchentoot-sse-generator
   :make-clack-sse-generator

   ;; SSE management
   :keep-sse-alive
   :close-sse-generator
   :ensure-connection-open

   ;; Macros
   :with-sse-connection
   :with-sse-response

   ;; Condition classes
   :datastar-error
   :signal-error
   :invalid-json-error
   :sse-connection-lost

   ;; Condition accessors
   :datastar-error-message
   :invalid-json-error-json-string
   :sse-connection-lost-generator))
