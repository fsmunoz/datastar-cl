;;;; Common Lisp SDK for Datastar
;;;;
;;;; Start servers for testing run

(in-package #:datastar-cl-tests)

;; Hunchentoot server
(start-hunchentoot-server)

;; Clack server
(start-clack-server)

(stop-clack-server)
(stop-hunchentoot-server)
