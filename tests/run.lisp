;;;; Test server startup script for CI/CD
;;;;
;;;; Usage: sbcl --non-interactive --load tests/run.lisp
;;;;
;;;; This script:
;;;; 1. Loads the datastar-cl-tests system
;;;; 2. Starts all three test servers (Hunchentoot, Clack+Woo, Clack+Hunchentoot)
;;;; 3. Keeps SBCL running to serve requests

(require :asdf)

;; Load the test system
(format t "~&Loading datastar-cl-tests system...~%")
(asdf:load-system :datastar-cl-tests)

;; Start all servers
(format t "~&Starting test servers...~%")
(datastar-cl-tests::start-all-servers)

;; Keep process alive
(format t "~&Servers running. Press Ctrl+C to stop.~%")
(loop (sleep 1))
