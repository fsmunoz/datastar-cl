;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; datastar-cl.asd

;;; Sub-system layout:
;;;
;;;   datastar-cl/core        -- Datastar SDK core; depends on lc-sse/core
;;;   datastar-cl/hunchentoot -- Datastar signal extraction for Hunchentoot
;;;   datastar-cl/clack       -- Datastar signal extraction for Clack
;;;   datastar-cl/brotli      -- compat alias: loads lc-sse/brotli
;;;   datastar-cl/woo         -- compat alias: loads lc-sse/woo
;;;   datastar-cl/snooze      -- optional Snooze routing integration
;;;   datastar-cl/registry    -- compat shim: re-exports lc-sse/registry
;;;   datastar-cl/profile     -- SBCL sb-sprof harness; development only (tests/)
;;;   datastar-cl             -- umbrella: loads hunchentoot + clack
;;;
;;; Generic SSE machinery (sse-generator, with-sse, compression, backends,
;;; registry) lives in lc-sse (lc-sse.asd). datastar-cl depends on lc-sse
;;; and adds Datastar protocol on top: patch-elements/-signals, execute-script,
;;; read-signals, action-string builders.
;;;
;;; Typical use:
;;;   (ql:quickload :datastar-cl)          ; both backends
;;;   (ql:quickload :datastar-cl/brotli)   ; add brotli compression support
;;;   (ql:quickload :datastar-cl/registry) ; connection registry for CQRS/pub-sub
;;;   (ql:quickload :datastar-cl/snooze)   ; Snooze routing integration
;;;   (ql:quickload :datastar-cl/profile)  ; profiling harness (SBCL only)
;;;
;;; Tests: datastar-cl-tests.asd (test fixtures + datastar-cl-tests/unit + /integration)
;;; Examples and load generator: each example has its own .asd in its directory.

(asdf:defsystem #:datastar-cl/core
  :description "Common Lisp Datastar SDK core"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:lc-sse/core
               #:split-sequence
               #:alexandria
               #:com.inuoe.jzon
               #:quri)
  :pathname "src/"
  :components ((:file "package")
               (:file "conditions")
               (:file "config")
               (:file "signals")
               (:file "events")
               (:file "actions")))

(asdf:defsystem #:datastar-cl/hunchentoot
  :description "Datastar SDK: Hunchentoot backend"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl/core
               #:lc-sse/hunchentoot
               #:hunchentoot)
  :pathname "src/"
  :components ((:file "hunchentoot")))

(asdf:defsystem #:datastar-cl/clack
  :description "Datastar SDK: Clack backend"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl/core
               #:lc-sse/clack
               #:flexi-streams)
  :pathname "src/"
  :components ((:file "clack")))

(asdf:defsystem #:datastar-cl/woo
  :description "Datastar SDK -- Woo event-loop dispatch (compat alias for lc-sse/woo)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:lc-sse/woo))

(asdf:defsystem #:datastar-cl/brotli
  :description "Datastar SDK, optional brotli compression (compat alias for lc-sse/brotli)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:lc-sse/brotli))

(asdf:defsystem #:datastar-cl/registry
  :description "Datastar SDK -- connection registry (compat shim over lc-sse/registry)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:lc-sse/registry)
  :pathname "src/"
  :components ((:file "registry")))

(asdf:defsystem #:datastar-cl/snooze
  :description "Datastar SDK -- Snooze routing integration"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl/hunchentoot
               #:datastar-cl/clack
               #:snooze)
  :pathname "src/"
  :components ((:file "snooze")))

(asdf:defsystem #:datastar-cl/snooze-doc
  :description "Doc-only system: documents the datastar-cl/snooze package in isolation.
Depends only on external :snooze so declt does not follow into the in-project
backends. Requires :datastar-cl to be loaded first (docs/make-docs.lisp does this)."
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:snooze)
  :pathname "src/"
  :components ((:file "snooze")))

(asdf:defsystem #:datastar-cl/profile
  :description "Datastar SDK, SBCL sb-sprof profiling"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl/hunchentoot
               #:datastar-cl/clack
               #:drakma)
  :pathname "tests/"
  :components ((:file "profile" :if-feature :sbcl)))

(asdf:defsystem #:datastar-cl
  :description "Common Lisp implementation of the Datastar SDK for Server-Sent Events (SSE)"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :maintainer "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/fsmunoz/datastar-cl"
  :bug-tracker "https://github.com/fsmunoz/datastar-cl/issues"
  :source-control (:git "https://github.com/fsmunoz/datastar-cl.git")
  :depends-on (#:datastar-cl/hunchentoot
               #:datastar-cl/clack)
  :in-order-to ((asdf:test-op (asdf:test-op #:datastar-cl-tests/integration))))
