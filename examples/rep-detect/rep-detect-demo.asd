;;;; -*- mode: lisp; coding: utf-8 -*-

;;;; rep-detect-demo.asd --- Geolocated SVG radar demo: three-system layout
;;;;
;;;; System hierarchy:
;;;;
;;;;   :rep-detect-demo              -- backend-neutral core (datastar-cl only)
;;;;       package store geo world-110m radar style index push commands simulator
;;;;
;;;;   :rep-detect-demo/hunchentoot  -- Hunchentoot HTTP transport
;;;;       handlers-hunchentoot server
;;;;
;;;;   :rep-detect-demo/clack        -- Clack + Woo HTTP transport
;;;;       handlers-clack server-clack

(asdf:defsystem #:rep-detect-demo
  :description "Geolocated SVG radar demo -- backend-neutral core"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :maintainer "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:datastar-cl
               #:datastar-cl/registry
               ;; #:datastar-cl/brotli is loaded opportunistically by the Docker
               ;; build (and in local dev) but is NOT a hard ASDF dependency:
               ;; cl-brotli is not in Quicklisp, so a hard dep breaks fresh-env
               ;; builds.  The Dockerfiles attempt (ql:quickload :datastar-cl/brotli)
               ;; before loading this system; when it succeeds brotli is
               ;; auto-prepended to *default-compression-priority* and negotiated
               ;; for clients that advertise Accept-Encoding: br.  When it fails
               ;; the build still succeeds and the demo falls back to zstd.
               #:spinneret
               #:lass
               #:bordeaux-threads
               #:bknr.datastore
               #:cl-maxminddb)
  :components ((:file "package")
               (:file "store")
               (:file "geo")
               (:file "world-110m")
               (:file "radar")
               (:file "style")
               (:file "index")
               (:file "push")
               (:file "commands")
               (:file "simulator")))

(asdf:defsystem #:rep-detect-demo/hunchentoot
  :description "Geolocated SVG radar demo -- Hunchentoot HTTP transport"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:rep-detect-demo
               #:datastar-cl/hunchentoot
               #:hunchentoot)
  :components ((:file "handlers-hunchentoot")
               (:file "server")))

(asdf:defsystem #:rep-detect-demo/clack
  :description "Geolocated SVG radar demo -- Clack + Woo HTTP transport"
  :author "Frederico Muñoz <fsmunoz@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:rep-detect-demo
               #:datastar-cl/clack
               #:datastar-cl/woo
               #:clack
               #:clack-handler-woo)
  :components ((:file "handlers-clack")
               (:file "server-clack")))
