;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PROFILE.LISP --- SBCL profiling harness (development only)
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; SBCL sb-sprof profiling harness. Load with: (ql:quickload :datastar-cl/profile)
;;;
;;; Entry points: PROFILE-IN-PROCESS, PROFILE-HTTP
;;;
;;; Quick start:
;;;   (datastar-cl/profile:profile-in-process)
;;;   (datastar-cl/profile:profile-in-process :mode :alloc)
;;;   (datastar-cl/profile:profile-in-process :iterations 200 :report :graph)
;;;   (datastar-cl-tests::start-all-servers)
;;;   (datastar-cl/profile:profile-http)

;;; sb-sprof is an SBCL contrib loaded on demand. The require must run
;;; before the reader encounters sb-sprof: symbols later in this file.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-sprof))

(defpackage #:datastar-cl/profile
  (:use #:cl)
  (:export #:profile-in-process
           #:profile-http
           #:*default-iterations*
           #:*default-report-type*))

(in-package #:datastar-cl/profile)

(defvar *default-iterations* 5000
  "Number of iterations for in-process profiling runs.")

(defvar *default-report-type* :flat
  "Default sb-sprof report style: :flat or :graph.")

(defvar *profile-payload* "{\"compression-test\": true, \"value\": 42}"
  "JSON payload used by the synthetic workload.")

;;; In-process synthetic workload

(defun make-synthetic-env ()
  "Minimal Clack env plist for the in-process synthetic workload."
  (list :request-method :get
        :path-info "/profile"
        :query-string nil
        :clack.io nil
        :headers (make-hash-table :test 'equal)))

(defun make-null-responder ()
  "Clack-style responder that discards all written bytes."
  (lambda (response-spec)
    (declare (ignore response-spec))
    (lambda (data &key close)
      (declare (ignore data close))
      nil)))

(defun in-process-iteration ()
  "Build one Clack generator, send one patch-signals event, close."
  (let ((gen (datastar-cl:make-clack-sse-generator
              (make-synthetic-env)
              (make-null-responder))))
    (datastar-cl:patch-signals gen *profile-payload*)
    (datastar-cl:close-sse-generator gen)))

;;; HTTP workload

(defun http-iteration (url &key accept-encoding)
  "Fire one HTTP GET to URL and discard the result.
   :force-binary t prevents Drakma from UTF-8-decoding a compressed body."
  (drakma:http-request url
                       :force-binary t
                       :additional-headers
                       (when accept-encoding
                         (list (cons "Accept-Encoding" accept-encoding)))))

;;; Entry points

(defun profile-in-process (&key (iterations *default-iterations*)
                                (mode :cpu)
                                (report *default-report-type*))
  "Profile the SDK in-process with ITERATIONS synthetic patch-signals calls.

   Uses the Clack backend for a pure serialization workload (no network/server overhead).
   For an end-to-end compressed-path profile, use PROFILE-HTTP.

   ITERATIONS: Number of calls (default: *default-iterations*).
   MODE: sb-sprof sampling mode -- :cpu (default) or :alloc.
   REPORT: sb-sprof report type -- :flat (default) or :graph."
  (format t "~&Profiling ~d in-process iterations (mode: ~a)...~%"
          iterations mode)
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode mode)
  (unwind-protect
       (loop repeat iterations do
         (in-process-iteration))
    (sb-sprof:stop-profiling))
  (sb-sprof:report :type report))

(defun profile-http (&key (iterations 500)
                          (url "http://localhost:7331/test-compression")
                          (accept-encoding "zstd")
                          (mode :cpu)
                          (report *default-report-type*))
  "Profile the SDK end-to-end via HTTP against a running test server.

   Requires the test servers to be running:
     (datastar-cl-tests::start-all-servers)

   URL: Target endpoint (default: port 7331 = Hunchentoot).
   ACCEPT-ENCODING: Header value (default: \"zstd\"). Pass NIL to disable.
   MODE: sb-sprof sampling mode -- :cpu (default) or :alloc.
   REPORT: sb-sprof report type -- :flat (default) or :graph.

   Backend ports:
     7331 = Hunchentoot (compression supported)
     7333 = Clack+Hunchentoot (compression supported)"
  (format t "~&Profiling ~d HTTP iterations (~a, mode: ~a)...~%"
          iterations url mode)
  (sb-sprof:reset)
  (sb-sprof:start-profiling :mode mode)
  (unwind-protect
       (loop repeat iterations do
         (http-iteration url :accept-encoding accept-encoding))
    (sb-sprof:stop-profiling))
  (sb-sprof:report :type report))
