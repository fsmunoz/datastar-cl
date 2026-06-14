;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PACKAGE.LISP --- Integration test package, suite definitions, shared helpers
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/tests/integration
  (:use #:cl #:fiveam)
  (:shadow #:run-all-tests)
  (:local-nicknames (:d*   :datastar-cl)
                    (:flex :flexi-streams))
  (:export #:run-all-tests
           #:run-suite))

(in-package #:datastar-cl/tests/integration)

(def-suite :datastar-cl/integration
  :description "Integration tests requiring running test servers (HTTP)"
  :in :datastar-cl)

(def-suite :datastar-cl/integration/smoke
  :description "Seam smoke: datastar-cl protocol + lc-sse transport compose correctly"
  :in :datastar-cl/integration)

(def-suite :datastar-cl/integration/sdk
  :description "Official Datastar SDK conformance tests (requires go in PATH)"
  :in :datastar-cl/integration)

;;; --- Server management ---------------------------------------------------

(defvar *started-by-us* nil
  "T when the integration suite started the servers.")

(defun servers-reachable-p ()
  (flet ((check (port)
           (handler-case
               (progn
                 (drakma:http-request (format nil "http://localhost:~a/" port)
                                      :connection-timeout 2)
                 t)
             (error () nil))))
    (and (check 7331) (check 7333))))

(defun ensure-servers ()
  (unless (servers-reachable-p)
    (datastar-cl-tests::try-load-optional-compression)
    (datastar-cl-tests::start-all-servers)
    (setf *started-by-us* t)
    (loop for i from 1 to 20
          until (servers-reachable-p)
          do (sleep 0.5))))

;;; --- Streaming helpers ---------------------------------------------------

(defun cqrs-test (stream-url command-url probe settle)
  (let (body-stream
        (result (make-array 0 :element-type 'character :fill-pointer 0
                               :adjustable t)))
    (bt:make-thread
      (lambda ()
        (handler-case
            (let* ((raw (nth-value 0 (drakma:http-request stream-url :want-stream t)))
                   (cs  (flex:make-flexi-stream raw :external-format :utf-8)))
              (setf body-stream raw)
              (loop for c = (read-char cs nil :eof)
                    until (eq c :eof)
                    do (vector-push-extend c result)))
          (error () nil)))
      :name "cqrs-reader")
    (loop repeat 50 until body-stream do (sleep 0.1))
    (sleep settle)
    (drakma:http-request (format nil "~a?probe=~a" command-url probe) :method :post)
    (sleep 1)
    (when body-stream (ignore-errors (close body-stream)))
    result))

;;; --- Decompression -------------------------------------------------------

(defun decompress-zstd-bytes (bytes)
  (let* ((in  (flex:make-in-memory-input-stream bytes))
         (ds  (zstd:make-decompressing-stream in))
         (out (flex:make-in-memory-output-stream)))
    (handler-case
        (loop for byte = (read-byte ds nil nil)
              while byte
              do (write-byte byte out))
      (error () nil))
    (flex:octets-to-string (flex:get-output-stream-sequence out)
                           :external-format :utf-8)))

;;; --- HTTP header helper --------------------------------------------------

(defun get-header (name headers)
  "Return the value of HTTP header NAME (keyword) from HEADERS alist, or NIL."
  (cdr (assoc name headers)))

;;; --- Server URL helpers --------------------------------------------------

(defconstant +ht-port+    7331)
(defconstant +clack-port+ 7333)

(defun ht-url (path)
  (format nil "http://localhost:~a~a" +ht-port+ path))

(defun clack-url (path)
  (format nil "http://localhost:~a~a" +clack-port+ path))

;;; --- go binary availability ----------------------------------------------

(defun go-available-p ()
  (zerop (nth-value 2
           (uiop:run-program (list "which" "go")
                             :ignore-error-status t
                             :output nil :error-output nil))))

;;; --- Entry point ---------------------------------------------------------

(defun run-suite (suite)
  "Run a single integration test SUITE keyword, starting servers if needed."
  (let ((*started-by-us* nil))
    (ensure-servers)
    (run! suite)))

(defun run-all-tests ()
  (let ((*started-by-us* nil))
    (ensure-servers)
    (run! :datastar-cl/integration)))
