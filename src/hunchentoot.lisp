;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; HUNCHENTOOT.LISP --- Hunchentoot backend: Datastar signal extraction
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; The generic Hunchentoot SSE machinery (hunchentoot-sse-generator class,
;;;; initialize-instance, close-sse-generator, tcp-nodelay-mixin, constructors)
;;;; lives in lc-sse (src/sse/hunchentoot.lisp). This file adds only the
;;;; Datastar-protocol methods that specialize on hunchentoot:request.

(in-package #:datastar-cl)

(defmethod read-signals ((request hunchentoot:request) &key catch-errors)
  "Extract raw signals from a Hunchentoot request. Error policy lives in the
   :around method defined in signals.lisp."
  (declare (ignore catch-errors))
  (let ((json (extract-json-data request (hunchentoot:request-method request))))
    (when json (parse-and-validate-json json))))

;;; Hunchentoot - GET method
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :get)))
  "Extract JSON from Hunchentoot GET request query string."
  (let ((datastar-param (hunchentoot:get-parameter "datastar" request)))
    (when datastar-param
      (if (a:emptyp datastar-param)
          (error 'invalid-json-error
                 :json-string ""
                 :message "Empty datastar query parameter")
          datastar-param))))

;;; Hunchentoot - POST method
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :post)))
  "Extract JSON from Hunchentoot POST request body."
  (let ((raw-data (hunchentoot:raw-post-data :request request :force-text t)))
    (if (or (null raw-data) (a:emptyp raw-data))
        (error 'invalid-json-error
               :json-string ""
               :message "Empty POST request body")
        raw-data)))

;;; Hunchentoot - DELETE (signals in query string, same as GET)
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :delete)))
  (extract-json-data request :get))

;;; Hunchentoot - PUT (signals in body, same as POST)
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :put)))
  (extract-json-data request :post))

;;; Hunchentoot - PATCH (signals in body, same as POST)
(defmethod extract-json-data ((request hunchentoot:request) (method (eql :patch)))
  (extract-json-data request :post))

(defmethod datastar-request-p ((request hunchentoot:request))
  (string-equal "true" (hunchentoot:header-in* :datastar-request request)))
