;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; CONDITIONS.LISP --- Datastar SDK conditions and error handling
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl)

;;; Check https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node339.html
;;; and https://lispcookbook.github.io/cl-cookbook/cl-cookbook/error_handling.html
;;; for an overview of the Common Lisp condition system.

;;; ADR Compliance Notes:
;;;
;;; 1. ReadSignals: "Must return error for invalid JSON" ->
;;; Implemented via INVALID-JSON-ERROR condition
;;;
;;; 2. ServerSentEventGenerator.send (ADR line 99): "Must return/throw
;;;     per language conventions" -> Implemented via CL's standard
;;;     STREAM-ERROR condition (no wrapping done)
;;;
;;; Note: COMPRESSION-ERROR is defined in lc-sse (src/sse/conditions.lisp)
;;; and re-exported from this package via the lc-sse use relationship.

(define-condition datastar-error (error)
  ((message :initarg :message
            :initform nil
            :accessor datastar-error-message
            :documentation "Human-readable error message."))
  (:documentation "Base condition for datastar-cl errors.")
  (:report (lambda (condition stream)
             (format stream "Datastar error~@[: ~A~]"
                     (datastar-error-message condition)))))

(define-condition invalid-json-error (datastar-error)
  ((json-string :initarg :json-string
                :accessor invalid-json-error-json-string
                :documentation "The invalid JSON string."))
  (:documentation "Signaled when JSON parsing fails.")
  (:report (lambda (condition stream)
             (format stream "Invalid JSON in datastar signals~@[: ~A~] (JSON: ~S)"
                     (datastar-error-message condition)
                     (invalid-json-error-json-string condition)))))
