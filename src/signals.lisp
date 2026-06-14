;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SIGNALS.LISP --- Client->server signal extraction and parsing
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl)

;;; PUBLIC API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric datastar-request-p (request-or-env)
  (:documentation "Return T if REQUEST-OR-ENV originates from the Datastar client.
   The Datastar JS client marks every request with a Datastar-Request: true header."))

(defgeneric read-signals (request-or-env &key catch-errors)
  (:documentation "Extract and parse Datastar signals from an HTTP request.

   Returns (values signals reason):

     SIGNALS - the parsed hash-table, or NIL if no payload or on error.
     REASON  - NIL normally; the DATASTAR-ERROR condition when CATCH-ERRORS
               \"swallows\" a parse failure.

   (values nil nil) = no datastar payload present.
   (values nil  c ) = parse error swallowed (CATCH-ERRORS was true).
   (values  ht nil) = parsed signals hash-table.

   Signals INVALID-JSON-ERROR if JSON parsing fails and CATCH-ERRORS is NIL.
   The CATCH-ERRORS policy (default *CATCH-ERRORS-P*) is implemented in the
   :around method (for all backends); primary methods perform raw extraction
   only."))

(defmethod read-signals :around (request-or-env &key (catch-errors *catch-errors-p*))
  "Uniform catch-errors policy for all read-signals methods.
   When CATCH-ERRORS, captures DATASTAR-ERROR and returns (values NIL
   condition), otherwise the error propagates; on success returns (values
   <hash-or-nil> NIL)."
  (declare (ignore request-or-env))
  (if catch-errors
      (handler-case (values (call-next-method) nil)
        (datastar-error (c)
          (format *error-output* "~&[datastar-cl] ~A~%" c)
          (values nil c)))
      (call-next-method)))

;; Convenience wrapper: calls read-signals and guards against NIL (no payload).
(defun read-signal (request-or-env key &optional default)
  "Read a single Datastar signal KEY (a string, e.g. \"sid\") from REQUEST-OR-ENV.

   Returns (values value reason):
     VALUE  - the signal value, or DEFAULT when signals are absent or KEY is not
              found. DEFAULT is NIL when not specified.
     REASON - propagated from READ-SIGNALS (NIL normally, or the DATASTAR-ERROR
              condition when a parse error was swallowed).

   NIL-safe: if READ-SIGNALS returns NIL, returns (values DEFAULT reason).
   For reading several signals, call READ-SIGNALS once and use GETHASH to
   avoid re-parsing."
  (multiple-value-bind (signals reason) (read-signals request-or-env)
    (values (if signals (gethash key signals default) default)
            reason)))

(defun %signal-binding-clauses (bindings signals-var)
  "Expand BINDINGS into a list of LET clauses keyed into SIGNALS-VAR.
   Each binding is (VAR KEY) or (VAR KEY DEFAULT)."
  (mapcar (lambda (binding)
            (destructuring-bind (var key &optional default) binding
              `(,var (if ,signals-var
                         (gethash ,key ,signals-var ,default)
                         ,default))))
          bindings))

(defmacro with-signals (bindings request-or-env &body body)
  "Bind local variables to Datastar signal values from REQUEST-OR-ENV.

   BINDINGS is a list of (VAR KEY) or (VAR KEY DEFAULT) entries:
     VAR     - the local variable name to bind.
     KEY     - the signal-name string (e.g. \"userId\").
     DEFAULT - value when the signal is absent or signals are NIL (default NIL).

   READ-SIGNALS is called exactly once regardless of binding count; each VAR
   receives (GETHASH KEY signals DEFAULT), or DEFAULT when no signals are
   present.  For the parse-error REASON value, call READ-SIGNALS directly.

   Argument order mirrors WITH-SLOTS and MULTIPLE-VALUE-BIND: bindings first,
   source second.

   Examples:
     ;; Read two signals; id defaults to a generated uuid if absent
     (with-signals ((name \"name\") (sid \"sid\" (make-uuid)))
         hunchentoot:*request*
       (register-client sid name))

     ;; All bindings default to nil when no signals are present
     (with-signals ((mode \"mode\") (limit \"limit\" 20))
         hunchentoot:*request*
       (query-results mode limit))"
  (let ((signals-var (gensym "SIGNALS-")))
    `(let ((,signals-var (read-signals ,request-or-env)))
       (let ,(%signal-binding-clauses bindings signals-var)
         ,@body))))

;;; EXTRACTION LAYER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extract-json-data (request method)
  (:documentation "Extract raw JSON data from request based on HTTP method.
Returns string of JSON data or NIL if no data present.
Signals conditions for error cases (empty body, etc.)."))

;; Used by all (?) backends
(defun parse-and-validate-json (json-string)
  "Parse JSON-STRING and validate it's a hash-table (JSON object).
   Returns hash-table or signals INVALID-JSON-ERROR.

   Per ADR: ReadSignals must return error for invalid JSON."
  (handler-case
      (let ((parsed (jzon:parse json-string)))
        ;; Validate that result is a hash-table (JSON object). This
        ;; should be the case when using jzon:parse with key:value
        (unless (hash-table-p parsed)
          (error 'invalid-json-error
                 :json-string json-string
                 :message (format nil "Expected JSON object, got ~A"
                                (type-of parsed))))
        parsed)
    (jzon:json-parse-error (e)
      (error 'invalid-json-error
             :json-string json-string
             :message (format nil "~A" e)))))
