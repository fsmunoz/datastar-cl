;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SNOOZE.LISP --- Snooze integration
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(defpackage #:datastar-cl/snooze
  (:use #:cl)
  (:local-nicknames (:a :alexandria))
  (:export #:with-sse
           #:with-signals
           #:read-signals
           #:defresource
           #:defroute))

(in-package #:datastar-cl/snooze)

;;; Provides macros for use inside SNOOZE:DEFROUTE bodies that dispatch on
;;; SNOOZE:*BACKEND* at runtime to produce the right SSE return value for
;;; Hunchentoot or Clack. The same route source works under both backends.
;;;
;;; Requires the Snooze make-clack-app function-payload patch
;;; (clack-stream branch / upstream PR) for the Clack arm to work.
;;;
;;; Usage:
;;;   (datastar-cl/snooze:defresource my-stream (verb ct &key datastar))
;;;
;;;   (datastar-cl/snooze:defroute my-stream (:get :text/event-stream &key datastar)
;;;     ;; (declare (ignore datastar)) is injected automatically by defroute
;;;     (let ((signals (datastar-cl/snooze:read-signals)))
;;;       (datastar-cl/snooze:with-sse (gen)
;;;         (when signals
;;;           (datastar-cl:patch-signals gen signals)))))

(defmacro with-sse ((gen &rest options) &body body)
  "Open an SSE response inside a Snooze route body.

Dispatches on SNOOZE:*BACKEND* at runtime:

- :HUNCHENTOOT -- opens an SSE response against HUNCHENTOOT:*REQUEST*,
  exactly like DATASTAR-CL:WITH-SSE.

- :CLACK -- captures SNOOZE:*CLACK-REQUEST-ENV* into a lexical, then
  returns a streaming-responder closure. Snooze passes this closure
  through to Clack (requires the make-clack-app function-payload patch
  on the clack-stream branch, or upstream Snooze once that PR merges).

OPTIONS are forwarded unchanged to DATASTAR-CL:WITH-SSE:
  :on-connect    function (gen) called once before BODY
  :on-disconnect function (gen) called once after BODY exits

GEN is bound to the SSE generator inside BODY. Body runs once;
connection lifetime is determined by what the body does."
  `(if (eq snooze:*backend* :clack)
       (let ((env snooze:*clack-request-env*))
         (lambda (responder)
           (datastar-cl:with-sse (,gen (env responder) ,@options)
             ,@body)))
       (datastar-cl:with-sse (,gen hunchentoot:*request* ,@options)
         ,@body)))

(defmacro defresource (name args &rest options)
  "Like SNOOZE:DEFRESOURCE but adds a READ-FOR-RESOURCE method that passes
JSON object values through as raw strings for this resource.

Datastar appends &datastar={...} to requests. Snooze's safe reader misreads
the { as the start of a package-qualified symbol and signals an error. Only
values starting with '{' are bypassed; everything else still goes through
Snooze's safe-simple-read-from-string, preserving its anti-DOS and
no-symbol-interning protections."
  `(progn
     (snooze:defresource ,name ,args ,@options)
     (defmethod snooze:read-for-resource ((resource (eql (function ,name)))
                                          (value string))
       ;; This is a naive, simple check to determine if the payload is
       ;; JSON It could be completely ignored like:
       ;;
       ;; (handler-case (call-next-method) (error () value)))))
       ;;
       ;; ... but then this would apply to all keyword parameters
       ;; passed, not only "datastar". This is cheap
       (if (a:starts-with #\{ value)
           value
           (call-next-method)))))

(defun read-signals ()
  "Read Datastar signals from the current Snooze request.

Dispatches on SNOOZE:*BACKEND*: reads from HUNCHENTOOT:*REQUEST* on
:HUNCHENTOOT, or SNOOZE:*CLACK-REQUEST-ENV* on :CLACK. Equivalent to
calling DATASTAR-CL:READ-SIGNALS with the appropriate request object."
  (ecase snooze:*backend*
    (:hunchentoot (datastar-cl:read-signals hunchentoot:*request*))
    (:clack       (datastar-cl:read-signals snooze:*clack-request-env*))))

(defun %find-datastar-sym (specialized-lambda)
  "Return the DATASTAR symbol from the &key section of SPECIALIZED-LAMBDA, or NIL.
String comparison is used so the symbol can come from any package."
  (let ((key-pos (position '&key specialized-lambda)))
    (when key-pos
      (some (lambda (item)
              (let ((sym (if (consp item) (first item) item)))
                (when (and (symbolp sym)
                           (string-equal (symbol-name sym) "DATASTAR"))
                  sym)))
            (subseq specialized-lambda (1+ key-pos))))))

(defun %inject-ignore (sym body)
  "Insert (declare (ignore SYM)) into BODY, after any leading docstring."
  (if (and body (stringp (first body)))
      (list* (first body) `(declare (ignore ,sym)) (rest body))
      (list* `(declare (ignore ,sym)) body)))

;; Macro approach
;;
;; Reference: https://github.com/joaotavora/snooze/issues/24
;;
;; > But you can implement the macro MANFRED:DEFROUTE* that does exactly what
;; > you want.This macro would emit not only the SNOOZE:DEFROUTE form but also
;; > two method additions for the SNOOZE:URI-TO-ARGUMENTS and
;; > SNOOZE:ARGUMENTS-TO-URI generic functions.
;;
;; Edge cases handled:
;;
;; - Routes without &key datastar (e.g. (:get :text/html)): no injection, macro
;;   is transparent
;;
;; - Routes with a docstring before the body: declare is inserted after the
;;   string
;;
;; - Mixed ignores ((declare (ignore datastar person)) in 404 fallbacks):
;;   auto-inject covers datastar, user writes (declare (ignore person)) for the
;;   path arg; two declare forms in a body is valid CL

(defmacro defroute (name specialized-lambda &body body)
  "Like SNOOZE:DEFROUTE but auto-inserts (declare (ignore datastar)) when
&key DATASTAR appears in SPECIALIZED-LAMBDA.

Datastar passes signals as &datastar=<JSON>. SNOOZE:DEFROUTE exposes this as
a keyword parameter, but the value is always consumed via READ-SIGNALS -- never
directly -- so declaring it ignored is structural noise. This macro eliminates it.

When the route also needs to ignore other parameters (e.g. a 404 fallback that
ignores a path arg), write a separate (declare (ignore ...)) for those; only
the DATASTAR ignore is injected automatically."
  (let ((ds-sym (%find-datastar-sym specialized-lambda)))
    `(snooze:defroute ,name ,specialized-lambda
       ,@(if ds-sym
             (%inject-ignore ds-sym body)
             body))))

(defmacro with-signals (bindings &body body)
  "Bind local variables to Datastar signal values from the current Snooze request.

   BINDINGS is a list of (VAR KEY) or (VAR KEY DEFAULT) entries:
     VAR     - local variable name to bind.
     KEY     - signal-name string (e.g. \"userId\").
     DEFAULT - value when the signal is absent or signals are NIL (default NIL).

   READ-SIGNALS is called exactly once; the backend is resolved from
   SNOOZE:*BACKEND* automatically (no request argument needed), consistent with
   the rest of the DATASTAR-CL/SNOOZE package.

   Note: unlike DATASTAR-CL:WITH-SIGNALS, this macro takes NO request argument
   because the Snooze package resolves the request via dynamic variables.

   Example:
     (with-signals ((name \"name\" \"stranger\") (greeting \"greeting\" \"Hello\"))
       (d*:patch-signals gen (list \"message\"
                                   (format nil \"~a, ~a!\" greeting name))))"
  (let ((signals-var (gensym "SIGNALS-")))
    `(let ((,signals-var (read-signals)))
       (let ,(datastar-cl:%signal-binding-clauses bindings signals-var)
         ,@body))))
