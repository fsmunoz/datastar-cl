;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; ACTIONS.LISP --- Datastar HTML/action-string helpers
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl)

;;; LIBRARY INCLUSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *datastar-version* "1.0.1"
  "Default Datastar JS library version used by DATASTAR-URL.")

(defparameter *datastar-cdn-base* "https://cdn.jsdelivr.net/gh/starfederation/datastar"
  "Base CDN URL for Datastar bundles; override to point at a mirror or private CDN.")

(defparameter *datastar-source* nil
  "If non-NIL, a string used verbatim as the Datastar script src
(e.g. \"/static/datastar.js\" or a custom URL). If NIL, DATASTAR-URL
builds a URL from *DATASTAR-CDN-BASE* and *DATASTAR-VERSION*.")

(defun datastar-url (&key (version *datastar-version*)
                          (cdn-base *datastar-cdn-base*)
                          (source *datastar-source*))
  "Return the URL or path from which to load the Datastar JS library.
If SOURCE is non-NIL it is returned verbatim; otherwise a jsDelivr CDN URL is
built from CDN-BASE and VERSION."
  (or source
      (format nil "~A@~A/bundles/datastar.js" cdn-base version)))

;;; URL ACTION BUILDERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These produce Datastar action expression strings for use inside HTML
;;; attributes such as data-on-click="@get('/path')". The optional second
;;; argument is either a raw JS object literal string (passed verbatim) OR a
;;; plist that is serialised to a JS object literal:
;;;
;;; Plist serialisation rules (%JS-VALUE):
;;;   T           -> true
;;;   NIL         -> false
;;;   number      -> numeric literal
;;;   keyword     -> single-quoted lowercase string  (:form -> 'form')
;;;   string "/..." -> verbatim (JS regex literal)
;;;   string      -> single-quoted JS string
;;;   ((k . v)...)  -> alist: {'k': v, ...}  (string keys, for :headers)
;;;   (k v ...)   -> nested plist: {camelKey: v, ...}  (for :filter-signals etc.)
;;;
;;; I would really like to replace this with something... Parenscript, JZON,
;;; they fall short of what's needed, especially around filterSignals. This
;;; looks like JS but it's d* specific in several ways. The easy way out would
;;; be to drop this idiomatic syntactic sugar conversion, but I find it helpful.


;;; We can't use Hunchentoot escape functions here since core doesn't depend on
;;; it... this likely doesn't cover all possible edge cases.
(defun %escape-js-string (string)
  "Escape STRING for safe interpolation into a single-quoted JS string literal.
   \\ is replaced first to avoid double-escaping subsequent sequences.
   Newline, CR, and tab are escaped because single-quoted JS strings reject
   literal line terminators and control characters."
  (with-output-to-string (out)
    (loop for char across string do
      (case char
        (#\\ (write-string "\\\\" out))
        (#\' (write-string "\\'"  out))
        (#\Newline (write-string "\\n"  out))
        (#\Return  (write-string "\\r"  out))
        (#\Tab     (write-string "\\t"  out))
        (t   (write-char char out))))))

(defun %js-value (value)
  "Serialise a Lisp VALUE to a JavaScript literal string fragment."
  (typecase value
    (null    "false")
    ((eql t) "true")
    (number  (format nil "~a" value))
    (keyword (format nil "'~a'" (string-downcase value)))
    (string
     ;; filterSignals regexp literal: emit verbatim (always author-supplied)
     (if (a:starts-with #\/ value)
         value
         (format nil "'~a'" (%escape-js-string value))))
    (list
     (if (keywordp (car value))
         ;; plist: (:keyword value ...), camelCase identifier keys
         (%js-object value)
         ;; alist: ((string . value) ...), quoted string keys
         (format nil "{~{~a~^, ~}}"
                 (mapcar (lambda (pair)
                           (format nil "'~a': ~a" (%escape-js-string (car pair)) (%js-value (cdr pair))))
                         value))))
    (t (format nil "'~a'" value))))


(defun %js-object (plist)
  "Serialise a plist to a JavaScript object literal string."
  (if (null plist)
      "{}"
      (format nil "{~{~a~^, ~}}"
              (loop for (key value) on plist by #'cddr
                    collect (format nil "~a: ~a"
                                    (%kebab->camel (string-downcase key))
                                    (%js-value value))))))

;; The actions proper
(defun %sse-action (verb url &optional opts)
  (if opts
      (format nil "@~a('~a', ~a)" verb url
              (typecase opts
                (string opts)
                (list   (%js-object opts))))
      (format nil "@~a('~a')" verb url)))

(defun sse-get (url &optional opts)
  "Build a Datastar @get(...) action expression for use in HTML attributes.
OPTS is a raw JS object literal string or a plist, e.g.:
  (sse-get \"/ep\" '(:open-when-hidden t :content-type :form))"
  (%sse-action "get" url opts))

(defun sse-post (url &optional opts)
  "Build a Datastar @post(...) action expression for use in HTML attributes.
OPTS is a raw JS object literal string or a plist, e.g.:
  (sse-post \"/ep\" '(:headers ((\"X-Csrf-Token\" . \"tok\"))))"
  (%sse-action "post" url opts))

(defun sse-put (url &optional opts)
  "Build a Datastar @put(...) action expression for use in HTML attributes.
OPTS is a raw JS object literal string or a plist."
  (%sse-action "put" url opts))

(defun sse-patch (url &optional opts)
  "Build a Datastar @patch(...) action expression for use in HTML attributes.
OPTS is a raw JS object literal string or a plist."
  (%sse-action "patch" url opts))

(defun sse-delete (url &optional opts)
  "Build a Datastar @delete(...) action expression for use in HTML attributes.
OPTS is a raw JS object literal string or a plist."
  (%sse-action "delete" url opts))

;; Sometimes, just being able to put things into lists goes a long way... "We
;; like lists because we don't want to die." (Umberto Eco). Here, too, we
;; provide support for that immortality.
(defun ds-expr (&rest parts)
  "Builds a Datastar-ready string from many individual statements in PARTS."
  (format nil "~{~a~^; ~}" parts))

(defun init-signals (&rest plist)
  "Build a data-signals JSON string from a PLIST of key/value pairs.
   See %plist-to-nested-hash-table for key handling. Values follow JZON
   encoding: NIL -> false, T -> true, the symbol CL:NULL ('null) -> null."
  (jzon:stringify (%plist-to-nested-hash-table plist)))
