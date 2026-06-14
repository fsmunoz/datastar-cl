;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; EVENTS.LISP --- Datastar protocol events and patch methods
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl)

;;; TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use +event-types+ as the single source for event type checking
(deftype event-type ()
  `(member ,@(mapcar #'car +event-types+)))

(deftype patch-mode ()
  '(member :outer :inner :remove :replace
           :prepend :append :before :after))

(deftype namespace ()
  '(member :html :svg :mathml))

(deftype attributes ()
  '(or hash-table list))

;;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline event-type-wire-name))

(defun event-type-wire-name (event-type)
  "Returns the on-the-wire string for EVENT-TYPE keyword. Uses +EVENT-TYPES+."
  (check-type event-type event-type)
  (cdr (assoc event-type +event-types+)))

(defun prefix-data-lines (prefix text)
  "Return a list of strings, one for each line of TEXT, each prefixed with PREFIX."
  (mapcar (lambda (line)
            (concatenate 'string prefix " " line))
          (split-sequence:split-sequence #\Newline text)))

(defun %escape-html-attr (string)
  "Escape STRING for safe interpolation into a double-quoted HTML attribute value.
   & is replaced first to avoid double-escaping subsequent replacements."
  (with-output-to-string (out)
    (loop for char across string do
      (case char
        (#\& (write-string "&amp;"  out))
        (#\" (write-string "&quot;" out))
        (#\< (write-string "&lt;"   out))
        (#\> (write-string "&gt;"   out))
        (t   (write-char char out))))))

;; Used by EXECUTE-SCRIPT. CL-WHO and Spinneret do this, but we are not
;; depending on them, so this helps.
(defun build-script-tag (script &key attributes auto-remove)
  "Build HTML <script> tag string with optional ATTRIBUTES and AUTO-REMOVE behavior.
   ATTRIBUTES is a hash-table (how it gets here: JZON parses the JSON object
   {\"k\":\"v\",...}) or a plist for direct Lisp callers, e.g. '(:type
   \"text/javascript\")."
  (with-output-to-string (out)
    (write-string "<script" out)
    ;; Accept both lists and hashes
    (etypecase attributes
      (null)
      (hash-table
       (maphash (lambda (key val)
                  (format out " ~a=\"~a\""
                          (%escape-html-attr (princ-to-string key))
                          (%escape-html-attr (princ-to-string val))))
                attributes))
      (list
       (a:doplist (key val attributes)
         (format out " ~a=\"~a\""
                 (%escape-html-attr (string-downcase key))
                 (%escape-html-attr (princ-to-string val))))))
    (when auto-remove (write-string " data-effect=\"el.remove()\"" out))
    (format out ">~a</script>" script)))

;; There's a kebab library, but we just need this single path.
(defun %kebab->camel (string)
  "Convert a kebab-case STRING to camelCase (e.g. \"selected-planet\" -> \"selectedPlanet\")."
  (let ((parts (split-sequence:split-sequence #\- (string-downcase string))))
    ;; ~:(~a~) capitalizes each part, cf. CLHS and STRING-CAPITALIZE
    (format nil "~a~{~:(~a~)~}" (first parts) (rest parts))))

;; This is needed because "RFC 7386 JSON Merge Patch" is used, and that uses
;; nested objects - trying to use a "flat" approach would fail (tried). The
;; specifics of this (especially on the set-signal-in side) make using an
;; external library (serapeum, access, ...)not worth it.

(defun %set-signal-in (hash-table path value)
  "Set VALUE in nested HASH-TABLE following dot-notated PATH string
   (\"glitch.active\" nests under \"glitch\")."
  ;; "OnLisp", p.21 - labels.
  (labels ((rec (ht parts)
             (let ((key (car parts))
                   (rest (cdr parts)))
               (if rest
                   (rec (a:ensure-gethash key ht (make-hash-table :test 'equal))
                        rest)
                   (setf (gethash key ht) value)))))
    (rec hash-table (split-sequence:split-sequence #\. path))))

(defun %plist-to-nested-hash-table (plist)
  "Convert a raw PLIST to a nested hash-table, normalizing keys: symbol keys
   are kebab->camel cased (:show-details -> \"showDetails\"); string keys pass
   through and may use dot-notation to nest
   (\"glitch.active\" -> {\"glitch\":{\"active\":...}})."
  (let ((result (make-hash-table :test 'equal)))
    (a:doplist (k v plist result)
      (let ((key (etypecase k
                   (string k)
                   (symbol (%kebab->camel (symbol-name k))))))
        (%set-signal-in result key v)))))

;;; Generic Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric patch-elements (generator elements
                            &key selector mode namespace
                                 use-view-transition view-transition-selector
                                 event-id retry-duration)
  (:documentation "Patch HTML ELEMENTS into the DOM using SSE."))

(defgeneric patch-signals (generator signals &key only-if-missing event-id retry-duration)
  (:documentation "Patch SIGNALS into the DOM using SSE."))

(defgeneric execute-script (generator script &key auto-remove attributes event-id retry-duration)
  (:documentation "Send a SCRIPT to the client for immediate execution via SSE."))

;;; Method Implementations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod patch-elements ((generator lc-sse:sse-generator) elements
                           &key selector
                             (mode *default-patch-mode*)
                             (namespace *default-namespace*)
                             use-view-transition
                             view-transition-selector
                             event-id
                             retry-duration)
  "Patch HTML elements into the DOM.

   ELEMENTS may be:
     NIL    -- send only mode/selector/transition metadata (no elements lines).
     STRING -- one or more HTML fragments; newlines produce multiple data lines.
     LIST   -- each string element produces its own elements data line(s).

   VIEW-TRANSITION-SELECTOR is emitted only when USE-VIEW-TRANSITION is also true.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
  (check-type mode patch-mode)
  (check-type namespace namespace)
  (check-type event-id event-id)
  (check-type retry-duration retry-duration)
  (let ((data-lines
          (append
           (when selector
             (list (format nil "selector ~a" selector)))
           (unless (eq mode :outer)
             (list (format nil "mode ~a" (string-downcase mode))))
           (when use-view-transition
             (list "useViewTransition true"))
           (when (and view-transition-selector use-view-transition)
             (list (format nil "viewTransitionSelector ~a" view-transition-selector)))
           (unless (eq namespace :html)
             (list (format nil "namespace ~a" (string-downcase namespace))))
           (etypecase elements
             (null   nil)
             (string (prefix-data-lines "elements" elements))
             (list   (loop for el in elements
                           append (prefix-data-lines "elements" el)))))))
    (when data-lines
      (send-event generator (event-type-wire-name :datastar-patch-elements) data-lines
                  :event-id event-id
                  :retry-duration retry-duration))))

(defun remove-element (generator selector
                       &key use-view-transition event-id retry-duration)
  "Remove element(s) matching SELECTOR from the DOM.
   Convenience wrapper around PATCH-ELEMENTS with mode \"remove\"; not an extension point."
  (patch-elements generator nil
                  :selector selector
                  :mode :remove
                  :use-view-transition use-view-transition
                  :event-id event-id
                  :retry-duration retry-duration))

(defmethod patch-signals ((generator lc-sse:sse-generator) (signals string)
                          &key only-if-missing
                            event-id
                            retry-duration)
  "Patch signals from JSON string into the DOM.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
  (check-type event-id event-id)
  (check-type retry-duration retry-duration)
  (let ((data-lines
          (append
           (when only-if-missing
             (list "onlyIfMissing true"))
           (prefix-data-lines "signals" signals))))
    (send-event generator (event-type-wire-name :datastar-patch-signals) data-lines
                :event-id event-id
                :retry-duration retry-duration)))

(defmethod patch-signals ((generator lc-sse:sse-generator) (signals hash-table)
                          &key only-if-missing event-id retry-duration)
  "Patch signals from a hash-table: serialize to JSON and delegate to the
   string method (the canonical patch path)."
  (patch-signals generator (jzon:stringify signals)
                 :only-if-missing only-if-missing
                 :event-id event-id
                 :retry-duration retry-duration))

(defmethod patch-signals ((generator lc-sse:sse-generator) (signals list)
                          &key only-if-missing event-id retry-duration)
  "Patch signals from a plist; see %plist-to-nested-hash-table for key handling."
  (patch-signals generator
                 (%plist-to-nested-hash-table signals)
                 :only-if-missing only-if-missing
                 :event-id event-id
                 :retry-duration retry-duration))

(defmethod execute-script ((generator lc-sse:sse-generator) (script string)
                           &key (auto-remove *default-auto-remove*)
                             attributes
                             event-id
                             retry-duration)
  "Execute a script on the client side via DOM injection.

   Signals: STREAM-ERROR if SSE transmission fails (via PATCH-ELEMENTS)."
  (check-type attributes attributes)
  (check-type event-id event-id)
  (check-type retry-duration retry-duration)
  (patch-elements generator
                  (build-script-tag script
                                    :attributes attributes
                                    :auto-remove auto-remove)
                  :mode :append
                  :selector "body"
                  :event-id event-id
                  :retry-duration retry-duration))

;;; Convenience helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun console-log (generator message &key event-id retry-duration)
  "Emit a console.log call in the browser via EXECUTE-SCRIPT.
MESSAGE is JSON-encoded to produce a safe JS string literal.
Convenience wrapper; not an extension point (specialize EXECUTE-SCRIPT instead)."
  (execute-script generator
                  (format nil "console.log(~a)" (jzon:stringify message))
                  :event-id event-id
                  :retry-duration retry-duration))

(defun console-error (generator message &key event-id retry-duration)
  "Emit a console.error call in the browser via EXECUTE-SCRIPT.
MESSAGE is JSON-encoded to produce a safe JS string literal.
Convenience wrapper; not an extension point (specialize EXECUTE-SCRIPT instead)."
  (execute-script generator
                  (format nil "console.error(~a)" (jzon:stringify message))
                  :event-id event-id
                  :retry-duration retry-duration))

(defun redirect (generator url &key event-id retry-duration)
  "Redirect the browser to URL via EXECUTE-SCRIPT.
URL is JSON-encoded to produce a safe JS string literal.
Convenience wrapper; not an extension point (specialize EXECUTE-SCRIPT instead)."
  (execute-script generator
                  (format nil "setTimeout(() => window.location.href = ~a)"
                          (jzon:stringify url))
                  :event-id event-id
                  :retry-duration retry-duration))
