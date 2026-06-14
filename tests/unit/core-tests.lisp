;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; CORE-TESTS.LISP --- Core SDK unit tests
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/tests/unit)

(in-suite :datastar-cl/core)

;;; What we should cover:
;;;   - datastar-url composition (*datastar-version*, *datastar-cdn-base*, *datastar-source*)
;;;   - SSE event formatting (send-event output shape)
;;;   - patch-elements / patch-signals output
;;;   - *default-compression-priority* defaults
;;;
;;; NB: the official Go SDK test suite is also run and that tests the
;;; API itself; we might add to it here, but initial focus in on
;;; internal aspects.

(setf fiveam:*run-test-when-defined* t)

(test build-script-tag-bare
  (is (string= "<script>alert(1)</script>"
               (datastar-cl::build-script-tag "alert(1)"))))

;; build-script-tag
(test build-script-tag-auto-remove
  (is (string= "<script data-effect=\"el.remove()\">alert(1)</script>"
               (datastar-cl::build-script-tag "alert(1)" :auto-remove t))))

(test build-script-tag-attributes-hash-table
  "jzon parses the JSON object {\"type\":\"text/javascript\"} as a hash-table."
  (let ((attrs (a:plist-hash-table '("type" "text/javascript") :test 'equal)))
    (is (string= "<script type=\"text/javascript\">alert(1)</script>"
                 (datastar-cl::build-script-tag "alert(1)" :attributes attrs)))))

(test build-script-tag-attributes-plist
  "Lisp caller path: keyword plist is more ergonomic than constructing a hash-table."
  (is (string= "<script type=\"text/javascript\">alert(1)</script>"
               (datastar-cl::build-script-tag "alert(1)"
                 :attributes '(:type "text/javascript")))))

(test build-script-tag-attributes-and-auto-remove
  (let ((attrs (a:plist-hash-table '("type" "text/javascript") :test 'equal)))
    (is (string= "<script type=\"text/javascript\" data-effect=\"el.remove()\">alert(1)</script>"
                 (datastar-cl::build-script-tag "alert(1)"
                   :attributes attrs
                   :auto-remove t)))))


;;; %plist-to-nested-hash-table, structural tests (no JSON
;;; %serialization order concerns)

(test plist-to-nested-hash-table-flat-key
  "A key without dots stays at the top level."
  (let ((ht (d*::%plist-to-nested-hash-table '("count" 5))))
    (is (= 5 (gethash "count" ht)))))

(test plist-to-nested-hash-table-single-dotted-key
  "A dotted key nests one level deep."
  (let* ((ht (d*::%plist-to-nested-hash-table '("glitch.active" t)))
         (inner (gethash "glitch" ht)))
    (is (hash-table-p inner))
    (is (eq t (gethash "active" inner)))))

(test plist-to-nested-hash-table-deep-nesting
  "A multi-dot key nests multiple levels."
  (let ((ht (d*::%plist-to-nested-hash-table '("a.b.c" 42))))
    (is (= 42 (gethash "c" (gethash "b" (gethash "a" ht)))))))

;;; init-signals - JSON output tests

(test init-signals-symbol-key
  "Symbol keys are camelCased and remain top-level."
  (is (string= "{\"showX\":true}"
               (d*:init-signals :show-x t))))

(test init-signals-dotted-string-key
  "A dotted string key produces nested JSON output."
  (is (string= "{\"glitch\":{\"active\":true}}"
               (d*:init-signals "glitch.active" t))))

(test init-signals-deep-nesting
  "Three-level dot-notation produces three-deep JSON."
  (is (string= "{\"a\":{\"b\":{\"c\":1}}}"
               (d*:init-signals "a.b.c" 1))))

;;; Action helpers
(test sse-get-url
  (is (string=
       (d*:sse-get "https://lambda-combine.net")
       "@get('https://lambda-combine.net')")))

(test sse-get-url-options
  "A raw string opts argument is passed through verbatim."
  (is (string=
       (d*:sse-get "https://lambda-combine.net" "options")
       "@get('https://lambda-combine.net', options)")))

(test sse-post-url
  (is (string=
       (d*:sse-post "https://lambda-combine.net")
       "@post('https://lambda-combine.net')")))

(test sse-put-url
  (is (string=
       (d*:sse-put "https://lambda-combine.net")
       "@put('https://lambda-combine.net')")))

(test sse-patch-url
  (is (string=
       (d*:sse-patch "https://lambda-combine.net")
       "@patch('https://lambda-combine.net')")))

;;; %js-value -- one case per branch

(test js-value-true
  (is (string= "true" (d*::%js-value t))))

(test js-value-false
  (is (string= "false" (d*::%js-value nil))))

(test js-value-number
  (is (string= "2000" (d*::%js-value 2000))))

(test js-value-keyword
  "Keywords become single-quoted lowercase JS strings."
  (is (string= "'form'" (d*::%js-value :form))))

(test js-value-plain-string
  (is (string= "'hello'" (d*::%js-value "hello"))))

(test js-value-regex-string
  "A string starting with / is emitted verbatim as a JS regex literal."
  (is (string= "/^foo$/" (d*::%js-value "/^foo$/"))))

(test js-value-alist
  "An alist of (string . value) pairs becomes a JS object with quoted string keys."
  (is (string= "{'X-Csrf-Token': 'tok'}"
               (d*::%js-value '(("X-Csrf-Token" . "tok"))))))

(test js-value-nested-plist
  "A list whose car is a keyword is serialised as a nested JS object."
  (is (string= "{include: /^foo$/}"
               (d*::%js-value '(:include "/^foo$/")))))

;;; %js-object -- structural tests

(test js-object-empty
  (is (string= "{}" (d*::%js-object nil))))

(test js-object-single-pair
  "Kebab-case key is converted to camelCase."
  (is (string= "{openWhenHidden: true}"
               (d*::%js-object '(:open-when-hidden t)))))

(test js-object-multiple-pairs
  "Multiple pairs are emitted in insertion order."
  (is (string= "{a: 1, b: 2}"
               (d*::%js-object '(:a 1 :b 2)))))

;;; sse-* helpers with plist opts

(test sse-get-plist-boolean
  (is (string= "@get('/ep', {openWhenHidden: true})"
               (d*:sse-get "/ep" '(:open-when-hidden t)))))

(test sse-get-plist-keyword-value
  (is (string= "@get('/ep', {contentType: 'form'})"
               (d*:sse-get "/ep" '(:content-type :form)))))

(test sse-get-plist-multiple-opts
  (is (string= "@get('/ep', {openWhenHidden: true, contentType: 'form'})"
               (d*:sse-get "/ep" '(:open-when-hidden t :content-type :form)))))

(test sse-post-plist-headers
  "Headers alist is emitted with quoted string keys."
  (is (string= "@post('/ep', {headers: {'X-Csrf-Token': 'tok'}})"
               (d*:sse-post "/ep" '(:headers (("X-Csrf-Token" . "tok")))))))

(test sse-get-plist-filter-signals-regex
  "filterSignals with a regex literal (verbatim string starting with /) round-trips correctly."
  (is (string= "@get('/ep', {filterSignals: {include: /^foo$/}})"
               (d*:sse-get "/ep" '(:filter-signals (:include "/^foo$/"))))))

;;; patch-elements, string argument

(test patch-elements-string-single
  "A single string produces one elements data line."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div id=a>hello</div>")
    (let ((r (get-output-stream-string stream)))
      (is (search "event: datastar-patch-elements" r))
      (is (search "data: elements <div id=a>hello</div>" r)))))

(test patch-elements-string-multiline
  "A string with newlines produces multiple elements data lines."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div id=a>a</div>
<div id=b>b</div>")
    (let ((r (get-output-stream-string stream)))
      (is (search "data: elements <div id=a>a</div>" r))
      (is (search "data: elements <div id=b>b</div>" r)))))

(test patch-elements-string-with-selector
  "String with selector produces selector data line too."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div>content</div>" :selector "#container")
    (let ((r (get-output-stream-string stream)))
      (is (search "selector #container" r))
      (is (search "data: elements <div>content</div>" r)))))

;;; patch-elements, list argument

(test patch-elements-list-single
  "A single-element list produces the same SSE output as a string."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen '("<div id=a>hello</div>"))
    (let ((r (get-output-stream-string stream)))
      (is (search "event: datastar-patch-elements" r))
      (is (search "data: elements <div id=a>hello</div>" r)))))

(test patch-elements-list-multiple
  "Multiple list elements each become their own elements data line."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen '("<div id=a>a</div>" "<div id=b>b</div>"))
    (let ((r (get-output-stream-string stream)))
      (is (search "data: elements <div id=a>a</div>" r))
      (is (search "data: elements <div id=b>b</div>" r)))))

(test patch-elements-list-with-selector
  "List elements with selector produce selector data line too."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen '("<div>a</div>" "<div>b</div>") :selector "#container")
    (let ((r (get-output-stream-string stream)))
      (is (search "selector #container" r))
      (is (search "data: elements <div>a</div>" r))
      (is (search "data: elements <div>b</div>" r)))))

(test patch-elements-list-empty
  "An empty list produces no SSE output."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen '())
    (is (string= "" (get-output-stream-string stream)))))

;;; patch-elements: namespace

(test patch-elements-namespace-svg
  "Non-default namespace emits a namespace data line."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<circle/>" :namespace :svg)
    (let ((r (get-output-stream-string stream)))
      (is (search "data: namespace svg" r)
          "Expected 'namespace svg' in output, got: ~s" r))))

(test patch-elements-namespace-html-default-omitted
  "Default :html namespace must NOT emit a namespace line (only non-defaults)."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div/>" :namespace :html)
    (let ((r (get-output-stream-string stream)))
      (is (not (search "namespace" r))
          "Expected no 'namespace' line for default :html, got: ~s" r))))

;;; patch-elements: viewTransitionSelector

(test patch-elements-view-transition-selector-emitted
  "viewTransitionSelector is emitted when use-view-transition is true and selector provided."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div/>"
                       :use-view-transition t
                       :view-transition-selector "#main")
    (let ((r (get-output-stream-string stream)))
      (is (search "data: viewTransitionSelector #main" r)
          "Expected 'viewTransitionSelector #main' in output, got: ~s" r))))

(test patch-elements-view-transition-selector-suppressed-without-transition
  "viewTransitionSelector must NOT be emitted when use-view-transition is nil."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<div/>"
                       :use-view-transition nil
                       :view-transition-selector "#main")
    (let ((r (get-output-stream-string stream)))
      (is (not (search "viewTransitionSelector" r))
          "Expected no 'viewTransitionSelector' line when use-view-transition nil, got: ~s" r))))

;;; patch-elements: ADR data-line order
;;; ADR mandates: selector -> mode -> useViewTransition -> viewTransitionSelector -> namespace -> elements

(test patch-elements-data-line-order
  "Data lines must appear in ADR-specified order: selector mode useViewTransition viewTransitionSelector namespace elements."
  (let ((gen (make-instance 'd*::sse-generator :request nil))
        (stream (make-string-output-stream)))
    (setf (slot-value gen 'lc::response) stream)
    (d*:patch-elements gen "<circle cx=5/>"
                       :selector "#vis"
                       :mode :append
                       :use-view-transition t
                       :view-transition-selector "#main"
                       :namespace :svg)
    (let* ((r (get-output-stream-string stream))
           (pos-selector   (search "selector #vis" r))
           (pos-mode       (search "mode append" r))
           (pos-uvt        (search "useViewTransition true" r))
           (pos-vts        (search "viewTransitionSelector #main" r))
           (pos-ns         (search "namespace svg" r))
           (pos-elements   (search "elements <circle" r)))
      (is (and pos-selector pos-mode pos-uvt pos-vts pos-ns pos-elements)
          "All expected data lines must be present, got: ~s" r)
      (is (< pos-selector pos-mode)
          "selector must precede mode, got: ~s" r)
      (is (< pos-mode pos-uvt)
          "mode must precede useViewTransition, got: ~s" r)
      (is (< pos-uvt pos-vts)
          "useViewTransition must precede viewTransitionSelector, got: ~s" r)
      (is (< pos-vts pos-ns)
          "viewTransitionSelector must precede namespace, got: ~s" r)
      (is (< pos-ns pos-elements)
          "namespace must precede elements, got: ~s" r))))



;;; --- with-signals ---

;;; Minimal test request type whose read-signals we can control.
(defclass test-request () ())

(defmethod d*:read-signals ((req test-request) &key catch-errors)
  (declare (ignore catch-errors))
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "name" ht) "Alice"
          (gethash "age"  ht) 30)
    ht))

(defclass nil-request () ())

(defmethod d*:read-signals ((req nil-request) &key catch-errors)
  (declare (ignore catch-errors))
  nil)

(test with-signals-present-keys
  "with-signals binds present signal values from the request."
  (let ((req (make-instance 'test-request)))
    (d*:with-signals ((name "name") (age "age" 0))
        req
      (is (string= "Alice" name)
          "Expected name=Alice, got: ~s" name)
      (is (= 30 age)
          "Expected age=30, got: ~s" age))))

(test with-signals-absent-key-uses-default
  "with-signals uses DEFAULT when the key is not in the signals hash-table."
  (let ((req (make-instance 'test-request)))
    (d*:with-signals ((missing "no-such-key" :fallback))
        req
      (is (eq :fallback missing)
          "Expected :fallback default for absent key, got: ~s" missing))))

(test with-signals-nil-signals-uses-defaults
  "with-signals uses DEFAULT for all bindings when read-signals returns NIL."
  (let ((req (make-instance 'nil-request)))
    (d*:with-signals ((name "name" "nobody") (age "age" -1))
        req
      (is (string= "nobody" name)
          "Expected 'nobody' default when signals nil, got: ~s" name)
      (is (= -1 age)
          "Expected -1 default when signals nil, got: ~s" age))))

(test with-signals-reads-signals-once
  "with-signals macroexpansion contains read-signals exactly once."
  (let* ((form '(d*:with-signals ((x "x") (y "y") (z "z")) req (list x y z)))
         (expansion (write-to-string (macroexpand-1 form)))
         (count 0))
    ;; Count READ-SIGNALS occurrences in the expansion string
    (let ((pos 0))
      (loop
        (let ((found (search "READ-SIGNALS" (string-upcase expansion) :start2 pos)))
          (unless found (return))
          (incf count)
          (setf pos (1+ found)))))
    (is (= 1 count)
        "Expected exactly 1 READ-SIGNALS in expansion, found ~d: ~a" count expansion)))
