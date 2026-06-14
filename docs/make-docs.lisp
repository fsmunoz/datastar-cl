;;;; docs/make-docs.lisp --- Generate reference manuals with declt
;;;;
;;;; Usage: sbcl --non-interactive --load docs/make-docs.lisp
;;;; Or via: make docs
;;;;
;;;; Outputs:
;;;;   docs/datastar-cl.texi        datastar-cl SDK (Datastar-specific API + lc-sse; see note)
;;;;   docs/datastar-cl-snooze.texi Snooze integration package (focused, no backend duplication)
;;;;
;;;; Each .texi is also converted to .txt and .html by the Makefile target.
;;;;
;;;; Note on lc-sse inclusion in the datastar-cl manual:
;;;; lc-sse lives at lc-sse/ inside the datastar-cl directory tree (git submodule).
;;;; declt filters in-project deps by directory, so it follows :datastar-cl's deps into
;;;; lc-sse/ and includes both layers in the datastar-cl manual.  This is intentional
;;;; while the submodule arrangement exists: one full-stack manual rather than two
;;;; partial ones.  When lc-sse is removed from this tree (Quicklisp dist inclusion or
;;;; explicit exclusion), the manual will slim to the Datastar-specific API only --
;;;; no changes to this file needed at that point.
;;;;
;;;; The lc-sse standalone manual is built in the lc-sse repo:
;;;;   cd lc-sse && make docs          (full, requires cl-brotli + patched woo)
;;;;   cd lc-sse && make docs-ci       (Quicklisp-only, hermetic)

(ql:quickload :datastar-cl :silent t)
(ql:quickload :net.didierverna.declt :silent t)

;;; Manual 1: datastar-cl SDK (full backend public API, includes lc-sse; see note above).
;;; The umbrella :datastar-cl depends on :datastar-cl/hunchentoot and :datastar-cl/clack,
;;; both under src/, which depend on :datastar-cl/core -- so all three are included
;;; automatically.  lc-sse systems under lc-sse/ are also followed (see note).
(net.didierverna.declt:declt :datastar-cl
  :library-name     "datastar-cl"
  :output-directory "docs/"
  :file-name        "datastar-cl"
  :info-name        "datastar-cl"
  :info-category    "Common Lisp"
  :declt-notice     nil)

;;; Manual 2: Snooze integration package only.
;;; :datastar-cl/snooze-doc depends only on external :snooze, so declt's directory
;;; filter does not follow into core/Hunchentoot/Clack.  src/snooze.lisp defines
;;; its own package (datastar-cl/snooze) which becomes the sole domestic package.
;;; :datastar-cl is already loaded above, so the datastar-cl/hunchentoot/alexandria
;;; packages are present at compile time even though snooze-doc doesn't declare them.
(net.didierverna.declt:declt :datastar-cl/snooze-doc
  :library-name     "datastar-cl/snooze"
  :output-directory "docs/"
  :file-name        "datastar-cl-snooze"
  :info-name        "datastar-cl-snooze"
  :info-category    "Common Lisp"
  :declt-notice     nil)
