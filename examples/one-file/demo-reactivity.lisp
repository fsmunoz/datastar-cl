;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO-REACTIVITY.LISP --- Client-side reactive attributes and loading
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Client-side data-* attributes that need no server round-trip:
;;; data-computed, data-show, data-attr. Plus data-indicator with a slow
;;; endpoint to show the loading pattern while a request is in flight.
;;;
;;; Note: HTML attribute names are lowercased by the browser before Datastar
;;; reads them. Signal names that appear as attribute keys (after the colon,
;;; e.g. data-computed:name, data-indicator:name) must be lowercase.
;;;
;;; Run with: sbcl --load demo-reactivity.lisp

(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot))

(defpackage #:reactivity
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl)))
(in-package #:reactivity)

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
     (:body :data-signals "{name: '', fetching: false}"

            (:h1 "Datastar-CL Reactivity Demo")

            ;; Client-only: no server round-trip for any of these.
            ;;
            ;; data-bind "name" creates signal $name, two-way bound to input.
            ;; data-computed:upper creates read-only derived signal $upper.
            ;; data-show shows/hides the preview paragraph.
            ;; data-attr:disabled / data-attr:aria-disabled are reactive attributes.

            (:h2 "Client reactivity")
            (:input :data-bind "name" :placeholder "Type something")
            (:p :data-show "$name != ''" :style "display:none"
                "Preview: "
                (:strong :|data-computed:upper| "$name.toUpperCase()"
                          :data-text "$upper"))
            (:button :|data-attr:disabled| "$name == ''"
                     :|data-attr:aria-disabled| "$name == ''"
                     "Save")

            (:hr)

            ;; Loading indicator: data-indicator sets $fetching while the
            ;; request is in flight. data-show uses it to toggle "Loading...".
            ;; The /slow endpoint sleeps before responding to make it visible.

            (:h2 "Loading indicator")
            (:button :|data-on:click| (d*:sse-get "/slow")
                     :|data-indicator:fetching| ""
                     "Fetch (slow)")
            (:span :data-show "$fetching" :style "display:none" " Loading...")
            (:span :id "result")))))

(hunchentoot:define-easy-handler (slow :uri "/slow") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (sleep 1.2)
    (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
      (d*:patch-elements gen
                         (format nil "<span>Response at ~2,'0d:~2,'0d:~2,'0d</span>" h m s)
                         :selector "#result"
                         :mode :inner))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
