;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DEMO-REMOVE.LISP --- remove-element and patch-elements append

;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates remove-element and patch-elements with :mode :append. The DOM
;;; holds a counter; each "Add" appends a new item, each item's "remove" button
;;; calls remove-element by ID. No server state in this demo.

(ql:quickload '(:hunchentoot :spinneret :datastar-cl/hunchentoot))

(defpackage #:remove-demo
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp :spinneret) (:d* :datastar-cl)))
(in-package #:remove-demo)

(defparameter *counter* 0)

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:script :type "module" :src (d*:datastar-url)))
     (:body
      (:h1 "Remove Element Demo")
      (:p "Add items and remove them individually.")
      (:button :|data-on:click| (d*:sse-get "/add") "Add item")
      (:ol :id "items")))))

(hunchentoot:define-easy-handler (add-endpoint :uri "/add") ()
  (d*:with-sse (gen hunchentoot:*request*)
    (incf *counter*)
    (let ((id (format nil "item-~A" *counter*)))
      (d*:patch-elements gen
                         (sp:with-html-string
                           (:li :id id
                                "Item " (:strong *counter*)
                                " "
                                (:button :|data-on:click|
                                          (format nil "@get('/remove?sel=~A')" id)
                                         "x")))
                         :selector "#items"
                         :mode :append))))

(hunchentoot:define-easy-handler (remove-endpoint :uri "/remove") ()
  (let ((sel (hunchentoot:get-parameter "sel")))
    (when sel
      (d*:with-sse (gen hunchentoot:*request*)
        (d*:remove-element gen (format nil "#~A" sel))))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8989))
(format t "~&Server started on http://localhost:8989~%")
