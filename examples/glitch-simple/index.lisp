;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; INDEX.LISP --- Index page, layout, and route
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-simple-demo)

;;; Page chrome - wrap body content with the standard terminal shell.

(defmacro with-page ((&key route) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html :lang "en"
      (:head
       (:meta :charset "UTF-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title "LAMBDA COMBINE - Symbolic Systems")
       (:script :type "module"
                :src (datastar-cl:datastar-url))
       (:link :rel "preconnect" :href "https://fonts.googleapis.com")
       (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "anonymous")
       (:link :rel "stylesheet" :href *google-fonts-url*)
       (:link :rel "stylesheet" :href *dejavu-fonts-url*)
       (:style (:raw *app-css*)))
      (:body :data-signals "{glitch: {active: false}, sid: ''}"
             :data-init (format nil "$sid = crypto.randomUUID(); ~A"
                                    (datastar-cl:sse-get "/updates"))
       (:div :class "terminal"
             :data-class "{glitching: $glitch.active}"
             (:h1 (:a :href "/"
                      (:span :class "acronym" "ΛↃ")
                      (:span :class "wordmark"
                             (:span "LAMBDA")
                             (:span :class "combine" "COMBINE"))))
        (:div :class "meta" "Symbolic Systems Infrastructure")
        ,@body
        (:p :class "prompt"
            ,(format nil "(eval ~(~A~).lisp)" route)
            (:span :class "cursor")))))))

;;; Routes

(defun render-index ()
  (with-page (:route "index")
             (:h1 "Datastar Common Lisp SDK: CQRS Demo")
             (:h5 :data-text "'Assigned UID: '+ $sid")
    (:button :id "glitch"
             :|data-on:click| (datastar-cl:sse-post "/glitch")
             "GLITCH")
    (:button :id "glitch-me"
             :|data-on:click| (datastar-cl:sse-post "/glitch-me")
             "GLITCH ME")
    (:p "All-purpose symbolic systems for computational infrastructure.")
    (:div :id "manif")
    (:div :class "manifesto" :id "manifesto")))

(hunchentoot:define-easy-handler (root-ht :uri "/") ()
  (hunchentoot:redirect "/index"))

(hunchentoot:define-easy-handler (index-ht :uri "/index") ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-index))



