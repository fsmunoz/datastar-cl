;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; INDEX.LISP --- HTML generation and routes
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/horizons-demo)

;; Routes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defresource index (verb content-type))

(defroute index (:get :text/html)
  "Index page"
  (generate-index-html))

(setf snooze:*home-resource* #'index)

;; HTML generation using Spinneret ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :datastar-cl/horizons-demo)

(defun generate-index-html ()
  "Generate the main HTML page for the horizons visualization"
  (sp:with-html-string
    (:doctype)
    (:html
     (:head
      (:script :type "module"
               :src (datastar-cl:datastar-url))
      (:style (:raw (generate-css))))
     (:body
      (:div :class "container"
            :id "main-view"
            ;; The init-signals helper outputs this:
            ;; "{"selectedPlanet":null,"showDetails":false}"
            ;; ... which could also be used directly
            :data-signals (d*:init-signals :selected-planet 'null :show-details nil)
            ;; Main view with canvas
            (:div :class "main-view"
                  ;; Stats header
                  (:div :class "stats"
                        (:h2 "Horizons - JPL Solar System Data Demo + NASA DSN Updates")
                        (:p "Click on a planet to see JPL Horizons details | NASA Deep Space Network logs update in the background (all antenas)"
                            "Demo for " (:span :class "highlight" "Datastar + Common Lisp")
                            " / by fsm " (:a :href "https://lambda-combine.net/hyper-guide" "ΛↃ lambda combine")))
                  ;; Canvas container with planets
                  (:div :class "canvas-container" :|data-init| (d*:sse-get "/dsn-stream")
                        (:div :id "bg-stream" :class "stream-overlay"
                              "Waiting for stream...")
                        (:planets-canvas                         
                         ;; The expression generated bellow is equivalent to:
                         ;;
                         ;; "$selectedPlanet = evt.detail.name; @get('/details'); $showDetails = evt.detail.name !== null"
                         ;;
                         ;; ... which can be used as-is, so this is
                         ;; OPTIONAL and mostly a preference thing,
                         ;; used here to exemplify.
                         :|data-on:planetclick| (d*:ds-expr
                                                 (ps:ps (setf $selected-planet evt.detail.name)) ; ps changes foo-bar to fooBar
                                                 (d*:sse-get "/details") 
                                                 (ps:ps (setf $show-details (not (eql (ps:@ evt detail name) nil))))))))
                          
            ;; Details pane
            (:div :class "details-pane"
                  :id "details"
                  :data-show "$showDetails"

                  (:div :id "details-content"
                        (:p :class "no-selection"
                            "Click on a planet to see details..."))))
      ;; External JS for web component
      (:script :src "planets-canvas.js")))))

