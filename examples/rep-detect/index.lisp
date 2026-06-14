;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; INDEX.LISP --- Page rendering
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fmt-time (ut)
  "Format a universal-time as HH:MM:SS, or NIL if UT is NIL."
  (when ut
    (multiple-value-bind (s m h) (decode-universal-time ut)
      (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))))

;;; REGISTRY FRAGMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Serialised as a Spinneret HTML fragment; patch-elements targets #registry.
;;; ACTION LOG FRAGMENT targets #action-log.

(defun render-stats ()
  (let ((at-large (length (replicants-with-status :at-large)))
        (retired  (length (replicants-with-status :retired)))
        (models   (remove-duplicates
                   (mapcar #'replicant-model (all-replicants)) :test #'equal)))
    (format nil "AT-LARGE: ~D  |  RETIRED: ~D  |  MODELS: ~{~A~^, ~}"
            at-large retired models)))

(defun render-registry ()
  (sp:with-html-string
    (:div :id "registry"
          (:p :class "stats" (render-stats))
          (:div :class "registry-scroll"
           (:table
            (:thead
             (:tr (:th "DESIGNATION") (:th "MODEL") (:th "ROLE")
                  (:th "INCEPT") (:th "STATUS") (:th "SIGHTINGS")
                  (:th "LAST SEEN") (:th "")))
            (:tbody
             (flet ((target-post (url)
                      (format nil "$target=el.dataset.value; ~A" (d*:sse-post url))))
               (dolist (r (all-replicants))
                 (let ((desig (replicant-designation r))
                       (class (string-downcase (symbol-name (replicant-status r)))))
                   (:tr :class class
                        (:td desig)
                        (:td (replicant-model r))
                        (:td (replicant-role r))
                        (:td (replicant-incept r))
                        (:td (symbol-name (replicant-status r)))
                        (:td (replicant-sightings r))
                        (:td (or (fmt-time (replicant-last-seen r)) "--"))
                        (:td (:button :data-value desig
                                      :|data-on:click| (target-post "/sight")
                                      "SIGHT")
                             (when (eq (replicant-status r) :at-large)
                               (:button :data-value desig
                                        :|data-on:click| (target-post "/retire")
                                        :|data-indicator:processing| ""
                                        "RETIRE")))))))))))))


(defun render-action-log ()
  (let ((events (bt:with-lock-held (*radar-lock*)
                  (subseq *radar-events* 0 (min 3 (length *radar-events*))))))
    (sp:with-html-string
      (:div :id "action-log" :class "stats"
            (if events
                (:ul (dolist (ev events)
                       (:li (format nil "> Agent ~A (~A): ~A ~A"
                                    (getf ev :agent "?")
                                    (getf ev :label "UNKNOWN")
                                    (string-upcase (symbol-name (getf ev :kind)))
                                    (getf ev :designation "?")))))
                (:p :class "meta" "NO EVENTS LOGGED"))))))

;;; PAGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; render-page returns the full HTML string; HTTP dispatch is done in the
;;; backend transport files (handlers-hunchentoot.lisp / handlers-clack.lisp).
;;; The #registry and #radar fragments are rendered inline here; subsequent
;;; updates arrive via SSE patch-elements.  The #radar SVG must exist in the
;;; initial HTML so the first morph has a target.

(defun render-page ()
  "Return the full application HTML page as a string."
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:meta :charset "UTF-8")
            (:link :rel "stylesheet" :href *google-fonts-url*)
            (:link :rel "stylesheet" :href *dejavu-fonts-url*)
            (:script :type "module" :src (d*:datastar-url))
            (:style (:raw *app-css*)))
     (:body :data-signals (d*:init-signals :designation "" :model (first *models*)
                                           :role "" :incept "" :target "" :msg "" :sid ""
                                           :processing nil)
            :data-init (format nil "$sid = crypto.randomUUID(); ~A" (d*:sse-get "/sse"))
            (:div :class "terminal"
                  (:h1 (:span :class "acronym" "ΛↃ")
                       (:span :class "wordmark"
                              (:span "LAMBDA")
                              (:span :class "combine" "COMBINE")))
                  (:p :class "meta"
                      "REP-DETECT Terminal v.2019")
                  (:p :class "meta"
                      (:a :class "meta" :href "https://lambda-combine.net/guide-prevalence"
                      "-> Datastar, Common Lisp, BKNR prevalence, CQRS"))
                  (:h2 "Replicant Registration Database")
                  (:p :class "meta"
                      "LXPD Blade Runner Division | Tyrell Corp. Subcontract LC-2018-BR")
                  (:p :class "meta" :data-text "$sid")
                  (:raw (render-registry))
                  (:hr)
                  (:p (:strong "REPORT UNREGISTERED UNIT: ")
                      (:input :type "text" :placeholder "Designation" :data-bind "designation"
                              :style "width:10em" :maxlength 10)
                      (:select :data-bind "model"
                               (dolist (m *models*) (:option :value m m)))
                      (:input :type "text" :placeholder "Role" :data-bind "role"
                              :style "width:8em" :maxlength 10)
                      (:input :type "text" :placeholder "Incept" :data-bind "incept"
                              :style "width:8em" :maxlength 10)
                      (:button :|data-on:click| (d*:sse-post "/spawn") "SPAWN"))
                  (:p :class "processing" :data-show "$processing" :style "display:none"
                      "> TRANSMITTING RETIREMENT ORDER TO CENTRAL...")
                  (:p :class "msg" :data-show "$msg != ''" :style "display:none"
                      :data-text "$msg")
                  (:hr)
                  (:hr)
                  (:h2 "Orbital Tracking · Last Known Positions")
                  (:p :class "meta"
                      "GeoLite2-City · Equirectangular projection · "
                      (format nil "Contacts fade after ~D seconds" *radar-event-ttl*))
                  ;; Initial SVG render; subsequent updates via patch-elements :namespace :svg
                  (:raw (render-radar))
                  (:p :class "meta" "Field Agent Activity Log - newest first")
                  (:raw (render-action-log))
                  (:div :class "footer"
                        "ΛↃ Lambda Combine  · "
                        (format nil "Persistent prevalence store: ~A  ·" *store-dir*)  
                        (:span :id "server-stats")))))))
