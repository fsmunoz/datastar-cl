;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DETAILS.LISP --- Planet details and initial layout data
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/horizons-demo)

;;; Planet data (hardcoded for simplicity, unlike data-spice which uses SPICE data)

(defparameter *planets*
  '((:name "Sun"     :x -30  :y 125 :radius 100 :color "#FFFF00" :horizons-id 10)
    (:name "Mercury" :x 86   :y 125 :radius 6  :color "#8C7853" :horizons-id 199)
    (:name "Venus"   :x 114  :y 125 :radius 12 :color "#FFC649" :horizons-id 299)
    (:name "Earth"   :x 149  :y 125 :radius 13 :color "#4169E1" :horizons-id 399)
    (:name "Mars"    :x 180  :y 125 :radius 8  :color "#CD5C5C" :horizons-id 499)
    (:name "Jupiter" :x 236  :y 125 :radius 35 :color "#DAA520" :horizons-id 599)
    (:name "Saturn"  :x 375  :y 125 :radius 30 :color "#F4A460" :horizons-id 699)
    (:name "Uranus"  :x 684  :y 125 :radius 18 :color "#4FD0E0" :horizons-id 799)
    (:name "Neptune" :x 1032 :y 125 :radius 18 :color "#4166F5" :horizons-id 899))
  "Hardcoded planet data - inner planets spaced to avoid overlap, outer planets at realistic distances")


;;; JPL Horizons API Integration

(defun get-horizons-data (planet-name)
  "Fetch JPL Horizons data for PLANET-NAME using today's date."
  (unless planet-name
    (return-from get-horizons-data
      (sp:with-html-string
        (:p :class "no-selection" "Click on a planet to see details..."))))  
  (let* ((planet (find planet-name *planets*
                       :key (lambda (p) (getf p :name))
                       :test #'string-equal))
         (id (getf planet :horizons-id)))
    (if id
        (handler-case
            (dex:get (format nil "https://ssd.jpl.nasa.gov/api/horizons.api?format=text&COMMAND='~a'" id)
                     :keep-alive nil)
          (error (e)
            (format nil "Error fetching data for ~a: ~a" planet-name e)))
        (format nil "No Horizons data available for ~a" planet-name))))


;; Routes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s*:defresource details (verb content-type &key datastar))

;;(defmethod snooze:read-for-resource ((resource (eql #'details)) value)
;;  (handler-case (call-next-method) (error () value)))

(s*:defroute details (:get :text/html &key datastar)
  (let ((signals (s*:read-signals)))
    (s*:with-sse (gen)
      (when signals
        (let* ((planet-name (gethash "selectedPlanet" signals))
               (planet-upper (when planet-name (string-upcase planet-name))))
          ;; Send loading message first
          (when planet-name
            (datastar-cl:patch-elements gen
                                        (format nil "Fetching data for ~a..."
                                                planet-upper)
                                        :selector "#details-content"
                                        :mode :inner
                                        :event-id "loading"))
          ;; Fetch and send actual data
          (let ((horizons-text (get-horizons-data planet-name)))
            (datastar-cl:patch-elements gen
                                        (format nil "~a" horizons-text)
                                        :selector "#details-content"
                                        :mode :inner                                        
                                        :event-id "data")))))))



  
