;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; RADAR.LISP --- SVG world map projection, event ring buffer, and renderer
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; SVG MAP CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 720*360 viewBox,  equirectangular projection, 2:1 aspect ratio.
;;;   x = (lon + 180) / 360 x 720
;;;   y = (90 - lat)  / 180 x 360
;;;
;;; *coastlines* is defined by world-110m.lisp (loaded as an ASDF component
;;; before this file). The same projection is used for both coastlines and
;;; blips: they are automatically aligned.

(defconstant +map-w+ 720)
(defconstant +map-h+ 360)

(defun project-to-xy (lat lon)
  "Equirectangular (values x y) in the 720x360 viewBox."
  (values (round (* (/ (+ lon 180.0d0) 360.0d0) +map-w+))
          (round (* (/ (- 90.0d0 lat) 180.0d0) +map-h+))))

(defun svg-poly-points (lonlat-pairs)
  "Convert list of (lon lat) pairs to an SVG polygon points string."
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (p)
                    (multiple-value-bind (x y)
                        (project-to-xy (second p) (first p))
                      (format nil "~D,~D" x y)))
                  lonlat-pairs)))

;;; RADAR EVENT RING BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Events are plists: (:id "eN" :lat F :lon F :label S :kind KW :at UT)
;;;   kinds: :spawn :sight :retire
;;;
;;; Pruned on each new event to at most *radar-max-events* entries, all
;;; within *radar-event-ttl* seconds.  No background timer: expired blips
;;; are already invisible via CSS animation forwards fill, and are silently
;;; removed on the next push.

(defparameter *radar-max-events* 12
  "Maximum number of blips kept in the ring buffer.")
(defparameter *radar-event-ttl*  20
  "Seconds before a blip entry is pruned from the buffer.")

(defvar *radar-events*  nil "Ring buffer of recent radar events (newest first).")
(defvar *radar-lock*    (bt:make-lock "radar") "Protects *radar-events*.")
(defvar *radar-counter* 0 "Monotonic counter for unique event IDs.")

(defun push-radar-event (kind designation ip &key location agent)
  "Prepend a radar event to *radar-events* and return T, or NIL on no location.
   If LOCATION (a (lat lon label) list) is supplied it is used directly,
   bypassing IP geolocation -- the simulator passes a precomputed random
   location here.  Otherwise IP is geolocated via EVENT-LOCATION, falling back
   to the themed pool.  AGENT is the session-id string of the acting client, or NIL."
  (multiple-value-bind (lat lon label)
      (if location
          (values-list location)
          (event-location designation ip))
    (when lat
      (let ((now (get-universal-time)))
        (bt:with-lock-held (*radar-lock*)
          (let* ((id       (format nil "e~D" (incf *radar-counter*)))
                 (new-ev   (list :id id :lat lat :lon lon
                                 :label label :kind kind :at now
                                 :designation designation
                                 :agent (or agent "?")))
                 (cutoff   (- now *radar-event-ttl*))
                 (fresh    (remove-if (lambda (e) (< (getf e :at) cutoff))
                                      *radar-events*))
                 (combined (cons new-ev fresh)))
            (setf *radar-events*
                  (if (> (length combined) *radar-max-events*)
                      (subseq combined 0 *radar-max-events*)
                      combined)))))
      t)))

;;; SVG RADAR RENDERER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; render-radar produces a complete <svg id="radar" ...> string. push-radar
;;; sends it via patch-elements with :namespace :svg -- this is the whole point:
;;; Datastar creates/morphs elements in the SVG XML namespace so they render
;;; correctly as vector graphics.
;;;
;;; The morph preserves <g id="blip-eN"> elements across patches: running CSS
;;; animations survive the re-render because the DOM node is reused (not
;;; replaced) when the id matches.  New blips get a fresh id -> animation
;;; restarts from frame 0.
;;;
;;; The comparable JS solution I had was ~160 LOC, plus the additional code
;;; needed for interacting with it -- a clear win here.


(defun render-radar ()
  "Return a <svg id=\"radar\" ...> string reflecting the current blip state."
  (let ((events (bt:with-lock-held (*radar-lock*) (copy-list *radar-events*))))
    (spinneret:with-html-string
      (:svg :id "radar"
            :viewBox (format nil "0 0 ~D ~D" +map-w+ +map-h+)
            :width "100%"
            :xmlns "http://www.w3.org/2000/svg"
        
        ;; Dark backdrop
        (:rect :width +map-w+ :height +map-h+ :fill "#050401")
        
        ;; Graticule -- longitude lines every 30deg
        (loop for lon from -150 to 150 by 30
              for x = (round (* (/ (+ lon 180.0) 360.0) +map-w+))
              do (:line :x1 x :y1 0 :x2 x :y2 +map-h+
                        :stroke "#1a0d00" :stroke-width "0.5"))
        
        ;; Graticule -- latitude lines
        (loop for lat in '(-60 -30 0 30 60)
              for y = (round (* (/ (- 90.0 lat) 180.0) +map-h+))
              do (:line :x1 0 :y1 y :x2 +map-w+ :y2 y
                        :stroke (if (zerop lat) "#2a1400" "#1a0d00")
                        :stroke-width (if (zerop lat) "1" "0.5")))
        
        ;; Natural Earth 110m coastline polygons
        (:g :fill "#0f0800" :stroke "#4d3800" 
            :stroke-width "0.5" :stroke-linejoin "round"
          (dolist (c *coastlines*)
            (:polygon :points (svg-poly-points c))))
        
        ;; Blips group
        (:g :id "blips"
          (dolist (ev events)
            (multiple-value-bind (x y)
                (project-to-xy (getf ev :lat) (getf ev :lon))
              (:g :id (format nil "blip-~A" (getf ev :id))
                  :class (format nil "ping-~A" (string-downcase (symbol-name (getf ev :kind))))
                (:circle :class "ping-dot" :cx x :cy y :r 3 :fill "currentColor")
                (:circle :class "ping-ring" :cx x :cy y :r 4 :fill "none"
                         :stroke "currentColor" :stroke-width "1.5")
                (:text :class "ping-label" :x x :y (+ y 14) :text-anchor "middle"
                  (getf ev :label))))))))))
