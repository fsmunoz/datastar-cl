;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; SIMULATOR.LISP --- Background activity loop for the demo
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sim-delay-min*    1.5 "Minimum seconds between simulated actions.")
(defparameter *sim-delay-max*    4.0 "Maximum seconds between simulated actions.")
(defparameter *sim-max-pop*       40 "Hard cap on registered replicants.")

(defparameter *sim-roles*
  #("Combat" "Pleasure" "Loader" "Labor" "Recon" "Soldier" "Scout" "Medic"
    "Infiltrator" "Logistics"))

;;; RANDOM DATA GENERATORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-elt (seq)
  "Return a uniformly random element of SEQ (list or vector), or NIL if empty."
  (when (plusp (length seq)) (elt seq (random (length seq)))))

(defun random-delay ()
  "Return a random float in [*sim-delay-min*, *sim-delay-max*)."
  (+ *sim-delay-min*
     (random (- *sim-delay-max* *sim-delay-min*))))

(defun random-model () (random-elt *models*))
(defun random-role ()  (random-elt *sim-roles*))

(defun random-incept ()
  "Generate a plausible incept date string, e.g. \"2017-05-12\"."
  (format nil "~4,'0D-~2,'0D-~2,'0D"
          (+ 2016 (random 5))
          (1+ (random 12))
          (1+ (random 28))))

(defun random-designation (&optional (prefix "N"))
  "Generate a Nexus-style designation, e.g. \"N6FAB99142\". Accepts a PREFIX,
useful to create more generic names (e.g. for agents)"
  (let ((gen   (+ 6 (random 3)))    ; 6, 7, or 8
        (l1    (code-char (+ (char-code #\A) (random 26))))
        (l2    (code-char (+ (char-code #\A) (random 26))))
        (l3    (code-char (+ (char-code #\A) (random 26)))))
    (format nil "~A~D~C~C~C~5,'0D" prefix gen l1 l2 l3 (random 100000))))

;;; SIMULATOR ACTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The sim thread calls simulate-one-action, which picks one of three
;;; actions weighted so the population stays roughly stable:
;;;   - sight (40 %): any existing unit; reactivates if retired (see store.lisp)
;;;   - spawn (35 %): only when below *sim-max-pop*; guard duplicate designation
;;;   - retire (25 %): a random at-large unit
;;;
;;; Each event is geolocated via random-exotic-location (geo.lisp): a random
;;; public IPv4 is queried against the local GeoLite2-City DB (no network, no
;;; rate limit).  ~99.8 % of random IPs resolve; the rare miss just skips the
;;; blip.  This gives worldwide geographic spread instead of the 8-city pool.

(defun pick-random-unit ()
  "Return a random replicant from all registered units, or NIL if none."
  (random-elt (all-replicants)))

(defun pick-random-at-large ()
  "Return a random retirable at-large replicant, or NIL if none exist.
   Protected units (e.g. Rachael) are excluded from the candidate pool;
   the retire-replicant transaction also enforces this authoritatively."
  (random-elt (remove-if #'protected-p (replicants-with-status :at-large))))

(defun sim-sight ()
  "Sight a random replicant (any status).  Reactivates retired units."
  (let ((r (pick-random-unit)))
    (when r
      (let ((desig (replicant-designation r)))
        (sight-replicant desig)
        (push-radar-event :sight desig nil
                          :location (multiple-value-list (random-exotic-location))
                          :agent (random-designation "S"))
        t))))

(defun sim-spawn ()
  "Spawn a new replicant if below the population cap."
  (when (< (length (all-replicants)) *sim-max-pop*)
    (let ((desig (random-designation)))
      ;; Retry once on the rare collision; drop silently on second collision.
      (when (replicant-with-designation desig)
        (setf desig (random-designation)))
      (unless (replicant-with-designation desig)
        (spawn-replicant desig (random-model) (random-role) (random-incept))
        (push-radar-event :spawn desig nil
                          :location (multiple-value-list (random-exotic-location))
                          :agent (random-designation "S"))
        t))))

(defun sim-retire ()
  "Retire a random at-large replicant."
  (let ((r (pick-random-at-large)))
    (when r
      (let ((desig (replicant-designation r)))
        (retire-replicant desig)
        (push-radar-event :retire desig nil
                          :location (multiple-value-list (random-exotic-location))
                          :agent (random-designation "S"))
        t))))

(defun simulate-one-action ()
  "Execute one random simulator action and broadcast the result."
  (let ((roll (random 100)))
    (cond ((< roll 40) (sim-sight))
          ((< roll 75) (sim-spawn))
          (t           (sim-retire))))
  (broadcast))

;;; THREAD MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *simulator-thread*   nil "The background simulator thread, or NIL.")
(defvar *simulator-running*  nil "T while the simulator loop should continue.")

(defun start-simulator ()
  "Spawn the background simulator thread if not already running."
  (when *simulator-thread*
    (stop-simulator))
  (setf *simulator-running* t)
  (setf *simulator-thread*
        (bt:make-thread
         (lambda ()
           (loop while *simulator-running*
                 do (sleep (random-delay))
                    (ignore-errors (simulate-one-action))))
         :name "rep-detect-simulator"))
  (format t "~&Simulator started (delay ~As--~As, pop cap ~D)~%"
          *sim-delay-min* *sim-delay-max* *sim-max-pop*))

(defun stop-simulator ()
  "Signal the simulator thread to stop; it exits within one delay interval."
  (setf *simulator-running* nil)
  (setf *simulator-thread* nil)
  (format t "~&Simulator stopped~%"))
