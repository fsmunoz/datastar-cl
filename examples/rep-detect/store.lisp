;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; STORE.LISP --- BKNR prevalence store, persistent classes, and transactions
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; DOMAIN CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single source of truth for the replicant model list.  Consumed by
;;; the spawn validator (handlers.lisp), the simulator (simulator.lisp),
;;; and the registration form <select> (index.lisp).

(defparameter *models* '("Nexus-6" "Nexus-7" "Nexus-8")
  "Authoritative list of valid replicant models.")

(defun protected-p (r)
  "T if replicant R is exempt from retirement (e.g. Rachael).
   Checked inside retire-replicant (authoritative) and by the simulator
   candidate filter (belt-and-suspenders)."
  (and r (replicant-protected r)))

;;; PERSISTENT CLASS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; One replicant record per unit.  The designation index provides O(1)
;;; lookup; the status hash-index groups by :at-large / :retired.
;;;
;;; protected: set once at spawn (never updated) -- T means the unit is
;;; exempt from retirement.  The retire-replicant transaction enforces
;;; this authoritatively; protected-p (above) is the shared predicate.

(bk:define-persistent-class replicant ()
  ((designation :read :index-type idx:string-unique-index
                      :index-reader replicant-with-designation
                      :index-values all-replicants)
   (model       :read)
   (role        :read)
   (incept      :read)
   (protected   :read :initform nil)
   (status      :update :initform :at-large
                        :index-type  idx:hash-index
                        :index-reader replicants-with-status
                        :index-keys   all-statuses)
   (sightings   :update :initform 0)
   (last-seen   :update :initform nil)))

;;; TRANSACTIONS -- must be defined BEFORE the store is opened ;;;;;;;;;;;;;;;;;

(bk:deftransaction spawn-replicant (designation model role incept &optional protected)
  (make-instance 'replicant :designation designation :model model
                             :role role :incept incept :protected protected))

(bk:deftransaction sight-replicant (designation)
  (let ((r (replicant-with-designation designation)))
    (when r
      (incf (replicant-sightings r))
      (setf (replicant-last-seen r) (get-universal-time))
      ;; Sighting a retired unit means it wasn't actually dead -- reactivate.
      (when (eq (replicant-status r) :retired)
        (setf (replicant-status r) :at-large)))))

(bk:deftransaction retire-replicant (designation)
  (let ((r (replicant-with-designation designation)))
    (when (and r (eq (replicant-status r) :at-large) (not (protected-p r)))
      (setf (replicant-status r) :retired))))

;;; STORE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *store-dir* "/tmp/rep-detect-store/"
  "Prevalence store directory.  Override via STORE_DIR env var at runtime.")

(defun open-store ()
  "Open the BKNR prevalence store, reading STORE_DIR from the environment.
   Seeds the four canonical replicants if the store is fresh.
   Safe to call multiple times: no-op if bk:*store* is already bound."
  (setf *store-dir* (uiop:ensure-directory-pathname
                     (or (uiop:getenv "STORE_DIR") *store-dir*)))
  (unless (boundp 'bk:*store*)
    (make-instance 'bk:mp-store :directory *store-dir*
                   :subsystems (list (make-instance 'bk:store-object-subsystem))))
  (when (null (all-replicants))
    (spawn-replicant "N6MAA10816" "Nexus-6" "Combat"       "2016-01-08")  ; Roy Batty
    (spawn-replicant "N6FAB21416" "Nexus-6" "Pleasure"     "2016-02-14")  ; Pris
    (spawn-replicant "N6MAC41717" "Nexus-6" "Loader"       "2017-04-10")  ; Leon
    (spawn-replicant "N6FAB61216" "Nexus-6" "Polit. Homicide" "2016-06-12")  ; Zhora    
    (spawn-replicant "N7FAA52318" "Nexus-7" "Experimental" "2018-05-23" t)))      ; Rachael (protected)

