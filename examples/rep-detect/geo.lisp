;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; GEO.LISP --- GeoLite2-City lookup, real client IP, and fallback pool
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; GEO DATABASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GeoLite2-City is mmap'd once at load time.  All queries run under
;;; *geo-lock* (cl-maxminddb thread-safety is not documented).
;;;
;;; DB record keys are keywords: :location, :latitude, :city, :names, :en, etc.
;;; geo-lookup returns (values lat lon label) or NIL on any failure.

(defvar *geo-db-path* "/tmp/GeoLite2-City.mmdb"
  "Absolute path to the GeoLite2-City .mmdb file.  Override via GEO_DB_PATH env var.")

(defvar *geo-db*   nil "Opened MaxMind DB handle, or NIL if unavailable.")
(defvar *geo-lock* (bt:make-lock "geo") "Serializes mmdb-query calls.")

(defun open-geo-db ()
  "Open the MaxMind GeoLite2-City database, reading GEO_DB_PATH from the environment.
   Silently sets *geo-db* to NIL if the file is absent -- radar falls back gracefully."
  (setf *geo-db-path* (or (uiop:getenv "GEO_DB_PATH") *geo-db-path*))
  (handler-case
      (progn
        (setf *geo-db* (geo:make-mmdb *geo-db-path*))
        (format t "~&GeoLite2-City opened: ~A~%" *geo-db-path*))
    (error (e)
      (format t "~&WARNING: geo DB unavailable (~A) -- radar will be empty.~%" e))))


(defun geo-lookup (ip)
  "Return (values lat lon label) for IP string, or NIL on any failure.
   Falls back to NIL silently if the IP is absent from the DB."
  (unless *geo-db* (return-from geo-lookup nil))
  (handler-case
      (let* ((rec    (bt:with-lock-held (*geo-lock*)
                       (geo:mmdb-query *geo-db* ip)))
             (lat    (geo:get-in rec :location :latitude))
             (lon    (geo:get-in rec :location :longitude))
             (city   (geo:get-in rec :city :names :en))
             (iso    (geo:get-in rec :country :iso-code))
             (cname  (geo:get-in rec :country :names :en))
             (label  (cond (city  (format nil "~A~@[, ~A~]" city iso))
                           (cname cname)
                           (t     "UNKNOWN"))))
        (values lat lon label))
    (error () nil)))

;;; CURATED IP POOL (FALLBACK) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Used only when the visitor's real IP is unavailable or not in GeoLite2 (e.g.
;;; direct localhost access). Each seeded replicant has a themed IP; newly
;;; spawned replicants get a stable hash-derived IP from *fallback-ips*.
;;;

(defparameter *ip-pool*
  '(("N6MAA10816" . "210.224.0.1")    ; Roy Batty   - Tokyo, JP
    ("N6FAB21416" . "223.5.5.5")      ; Pris        - Hangzhou, CN
    ("N6MAC41717" . "4.4.0.1")        ; Leon        - Chicago, US
    ("N7FAB00001" . "75.101.0.1"))    ; Rachael     - San Rafael, US
  "Designation to fallback IP when visitor's real IP can't be geolocated.")

(defparameter *fallback-ips*
  #("45.95.0.1"      ; Frankfurt am Main, DE
    "125.209.0.1"    ; Seongnam-si, KR
    "139.228.0.1"    ; Surabaya, ID
    "175.45.176"     ; Pyongyang, DPRK
    "201.0.0.1"      ; Vinhedo, BR
    "196.207.0.1"    ; Lagos, NG
    "181.0.0.1"      ; San Isidro, AR
    "102.0.0.1"      ; Nairobi, KE
    "89.0.0.1")      ; Cologne, DE
  "Fallback IPs for units not in *ip-pool*; index = (mod (sxhash designation) 8).")

(defun ip-for (designation)
  "Return a stable fallback IP for DESIGNATION."
  (or (cdr (assoc designation *ip-pool* :test #'string=))
      (aref *fallback-ips*
            (mod (sxhash designation) (length *fallback-ips*)))))

;;; RANDOM WORLDWIDE GEOLOCATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Used by the background simulator to generate globally varied radar blips.
;;; The local GeoLite2-City DB has no query limit; ~99.8% of random public IPs
;;; resolve to a location, so a NIL miss is effectively impossible.

(defun random-public-ipv4 ()
  "Return a random routable-looking public IPv4 string.
   Skips first-octet blocks that are private/reserved/multicast so the address
   is very likely (>99%) present in GeoLite2-City."
  (loop
    (let ((a (1+ (random 223))))             ; 1..223 (skip 224+ multicast)
      (unless (member a '(0 10 100 127 169 172 192 198))
        (return (format nil "~D.~D.~D.~D"
                        a (random 256) (random 256) (1+ (random 254))))))))

(defun random-exotic-location (&optional (tries 4))
  "Return (values lat lon label) for a randomly geolocated public IP.
   Tries up to TRIES random addresses and returns the first hit, or NIL. With a
   >99% per-try success rate, 4 tries make NIL essentially impossible; a NIL
   result simply yields no radar blip -- the caller does not need to guard."
  (loop repeat tries
        do (multiple-value-bind (lat lon label)
               (geo-lookup (random-public-ipv4))
             (when lat (return (values lat lon label))))))

(defun event-location (designation ip)
  "Return (values lat lon label) for a radar event.
   Tries IP first (the visitor's real IP string, or NIL); falls back to the
   themed pool keyed by designation."
  (multiple-value-bind (lat lon label)
      (geo-lookup ip)
    (if lat
        (values lat lon label)
        (geo-lookup (ip-for designation)))))
