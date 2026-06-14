;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; DSN.LISP --- NASA DSN stream and utilities
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/horizons-demo)

;; 
(defparameter *nasa-dsn-url* "https://eyes.nasa.gov/dsn/data/dsn.xml"
  "URL for the NASA DSN endpoint")


(defvar *dsn-text* ""
  "DSN activity text")

(defun dsn-updater()
  "Background updater for DSN information; this is merely to avoid
hitting NASA's site directly on each connect, and instead keep a
global variable that is updated every 10 seconds."
  (bt:make-thread
   (lambda ()
     (loop
       (setf *dsn-text* (get-dsn-log))
       (sleep 10)))
   :name "dsn-updater"))

;; This is pure eye-candy, but useful to see the streaming updates:
;; the DSN log is streamed one letter at a time.
(defun transmit-message (message gen selector &key (interval 0.01))
  "Type out MESSAGE character-by-character into the DOM element at SELECTOR."
  (loop with acc = ""
        for c across message
        do (progn
             (setf acc (concatenate 'string acc (string c)))
             (datastar-cl:patch-elements gen acc
                                         :selector selector
                                         :mode :inner
                                         :event-id "transmiting")
             (sleep interval))))


(defun pretty-print-info (data prefix)
  "Print helper, loops through DATA (a list) and prints CAR and CDR, starting the line with PREFIX (a string)"
  (format t "~A" prefix)  
  (dolist (info (xmls:node-attrs data))
    (format t "~:@(~A~): ~:@(~A~)  " (car info) (cadr info)))
  (format t "~%"))

(defun get-dsn-log (&optional (dsn-url *nasa-dsn-url*))
  "Get NASA DSN latest XML and print the result in a plain text log format"
  (let* ((dsn-text (dex:get dsn-url))
         (dsn (xmls:parse dsn-text))
         (stations (xmls:node-children dsn)))
    (with-output-to-string (*standard-output*)
    (dolist (station stations)
      (let ((key (xmls:node-name station)))
        (cond ((string= key "station")
               (pretty-print-info station ">>> STATION: "))
              ((string= key "dish")
               (pretty-print-info station "       DISH: "))))))))


;; Debug help
;;(setf snooze:*catch-errors* :verbose)


;; Snooze routes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; s*:defresource automatically adds a READ-FOR-RESOURCE override that
;; passes JSON objects ({...}) through as raw strings, so Datastar's
;; &datastar=<JSON> parameter does not trip Snooze's safe reader.

(s*:defresource dsn-stream (verb content-type &key datastar))

(s*:defroute dsn-stream (:get :text/html &key datastar)
  (s*:with-sse (gen)
    (loop
      (transmit-message *dsn-text* gen "#bg-stream" :interval 0.001)
      (sleep 10))))

   


