;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; CLACK.LISP --- Clack backend: Datastar signal extraction
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT
;;;;
;;;; The generic Clack SSE machinery (clack-sse-generator class,
;;;; buffering-writer-stream, initialize-instance, get-clack-header, constructor)
;;;; lives in lc-sse (src/sse/clack.lisp).
;;;; This file adds only the Datastar-protocol methods that specialize on
;;;; Clack env plists.

(in-package #:datastar-cl)

;;; Clack - GET method
(defmethod extract-json-data ((env list) (method (eql :get)))
  "Extract JSON from Clack GET request query string."
  (let* ((query-string (getf env :query-string))
         (datastar-param (when query-string
                           (extract-datastar-from-query query-string))))
    (when datastar-param
      (if (a:emptyp datastar-param)
          (error 'invalid-json-error
                 :json-string ""
                 :message "Empty datastar query parameter")
          datastar-param))))

(defun %read-body-to-string (stream content-length)
  "Read the HTTP POST body from STREAM, return as UTF-8 string.
   Loops on short reads when CONTENT-LENGTH is provided; reads to EOF when NIL
   (chunked-transfer or absent Content-Length header)."
  (let ((bytes
          (if content-length
              (let ((buf (make-array content-length :element-type '(unsigned-byte 8))))
                (loop with pos = 0
                      for next = (read-sequence buf stream :start pos)
                      while (and (< pos content-length) (> next pos))
                      do (setf pos next)
                      finally (return (if (= pos content-length) buf (subseq buf 0 pos)))))
              (flexi-streams:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                (loop with chunk = (make-array 4096 :element-type '(unsigned-byte 8))
                      for n = (read-sequence chunk stream)
                      while (plusp n)
                      do (write-sequence chunk out :end n))))))
    (flexi-streams:octets-to-string bytes :external-format :utf-8)))

;;; Clack - POST method
(defmethod extract-json-data ((env list) (method (eql :post)))
  "Extract JSON from Clack POST request body (binary stream).
   Handles both Content-Length and chunked-transfer (no Content-Length) requests."
  (let ((raw-body       (getf env :raw-body))
        (content-length (getf env :content-length)))
    (if (or (null raw-body)
            (and content-length (zerop content-length)))
        (error 'invalid-json-error
               :json-string ""
               :message "Empty POST request body")
        (let ((body (%read-body-to-string raw-body content-length)))
          (if (a:emptyp body)
              (error 'invalid-json-error
                     :json-string ""
                     :message "Empty POST request body")
              body)))))

;;; Clack - DELETE (signals in query string, same as GET)
(defmethod extract-json-data ((env list) (method (eql :delete)))
  (extract-json-data env :get))

;;; Clack - PUT (signals in body, same as POST)
(defmethod extract-json-data ((env list) (method (eql :put)))
  (extract-json-data env :post))

;;; Clack - PATCH (signals in body, same as POST)
(defmethod extract-json-data ((env list) (method (eql :patch)))
  (extract-json-data env :post))

(defmethod datastar-request-p ((env list))
  (string-equal "true" (get-clack-header env :datastar-request)))

(defun extract-datastar-from-query (query-string)
  "Extract and URL-decode the datastar parameter from a query string.
   Handles multi-param query strings and percent-encoding correctly."
  (cdr (assoc "datastar" (quri:url-decode-params query-string)
              :test #'string=)))

(defmethod read-signals ((env list) &key catch-errors)
  "Extract raw signals from a Clack env. Error policy lives in the :around
   method defined in signals.lisp."
  (declare (ignore catch-errors))
  (let ((json (extract-json-data env (getf env :request-method))))
    (when json (parse-and-validate-json json))))
