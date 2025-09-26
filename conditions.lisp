;;;; Common Lisp SDK for Datastar
;;;;
;;;; Conditions and error handling
;;;;
;;;; Frederico Muñoz <fsmunoz@gmail.com>
;;;;

(in-package #:datastar-cl)

;;; Check https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node339.html
;;; and https://lispcookbook.github.io/cl-cookbook/error_handling.html
;;; for an overview of the Common Lisp condition system.

;;; ADR Compliance Notes:
;;;
;;; 1. ReadSignals: "Must return error for invalid JSON" ->
;;; Implemented via INVALID-JSON-ERROR condition
;;;
;;; 2. ServerSentEventGenerator.send (ADR line 99): "Must return/throw
;;;     per language conventions" -> Implemented via CL's standard
;;;     STREAM-ERROR condition (no wrapping done)

(define-condition datastar-error (error)
  ((message :initarg :message 
            :initform nil 
            :accessor datastar-error-message
            :documentation "Human-readable error message."))
  (:documentation "Base condition for datastar-cl errors.")
  (:report (lambda (condition stream)
             (format stream "Datastar error~@[: ~A~]" 
                     (datastar-error-message condition)))))

;;; Signal processing errors
;;
;; If needed, we can add more, but the signals and json errors are the
;; ones that are most needed

(define-condition signal-error (datastar-error)
  ()
  (:documentation "Base condition for signal processing errors."))

(define-condition invalid-json-error (signal-error)
  ((json-string :initarg :json-string
                :accessor invalid-json-error-json-string
                :documentation "The invalid JSON string."))
  (:documentation "Signaled when JSON parsing fails.")
  (:report (lambda (condition stream)
             (format stream "Invalid JSON in datastar signals~@[: ~A~] (JSON: ~S)"
                     (datastar-error-message condition)
                     (invalid-json-error-json-string condition)))))

;;; SSE connection errors

(define-condition sse-connection-lost (stream-error)
  ((generator :initarg :generator
              :accessor sse-connection-lost-generator
              :documentation "The SSE generator with lost connection."))
  (:documentation "Signaled when SSE connection is detected as closed by client.")
  (:report (lambda (condition stream)
             (format stream "SSE connection lost (client disconnected)"))))

