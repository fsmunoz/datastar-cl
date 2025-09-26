;;;; Common Lisp SDK for Datastar
;;;;
;;;; Clack specific methods and functions

(in-package #:datastar-cl)

;;; Classes and methods

(defclass clack-sse-generator (sse-generator)
  ((env :initarg :env
        :accessor env
        :documentation "The Clack environment hash-table.")
   (responder :initarg :responder
              :accessor responder
              :documentation "The Clack response callback function."))
  (:documentation "Clack-specific SSE generator implementation."))

;;; Method implementations for Clack

(defmethod initialize-instance :after ((generator clack-sse-generator) &key env responder &allow-other-keys)
  "Set up SSE headers and response stream for Clack."
  ;; Store the environment and responder
  (setf (env generator) env
        (responder generator) responder)
  ;; Set up SSE headers and get writer function
  (let ((headers '(:content-type "text/event-stream; charset=utf-8"
                   :cache-control "no-cache"
                   :connection "keep-alive")))
    (setf (response generator)
	  (lack/util/writer-stream:make-writer-stream
	   (funcall responder `(200 ,headers))))))

(defmethod read-signals ((env list))
  "Extract and parse datastar signals from Clack environment plist."
  (let* ((method (getf env :request-method))
         (json-data
           (cond
             ((eq method :get)
	      (hunchentoot:url-decode (cadr (split-sequence:split-sequence "=" (getf env :query-string) :test 'string=))))
	      ;;(cdr (assoc "datastar" (getf env :query-string) :test #'string=)))
             ((eq method :post)
              (getf env :raw-body)))))
    (when json-data
      (jzon:parse json-data))))

;;; Constructor

(defun make-clack-sse-generator (env responder)
  "Create a Clack SSE generator with environment ENV."
  (make-instance 'clack-sse-generator :env env :responder responder))

;;; Helpers

(defun make-clack-datastar-handler (handler-function)
  "Create a Clack/Lack application that handles datastar requests with
streaming SSE. HANDLER-FUNCTION should accept (signals generator) and
process the signals."
  (lambda (env)
    (lambda (responder)
      (let* ((signals (read-signals env))
             (generator (make-clack-sse-generator env responder)))
        ;; Process the signals
        (funcall handler-function signals generator)
	;; Explicitly close the connection for Woo compatibility
	;(funcall (writer generator))
	(finish-output (response generator))
	))))

