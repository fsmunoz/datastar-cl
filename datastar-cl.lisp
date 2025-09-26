;;;; Common Lisp SDK for Datastar
;;;;
;;;; Frederico Muñoz <fsmunoz@gmail.com>
;;;;

(in-package #:datastar-cl)

;;; Parameters

(defparameter *default-retry-duration* 1000
  "Reconnection delay after connection loss (milliseconds).")

(defparameter *default-patch-mode* "outer"
  "Default element patch mode.")

(defparameter *default-auto-remove* t
  "Default auto remove for script execution.")

;;; Types

(deftype event-type ()
  '(member :datastar-patch-elements :datastar-patch-signals))

(deftype data-lines ()
  "List of strings to be sent as SSE data lines."
  'list)

(deftype event-id ()
  '(or null string))

(deftype retry-duration ()
  '(integer 0 *))

(deftype patch-mode ()
  '(member "outer" "inner" "remove" "replace" "prepend" "append" "before" "after"))

(deftype attributes ()
  '(or null hash-table))

;;; Utilities

(defun prefix-data-lines (prefix text)
  "Return a list of strings, one for each line of TEXT, each prefixed with PREFIX."
  (mapcar (lambda (line)
            (format nil "~a ~a" prefix line))
          (split-sequence:split-sequence #\Newline text)))

(defun hash-table-to-html (table)
  "Create HTML attribute string from hash TABLE (key=\"value\" pairs)."
  (plist-to-html (alexandria:hash-table-plist table)))

(defun plist-to-html (plist)
  "Convert PLIST of attributes into HTML attribute string."
  (with-output-to-string (out)
    (alexandria:doplist (key val plist)
      (format out " ~a=\"~a\"" key val))))

(defun build-script-tag (script &key attributes auto-remove)
  "Build HTML <script> tag string with optional ATTRIBUTES and AUTO-REMOVE behavior."
  (let ((attr-parts (remove nil
                            (list
                             (when attributes (hash-table-to-html attributes))
                             (when auto-remove "data-effect=\"el.remove()\"")))))
    (format nil "<script ~{~a~}>~a</script>" attr-parts script)))

;;; Classes

(defclass sse-generator ()
  ((request :initarg :request
            :accessor request
            :documentation "The HTTP request object.")
   (response :initarg :response
             :accessor response
             :documentation "The response stream.")
   (lock :initform (bt:make-lock)
         :reader lock
         :documentation "Thread safety lock for stream operations."))
  (:documentation "Base class for Server-Sent Events generators."))



;;; Generic Functions and Methods

(defgeneric send-event (generator event-type data-lines &key event-id retry-duration)
  (:documentation "Send a Server-Sent Event through the GENERATOR."))

(defgeneric patch-elements (generator elements &key selector mode use-view-transition event-id retry-duration)
  (:documentation "Patch HTML ELEMENTS into the DOM using SSE."))

(defgeneric patch-signals (generator signals &key only-if-missing event-id retry-duration)
  (:documentation "Patch SIGNALS into the DOM using SSE."))

(defgeneric execute-script (generator script &key auto-remove attributes event-id retry-duration)
  (:documentation "Send a SCRIPT to the client for immediate execution via SSE."))

(defgeneric read-signals (request)
  (:documentation "Extract and parse datastar signals from HTTP REQUEST."))

;;; Method Implementations

(defmethod send-event ((generator sse-generator) event-type data-lines
                       &key event-id retry-duration)
  "Send an SSE event with thread safety."
  (bt:with-lock-held ((lock generator))
    (let ((stream (response generator)))
      (format stream "event: ~a~%" (string-downcase (string event-type)))      
      (when event-id
        (format stream "id: ~a~%" event-id))
      (when retry-duration
        (format stream "retry: ~a~%" retry-duration))
      (dolist (line data-lines)
        (format stream "data: ~a~%" line))
      (format stream "~%")
      ;; flush immediately       
      (force-output stream))))

(defmethod patch-elements ((generator sse-generator) (elements string)
                           &key selector
                             (mode *default-patch-mode*)
                             use-view-transition
                             event-id
                             retry-duration)
  "Patch HTML elements string into the DOM."
  (let ((data-lines
          (remove nil
                  (append
                   (unless (string= mode "outer")
                     (list (format nil "mode ~a" mode)))
                   (when selector 
                     (list (format nil "selector ~a" selector)))
                   (when use-view-transition 
                     (list "useViewTransition true"))
                   (prefix-data-lines "elements" elements)))))
    (send-event generator :datastar-patch-elements data-lines
                :event-id event-id
                :retry-duration retry-duration)))

(defmethod patch-elements ((generator sse-generator) (elements null)
                           &key selector
                             (mode *default-patch-mode*)
                             use-view-transition
                             event-id
                             retry-duration)
  "Handle null elements case for DOM patching."
  (let ((data-lines
          (remove nil
                  (append
                   (unless (string= mode "outer")
                     (list (format nil "mode ~a" mode)))
                   (when selector 
                     (list (format nil "selector ~a" selector)))
                   (when use-view-transition 
                     (list "useViewTransition true"))))))
    (when data-lines  ; Only send if there's something to send
      (send-event generator :datastar-patch-elements data-lines
                  :event-id event-id
                  :retry-duration retry-duration))))

(defmethod patch-signals ((generator sse-generator) (signals string)
                          &key only-if-missing
                            event-id
                            retry-duration)
  "Patch signals from JSON string into the DOM."
  (let ((data-lines
          (append
           (when only-if-missing
             (list "onlyIfMissing true"))
           (prefix-data-lines "signals" signals))))
    (send-event generator :datastar-patch-signals data-lines
                :event-id event-id
                :retry-duration retry-duration)))

(defmethod patch-signals ((generator sse-generator) (signals hash-table)
                          &key only-if-missing
                            event-id
                            retry-duration)
  "Patch signals from hash-table into the DOM."
  (let ((data-lines
          (append
           (when only-if-missing
             (list "onlyIfMissing true"))
           (prefix-data-lines "signals" (jzon:stringify signals)))))
    (send-event generator :datastar-patch-signals data-lines
                :event-id event-id
                :retry-duration retry-duration)))

(defmethod execute-script ((generator sse-generator) (script string)
                           &key (auto-remove *default-auto-remove*)
                             attributes
                             event-id
                             (retry-duration *default-retry-duration*))
  "Execute a script on the client side via DOM injection."
  (let* ((script-tag (build-script-tag script
                                       :attributes attributes
                                       :auto-remove auto-remove))
         (data-lines (append '("mode append" "selector body")
                             (prefix-data-lines "elements" script-tag))))
    (send-event generator :datastar-patch-elements data-lines
                :event-id event-id
                :retry-duration retry-duration)))


;;; Constructor

(defun make-sse-generator (backend-class request)
  "Create an SSE generator of type BACKEND-CLASS with REQUEST."
  (make-instance backend-class :request request :response nil))

