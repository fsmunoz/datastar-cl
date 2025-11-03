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

;; Inspired by Hunchentoot's approach (but not identical):
;;   [Special variable]
;;   *catch-errors-p*

;;   If the value of this variable is NIL (the default is T), then
;;   errors which happen while a request is handled aren't caught as
;;   usual, but instead your Lisp's debugger is invoked. This variable
;;   should obviously always be set to a true value in a production
;;   environment. See MAYBE-INVOKE-DEBUGGER if you want to fine-tune
;;   this behaviour.
;;
;; We do not implement MAYBY-INVOKE-DEBUGER or anything else: use
;; Hunchentoot's or Clack's debugging variables for dealing with the
;; backends: this *catch-errors-p* is exclusively related with
;; datastar-cl

(defparameter *catch-errors-p* nil
  "Controls error handling in READ-SIGNALS only.

   When T: Catches DATASTAR-ERROR conditions, logs to *ERROR-OUTPUT*,
                     and returns NIL. Suitable for production.

   When NIL (default): Errors propagate to debugger. Suitable for development.

   NOTE: Does NOT affect SSE sending methods (SEND-EVENT, PATCH-*,
         EXECUTE-SCRIPT). Stream errors from those methods always
         propagate per CL conventions. Does NOT affect
         WITH-SSE-CONNECTION error handling, which always catches
         connection lifecycle errors for proper cleanup.")

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
   (compressed-stream :initarg :compressed-stream
                      :accessor compressed-stream
                      :documentation "The compressed stream (depending on the algorithm)")
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

(defgeneric read-signals (request-or-env &key catch-errors)
  (:documentation "Extract and parse datastar signals from HTTP request.

   Returns hash-table of parsed JSON signals, or NIL if parsing fails
   and CATCH-ERRORS is true.

   Signals: INVALID-JSON-ERROR if JSON parsing fails (per ADR requirement).
            Behavior controlled by CATCH-ERRORS (defaults to *CATCH-ERRORS-P*)."))

(defgeneric ensure-connection-open (generator)
  (:documentation "Verify SSE connection is still open. Signals error if closed.

   Default behavior: do nothing (rely on stream I/O to detect errors naturally).
   Backend-specific implementations can proactively check connection state.

   Signals: SSE-CONNECTION-LOST if connection is detected as closed."))


;;; Method Implementations

(defmethod ensure-connection-open ((generator sse-generator))
  "Default implementation: no proactive checking.
   Stream errors will be caught naturally during I/O operations."
  (values))

(defmethod send-event :before ((generator sse-generator) event-type data-lines
                               &key event-id retry-duration)
  "Check connection health before sending SSE event."
  (declare (ignore event-type data-lines event-id retry-duration))
  (ensure-connection-open generator))

;; (defmethod send-event ((generator sse-generator) event-type data-lines
;;                        &key event-id retry-duration)
;;   "Send an SSE event with thread safety.

;;    Signals: STREAM-ERROR if the response stream is closed or writing fails.
;;             This is standard CL behavior - no wrapping is performed."
;;   (bt:with-lock-held ((lock generator))
;;     (let ((stream (response generator)))
;;       (format stream "event: ~a~%" (string-downcase (string event-type)))
;;       (when event-id
;;         (format stream "id: ~a~%" event-id))
;;       (when retry-duration
;;         (format stream "retry: ~a~%" retry-duration))
;;       (dolist (line data-lines)
;;         (format stream "data: ~a~%" line))
;;       (format stream "~%")
;;       ;; flush immediately
;;       (force-output stream))))


(defmethod send-event ((generator sse-generator) event-type data-lines
                       &key event-id retry-duration)
  "Send an SSE event with thread safety.

   Signals: STREAM-ERROR if the response stream is closed or writing fails.
            This is standard CL behavior - no wrapping is performed."
  (bt:with-lock-held ((lock generator))
    (let ((stream (response generator))
          (compressed-stream (compressed-stream generator)))
      (format stream "event: ~a~%" (string-downcase (string event-type)))
      (when event-id
        (format stream "id: ~a~%" event-id))
      (when retry-duration
        (format stream "retry: ~a~%" retry-duration))
      (dolist (line data-lines)
        (format stream "data: ~a~%" line))
      (format stream "~%")
      ;; flush immediately
      (when compressed-stream
        (finish-output (compressed-stream generator)))
      (force-output stream))))

(defmethod patch-elements ((generator sse-generator) (elements string)
                           &key selector
                             (mode *default-patch-mode*)
                             use-view-transition
                             event-id
                             retry-duration)
  "Patch HTML elements string into the DOM.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
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
  "Patch signals from JSON string into the DOM.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
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
  "Patch signals from hash-table into the DOM.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
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
                             retry-duration)
  "Execute a script on the client side via DOM injection.

   Signals: STREAM-ERROR if SSE transmission fails (via SEND-EVENT)."
  (let* ((script-tag (build-script-tag script
                                       :attributes attributes
                                       :auto-remove auto-remove))
         (data-lines (append '("mode append" "selector body")
                             (prefix-data-lines "elements" script-tag))))
    ;; The ADR says to only pass retry-duration if it's not the default (1000),
    ;; so we add this here
    (send-event generator :datastar-patch-elements data-lines
                :event-id event-id
                :retry-duration (when (and retry-duration
                                          (/= retry-duration *default-retry-duration*))
                                  retry-duration))))


;;; Utility and conditions

(defgeneric extract-json-data (request method)
  (:documentation "Extract raw JSON data from request based on HTTP method.
Returns string of JSON data or NIL if no data present.
Signals conditions for error cases (empty body, etc.)."))

;; Used by all (?) backends
(defun parse-and-validate-json (json-string)
  "Parse JSON-STRING and validate it's a hash-table (JSON object).
   Returns hash-table or signals INVALID-JSON-ERROR.

   Per ADR: ReadSignals must return error for invalid JSON."
  (handler-case
      (let ((parsed (jzon:parse json-string)))
        ;; Validate that result is a hash-table (JSON object). This
        ;; should be the case when using jzon:parse with key:value
        (unless (hash-table-p parsed)
          (error 'invalid-json-error
                 :json-string json-string
                 :message (format nil "Expected JSON object, got ~A" 
                                (type-of parsed))))
        parsed)
    (jzon:json-parse-error (e)
      (error 'invalid-json-error
             :json-string json-string
             :message (format nil "~A" e)))))

;;; Constructor

(defun make-sse-generator (backend-class request)
  "Create an SSE generator of type BACKEND-CLASS with REQUEST."
  (make-instance backend-class :request request :response nil))


;;; SSE management

(defgeneric keep-sse-alive (generator)
  (:documentation "Send keep-alive through SSE stream."))

(defgeneric close-sse-generator (generator)
  (:documentation "Close the SSE generator's response stream."))

(defmethod close-sse-generator ((generator sse-generator))
  "Closes the response stream stored in the generator's RESPONSE slot,
   (the same for all SSE generator types)."
  (let ((stream (response generator)))
    (when (and stream (open-stream-p stream))
      (force-output stream)
      (finish-output stream)
      (close stream))))

(defmacro with-sse-connection ((generator-var request-or-env-responder
                                &key (keep-alive-interval 30)
                                     (body-interval 0)
                                     on-connect
                                     on-disconnect)
                              &body body)
  "Maintain a LONG-LIVED SSE connection while executing BODY in a loop.

   Use this when:
   - Streaming periodic updates to clients
   - Maintaining real-time push connections
   - Need keep-alive management to prevent proxy timeouts
   - Server needs to push data continuously (server → client direction)

   For ONE-SHOT requests that close immediately, use MAKE-*-DATASTAR-HANDLER instead.

   This macro handles:
   - Connection lifecycle (on-connect, on-disconnect hooks)
   - Keep-alive management (prevents proxy/firewall timeouts)
   - Error handling and cleanup (guaranteed via unwind-protect)
   - Backend auto-detection (Hunchentoot vs Clack)

   Arguments:
     GENERATOR-VAR: Symbol to bind the sse-generator to (used in the body)
     REQUEST-OR-ENV-RESPONDER: Either a single request (Hunchentoot)
                              or (env responder) list (Clack)

   Keywords:
     :KEEP-ALIVE-INTERVAL - Seconds between keep-alive messages (default 30)
                           Set to NIL to disable keep-alive
     :BODY-INTERVAL - Seconds to sleep between body executions (default 0)
     :ON-CONNECT - Function called with generator when connected
     :ON-DISCONNECT - Function called with generator and error when disconnected

   BODY is executed repeatedly in an infinite loop, sleeping BODY-INTERVAL
   seconds between executions. Keep-alive messages are sent every
   KEEP-ALIVE-INTERVAL seconds independently to prevent connection timeouts.

   Error Handling:
     Connection lifecycle errors (END-OF-FILE, STREAM-ERROR) are always caught
     to ensure proper cleanup via CLOSE-SSE-GENERATOR, regardless of
     *CATCH-ERRORS-P*. This prevents leaked connections and worker threads.

     ON-DISCONNECT hooks receive the error condition for logging/handling.

   Examples:
     ;; Stream updates every 10 seconds with keep-alive
     (with-sse-connection (gen hunchentoot:*request*
                            :keep-alive-interval 30
                            :body-interval 10)
       (patch-signals gen (get-current-data)))

     ;; Continuous polling, no keep-alive needed
     (with-sse-connection (gen (env responder)
                            :keep-alive-interval nil
                            :body-interval 0)
       (poll-and-send gen))

     ;; With lifecycle hooks
     (with-sse-connection (gen hunchentoot:*request*
                            :on-connect #'log-connection
                            :on-disconnect #'log-disconnection)
       (stream-notifications gen))"
  (let ((error-var (gensym "ERROR"))
        (last-keepalive-var (gensym "LAST-KEEPALIVE")))
    `(let ((,generator-var ,(if (listp request-or-env-responder)
                                `(make-clack-sse-generator
                                  ,(first request-or-env-responder)
                                  ,(second request-or-env-responder))
                                `(make-hunchentoot-sse-generator
                                  ,request-or-env-responder)))
           (,last-keepalive-var (get-universal-time)))


       (unwind-protect
            (progn
              ;; Connection established hook
              ,@(when on-connect
                  `((funcall ,on-connect ,generator-var)))

              ;; Main loop
              (handler-case
                  (loop
                    ;; Execute body
                    ,@body

                    ;; Sleep for body interval
                    (when (> ,body-interval 0)
                      (sleep ,body-interval))

                    ;; Send keep-alive only if interval passed _AND_ keep-alive enabled
                    ,@(when keep-alive-interval
                        `((when (>= (- (get-universal-time) ,last-keepalive-var)
                                    ,keep-alive-interval)
                            (keep-sse-alive ,generator-var)
                            (setf ,last-keepalive-var (get-universal-time))))))

                ;; Disconnection handlers
                (end-of-file (,error-var)
                  ,@(when on-disconnect
                      `((funcall ,on-disconnect ,generator-var ,error-var))))

                (stream-error (,error-var)
                  ,@(when on-disconnect
                      `((funcall ,on-disconnect ,generator-var ,error-var))))

                (error (,error-var)
                  ,@(when on-disconnect
                      `((funcall ,on-disconnect ,generator-var ,error-var)))))

              nil) ; Return nil on normal exit

         ;; This is _guaranteed_ to run, closing the stream and freeing the
         ;; worker (see previous comment)
         (close-sse-generator ,generator-var)))))

(defmacro with-sse-response ((generator-var request-or-env-responder)
                             &body body)
  "Execute BODY once with an SSE generator, then close connection.

   Use for ONE-SHOT responses (connection closes after sending):
   - Form submission responses
   - Single action responses
   - Polling endpoints (client polls, server responds once)

   For LONG-LIVED streaming (connection stays open), use WITH-SSE-CONNECTION.

   Backend auto-detection (same pattern as WITH-SSE-CONNECTION):
   - Hunchentoot: pass single request object
   - Clack: pass (env responder) list

   Arguments:
     GENERATOR-VAR: Symbol to bind the sse-generator to (used in the body)
     REQUEST-OR-ENV-RESPONDER: Either a single request (Hunchentoot)
                              or (env responder) list (Clack)

   BODY executes once, connection closes when handler returns.

   Examples:
     ;; Hunchentoot
     (with-sse-response (gen hunchentoot:*request*)
       (patch-signals gen (process-data)))

     ;; Clack polling endpoint
     (with-sse-response (gen (env responder))
       (funcall updater gen))"
  `(let ((,generator-var ,(if (listp request-or-env-responder)
                              ;; Clack: (env responder) list
                              `(make-clack-sse-generator
                                ,(first request-or-env-responder)
                                ,(second request-or-env-responder))
                              ;; Hunchentoot: single request object
                              `(make-hunchentoot-sse-generator
                                ,request-or-env-responder))))
     (unwind-protect
          (progn
            ,@body)
       ;; Guaranteed cleanup - even if body signals error
       (close-sse-generator ,generator-var))
     nil))


