;;;; Common Lisp SDK for Datastar
;;;;
;;;; Woo async SSE support using libev timers
;;;;
;;;; Frederico Muñoz <fsmunoz@gmail.com>
;;;;
;;;; This module enables non-blocking SSE connections with Woo by
;;;; using libev's ev-timer instead of blocking Lisp loops. This
;;;; allows a single Woo worker to handle many SSE connections.
;;;;
;;;; Read SSE-AND-WOO.org for more details on why it was added.
;;;;
;;;; When with-sse-connection detects Woo (via woo-async-available-p),
;;;; it automatically uses this module. Set *woo-sse-debug* to T for
;;;; verbose error logging.
;;;;
;;;; Approach:
;;;;   - Each SSE connection is registered in a global registry
;;;;   - Each Woo worker's event loop gets one shared timer
;;;;   - Timer callback periodically executes all connection updates
;;;;   - Handler returns immediately, freeing the worker
;;;;
;;;; Dependencies:
;;;;   - woo.ev:*evloop* (public, accessed at runtime)
;;;;   - lev package (CFFI bindings to libev)
;;;;   - cffi (for timer allocation and callbacks)
;;;;
;;;; The "freeing the worker" is fundamental since without this, each
;;;; client connection would block a worker: Hunchentoot starts
;;;; threads, but that isn't the approach of Woo at all.
;;;;
;;;; I'm not absolutely sure if the libev approach is "portable", in
;;;; that it might break in the future since it's not directly exposed
;;;; through Clack/Woo. I didn't found another way, but improvements
;;;; will be added as needed.


(in-package #:datastar-cl)

;;; Check if Woo async support is available, we need both woo.ev and
;;; lev packages to be present This goes a bit deeper in terms of
;;; abstraction than I wanted, but I couldn't find a way to do it at
;;; the Clack or Lack level :/

(defun woo-async-available-p ()
  "Check if Woo async SSE support is available.
   Returns T if woo.ev:*evloop* is bound and the lev package is available."
  (and (find-package "WOO.EV")
       (find-package "LEV")
       (let ((evloop-sym (find-symbol "*EVLOOP*" "WOO.EV")))
         (and evloop-sym
              (boundp evloop-sym)
              (symbol-value evloop-sym)))))

;;; Connection Registry
;;;
;;; Maps event loop pointers to lists of SSE connections.
;;; Each connection contains: generator, updater function, metadata.
;;; This essentially uses a "reactor pattern" on top of the evloop
;;; We use a struct here, no need for a class atm.

(defstruct (woo-sse-connection (:conc-name woo-sse-conn-))
  "A registered SSE connection for Woo async mode."
  (id (gensym "SSE-CONN-") :type symbol)
  (generator nil :type t)
  (updater nil :type (or null function))
  (keep-alive-interval 30 :type (or null fixnum))
  (body-interval 0 :type number)
  (last-keepalive (get-universal-time) :type integer)
  (on-disconnect nil :type (or null function))
  (evloop nil :type t))  ; Store the evloop pointer for this connection

(defvar *woo-sse-debug* nil
  "When T, log all SSE errors including expected disconnections.
   When NIL (default), only log truly unexpected errors.")

(defvar *woo-sse-connections* (make-hash-table :test 'eql)
  "Registry mapping evloop pointers to lists of woo-sse-connection structs.")

(defvar *woo-sse-timers* (make-hash-table :test 'eql)
  "Registry mapping evloop pointers to their ev-timer foreign pointers.")

(defvar *woo-sse-lock* (bt:make-lock "woo-sse-registry")
  "Lock protecting the connection and timer registries.")

;;; Registry Operations

(defun get-connections-for-evloop (evloop-ptr)
  "Get all SSE connections for the given event loop.
   EVLOOP-PTR should be a CFFI pointer to the libev event loop."
  (bt:with-lock-held (*woo-sse-lock*)
    (gethash (cffi:pointer-address evloop-ptr) *woo-sse-connections*)))

(defun register-woo-sse-connection (conn)
  "Register a new SSE connection in the registry."
  (bt:with-lock-held (*woo-sse-lock*)
    (let ((key (cffi:pointer-address (woo-sse-conn-evloop conn))))
      (push conn (gethash key *woo-sse-connections*)))))

(defun unregister-woo-sse-connection (conn)
  "Remove an SSE connection from the registry."
  (bt:with-lock-held (*woo-sse-lock*)
    (let ((key (cffi:pointer-address (woo-sse-conn-evloop conn))))
      (setf (gethash key *woo-sse-connections*)
            (remove conn (gethash key *woo-sse-connections*) :test #'eq)))))

(defun connection-count-for-evloop (evloop-ptr)
  "Return the number of connections for the given event loop."
  (length (get-connections-for-evloop evloop-ptr)))

;;; Timer Management
;;;
;;; Each event loop gets one timer that handles all SSE connections.
;;; The timer interval is the minimum of all connection
;;; body-intervals. This aligns with the original with-sse-connection
;;; approach in which :BODY-INTERVAL was always the "sleep", and
;;; assuming a single application on top should be the same, but
;;; imagening that there are multiple streams with different
;;; intervals, we must wake up at the smallest one.

(defun get-min-body-interval (evloop-ptr)
  "Get the minimum body-interval across all connections for this evloop."
  (let ((connections (get-connections-for-evloop evloop-ptr)))
    (if connections
        (reduce #'min (mapcar #'woo-sse-conn-body-interval connections)
                :initial-value 1.0)
        0.1)))

;;; Timer callback
;;;
;;; This is the core of the async SSE implementation, it runs in the
;;; event loop thread, so writes should be safe...
;;;
;;; It depends on libev timer callbacks.

;;; This includes some of the logic of the original
;;; with-sse-connection macro.

(defun connection-error-p (error)
  "Return T if ERROR appears to be a connection-related error.
   Used to suppress logging of expected disconnection errors."
  (let ((msg (ignore-errors (princ-to-string error))))
    (and msg
         (or (search "closed" msg :test #'char-equal)
             (search "connection" msg :test #'char-equal)
             (search "socket" msg :test #'char-equal)
             (search "broken pipe" msg :test #'char-equal)
             (search "reset by peer" msg :test #'char-equal)
             (search "end of file" msg :test #'char-equal)))))

(defun process-woo-sse-connections (evloop-ptr)
  "Process all SSE connections for the given event loop.
   Called from the libev timer callback."
  (let ((connections (get-connections-for-evloop evloop-ptr))
        (now (get-universal-time)))
    (dolist (conn connections)
      (handler-case
          (let ((gen (woo-sse-conn-generator conn)))
            ;; Execute the updater body
            (when (woo-sse-conn-updater conn)
              (funcall (woo-sse-conn-updater conn) gen))

            ;; Send keep-alive if needed
            (let ((ka-interval (woo-sse-conn-keep-alive-interval conn)))
              (when (and ka-interval
                         (>= (- now (woo-sse-conn-last-keepalive conn))
                             ka-interval))
                (keep-sse-alive gen)
                (setf (woo-sse-conn-last-keepalive conn) now))))

        ;; Handle disconnection errors - these are expected and handled silently
        (end-of-file (e)
          (handle-woo-sse-disconnect conn e))
        (stream-error (e)
          (handle-woo-sse-disconnect conn e))
        (sse-connection-lost (e)
          (handle-woo-sse-disconnect conn e))
        ;; Catch-all: only log if it's truly unexpected or debug is enabled
        (error (e)
          (when (or *woo-sse-debug*
                    (not (connection-error-p e)))
            (format *error-output* "~&[datastar-cl] Woo SSE error: ~A~%" e))
          (handle-woo-sse-disconnect conn e))))))

(defun handle-woo-sse-disconnect (conn error)
  "Handle disconnection of a Woo SSE connection."
  (let ((on-disconnect (woo-sse-conn-on-disconnect conn))
        (gen (woo-sse-conn-generator conn)))
    ;; Call disconnect callback
    (when on-disconnect
      (ignore-errors
        (funcall on-disconnect gen error)))
    ;; Close generator
    (ignore-errors
      (close-sse-generator gen))
    ;; Remove from registry
    (unregister-woo-sse-connection conn)
    ;; Check if we should stop the timer
    (let ((evloop (woo-sse-conn-evloop conn)))
      (when (zerop (connection-count-for-evloop evloop))
        (stop-woo-sse-timer evloop)))))

;;; libev timer integration
;;;
;;; We use CFFI to interface with libev's ev-timer (I couldn't find a
;;; better way, or a way that avoided using the FFI directly).

(defvar *woo-sse-timer-callback* nil
  "The CFFI callback for the SSE timer. Set during initialization.")

(defun ensure-woo-sse-timer-callback ()
  "Ensure the CFFI callback is defined."
  (unless *woo-sse-timer-callback*
    ;; Define the callback at runtime, lev may not be loaded yet!
    (let ((lev-pkg (find-package "LEV")))
      (when lev-pkg
        (setf *woo-sse-timer-callback*
              (cffi:defcallback woo-sse-timer-cb :void
                  ((evloop :pointer) (timer :pointer) (events :int))
                (declare (ignore events))
                (process-woo-sse-connections evloop)))))))

(defun start-woo-sse-timer (evloop-ptr interval)
  "Start a timer on the given event loop for SSE processing.
   INTERVAL is in seconds."
  (ensure-woo-sse-timer-callback)
  (let ((lev-pkg (find-package "LEV")))
    (when lev-pkg
      (let ((ev-timer-init (find-symbol "EV-TIMER-INIT" lev-pkg))
            (ev-timer-start (find-symbol "EV-TIMER-START" lev-pkg))
            (ev-timer-struct (list :struct (find-symbol "EV-TIMER" lev-pkg))))
        (when (and ev-timer-init ev-timer-start)
          (let ((timer (cffi:foreign-alloc ev-timer-struct)))
            ;; Initialize timer: after=0.0 (start immediately), repeat=interval
            (funcall ev-timer-init timer 'woo-sse-timer-cb
                     (coerce 0.0 'double-float)
                     (coerce interval 'double-float))
            ;; Start the timer...
            (funcall ev-timer-start evloop-ptr timer)
            ;; ... and store in registry
            (bt:with-lock-held (*woo-sse-lock*)
              (setf (gethash (cffi:pointer-address evloop-ptr) *woo-sse-timers*)
                    timer))
            timer))))))

(defun stop-woo-sse-timer (evloop-ptr)
  "Stop and free the timer for the given event loop."
  (let ((lev-pkg (find-package "LEV")))
    (when lev-pkg
      (let ((ev-timer-stop (find-symbol "EV-TIMER-STOP" lev-pkg)))
        (bt:with-lock-held (*woo-sse-lock*)
          (let* ((key (cffi:pointer-address evloop-ptr))
                 (timer (gethash key *woo-sse-timers*)))
            (when timer
              ;; Stop the timer
              (when ev-timer-stop
                (ignore-errors
                  (funcall ev-timer-stop evloop-ptr timer)))
              ;; Free CFFI memory
              (ignore-errors
                (cffi:foreign-free timer))
              ;; Remove from registry
              (remhash key *woo-sse-timers*))))))))

(defun get-woo-sse-timer (evloop-ptr)
  "Get the timer for the given event loop, or NIL if none."
  (bt:with-lock-held (*woo-sse-lock*)
    (gethash (cffi:pointer-address evloop-ptr) *woo-sse-timers*)))

(defun ensure-woo-sse-timer (evloop-ptr interval)
  "Ensure a timer is running for the given event loop."
  (unless (get-woo-sse-timer evloop-ptr)
    (start-woo-sse-timer evloop-ptr interval)))

;;; Higher level API for SDK implementation
;;;
;;; These functions are called from with-sse-connection when in Woo mode.
;;; These allows the original macros to work

(defun setup-woo-async-sse (generator updater-fn
                             &key (keep-alive-interval 30)
                                  (body-interval 0.1)
                                  on-connect
                                  on-disconnect)
  "Set up an async SSE connection for Woo.
   Returns immediately after registering the connection.

   GENERATOR - The SSE generator (already created)
   UPDATER-FN - Function of one arg (generator) called periodically
   KEEP-ALIVE-INTERVAL - Seconds between keep-alive messages
   BODY-INTERVAL - Seconds between updater calls (timer interval)
   ON-CONNECT - Called immediately with generator
   ON-DISCONNECT - Called with generator and error on disconnection"
  (let* ((woo-ev-pkg (find-package "WOO.EV"))
         (evloop-sym (when woo-ev-pkg (find-symbol "*EVLOOP*" woo-ev-pkg)))
         (evloop (when evloop-sym (symbol-value evloop-sym))))
    (unless evloop
      (error "Woo async SSE requires woo.ev:*evloop* to be bound"))

    ;; Create connection record
    (let ((conn (make-woo-sse-connection
                 :generator generator
                 :updater updater-fn
                 :keep-alive-interval keep-alive-interval
                 :body-interval (max body-interval 0.01)  ; Minimum 10ms
                 :on-disconnect on-disconnect
                 :evloop evloop)))

      ;; Call on-connect
      (when on-connect
        (funcall on-connect generator))

      ;; Register connection
      (register-woo-sse-connection conn)

      ;; Ensure timer is running
      (ensure-woo-sse-timer evloop (max body-interval 0.01))

      ;; Return the connection for reference
      conn)))

;;; Macro Support
;;;
;;; Helper for with-sse-connection to expand differently for Woo.

(defun expand-woo-sse-connection (generator-var env responder
                                   body keep-alive-interval body-interval
                                   on-connect on-disconnect)
  "Generate expansion for with-sse-connection in Woo async mode.
   This creates a generator, wraps body in a closure, and registers everything."
  (let ((conn-var (gensym "CONN")))
    `(let ((,generator-var (make-clack-sse-generator ,env ,responder)))
       (let ((,conn-var
               (setup-woo-async-sse
                ,generator-var
                (lambda (,generator-var)
                  ,@body)
                :keep-alive-interval ,keep-alive-interval
                :body-interval ,(if (zerop body-interval) 0.1 body-interval)
                :on-connect ,on-connect
                :on-disconnect ,on-disconnect)))
         (declare (ignore ,conn-var))
         ;; Return nil - handler exits, worker freed
         nil))))
