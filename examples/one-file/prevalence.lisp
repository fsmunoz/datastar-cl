;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; PREVALENCE.LISP --- Blade Runner replicant registry (BKNR datastore)
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

;;; Demonstrates the prevalence model using BKNR Datastore. Read alongside
;;; event-sourcing.lisp (#11), which models the same write/query/push pattern
;;; with an event log instead.
;;;
;;; WHAT IS PREVALENCE?
;;;
;;; The prevalence model (Prevayler, Klaus Wüstefeld, 2001) keeps the entire
;;; domain in RAM, here (using BKNR datastore, inspired by Prevayler and similar
;;; libraries) as live CLOS objects, logs every mutation (a "transaction") to
;;; disk atomically, and periodically saves a full snapshot. On restart, load
;;; the latest snapshot, then replay the transaction log to reach the current
;;; state. The same overall recovery mechanism as event sourcing, but the
;;; *emphasis* is different. This is what I came up with:
;;;
;;; COMPARISON WITH EVENT SOURCING:
;;;
;;;  |Aspect             |Event Sourcing (cf. #11)         |Prevalence (this file)
;;;  |-------------------+---------------------------------+--------------------------
;;;  |Source of truth    |the event log                    |live CLOS objects in RAM
;;;  |Current state      |derived by folding events        |read directly off object
;;;  |Log role           |primary store                    |durability / replay
;;;  |Reads              |"fold" or build a projection     |indexed object query
;;;  |Writes             |append a domain event            |mutate object in logged txn
;;;  |Restart            |replay log from zero             |snapshot + log replay
;;;  |Audit "why"        |strong, events are domain facts  |weaker, logs commands
;;;  |Time travel        |replay to any prefix             |restore-store :until <ts>
;;;  |Lineage            |DDD/CQRS, emerged ~2000s         |Prevayler, 2001
;;;  |-------------------+---------------------------------+---------------------------
;;;
;;; Prevalence already contained log + replay + snapshot before event sourcing
;;; made the log the primary model. ES's key move seems to be philosophical, to
;;; elevate the log from a recovery artefact to the single source of truth, at
;;; the cost of needing a "projection".
;;;
;;; Prevalence:
;;;   - Objects are long-lived, mutable, and their current state is what matters.
;;;   - You need indexed reads across many objects (no fold required).
;;;   - You want persistence without a separate DB process.
;;;   - The domain is well-understood and schema evolution is manageable.
;;; Event sourcing:
;;;   - The history itself is a first-class business artefact (audit, compliance).
;;;   - Multiple independent read models need to project the same event stream.
;;;   - Temporal queries ("what was the state on date X?") are frequent.
;;;
;;; PERSISTENCE DEMO:
;;;   1. Start the server, mutate state (sight, retire, spawn).
;;;   2. Ctrl-C, then: sbcl --load prevalence.lisp
;;;   State survives: registry loaded from /tmp/prevalence-store/.
;;;   The seed block does NOT re-run (objects present, guard fires).
;;;   To snapshot manually in the REPL: (bknr.datastore:snapshot)
;;;
;;; Run with: sbcl --load prevalence.lisp
;;;
;;; To build a standalone executable (SBCL):
;;;   sbcl --non-interactive \
;;;        --eval '(load "prevalence.lisp")' \
;;;        --eval '(in-package :prevalence)' \
;;;        --eval '(progn (ht:stop *server*) (setf *server* nil))' \
;;;        --eval '(sb-ext:save-lisp-and-die "rep-detect" :toplevel (function prevalence::main) :executable t)'
;;;   ./rep-detect
;;; The stop/nil step removes the load-time acceptor thread so the core dumps
;;; cleanly (SBCL refuses save-lisp-and-die with multiple threads running).

;;; SYSTEM DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload '(:hunchentoot :spinneret :bordeaux-threads
                :datastar-cl/hunchentoot :datastar-cl/registry :bknr.datastore))

(defpackage #:prevalence
  (:use #:cl #:hunchentoot)
  (:local-nicknames (:sp  :spinneret)
                    (:d*  :datastar-cl)
                    (:reg :datastar-cl.registry)
                    (:bt  :bordeaux-threads)
                    (:ht  :hunchentoot)
                    (:bk  :bknr.datastore)
                    (:idx :bknr.indices)))

(in-package #:prevalence)

;;; PERSISTENT CLASS: objects are the model; slots are mutable state ;;;;;;;;;;
;;;
;;; DEFINE-PERSISTENT-CLASS is DEFCLASS with (:metaclass persistent-class).
;;; Slot option :READ generates a reader; :UPDATE generates a reader + writer.
;;; The metaclass enforces that :UPDATE slot writes only happen inside a logged
;;; transaction: an attempt outside raises a SIMPLE-ERROR.
;;;
;;; Indices are *transient* and rebuild automatically on every store open/restore.
;;; STRING-UNIQUE-INDEX uses EQUAL as hash-test (string-aware).
;;; HASH-INDEX uses EQL, right for keyword values like :AT-LARGE, :RETIRED.

(bk:define-persistent-class replicant ()
  ((designation :read :index-type idx:string-unique-index
                      :index-reader replicant-with-designation
                      :index-values all-replicants)
   (model       :read)                          ; string, distinct values below
   (role        :read)
   (incept      :read)
   (status      :update :initform :at-large
                        :index-type  idx:hash-index
                        :index-reader replicants-with-status
                        :index-keys   all-statuses)
   (sightings   :update :initform 0)
   (last-seen   :update :initform nil)))

;;; TRANSACTIONS -- must be defined BEFORE the store is opened ;;;;;;;;;;;;;;;;;
;;;
;;; DEFTRANSACTION defines NAME (public: logs + executes) and TX-NAME (the
;;; implementation, called only inside a transaction context). The log stores
;;; the function symbol and its arguments, it's not a domain event, but a
;;; command. On restore, each logged call is re-executed against the live object
;;; graph.
;;;
;;; Critical ordering: make-instance 'mp-store immediately replays the
;;; transaction log. TX-SPAWN-REPLICANT etc. must exist before that call or the
;;; restore will signal UNDEFINED-FUNCTION.

(bk:deftransaction spawn-replicant (designation model role incept)
  (make-instance 'replicant :designation designation :model model
                             :role role :incept incept))

(bk:deftransaction sight-replicant (designation)
  (let ((r (replicant-with-designation designation)))
    (when r
      (incf (replicant-sightings r))
      (setf (replicant-last-seen r) (get-universal-time)))))

(bk:deftransaction retire-replicant (designation)
  (let ((r (replicant-with-designation designation)))
    (when (and r (eq (replicant-status r) :at-large))
      (setf (replicant-status r) :retired))))

;;; STORE: single global instance, idempotent on reload ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; make-instance 'mp-store opens (or restores) the store and binds *store*.
;;; The UNLESS guard makes file-reload in a live SLIME session harmless.

(defparameter *store-dir* "/tmp/prevalence-store/")

(unless (boundp 'bk:*store*)
  (make-instance 'bk:mp-store :directory *store-dir*
                 :subsystems (list (make-instance 'bk:store-object-subsystem))))

;;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make it pretty. Extracted from the LASS version of the house style and copied
(defparameter *css*
  "body{background:#0a0803;color:#ffb000;font-family:'Cousine','Courier New',monospace;
        margin:0;padding:40px 20px;display:flex;justify-content:center}
   body::before{content:'';position:fixed;inset:0;
                background:linear-gradient(rgba(18,16,16,0) 50%,rgba(0,0,0,.25) 50%);
                background-size:100% 3px;pointer-events:none;z-index:100}
   .terminal{max-width:1000px;width:100%;border:1px solid #996a00;padding:30px;
             box-shadow:0 0 15px rgba(255,176,0,.15)}
   h1{font-family:'Saira Extra Condensed','Saira Semi Condensed','Courier New',monospace;
       display:grid;grid-template-columns:auto 1fr;column-gap:.6rem;font-size:2rem;
       margin:0 0 5px;text-transform:uppercase;font-weight:500;
       border-bottom:1px dashed #996a00;padding-bottom:10px}
   .acronym{font-family:'DejaVu Sans',sans-serif;font-size:3.6rem;font-weight:700;
             color:#ffcc00;line-height:.9;letter-spacing:-11px;margin-right:10px}
   .wordmark{font-family:'Saira Extra Condensed','Courier New',monospace;
              display:flex;flex-direction:column;justify-content:center;
              line-height:.73;letter-spacing:12px}
   .combine{letter-spacing:9px}
   .meta{color:#a87d00;font-size:.85rem;text-transform:uppercase;letter-spacing:1px;margin:.3em 0}
   h2{font-family:'Saira Extra Condensed','Courier New',monospace;font-size:1.5rem;
       text-transform:uppercase;letter-spacing:2px;margin:1em 0 .3em;color:#ffcc00}
   table{border-collapse:collapse;width:100%;margin:.5em 0}
   th{color:#ffcc00;border-bottom:1px solid #996a00;padding:.3em .6em;
       text-align:left;text-transform:uppercase;letter-spacing:1px}
   td{padding:.2em .6em;border-bottom:1px solid #1a1000}
   .at-large{color:#ffb000}.retired td{color:#664d00}
   .stats{color:#f89d00;font-size:.85rem;text-transform:uppercase;letter-spacing:1px;margin:.5em 0;padding:1rem}
   button{background:transparent;border:1px solid #996a00;color:#ffb000;font-family:inherit;
          font-size:.65rem;padding:4px 12px;cursor:pointer;
          letter-spacing:1px;text-transform:uppercase;margin-right:.3em}
   button:hover{background:#ffb000;color:#0a0803}
   input,select{background:transparent;border:1px solid #996a00;color:#ffb000;
                font-family:inherit;font-size:.85rem;padding:4px 8px;margin-right:.3em}
   input:focus,select:focus{outline:none;border-color:#ffb000}
   .footer{margin-top:20px;padding-top:10px;border-top:1px dashed #996a00;
           color:#a87d00;font-size:.72rem;letter-spacing:1px;text-transform:uppercase}
   .msg{color:#ff4400;min-height:1.2em;margin:.3em 0}
   .processing{color:#ffb000;min-height:1.2em;margin:.3em 0;
               letter-spacing:1px;animation:blink 1s steps(2) infinite}
   @keyframes blink{50%{opacity:.3}}
   hr{border:none;border-top:1px dashed #996a00;margin:1em 0}")

(defun fmt-time (ut)
  (when ut
    (multiple-value-bind (s m h) (decode-universal-time ut)
      (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))))

(defun render-stats ()
  ;; Indexed O(1) reads. Compare to event-sourcing.lisp's balance-from which
  ;; must reduce the whole log to answer this question. Of course, there are
  ;; others ways of doing it there as well, so this is not an "advantage" in
  ;; general, but specific to the implementation here
  (let ((at-large (length (replicants-with-status :at-large)))
        (retired  (length (replicants-with-status :retired)))
        (models   (remove-duplicates
                   (mapcar #'replicant-model (all-replicants)) :test #'equal)))
    (format nil "AT-LARGE: ~D  |  RETIRED: ~D  |  MODELS: ~{~A~^, ~}"
            at-large retired models)))

(defun render-registry ()
  (sp:with-html-string
    (:div :id "registry"
          (:p :class "stats" (:raw (render-stats)))
          (:table
           (:thead
            (:tr (:th "DESIGNATION") (:th "MODEL") (:th "ROLE")
                 (:th "INCEPT") (:th "STATUS") (:th "SIGHTINGS")
                 (:th "LAST SEEN") (:th "")))
           (:tbody
            (dolist (r (all-replicants))
              (let ((desig (replicant-designation r))
                    (class (string-downcase (symbol-name (replicant-status r)))))
                (:tr :class class
                     (:td desig)
                     (:td (replicant-model r))
                     (:td (replicant-role r))
                     (:td (replicant-incept r))
                     (:td (symbol-name (replicant-status r)))
                     (:td (replicant-sightings r))
                     (:td (or (fmt-time (replicant-last-seen r)) "--"))
                     (:td (:button :|data-on:click|
                                   (format nil "$target='~A'; ~A"
                                           desig (d*:sse-post "/sight"))
                                   "SIGHT")
                          (when (eq (replicant-status r) :at-large)
                            (:button :|data-on:click|
                                     (format nil "$target='~A'; ~A"
                                             desig (d*:sse-post "/retire"))
                                     :|data-indicator:processing| ""
                                     "RETIRE")))))))))))

(defun push-registry (gen)
  (d*:patch-elements gen (render-registry) :selector "#registry"))

;;; SUBSCRIPTION (CQRS push layer) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *CLIENTS* is a keyed registry: NOTIFY-SUBSCRIBERS broadcasts to all
;;; connected generators; NOTIFY-SUBSCRIBER + sid key targets one.
;;; The sid is a UUID assigned by the browser at page load (crypto.randomUUID)
;;; and carried on every request as a Datastar signal.

(defvar *clients* (reg:make-keyed-sse-registry "prevalence"))
(defvar *server* nil)

(defparameter *command-delay* 5
  "Seconds RETIRE is held before processing. Creates a staleness window in which
   a concurrent RETIRE from another tab will conflict.  (setf *command-delay* 0) to disable.")

(defun broadcast ()
  (reg:notify-subscribers *clients* #'push-registry))

;;; REPLY-TO delivers the command outcome asynchronously to one client over its
;;; already-open SSE stream; empty MSG string clears a previous warning.
(defun reply-to (sid &optional (msg ""))
  (reg:notify-subscriber *clients* sid
                         (lambda (g) (d*:patch-signals g (list :msg msg)))))

;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ht:define-easy-handler (index :uri "/") ()
  (setf (ht:content-type*) "text/html")
  (sp:with-html-string
    (:doctype)
    (:html
     (:head (:meta :charset "UTF-8")
            (:link :rel "stylesheet"
                   :href "https://fonts.googleapis.com/css2?family=Cousine:ital,wght@0,400;0,700;1,400;1,700&family=Saira+Extra+Condensed:wght@400;500;700&family=Saira+Semi+Condensed:wght@400&display=swap")
            (:link :rel "stylesheet"
                   :href "https://fonts.cdnfonts.com/css/dejavu-sans?styles=253,247")
            (:script :type "module" :src (d*:datastar-url))
            (:style (:raw *css*)))
     (:body :data-signals (d*:init-signals :designation "" :model "Nexus-6"
                                           :role "" :incept "" :target "" :msg "" :sid ""
                                           :processing nil)
            :data-init (format nil "$sid = crypto.randomUUID(); ~A" (d*:sse-get "/sse"))
            (:div :class "terminal"
                  (:h1 (:span :class "acronym" "ΛↃ")
                       (:span :class "wordmark"
                              (:span "LAMBDA")
                              (:span :class "combine" "COMBINE")))
                  (:p :class "meta"
                      "Integrated Tracking Systems Division -- "
                      "REP-DETECT Terminal v.2019")
                  (:h2 "Replicant Registration Database")
                  (:p :class "meta"
                      "LAPD Blade Runner Division -- Tyrell Corp. Subcontract LC-2018-BR")
                  (:raw (render-registry))
                  (:hr)
                  (:p (:strong "REPORT UNREGISTERED UNIT: ")
                      (:input :type "text" :placeholder "Designation" :data-bind "designation"
                              :style "width:10em" :maxlength 10)
                      (:select :data-bind "model"
                               (:option :value "Nexus-6" "Nexus-6")
                               (:option :value "Nexus-7" "Nexus-7")
                               (:option :value "Nexus-8" "Nexus-8"))
                      (:input :type "text" :placeholder "Role" :data-bind "role"
                              :style "width:8em" :maxlength 10)
                      (:input :type "text" :placeholder "Incept" :data-bind "incept"
                              :style "width:8em" :maxlength 10)
                      (:button :|data-on:click| (d*:sse-post "/spawn") "SPAWN"))
                  (:p :class "processing" :data-show "$processing" :style "display:none"
                      "> TRANSMITTING RETIREMENT ORDER TO CENTRAL...")
                  (:p :class "msg" :data-show "$msg != ''" :style "display:none"
                      :data-text "$msg")
                  (:div :class "footer"
                        "ΛↃ Lambda Combine · Integrated Tracking Systems ·"
                        "Persistent prevalence store: " *store-dir*))))))

;;; Pure-push SSE: empty body, :keep-alive drives heartbeat. :on-connect pushes
;;; current registry before registering so the first render is immediate (not
;;; waiting for the next broadcast). The sid from the browser's
;;; crypto.randomUUID() is registered so POST handlers can unicast replies back
;;; to exactly this client.
(ht:define-easy-handler (sse-handler :uri "/sse") ()
  (d*:with-signals ((sid "sid" "")) ht:*request*
    (d*:with-sse (gen ht:*request*
                 :on-connect    (lambda (g) (push-registry g) (reg:register *clients* g :key sid))
                 :on-disconnect (lambda (g) (reg:unregister *clients* g))
                 :keep-alive    t))))

;;; Command endpoints -- fire-and-forget (204 No Content).
;;;
;;; Commands return immediately; outcomes arrive over the push channel:
;;;   * success:   broadcast new registry to all subscribers;
;;;                unicast empty $msg to requester to clear any previous warning.
;;;   * rejection: unicast rejection message to requester's stream only.
;;;
;;; RETIRE sleeps *COMMAND-DELAY* seconds before processing: this opens a
;;; staleness window so a second operator acting on the stale view can issue a
;;; conflicting RETIRE. RETIRE then checks the transaction's return value,
;;; non-NIL only if THIS call actually performed the retire, and warns the
;;; requester if it lost the race. SPAWN and SIGHT are additive and run
;;; immediately.

(ht:define-easy-handler (spawn-handler :uri "/spawn") ()
  (d*:with-signals ((desig  "designation" "") (model  "model"  "Nexus-6")
                    (role   "role"        "") (incept "incept" "")
                    (sid    "sid"         "")) ht:*request*
    (cond ((zerop (length desig)) (reply-to sid "Designation required"))
          ((replicant-with-designation desig)
           (reply-to sid "Designation already registered"))
          (t (spawn-replicant desig model role incept) (broadcast) (reply-to sid))))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

;; We do not check if it's on a retired unit. Maybe it wasn't retired...
(ht:define-easy-handler (sight-handler :uri "/sight") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) ht:*request*
    (when (plusp (length target))
      (sight-replicant target)             ; additive, no conflict possible
      (broadcast)
      (reply-to sid)))                     ; clear any prior warning
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

;; We actually block here, and wait for the answer; this way we can use
;; data-indicator directly.
(ht:define-easy-handler (retire-handler :uri "/retire") ()
  (d*:with-signals ((target "target" "") (sid "sid" "")) ht:*request*
    (when (plusp (length target))
      (sleep *command-delay*)              ; another operator may retire it in this window
      (cond ((string= target "N7FAB00001") ; Rachael can't be retired, an
                                           ; example of a "business rule"
             (reply-to sid
               (format nil "~A retirement order failed -- system error code RACH32fa00a139"
                       target)))
            ((retire-replicant target)     ; non-nil only if THIS call performed the retire
             (broadcast) (reply-to sid))
            (t (reply-to sid
                 (format nil "~A was already retired by another unit -- command void"
                         target))))))
  (setf (ht:return-code*) ht:+http-no-content+)
  "")

;;; SEED: only when the store is empty (skipped on restart -- that is the point)
(when (null (all-replicants))
  (spawn-replicant "N6MAA10816" "Nexus-6" "Combat"       "2016-01-08")  ; Roy Batty
  (spawn-replicant "N6FAB21416" "Nexus-6" "Pleasure"     "2016-02-14")  ; Pris
  (spawn-replicant "N6MAC41717" "Nexus-6" "Loader"       "2017-04-10")  ; Leon
  (spawn-replicant "N7FAB00001" "Nexus-7" "Experimental" "--"))         ; Rachael

;;; STARTUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; START-SERVER is non-blocking: Hunchentoot serves on its own background
;;; threads, so calling it leaves a live server and a usable REPL prompt.
;;;
;;; MAIN is the entry point for a saved executable.  It starts the server then
;;; parks the main thread -- without the parking loop the image would exit the
;;; moment the toplevel function returned.  Defining MAIN is inert for REPL
;;; use; only the top-level (start-server) call below runs at load time.
;;;
;;; Top-level forms do NOT re-run when a dumped image boots -- only MAIN does.

(defun start-server (&key (port 8989))
  (when *server* (ht:stop *server*))
  (setf *server* (make-instance 'ht:easy-acceptor :port port))
  (ht:start *server*)
  (format t "~&REP-DETECT terminal online -> http://localhost:~D~%" port)
  (format t "~&Store: ~A  |  Replicants on file: ~D~%"
          *store-dir* (length (all-replicants)))
  (format t "~&To snapshot: (bknr.datastore:snapshot)~%")
  *server*)

(defun main ()
  "Entry point for SAVE-LISP-AND-DIE :toplevel; starts the server and parks."
  (start-server)
  (handler-case (loop (sleep 60))           ; park; serving happens on bg threads
    (#+sbcl sb-sys:interactive-interrupt
     #-sbcl error ()
      (when *server* (ht:stop *server*)))))

;; REPL / `sbcl --load` convenience -- start now and return.
(start-server)
