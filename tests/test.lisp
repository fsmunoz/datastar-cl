;;;; Common Lisp SDK for Datastar
;;;;
;;;; Generic utilities for testing

(in-package #:datastar-cl-tests)

(defun handle-datastar-signals (signals generator)
  "Process parsed datastar SIGNALS and dispatch them to SSE GENERATOR."
  (let ((events (gethash "events" signals))) ;; vector
    (loop for event across events do
      (let ((type (gethash "type" event))
            (payload event))
        (cond
          ((string= type "patchElements")
           (datastar-cl:patch-elements generator (gethash "elements" payload)
                           :selector (gethash "selector" payload)
                           :mode (or (gethash "mode" payload)
                                     datastar-cl:*default-patch-mode*)
                           :use-view-transition (gethash "useViewTransition" payload)
                           :event-id (gethash "eventId" payload)
                           :retry-duration (gethash "retryDuration" payload)))

          ((string= type "patchSignals")
           (datastar-cl:patch-signals generator (or (gethash "signals-raw" payload)
                                        (gethash "signals" payload))
                          :only-if-missing (gethash "onlyIfMissing" payload)
                          :event-id (gethash "eventId" payload)
                          :retry-duration (gethash "retryDuration" payload)))

          ((string= type "executeScript")
           (datastar-cl:execute-script generator (gethash "script" payload)
                           :auto-remove (if (gethash "autoRemove" payload)
                                            (gethash "autoRemove" payload)
                                            t)
                           :attributes (gethash "attributes" payload)
                           :event-id (gethash "eventId" payload)
                           :retry-duration (gethash "retryDuration" payload)))

          (t (warn "Unknown event type: ~a" type)))))))


