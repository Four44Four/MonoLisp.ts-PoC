;;;; src/async/promise.lisp -- thread-safe promises with JS-like semantics
;;;;
;;;; A promise may be resolved/rejected from any thread.  Reaction callbacks registered with
;;;; THEN are executed on the event loop that was current when THEN was called (so they never
;;;; run re-entrantly inside RESOLVE), or synchronously when no loop is available.

(in-package :monolisp.async)

(define-condition promise-error (error)
  ((reason :initarg :reason :reader promise-error-reason))
  (:report (lambda (c s) (format s "Promise rejected with non-condition reason: ~S" (promise-error-reason c)))))

(define-condition timeout-error (error)
  ((what :initarg :what :initform "operation" :reader timeout-error-what)
   (seconds :initarg :seconds :initform nil :reader timeout-error-seconds))
  (:report (lambda (c s) (format s "~A timed out~@[ after ~As~]" (timeout-error-what c) (timeout-error-seconds c)))))

(define-condition aggregate-error (error)
  ((reasons :initarg :reasons :reader aggregate-error-reasons))
  (:report (lambda (c s) (format s "All promises rejected: ~S" (aggregate-error-reasons c)))))

(defvar *unhandled-rejection-hook*
  (lambda (promise reason)
    (declare (ignore promise))
    (warn "Unhandled promise rejection: ~A" reason))
  "Called with (PROMISE REASON) when a rejected promise had no rejection handler.")

(defstruct (promise (:constructor %make-promise ())
                    (:predicate promisep)
                    (:print-function print-promise))
  (state :pending :type (member :pending :fulfilled :rejected))
  (value nil)
  ;; list of reaction records: (loop on-fulfilled on-rejected)
  (callbacks '() :type list)
  (lock (sb-thread:make-mutex :name "promise"))
  (cv (sb-thread:make-waitqueue))
  (handled nil))

(defun print-promise (p stream depth)
  (declare (ignore depth))
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "~A~@[ ~S~]" (promise-state p)
            (unless (eq (promise-state p) :pending) (promise-value p)))))

(defun make-promise (&optional executor)
  "Create a promise.  EXECUTOR, if given, is called with (RESOLVE-FN REJECT-FN); errors it signals reject the promise."
  (let ((p (%make-promise)))
    (when executor
      (handler-case
          (funcall executor
                   (lambda (v) (resolve p v))
                   (lambda (r) (reject p r)))
        (error (e) (reject p e))))
    p))

(declaim (inline promise-pending-p promise-settled-p))
(defun promise-pending-p (p) (eq (promise-state p) :pending))
(defun promise-settled-p (p) (not (eq (promise-state p) :pending)))

(defun resolved (value)
  (let ((p (%make-promise))) (resolve p value) p))

(defun rejected (reason)
  (let ((p (%make-promise))) (reject p reason) p))

;;; ------------------------------------------------------------------ settle

(declaim (ftype function loop-post schedule-unhandled-check))

(defun %settle (p state value)
  "Atomically settle P; returns the callbacks to run or NIL if already settled."
  (let ((callbacks nil) (did-settle nil))
    (sb-thread:with-mutex ((promise-lock p))
      (when (eq (promise-state p) :pending)
        (setf (promise-value p) value
              (promise-state p) state
              callbacks (promise-callbacks p)
              (promise-callbacks p) '()
              did-settle t)
        (sb-thread:condition-broadcast (promise-cv p))))
    (when did-settle
      (dolist (cb (reverse callbacks))
        (%dispatch-reaction cb state value))
      (when (and (eq state :rejected) (null callbacks) (not (promise-handled p)))
        (schedule-unhandled-check p value)))
    did-settle))

(defun %dispatch-reaction (reaction state value)
  (destructuring-bind (loop on-fulfilled on-rejected) reaction
    (let ((thunk (lambda ()
                   (if (eq state :fulfilled)
                       (funcall on-fulfilled value)
                       (funcall on-rejected value)))))
      (if loop
          (loop-post loop thunk)
          (funcall thunk)))))

(defun resolve (p value)
  "Fulfil P with VALUE.  If VALUE is itself a promise, P adopts its eventual state."
  (cond ((eq value p) (reject p (make-condition 'simple-error :format-control "Promise cannot resolve to itself")))
        ((promisep value)
         (then value (lambda (v) (resolve p v)) (lambda (r) (reject p r)))
         t)
        (t (%settle p :fulfilled value))))

(defun reject (p reason)
  (%settle p :rejected reason))

;;; ------------------------------------------------------------------ then

(defvar *loop* nil "The current event loop (bound by WITH-EVENT-LOOP / RUN-LOOP).")

(defun %call-handler (handler value target)
  "Run HANDLER on VALUE and settle TARGET with the result (adopting promises)."
  (handler-case (resolve target (funcall handler value))
    (error (e) (reject target e))))

(defun then (p on-fulfilled &optional on-rejected)
  "Register reactions.  Returns a new promise settled with the handler's result."
  (check-type p promise)
  (let* ((target (%make-promise))
         (loop *loop*)
         (reaction (list loop
                         (lambda (v)
                           (if on-fulfilled
                               (%call-handler on-fulfilled v target)
                               (resolve target v)))
                         (lambda (r)
                           (if on-rejected
                               (%call-handler on-rejected r target)
                               (reject target r)))))
         (immediate nil))
    (sb-thread:with-mutex ((promise-lock p))
      (setf (promise-handled p) t)
      (if (eq (promise-state p) :pending)
          (push reaction (promise-callbacks p))
          (setf immediate (list (promise-state p) (promise-value p)))))
    (when immediate
      (%dispatch-reaction reaction (first immediate) (second immediate)))
    target))

(defun catch* (p on-rejected)
  (then p nil on-rejected))

(defun finally* (p thunk)
  "Run THUNK regardless of outcome; result passes the original value/reason through."
  (then p
        (lambda (v) (funcall thunk) v)
        (lambda (r) (funcall thunk) (rejected r))))

;;; ------------------------------------------------------------------ combinators

(defun %as-promise (x) (if (promisep x) x (resolved x)))

(defun all (promises)
  "Promise of a list of all values, in order; rejects on first rejection."
  (let* ((items (coerce promises 'list))
         (n (length items))
         (result (%make-promise)))
    (if (zerop n)
        (resolve result '())
        (let ((values (make-array n))
              (remaining n)
              (lock (sb-thread:make-mutex :name "all")))
          (loop for item in items
                for i from 0
                do (let ((i i))
                     (then (%as-promise item)
                           (lambda (v)
                             (let ((done nil))
                               (sb-thread:with-mutex (lock)
                                 (setf (aref values i) v)
                                 (when (zerop (decf remaining)) (setf done t)))
                               (when done (resolve result (coerce values 'list)))))
                           (lambda (r) (reject result r)))))))
    result))

(defun race (promises)
  (let ((result (%make-promise)))
    (dolist (p (coerce promises 'list) result)
      (then (%as-promise p)
            (lambda (v) (resolve result v))
            (lambda (r) (reject result r))))))

(defun any (promises)
  "Resolves with the first fulfilled value; rejects with AGGREGATE-ERROR if all reject."
  (let* ((items (coerce promises 'list))
         (n (length items))
         (result (%make-promise)))
    (if (zerop n)
        (reject result (make-condition 'aggregate-error :reasons '()))
        (let ((reasons (make-array n))
              (remaining n)
              (lock (sb-thread:make-mutex :name "any")))
          (loop for item in items
                for i from 0
                do (let ((i i))
                     (then (%as-promise item)
                           (lambda (v) (resolve result v))
                           (lambda (r)
                             (let ((done nil))
                               (sb-thread:with-mutex (lock)
                                 (setf (aref reasons i) r)
                                 (when (zerop (decf remaining)) (setf done t)))
                               (when done
                                 (reject result (make-condition 'aggregate-error
                                                                :reasons (coerce reasons 'list)))))))))))
    result))

(defun all-settled (promises)
  "Promise of a list of (:fulfilled value) / (:rejected reason) entries."
  (all (mapcar (lambda (p)
                 (then (%as-promise p)
                       (lambda (v) (list :fulfilled v))
                       (lambda (r) (list :rejected r))))
               (coerce promises 'list))))

(defun promisify (function)
  "Return a function that calls FUNCTION and returns a promise of its result (errors -> rejection)."
  (lambda (&rest args)
    (handler-case (resolved (apply function args))
      (error (e) (rejected e)))))

;;; ------------------------------------------------------------------ blocking wait

(declaim (ftype function loop-thread-p run-loop-until))

(defun %signal-rejection (reason)
  (if (typep reason 'condition)
      (error reason)
      (error 'promise-error :reason reason)))

(defun promise-wait (p &key timeout)
  "Block until P settles and return its value (re-signalling rejections).

When called on the event-loop thread the loop is driven until P settles, so this is safe in
tests and at the top level; from other threads it blocks on a condition variable."
  (check-type p promise)
  (sb-thread:with-mutex ((promise-lock p)) (setf (promise-handled p) t))
  (cond ((promise-settled-p p))
        ((and *loop* (loop-thread-p *loop*))
         ;; A no-op reaction makes a cross-thread settle post to (and wake) this loop.
         (then p (lambda (v) v) (lambda (r) (declare (ignore r)) nil))
         (run-loop-until *loop* (lambda () (promise-settled-p p)) timeout :idle-ok nil))
        (t
         (let ((deadline (and timeout (+ (monotonic-ms) (* 1000 timeout)))))
           (sb-thread:with-mutex ((promise-lock p))
             (loop while (eq (promise-state p) :pending)
                   do (let ((remaining (and deadline (/ (- deadline (monotonic-ms)) 1000d0))))
                        (when (and remaining (<= remaining 0)) (return))
                        (sb-thread:condition-wait (promise-cv p) (promise-lock p)
                                                  :timeout (and remaining (max 0.001d0 remaining)))))))))
  (ecase (promise-state p)
    (:fulfilled (promise-value p))
    (:rejected (%signal-rejection (promise-value p)))
    (:pending (error 'timeout-error :what "promise-wait" :seconds timeout))))
