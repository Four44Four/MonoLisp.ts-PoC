;;;; src/async/loop.lisp -- single-threaded event loop (task queue + timer heap + reactor poll)
;;;;
;;;; Model (mirrors libuv/Node):
;;;;   * one thread drives a loop; callbacks never run concurrently with each other
;;;;   * other threads communicate with it by LOOP-POST, which is lock-free and wakes the poller
;;;;   * the loop exits when it has no queued tasks, no live timers, no watched fds and no refs,
;;;;     or when STOP-LOOP is called

(in-package :monolisp.async)

(define-condition loop-error (error)
  ((original :initarg :original :reader loop-error-original)
   (context :initarg :context :initform nil :reader loop-error-context))
  (:report (lambda (c s) (format s "Error in event-loop callback~@[ (~A)~]: ~A"
                                 (loop-error-context c) (loop-error-original c)))))

(defvar *loop-error-hook*
  (lambda (condition context)
    (warn "event loop: ~A callback signalled: ~A" context condition))
  "Called with (CONDITION CONTEXT) when a callback run by the loop signals an unhandled error.")

(defstruct (timer (:constructor %make-timer))
  (deadline 0d0 :type double-float)
  (interval nil)
  (callback nil :type (or null function))
  (id 0 :type fixnum)
  (cancelled nil)
  (refp t)
  (loop nil))

(defstruct (event-loop (:constructor %make-event-loop))
  (queue (sb-concurrency:make-queue :name "loop-tasks"))
  (timers (make-array 16 :adjustable t :fill-pointer 0))
  (timer-seq 0 :type sb-ext:word)
  (live-timers 0 :type sb-ext:word)
  (thread nil)
  (running nil)
  (stop-requested nil)
  (refs 0 :type sb-ext:word)
  (reactor nil)
  ;; stats
  (iterations 0 :type fixnum)
  (tasks-run 0 :type fixnum)
  (timers-fired 0 :type fixnum)
  (max-tasks-per-tick 4096 :type fixnum))

(defun make-event-loop (&key (max-tasks-per-tick 4096))
  (%make-event-loop :max-tasks-per-tick max-tasks-per-tick))

(defun loop-thread-p (&optional (loop *loop*))
  (and loop (eq (event-loop-thread loop) sb-thread:*current-thread*)))

(defun loop-running-p (&optional (loop *loop*))
  (and loop (event-loop-running loop)))

(defun loop-stats (&optional (loop *loop*))
  (list :iterations (event-loop-iterations loop)
        :tasks-run (event-loop-tasks-run loop)
        :timers-fired (event-loop-timers-fired loop)
        :live-timers (event-loop-live-timers loop)
        :refs (event-loop-refs loop)))

(defun loop-ref (&optional (loop *loop*))
  "Keep LOOP alive (e.g. a listening socket or in-flight worker job)."
  (sb-ext:atomic-incf (event-loop-refs loop)))

(defun loop-unref (&optional (loop *loop*))
  (sb-ext:atomic-decf (event-loop-refs loop)))

;;; ------------------------------------------------------------------ posting

(declaim (ftype function %reactor-wakeup %reactor-poll %reactor-active-p %reactor-close))

(defun loop-post (loop thunk)
  "Schedule THUNK to run on LOOP's thread as soon as possible.  Safe from any thread."
  (declare (type function thunk))
  (sb-concurrency:enqueue thunk (event-loop-queue loop))
  (unless (loop-thread-p loop)
    (%reactor-wakeup loop))
  thunk)

(defun post (thunk &optional (loop *loop*))
  (if loop
      (loop-post loop thunk)
      (funcall thunk)))

(defun schedule-unhandled-check (promise reason)
  "Report REASON through *UNHANDLED-REJECTION-HOOK* on the next tick unless a handler has been attached by then.
Without a loop there is no 'next tick', so nothing is reported."
  (when *loop*
    (loop-post *loop*
               (lambda ()
                 (unless (promise-handled promise)
                   (funcall *unhandled-rejection-hook* promise reason))))))

(defun next-tick (thunk) (post thunk))

;;; ------------------------------------------------------------------ timers (binary heap)

(defun %heap-swap (v i j)
  (rotatef (aref v i) (aref v j)))

(defun %timer-less (a b)
  (or (< (timer-deadline a) (timer-deadline b))
      (and (= (timer-deadline a) (timer-deadline b))
           (< (timer-id a) (timer-id b)))))

(defun %heap-push (loop timer)
  (let ((v (event-loop-timers loop)))
    (vector-push-extend timer v)
    (let ((i (1- (fill-pointer v))))
      (loop while (plusp i)
            do (let ((parent (floor (1- i) 2)))
                 (if (%timer-less (aref v i) (aref v parent))
                     (progn (%heap-swap v i parent) (setf i parent))
                     (return)))))))

(defun %heap-pop (loop)
  (let* ((v (event-loop-timers loop))
         (n (fill-pointer v)))
    (when (plusp n)
      (let ((top (aref v 0))
            (last (vector-pop v)))
        (when (> n 1)
          (setf (aref v 0) last)
          (let ((i 0) (len (fill-pointer v)))
            (loop
              (let ((l (+ (* 2 i) 1)) (r (+ (* 2 i) 2)) (smallest i))
                (when (and (< l len) (%timer-less (aref v l) (aref v smallest))) (setf smallest l))
                (when (and (< r len) (%timer-less (aref v r) (aref v smallest))) (setf smallest r))
                (if (= smallest i)
                    (return)
                    (progn (%heap-swap v i smallest) (setf i smallest)))))))
        top))))

(defun %heap-peek (loop)
  (let ((v (event-loop-timers loop)))
    (when (plusp (fill-pointer v)) (aref v 0))))

(defun %add-timer (loop callback ms interval refp)
  (check-type loop event-loop)
  (let ((timer (%make-timer :deadline (+ (monotonic-ms) (max 0 ms))
                            :interval interval
                            :callback callback
                            :id (sb-ext:atomic-incf (event-loop-timer-seq loop))
                            :refp refp
                            :loop loop)))
    (when refp (sb-ext:atomic-incf (event-loop-live-timers loop)))
    (if (loop-thread-p loop)
        (%heap-push loop timer)
        (loop-post loop (lambda () (%heap-push loop timer))))
    timer))

(defun set-timeout (callback ms &key (loop *loop*) (ref t))
  "Run CALLBACK once after MS milliseconds.  Returns a timer usable with CLEAR-TIMER."
  (%add-timer loop callback ms nil ref))

(defun set-interval (callback ms &key (loop *loop*) (ref t))
  (%add-timer loop callback ms (max 1 ms) ref))

(defun clear-timer (timer)
  (when (and timer (not (timer-cancelled timer)))
    (setf (timer-cancelled timer) t)
    (when (timer-refp timer) (sb-ext:atomic-decf (event-loop-live-timers (timer-loop timer))))
    t))

(defun delay (ms &optional value)
  "Promise that resolves to VALUE after MS milliseconds."
  (let ((p (%make-promise)))
    (set-timeout (lambda () (resolve p value)) ms)
    p))

(defun %run-expired-timers (loop)
  (let ((now (monotonic-ms)))
    (loop
      (let ((top (%heap-peek loop)))
        (unless (and top (<= (timer-deadline top) now)) (return))
        (%heap-pop loop)
        (cond ((timer-cancelled top))
              (t
               (if (timer-interval top)
                   (progn (setf (timer-deadline top) (+ now (timer-interval top)))
                          (%heap-push loop top))
                   (progn (when (timer-refp top) (sb-ext:atomic-decf (event-loop-live-timers loop)))
                          (setf (timer-cancelled top) t)))
               (incf (event-loop-timers-fired loop))
               (%safe-call loop (timer-callback top) "timer")))))))

(defun %next-timer-wait (loop)
  "Milliseconds until the next live timer, or NIL when none."
  (loop
    (let ((top (%heap-peek loop)))
      (cond ((null top) (return nil))
            ((timer-cancelled top) (%heap-pop loop))
            (t (return (max 0d0 (- (timer-deadline top) (monotonic-ms)))))))))

(defun %live-ref-timers-p (loop)
  (plusp (event-loop-live-timers loop)))

;;; ------------------------------------------------------------------ running

(defun %safe-call (loop fn context)
  (handler-case (funcall fn)
    (error (e)
      (handler-case (funcall *loop-error-hook* e context)
        (error (e2) (warn "loop error hook failed: ~A" e2))))
    (sb-sys:interactive-interrupt (c) (error c)))
  (incf (event-loop-tasks-run loop)))

(defun %drain-queue (loop)
  "Run queued tasks; returns true if work remains (we hit the per-tick cap)."
  (let ((q (event-loop-queue loop))
        (budget (event-loop-max-tasks-per-tick loop)))
    (loop
      (multiple-value-bind (thunk ok) (sb-concurrency:dequeue q)
        (unless ok (return nil))
        (%safe-call loop thunk "task")
        (when (zerop (decf budget)) (return t))))))

(defun %loop-alive-p (loop)
  (or (not (sb-concurrency:queue-empty-p (event-loop-queue loop)))
      (%live-ref-timers-p loop)
      (%reactor-active-p loop)
      (plusp (event-loop-refs loop))))

(defun %loop-iteration (loop block-p &optional stop-p)
  (incf (event-loop-iterations loop))
  (let* ((more (%drain-queue loop)))
    (%run-expired-timers loop)
    ;; Re-check the caller's exit condition before we go to sleep in the poller.
    (unless (or (event-loop-stop-requested loop)
                (and stop-p (funcall stop-p)))
      (let* ((timer-wait (%next-timer-wait loop))
             (wait (cond ((or more (not (sb-concurrency:queue-empty-p (event-loop-queue loop)))) 0d0)
                         ((not block-p) 0d0)
                         (timer-wait (min timer-wait 1000d0))
                         (t 1000d0))))
        (%reactor-poll loop wait)
        (%run-expired-timers loop)))))

(defun run-once (&optional (loop *loop*))
  "Run a single non-blocking iteration of LOOP."
  (let ((*loop* loop))
    (let ((prev-thread (event-loop-thread loop)))
      (setf (event-loop-thread loop) sb-thread:*current-thread*)
      (unwind-protect (%loop-iteration loop nil)
        (setf (event-loop-thread loop) prev-thread)))))

(defun run-loop (&optional (loop *loop*))
  "Drive LOOP until it has nothing left to do or STOP-LOOP is called."
  (run-loop-until loop (constantly nil) nil))

(defun run-loop-until (loop predicate timeout-seconds &key (idle-ok t))
  "Drive LOOP until PREDICATE returns true, TIMEOUT-SECONDS elapse, STOP-LOOP is called or
(when IDLE-OK) the loop has nothing left to do."
  (let ((*loop* loop)
        (deadline (and timeout-seconds (+ (monotonic-ms) (* 1000d0 timeout-seconds))))
        (outer-running (event-loop-running loop))
        (prev-thread (event-loop-thread loop)))
    (setf (event-loop-thread loop) sb-thread:*current-thread*
          (event-loop-running loop) t
          (event-loop-stop-requested loop) nil)
    (unwind-protect
         (loop
           (when (funcall predicate) (return :done))
           (when (event-loop-stop-requested loop) (return :stopped))
           (when (and deadline (>= (monotonic-ms) deadline)) (return :timeout))
           (when (and idle-ok (not (%loop-alive-p loop))) (return :idle))
           (%loop-iteration loop t predicate))
      (unless outer-running
        (setf (event-loop-running loop) nil
              ;; keep ownership with whoever owned the loop before (WITH-EVENT-LOOP sets it),
              ;; so later PROMISE-WAITs on this thread keep driving the loop
              (event-loop-thread loop) prev-thread)))))

(defun stop-loop (&optional (loop *loop*))
  (setf (event-loop-stop-requested loop) t)
  (%reactor-wakeup loop))

(defmacro with-event-loop ((&key (var '*loop*) (run t)) &body body)
  "Create a fresh loop, run BODY with it current, then (when RUN) drive the loop to completion."
  (let ((l (gensym "LOOP")))
    `(let* ((,l (make-event-loop))
            (,var ,l)
            (*loop* ,l))
       (unwind-protect
            (progn
              (setf (event-loop-thread ,l) sb-thread:*current-thread*)
              (multiple-value-prog1 (progn ,@body)
                ,@(when run `((run-loop ,l)))))
         (%reactor-close ,l)))))
