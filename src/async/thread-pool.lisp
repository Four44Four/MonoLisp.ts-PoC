;;;; src/async/thread-pool.lisp -- bounded worker pool; blocking/CPU work becomes a promise

(in-package :monolisp.async)

(define-condition pool-saturated (error)
  ((pool :initarg :pool :reader pool-saturated-pool))
  (:report (lambda (c s) (format s "thread pool ~A queue is full" (thread-pool-name (pool-saturated-pool c))))))

(defstruct (thread-pool (:constructor %make-thread-pool))
  (name "pool")
  (size 4 :type fixnum)
  (threads '())
  (mailbox (sb-concurrency:make-mailbox :name "pool-jobs"))
  (max-queue 10000 :type fixnum)
  (shutdown nil)
  (active 0 :type sb-ext:word)
  (completed 0 :type sb-ext:word)
  (rejected 0 :type sb-ext:word))

(defun %worker-main (pool)
  (loop
    (let ((job (sb-concurrency:receive-message (thread-pool-mailbox pool))))
      (when (eq job :shutdown) (return))
      (destructuring-bind (fn promise loop) job
        (sb-ext:atomic-incf (thread-pool-active pool))
        (unwind-protect
             (handler-case
                 (let ((result (funcall fn)))
                   (resolve promise result))
               (error (e) (reject promise e)))
          (sb-ext:atomic-decf (thread-pool-active pool))
          (sb-ext:atomic-incf (thread-pool-completed pool))
          (when loop (loop-unref loop)))))))

(defun make-thread-pool (&key (size 4) (name "pool") (max-queue 10000))
  (let ((pool (%make-thread-pool :name name :size size :max-queue max-queue)))
    (setf (thread-pool-threads pool)
          (loop for i below size
                collect (sb-thread:make-thread (lambda () (%worker-main pool))
                                               :name (format nil "~A-worker-~D" name i))))
    pool))

(defvar *default-pool* nil)
(defvar *default-pool-lock* (sb-thread:make-mutex :name "default-pool"))

(defun default-pool ()
  (or *default-pool*
      (sb-thread:with-mutex (*default-pool-lock*)
        (or *default-pool*
            (setf *default-pool*
                  (make-thread-pool :size (max 4 (%cpu-count)) :name "default"))))))

(defun %cpu-count ()
  "Number of online CPUs: NUMBER_OF_PROCESSORS on Windows, sysconf(_SC_NPROCESSORS_ONLN) on
Unix-likes, 4 when neither can be determined."
  (flet ((plausible (n) (and (integerp n) (< 0 n 4096) n)))
    (or (ignore-errors
         (let ((env (sb-ext:posix-getenv "NUMBER_OF_PROCESSORS")))
           (and env (plausible (parse-integer env :junk-allowed t)))))
        #-win32
        (ignore-errors
         (plausible (alien-funcall (extern-alien "sysconf" (function long int))
                                   #+linux 84 #-linux 58))) ; _SC_NPROCESSORS_ONLN (glibc / BSD, macOS)
        4)))

(defun offload (fn &key (pool nil) (loop *loop*))
  "Run FN on a worker thread; returns a promise of its result.  Keeps LOOP alive while in flight."
  (let* ((pool (or pool (default-pool)))
         (promise (%make-promise)))
    (cond ((thread-pool-shutdown pool)
           (reject promise (make-condition 'simple-error :format-control "thread pool is shut down")))
          ((>= (sb-concurrency:mailbox-count (thread-pool-mailbox pool)) (thread-pool-max-queue pool))
           (sb-ext:atomic-incf (thread-pool-rejected pool))
           (reject promise (make-condition 'pool-saturated :pool pool)))
          (t
           (when loop (loop-ref loop))
           (sb-concurrency:send-message (thread-pool-mailbox pool) (list fn promise loop))))
    promise))

(defun pool-stats (&optional (pool (default-pool)))
  (list :size (thread-pool-size pool)
        :active (thread-pool-active pool)
        :queued (sb-concurrency:mailbox-count (thread-pool-mailbox pool))
        :completed (thread-pool-completed pool)
        :rejected (thread-pool-rejected pool)))

(defun shutdown-pool (pool &key (wait t))
  (setf (thread-pool-shutdown pool) t)
  (dotimes (i (thread-pool-size pool))
    (sb-concurrency:send-message (thread-pool-mailbox pool) :shutdown))
  (when wait
    (dolist (th (thread-pool-threads pool))
      (ignore-errors (sb-thread:join-thread th :default nil :timeout 5))))
  (when (eq pool *default-pool*) (setf *default-pool* nil))
  t)

(defmacro with-pool ((var &rest args) &body body)
  `(let ((,var (make-thread-pool ,@args)))
     (unwind-protect (progn ,@body)
       (shutdown-pool ,var))))
