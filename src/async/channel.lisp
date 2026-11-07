;;;; src/async/channel.lisp -- promise based channels and semaphores

(in-package :monolisp.async)

(define-condition channel-closed (error)
  ((channel :initarg :channel :reader channel-closed-channel))
  (:report (lambda (c s) (declare (ignore c)) (format s "channel is closed"))))

(defstruct (channel (:constructor %make-channel))
  (items '())              ; queued values (FIFO: push to tail via append on a small list)
  (tail nil)
  (count 0 :type fixnum)
  (capacity nil)           ; nil = unbounded
  (receivers '())          ; waiting receiver promises (FIFO)
  (senders '())            ; waiting (value . promise) when bounded and full
  (closed nil)
  (lock (sb-thread:make-mutex :name "channel")))

(defun make-channel (&key capacity)
  (%make-channel :capacity capacity))

(defun channel-closed-p (ch) (channel-closed ch))

(defun %enqueue-item (ch value)
  (let ((cell (list value)))
    (if (channel-items ch)
        (setf (cdr (channel-tail ch)) cell)
        (setf (channel-items ch) cell))
    (setf (channel-tail ch) cell)
    (incf (channel-count ch))))

(defun %dequeue-item (ch)
  (let ((cell (channel-items ch)))
    (setf (channel-items ch) (cdr cell))
    (when (null (channel-items ch)) (setf (channel-tail ch) nil))
    (decf (channel-count ch))
    (car cell)))

(defun channel-send (ch value)
  "Send VALUE.  Returns a promise that resolves to T once the value has been accepted."
  (let ((to-resolve nil) (result (%make-promise)))
    (sb-thread:with-mutex ((channel-lock ch))
      (cond ((channel-closed ch)
             (setf to-resolve (list (cons result (make-condition 'channel-closed :channel ch)))))
            ((channel-receivers ch)
             (let ((receiver (pop (channel-receivers ch))))
               (setf to-resolve (list (cons receiver value) (cons result t)))))
            ((and (channel-capacity ch) (>= (channel-count ch) (channel-capacity ch)))
             (setf (channel-senders ch) (append (channel-senders ch) (list (cons value result)))))
            (t (%enqueue-item ch value)
               (setf to-resolve (list (cons result t))))))
    (dolist (pair to-resolve)
      (if (typep (cdr pair) 'channel-closed)
          (reject (car pair) (cdr pair))
          (resolve (car pair) (cdr pair))))
    result))

(defun channel-receive (ch)
  "Promise of the next value; rejects with CHANNEL-CLOSED when the channel is closed and drained."
  (let ((p (%make-promise)) (to-resolve '()))
    (sb-thread:with-mutex ((channel-lock ch))
      (cond ((plusp (channel-count ch))
             (push (cons p (%dequeue-item ch)) to-resolve)
             ;; make room for a blocked sender
             (when (channel-senders ch)
               (destructuring-bind (value . sender-promise) (pop (channel-senders ch))
                 (%enqueue-item ch value)
                 (push (cons sender-promise t) to-resolve))))
            ((channel-closed ch)
             (push (cons p :closed) to-resolve))
            (t (setf (channel-receivers ch) (append (channel-receivers ch) (list p))))))
    (dolist (pair to-resolve)
      (if (eq (cdr pair) :closed)
          (reject (car pair) (make-condition 'channel-closed :channel ch))
          (resolve (car pair) (cdr pair))))
    p))

(defun channel-close (ch)
  (let ((waiting '()))
    (sb-thread:with-mutex ((channel-lock ch))
      (setf (channel-closed ch) t
            waiting (append (channel-receivers ch) (mapcar #'cdr (channel-senders ch)))
            (channel-receivers ch) '()
            (channel-senders ch) '()))
    (dolist (p waiting) (reject p (make-condition 'channel-closed :channel ch)))
    t))

;;; ------------------------------------------------------------------ semaphore

(defstruct (semaphore (:constructor %make-semaphore))
  (count 0 :type fixnum)
  (waiters '())
  (lock (sb-thread:make-mutex :name "async-semaphore")))

(defun make-async-semaphore (count)
  (%make-semaphore :count count))

(defun semaphore-acquire (sem)
  "Promise that resolves once a permit is held."
  (let ((p (%make-promise)) (immediate nil))
    (sb-thread:with-mutex ((semaphore-lock sem))
      (if (plusp (semaphore-count sem))
          (progn (decf (semaphore-count sem)) (setf immediate t))
          (setf (semaphore-waiters sem) (append (semaphore-waiters sem) (list p)))))
    (when immediate (resolve p t))
    p))

(defun semaphore-release (sem)
  (let ((waiter nil))
    (sb-thread:with-mutex ((semaphore-lock sem))
      (if (semaphore-waiters sem)
          (setf waiter (pop (semaphore-waiters sem)))
          (incf (semaphore-count sem))))
    (when waiter (resolve waiter t))
    t))

(defmacro with-semaphore ((sem) &body body)
  "Acquire SEM, run BODY (which may return a promise), release when the result settles."
  (let ((s (gensym "SEM")))
    `(let ((,s ,sem))
       (then (semaphore-acquire ,s)
             (lambda (ok) (declare (ignore ok))
               (finally* (%as-promise (handler-case (progn ,@body) (error (e) (rejected e))))
                         (lambda () (semaphore-release ,s))))))))
