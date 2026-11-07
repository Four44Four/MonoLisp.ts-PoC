;;;; src/async/reactor.lisp -- readiness reactor over non-blocking sockets using select(2)
;;;;
;;;; This is the libuv-equivalent of the toolkit.  It is deliberately small: a hash of fd -> watcher,
;;;; a select() poll, and a UDP "self socket" used to wake the poller from other threads.
;;;; select() is the only readiness API available on every platform SBCL supports without extra
;;;; libraries; an epoll/kqueue/IOCP backend can be dropped in by replacing %REACTOR-POLL.

(in-package :monolisp.async)

(defconstant +max-watchers+ 1024
  "Maximum number of simultaneously watched descriptors (FD_SETSIZE).")

;;; ------------------------------------------------------------------ foreign bits

#+win32
(progn
  (define-alien-type nil
    (struct win-fd-set
      (count unsigned-int)
      (fds (array unsigned-long-long 1024))))
  (define-alien-type nil
    (struct timeval (sec long) (usec long)))
  (define-alien-routine ("select" %win-select) int
    (nfds int)
    (readfds (* (struct win-fd-set)))
    (writefds (* (struct win-fd-set)))
    (exceptfds (* (struct win-fd-set)))
    (timeout (* (struct timeval))))
  (define-alien-routine ("WSAGetLastError" %wsa-get-last-error) int))

#-win32
(progn
  (define-alien-type nil
    (struct unix-fd-set
      (bits (array unsigned-long 16))))
  (define-alien-type nil
    (struct timeval (sec long) (usec long)))
  (define-alien-routine ("select" %unix-select) int
    (nfds int)
    (readfds (* (struct unix-fd-set)))
    (writefds (* (struct unix-fd-set)))
    (exceptfds (* (struct unix-fd-set)))
    (timeout (* (struct timeval)))))

(defun socket-last-error ()
  "Platform specific last socket error code."
  #+win32 (%wsa-get-last-error)
  #-win32 (sb-alien:get-errno))

(defun would-block-p (code)
  "True if socket error CODE means 'try again later' (EAGAIN/EWOULDBLOCK/EINPROGRESS/WSAEWOULDBLOCK)."
  (member code '(11 35 115 36 10035 10036)))

(defun interrupted-p (code)
  (member code '(4 10004)))

;;; ------------------------------------------------------------------ watchers

(defstruct (watcher (:constructor %make-watcher))
  (fd 0 :type (integer 0))
  (socket nil)
  (on-readable nil)
  (on-writable nil)
  (read-interest nil)
  (write-interest nil)
  (refp t)
  (active t))

(defstruct (reactor (:constructor %make-reactor))
  (watchers (make-hash-table) :type hash-table)   ; fd -> watcher
  (wake-socket nil)
  (wake-fd nil)
  (polls 0 :type fixnum)
  (events 0 :type fixnum)
  (read-set nil)
  (write-set nil)
  (except-set nil)
  (timeval nil))

(defun %ensure-reactor (loop)
  (or (event-loop-reactor loop)
      (setf (event-loop-reactor loop) (%init-reactor))))

(defun %init-reactor ()
  (let ((r (%make-reactor)))
    #+win32
    (setf (reactor-read-set r) (make-alien (struct win-fd-set))
          (reactor-write-set r) (make-alien (struct win-fd-set))
          (reactor-except-set r) (make-alien (struct win-fd-set))
          (reactor-timeval r) (make-alien (struct timeval)))
    #-win32
    (setf (reactor-read-set r) (make-alien (struct unix-fd-set))
          (reactor-write-set r) (make-alien (struct unix-fd-set))
          (reactor-except-set r) (make-alien (struct unix-fd-set))
          (reactor-timeval r) (make-alien (struct timeval)))
    ;; Wakeup channel: a UDP socket connected to itself on loopback.
    (handler-case
        (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
          (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
          (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name s)
            (sb-bsd-sockets:socket-connect s addr port))
          (setf (sb-bsd-sockets:non-blocking-mode s) t)
          (setf (reactor-wake-socket r) s
                (reactor-wake-fd r) (sb-bsd-sockets:socket-file-descriptor s)))
      (error (e) (warn "reactor: could not create wakeup socket (~A); cross-thread posts will be polled" e)))
    r))

(defun %reactor-close (loop)
  (let ((r (event-loop-reactor loop)))
    (when r
      (when (reactor-wake-socket r)
        (ignore-errors (sb-bsd-sockets:socket-close (reactor-wake-socket r)))
        (setf (reactor-wake-socket r) nil))
      (maphash (lambda (fd w) (declare (ignore fd))
                 (when (watcher-socket w) (ignore-errors (sb-bsd-sockets:socket-close (watcher-socket w)))))
               (reactor-watchers r))
      (clrhash (reactor-watchers r))
      (when (reactor-read-set r) (free-alien (reactor-read-set r)) (setf (reactor-read-set r) nil))
      (when (reactor-write-set r) (free-alien (reactor-write-set r)) (setf (reactor-write-set r) nil))
      (when (reactor-except-set r) (free-alien (reactor-except-set r)) (setf (reactor-except-set r) nil))
      (when (reactor-timeval r) (free-alien (reactor-timeval r)) (setf (reactor-timeval r) nil))
      (setf (event-loop-reactor loop) nil))))

(defvar *wake-byte* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1))

(defun %reactor-wakeup (loop)
  (let ((r (event-loop-reactor loop)))
    (when (and r (reactor-wake-socket r))
      (ignore-errors (sb-bsd-sockets:socket-send (reactor-wake-socket r) *wake-byte* 1)))))

(defun %drain-wakeups (r)
  (let ((s (reactor-wake-socket r))
        (buf (make-array 64 :element-type '(unsigned-byte 8))))
    (when s
      (loop repeat 16
            do (handler-case
                   (multiple-value-bind (b len) (sb-bsd-sockets:socket-receive s buf 64)
                     (declare (ignore b))
                     (when (or (null len) (not (and (integerp len) (< 0 len 65))))
                       (return)))
                 (error () (return)))))))

(defun %reactor-active-p (loop)
  (let ((r (event-loop-reactor loop)))
    (and r
         (loop for w being the hash-values of (reactor-watchers r)
               thereis (and (watcher-active w) (watcher-refp w)
                            (or (watcher-read-interest w) (watcher-write-interest w)))))))

(defun watch-fd (socket &key on-readable on-writable (read t) (write nil) (ref t) (loop *loop*))
  "Register SOCKET (an sb-bsd-sockets socket, set to non-blocking) with the loop's reactor.
ON-READABLE / ON-WRITABLE are called with the watcher when the fd is ready."
  (let* ((r (%ensure-reactor loop))
         (fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (when (>= (hash-table-count (reactor-watchers r)) (1- +max-watchers+))
      (error "reactor: watcher limit (~D) reached" +max-watchers+))
    #-win32 (when (>= fd +max-watchers+) (error "reactor: fd ~D exceeds FD_SETSIZE" fd))
    (setf (sb-bsd-sockets:non-blocking-mode socket) t)
    (let ((w (%make-watcher :fd fd :socket socket :on-readable on-readable :on-writable on-writable
                            :read-interest (and read t) :write-interest (and write t) :refp ref)))
      (setf (gethash fd (reactor-watchers r)) w)
      w)))

(defun fd-interest (watcher &key (read nil read-p) (write nil write-p))
  (when read-p (setf (watcher-read-interest watcher) (and read t)))
  (when write-p (setf (watcher-write-interest watcher) (and write t)))
  watcher)

(defun unwatch-fd (watcher &key (loop *loop*))
  (let ((r (event-loop-reactor loop)))
    (setf (watcher-active watcher) nil
          (watcher-read-interest watcher) nil
          (watcher-write-interest watcher) nil)
    (when r (remhash (watcher-fd watcher) (reactor-watchers r)))
    watcher))

;;; ------------------------------------------------------------------ polling

#+win32
(progn
  (declaim (inline %fdset-clear %fdset-add %fdset-member))
  (defun %fdset-clear (set) (setf (slot set 'count) 0))
  (defun %fdset-add (set fd)
    (let ((n (slot set 'count)))
      (setf (deref (slot set 'fds) n) fd
            (slot set 'count) (1+ n))))
  (defun %fdset-member (set fd)
    (loop for i below (slot set 'count)
          thereis (= (deref (slot set 'fds) i) fd))))

#-win32
(progn
  (declaim (inline %fdset-clear %fdset-add %fdset-member))
  (defun %fdset-clear (set)
    (dotimes (i (/ +max-watchers+ 64)) (setf (deref (slot set 'bits) i) 0)))
  (defun %fdset-add (set fd)
    (multiple-value-bind (word bit) (floor fd 64)
      (setf (deref (slot set 'bits) word)
            (logior (deref (slot set 'bits) word) (ash 1 bit)))))
  (defun %fdset-member (set fd)
    (multiple-value-bind (word bit) (floor fd 64)
      (logbitp bit (deref (slot set 'bits) word)))))

(defun %reactor-poll (loop wait-ms)
  "Wait up to WAIT-MS for readiness (or a wakeup) and dispatch watcher callbacks."
  (let ((r (%ensure-reactor loop)))
    (incf (reactor-polls r))
    (let ((rs (reactor-read-set r))
          (ws (reactor-write-set r))
          (es (reactor-except-set r))
          (tv (reactor-timeval r))
          (maxfd -1)
          (any-read nil) (any-write nil))
      (%fdset-clear rs) (%fdset-clear ws) (%fdset-clear es)
      (when (reactor-wake-fd r)
        (%fdset-add rs (reactor-wake-fd r))
        (setf any-read t maxfd (max maxfd (reactor-wake-fd r))))
      (loop for w being the hash-values of (reactor-watchers r)
            when (watcher-active w)
              do (when (watcher-read-interest w)
                   (%fdset-add rs (watcher-fd w)) (setf any-read t maxfd (max maxfd (watcher-fd w))))
                 (when (watcher-write-interest w)
                   ;; Winsock reports failed non-blocking connects through exceptfds, so writers
                   ;; are polled for exceptions too and an exception counts as "writable".
                   (%fdset-add ws (watcher-fd w)) (%fdset-add es (watcher-fd w))
                   (setf any-write t maxfd (max maxfd (watcher-fd w)))))
      (cond
        ((and (not any-read) (not any-write))
         ;; Nothing to poll (no wakeup socket either): just sleep.
         (when (plusp wait-ms) (sleep (/ wait-ms 1000d0)))
         0)
        (t
         (multiple-value-bind (sec usec) (floor (round (* wait-ms 1000)) 1000000)
           (setf (slot tv 'sec) sec (slot tv 'usec) usec))
         (let ((n (#+win32 %win-select #-win32 %unix-select
                   (1+ maxfd)
                   (if any-read rs nil)
                   (if any-write ws nil)
                   (if any-write es nil)
                   tv)))
           (cond
             ((< n 0)
              (let ((code (socket-last-error)))
                (unless (interrupted-p code)
                  ;; A bad fd in the set would spin the loop; drop watchers whose sockets are dead.
                  (%prune-dead-watchers r)
                  (funcall *loop-error-hook*
                           (make-condition 'simple-error :format-control "select() failed with code ~D"
                                                         :format-arguments (list code))
                           "reactor")))
              0)
             ((zerop n) 0)
             (t
              (incf (reactor-events r) n)
              (when (and (reactor-wake-fd r) (%fdset-member rs (reactor-wake-fd r)))
                (%drain-wakeups r))
              ;; Snapshot: callbacks may add/remove watchers.
              (let ((ready '()))
                (loop for w being the hash-values of (reactor-watchers r)
                      when (watcher-active w)
                        do (let ((rd (and (watcher-read-interest w) (%fdset-member rs (watcher-fd w))))
                                 (wr (and (watcher-write-interest w)
                                          (or (%fdset-member ws (watcher-fd w))
                                              (%fdset-member es (watcher-fd w))))))
                             (when (or rd wr) (push (list w rd wr) ready))))
                (dolist (entry ready)
                  (destructuring-bind (w rd wr) entry
                    (when (and rd (watcher-active w) (watcher-on-readable w))
                      (%safe-call loop (lambda () (funcall (watcher-on-readable w) w)) "readable"))
                    (when (and wr (watcher-active w) (watcher-on-writable w))
                      (%safe-call loop (lambda () (funcall (watcher-on-writable w) w)) "writable")))))
              n))))))))

(defun %prune-dead-watchers (r)
  (let ((dead '()))
    (maphash (lambda (fd w)
               (when (and (watcher-socket w)
                          (not (ignore-errors (sb-bsd-sockets:socket-open-p (watcher-socket w)))))
                 (push fd dead)))
             (reactor-watchers r))
    (dolist (fd dead) (remhash fd (reactor-watchers r)))))
