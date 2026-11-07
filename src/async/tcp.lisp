;;;; src/async/tcp.lisp -- asynchronous TCP server and client connections on the reactor
;;;;
;;;; Connections expose a promise API (CONN-READ / CONN-WRITE) which composes with ASYNC/AWAIT,
;;;; plus optional callbacks.  All socket IO is non-blocking and happens on the loop thread.

(in-package :monolisp.async)

;;; ------------------------------------------------------------------ raw send/recv

(define-alien-routine ("recv" %sys-recv) int
  (fd #+win32 unsigned-long-long #-win32 int)
  (buf (* t))
  (len int)
  (flags int))

(define-alien-routine ("send" %sys-send) int
  (fd #+win32 unsigned-long-long #-win32 int)
  (buf (* t))
  (len int)
  (flags int))

(define-alien-routine ("getsockopt" %sys-getsockopt) int
  (fd #+win32 unsigned-long-long #-win32 int)
  (level int)
  (optname int)
  (optval (* t))
  (optlen (* int)))

(defconstant +send-flags+ #+linux 16384 #-linux 0 "MSG_NOSIGNAL on Linux")
(defconstant +sol-socket+ #+linux 1 #-linux #xffff)
(defconstant +so-error+ #+linux 4 #-linux #x1007)

(defun %socket-pending-error (fd)
  "SO_ERROR for a connecting socket; 0 when the connection succeeded."
  (with-alien ((val int) (len int))
    (setf val 0 len 4)
    (if (zerop (%sys-getsockopt fd +sol-socket+ +so-error+ (addr val) (addr len)))
        val
        (socket-last-error))))

(defun %recv-into (fd buffer)
  "Receive into BUFFER.  Returns (values count status) with status :ok, :eof, :would-block or :error."
  (declare (type octets buffer))
  (let ((n (sb-sys:with-pinned-objects (buffer)
             (%sys-recv fd (sb-sys:vector-sap buffer) (length buffer) 0))))
    (cond ((> n 0) (values n :ok))
          ((= n 0) (values 0 :eof))
          (t (let ((code (socket-last-error)))
               (cond ((would-block-p code) (values 0 :would-block))
                     ((interrupted-p code) (values 0 :would-block))
                     (t (values code :error))))))))

(defun %send-from (fd buffer start end)
  "Send BUFFER[start,end).  Returns (values count status)."
  (declare (type octets buffer) (type fixnum start end))
  (let ((n (sb-sys:with-pinned-objects (buffer)
             (%sys-send fd (sb-sys:sap+ (sb-sys:vector-sap buffer) start) (- end start) +send-flags+))))
    (cond ((>= n 0) (values n :ok))
          (t (let ((code (socket-last-error)))
               (cond ((or (would-block-p code) (interrupted-p code)) (values 0 :would-block))
                     (t (values code :error))))))))

;;; ------------------------------------------------------------------ connections

(define-condition connection-closed (error)
  ((conn :initarg :conn :reader connection-closed-conn))
  (:report (lambda (c s) (declare (ignore c)) (format s "connection closed"))))

(defstruct (tcp-connection (:constructor %make-conn) (:conc-name conn-))
  socket
  (fd 0)
  watcher
  server
  peer
  (read-chunks '())            ; pending received octet chunks (FIFO list)
  (read-tail nil)
  (buffered 0 :type fixnum)
  (max-buffer (* 1024 1024) :type fixnum)
  (read-promise nil)           ; outstanding CONN-READ promise
  (write-queue '())            ; list of (octets start end promise)
  (write-tail nil)
  (eof nil)
  (closed nil)
  (timed-out nil)
  (close-on-timeout t)
  (timeout-timer nil)
  (idle-ms nil)
  (on-close nil)
  (bytes-in 0 :type fixnum)
  (bytes-out 0 :type fixnum)
  (recv-buffer (make-octet-buffer 65536) :type octets))

(defun conn-closed-p (conn) (conn-closed conn))

(defun %conn-touch (conn)
  "Reset the idle timer."
  (when (conn-idle-ms conn)
    (when (conn-timeout-timer conn) (clear-timer (conn-timeout-timer conn)))
    (setf (conn-timeout-timer conn)
          (set-timeout (lambda () (%conn-timeout conn)) (conn-idle-ms conn) :ref nil))))

(defun %conn-timeout (conn)
  (unless (conn-closed conn)
    (setf (conn-timed-out conn) t)
    (let ((p (conn-read-promise conn)))
      (setf (conn-read-promise conn) nil)
      (when p (reject p (make-condition 'timeout-error :what "connection idle"
                                                       :seconds (/ (conn-idle-ms conn) 1000d0)))))
    (if (conn-close-on-timeout conn)
        (conn-close conn)
        ;; Stop reading; the owner may still write a final response, then must close.
        (when (conn-watcher conn) (fd-interest (conn-watcher conn) :read nil)))))

(defun conn-set-timeout (conn ms &key (close t))
  "Fail pending reads with TIMEOUT-ERROR after MS idle milliseconds; NIL disables.  When CLOSE
(the default) the connection is also closed; otherwise reads stop but writes still work so a
caller can send e.g. a 408 before closing."
  (setf (conn-idle-ms conn) ms
        (conn-close-on-timeout conn) close
        (conn-timed-out conn) nil)
  (if ms
      (%conn-touch conn)
      (when (conn-timeout-timer conn)
        (clear-timer (conn-timeout-timer conn))
        (setf (conn-timeout-timer conn) nil)))
  conn)

(defun %conn-push-chunk (conn chunk)
  (let ((cell (list chunk)))
    (if (conn-read-chunks conn)
        (setf (cdr (conn-read-tail conn)) cell)
        (setf (conn-read-chunks conn) cell))
    (setf (conn-read-tail conn) cell)
    (incf (conn-buffered conn) (length chunk))))

(defun %conn-pop-chunk (conn)
  (let ((chunk (pop (conn-read-chunks conn))))
    (when (null (conn-read-chunks conn)) (setf (conn-read-tail conn) nil))
    (decf (conn-buffered conn) (length chunk))
    chunk))

(defun %conn-on-readable (conn)
  (loop
    (multiple-value-bind (n status) (%recv-into (conn-fd conn) (conn-recv-buffer conn))
      (ecase status
        (:ok
         (incf (conn-bytes-in conn) n)
         (%conn-touch conn)
         (let ((chunk (subseq-octets (conn-recv-buffer conn) 0 n))
               (p (conn-read-promise conn)))
           (cond (p (setf (conn-read-promise conn) nil)
                    (resolve p chunk))
                 (t (%conn-push-chunk conn chunk)
                    ;; backpressure: stop reading until the consumer catches up
                    (when (>= (conn-buffered conn) (conn-max-buffer conn))
                      (fd-interest (conn-watcher conn) :read nil)
                      (return)))))
         (when (< n (length (conn-recv-buffer conn))) (return)))
        (:eof
         (setf (conn-eof conn) t)
         (fd-interest (conn-watcher conn) :read nil)
         (let ((p (conn-read-promise conn)))
           (when p (setf (conn-read-promise conn) nil) (resolve p nil)))
         (when (null (conn-write-queue conn)) (conn-close conn))
         (return))
        (:would-block (return))
        (:error
         (%conn-fail conn (make-condition 'simple-error :format-control "recv failed (code ~D)" :format-arguments (list n)))
         (return))))))

(defun %conn-fail (conn condition)
  (let ((p (conn-read-promise conn)))
    (setf (conn-read-promise conn) nil)
    (when p (reject p condition)))
  (dolist (entry (conn-write-queue conn))
    (reject (fourth entry) condition))
  (setf (conn-write-queue conn) '() (conn-write-tail conn) nil)
  (conn-close conn))

(defun conn-read (conn)
  "Promise of the next chunk of octets, or NIL at end of stream."
  (cond ((conn-read-chunks conn)
         (let ((chunk (%conn-pop-chunk conn)))
           (when (and (conn-watcher conn) (not (conn-eof conn)) (not (conn-closed conn))
                      (< (conn-buffered conn) (conn-max-buffer conn)))
             (fd-interest (conn-watcher conn) :read t))
           (resolved chunk)))
        ((conn-timed-out conn)
         (rejected (make-condition 'timeout-error :what "connection idle")))
        ((or (conn-eof conn) (conn-closed conn))
         (resolved nil))
        ((conn-read-promise conn)
         (rejected (make-condition 'simple-error :format-control "conn-read already pending")))
        (t (let ((p (%make-promise)))
             (setf (conn-read-promise conn) p)
             (fd-interest (conn-watcher conn) :read t)
             p))))

(defun %conn-flush (conn)
  "Try to send queued data; returns T when the queue is empty."
  (loop
    (let ((entry (car (conn-write-queue conn))))
      (unless entry (return t))
      (destructuring-bind (buf start end promise) entry
        (multiple-value-bind (n status) (%send-from (conn-fd conn) buf start end)
          (ecase status
            (:ok
             (incf (conn-bytes-out conn) n)
             (incf start n)
             (setf (second entry) start)
             (when (>= start end)
               (pop (conn-write-queue conn))
               (when (null (conn-write-queue conn)) (setf (conn-write-tail conn) nil))
               (resolve promise t)))
            (:would-block (return nil))
            (:error
             (%conn-fail conn (make-condition 'simple-error :format-control "send failed (code ~D)" :format-arguments (list n)))
             (return t))))))))

(defun %conn-on-writable (conn)
  (when (%conn-flush conn)
    (when (conn-watcher conn) (fd-interest (conn-watcher conn) :write nil))
    (when (conn-eof conn) (conn-close conn))))

(defun conn-write (conn data)
  "Queue DATA (octets or string) for sending.  Promise resolves to T once fully handed to the kernel."
  (let ((buf (if (stringp data) (string-to-octets data) data))
        (p (%make-promise)))
    (cond ((conn-closed conn) (reject p (make-condition 'connection-closed :conn conn)))
          (t
           (let ((cell (list (list buf 0 (length buf) p))))
             (if (conn-write-queue conn)
                 (setf (cdr (conn-write-tail conn)) cell)
                 (setf (conn-write-queue conn) cell))
             (setf (conn-write-tail conn) cell))
           (%conn-touch conn)
           (unless (%conn-flush conn)
             (fd-interest (conn-watcher conn) :write t))))
    p))

(defun conn-close (conn)
  (unless (conn-closed conn)
    (setf (conn-closed conn) t)
    (when (conn-timeout-timer conn) (clear-timer (conn-timeout-timer conn)))
    (when (conn-watcher conn) (unwatch-fd (conn-watcher conn)))
    (ignore-errors (sb-bsd-sockets:socket-close (conn-socket conn)))
    (let ((p (conn-read-promise conn)))
      (setf (conn-read-promise conn) nil)
      (when p (resolve p nil)))
    (dolist (entry (conn-write-queue conn))
      (reject (fourth entry) (make-condition 'connection-closed :conn conn)))
    (setf (conn-write-queue conn) '())
    (let ((server (conn-server conn)))
      (when server
        (remhash conn (tcp-server-connections server))
        (loop-unref)))
    (when (conn-on-close conn)
      (%safe-call *loop* (lambda () (funcall (conn-on-close conn) conn)) "on-close"))
    t))

(defun %wrap-socket (socket &key server)
  (let ((conn (%make-conn :socket socket
                          :fd (sb-bsd-sockets:socket-file-descriptor socket)
                          :server server
                          :peer (ignore-errors (multiple-value-list (sb-bsd-sockets:socket-peername socket))))))
    (setf (conn-watcher conn)
          (watch-fd socket
                    :on-readable (lambda (w) (declare (ignore w)) (%conn-on-readable conn))
                    :on-writable (lambda (w) (declare (ignore w)) (%conn-on-writable conn))
                    :read t :write nil
                    ;; server connections keep the loop alive through the server's ref
                    :ref (null server)))
    conn))

;;; ------------------------------------------------------------------ server

(defstruct (tcp-server (:constructor %make-tcp-server))
  socket
  watcher
  (port 0)
  handler
  (connections (make-hash-table :test 'eq))
  (max-connections 512)
  (accepted 0 :type fixnum)
  (refused 0 :type fixnum)
  (closed nil))

(defun %parse-address (host)
  (cond ((or (null host) (string-equal host "localhost")) #(127 0 0 1))
        ((string= host "0.0.0.0") #(0 0 0 0))
        ((every (lambda (c) (or (digit-char-p c) (char= c #\.))) host)
         (let ((parts (loop with start = 0
                            for pos = (position #\. host :start start)
                            collect (parse-integer host :start start :end pos)
                            while pos do (setf start (1+ pos)))))
           (unless (and (= (length parts) 4) (every (lambda (x) (<= 0 x 255)) parts))
             (error "bad IPv4 address ~S" host))
           (coerce parts '(vector (unsigned-byte 8) 4))))
        (t nil)))

(defun tcp-listen (handler &key (host "127.0.0.1") (port 0) (backlog 128) (max-connections 512)
                             (loop *loop*))
  "Listen on HOST:PORT; HANDLER is called with each accepted TCP-CONNECTION.  Returns a TCP-SERVER."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (address (or (%parse-address host) (error "tcp-listen: HOST must be an IPv4 literal"))))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket address port)
    (sb-bsd-sockets:socket-listen socket backlog)
    (setf (sb-bsd-sockets:non-blocking-mode socket) t)
    (let ((server (%make-tcp-server :socket socket :handler handler :max-connections max-connections)))
      (setf (tcp-server-port server) (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (setf (tcp-server-watcher server)
            (watch-fd socket :on-readable (lambda (w) (declare (ignore w)) (%accept-loop server))
                             :read t :ref t :loop loop))
      server)))

(defun %accept-loop (server)
  (loop repeat 64
        do (let ((client (handler-case (sb-bsd-sockets:socket-accept (tcp-server-socket server))
                           (error () nil))))
             (unless client (return))
             (cond ((>= (hash-table-count (tcp-server-connections server)) (tcp-server-max-connections server))
                    ;; Over capacity: refuse immediately instead of queueing (DoS protection).
                    (incf (tcp-server-refused server))
                    (ignore-errors (sb-bsd-sockets:socket-close client)))
                   (t
                    (incf (tcp-server-accepted server))
                    (let ((conn (%wrap-socket client :server server)))
                      (setf (gethash conn (tcp-server-connections server)) t)
                      (loop-ref)
                      (%safe-call *loop* (lambda () (funcall (tcp-server-handler server) conn)) "accept")))))))

(defun tcp-server-close (server)
  (unless (tcp-server-closed server)
    (setf (tcp-server-closed server) t)
    (unwatch-fd (tcp-server-watcher server))
    (ignore-errors (sb-bsd-sockets:socket-close (tcp-server-socket server)))
    (let ((conns (loop for c being the hash-keys of (tcp-server-connections server) collect c)))
      (dolist (c conns) (conn-close c)))
    t))

(defun tcp-close (thing)
  (etypecase thing
    (tcp-server (tcp-server-close thing))
    (tcp-connection (conn-close thing))))

;;; ------------------------------------------------------------------ client

(defun %resolve-host (host)
  "Promise of an IPv4 address vector."
  (let ((literal (%parse-address host)))
    (if literal
        (resolved literal)
        (offload (lambda ()
                   (let ((entry (sb-bsd-sockets:get-host-by-name host)))
                     (or (sb-bsd-sockets:host-ent-address entry)
                         (error "could not resolve ~A" host))))))))

(defun tcp-connect (host port &key (timeout 10000))
  "Promise of a connected TCP-CONNECTION."
  (let ((result (%make-promise)))
    (then (%resolve-host host)
          (lambda (address)
            (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
              (setf (sb-bsd-sockets:non-blocking-mode socket) t)
              (let ((immediate
                      (handler-case (progn (sb-bsd-sockets:socket-connect socket address port) t)
                        (sb-bsd-sockets:socket-error (e)
                          (if (would-block-p (sb-bsd-sockets::socket-error-errno e))
                              nil
                              (progn (ignore-errors (sb-bsd-sockets:socket-close socket))
                                     (reject result e)
                                     :failed))))))
                (flet ((finish ()
                         (let ((err (%socket-pending-error (sb-bsd-sockets:socket-file-descriptor socket))))
                           (if (zerop err)
                               (resolve result (%wrap-socket socket))
                               (progn (ignore-errors (sb-bsd-sockets:socket-close socket))
                                      (reject result (make-condition 'simple-error
                                                                     :format-control "connect to ~A:~D failed (code ~D)"
                                                                     :format-arguments (list host port err))))))))
                  (cond
                    ((eq immediate :failed))
                    (immediate (finish))
                    (t
                      (let* ((watcher nil) (timer nil))
                        (setf watcher (watch-fd socket
                                                :read nil :write t
                                                :on-writable (lambda (w)
                                                               (unwatch-fd w)
                                                               (when timer (clear-timer timer))
                                                               (finish))))
                        (when timeout
                          (setf timer (set-timeout (lambda ()
                                                     (unwatch-fd watcher)
                                                     (ignore-errors (sb-bsd-sockets:socket-close socket))
                                                     (reject result (make-condition 'timeout-error :what "connect"
                                                                                                   :seconds (/ timeout 1000d0))))
                                                   timeout))))))))))
          (lambda (r) (reject result r)))
    result))
