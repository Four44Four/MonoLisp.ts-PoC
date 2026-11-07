;;;; src/rpc/security.lisp -- limits, request context, authentication hooks, rate limiting

(in-package :monolisp.rpc)

;;; ------------------------------------------------------------------ limits

(defstruct (rpc-limits (:copier copy-rpc-limits))
  (max-batch 32 :type fixnum)
  (max-method-length 128 :type fixnum)
  (max-id-length 128 :type fixnum)
  (max-payload-bytes (* 1024 1024) :type fixnum)
  (call-timeout-ms 30000 :type fixnum)
  (json-limits (make-json-limits :max-depth 32 :max-bytes (* 1024 1024)) :type json-limits))

(defvar *rpc-limits* (make-rpc-limits))

;;; ------------------------------------------------------------------ context

(defstruct (rpc-context (:conc-name context-))
  (remote-address "unknown" :type string)
  (headers '() :type list)                 ; alist of (lowercase-name . value), already size-limited by HTTP layer
  (principal nil)                          ; set by *authenticator*
  (request-id nil)
  (started-at (get-internal-real-time)))

(defun context-header (context name)
  (cdr (assoc (string-downcase name) (context-headers context) :test #'string=)))

(defun context-authenticated-p (context) (and (context-principal context) t))

(defvar *request-context* nil
  "Bound to the RPC-CONTEXT while a server function runs.")

(defvar *authenticator* nil
  "Function of (CONTEXT) returning a principal (any non-NIL object) or NIL.  Called once per request
before any :authenticated function runs.  Must not signal; errors are treated as unauthenticated.")

(defvar *error-logger*
  (lambda (condition context)
    (declare (ignore context))
    (format *error-output* "~&[rpc] internal error: ~A~%" condition))
  "Called with (CONDITION CONTEXT) when a server function fails unexpectedly.")

(defvar *debug-mode* nil
  "When true, internal error responses include the condition text.  Never enable in production.")

;;; ------------------------------------------------------------------ constant-time comparison

(defun constant-time-equal (a b)
  "Compare two strings/octet vectors without leaking where they differ through timing."
  (let* ((a (if (stringp a) (sb-ext:string-to-octets a :external-format :utf-8) a))
         (b (if (stringp b) (sb-ext:string-to-octets b :external-format :utf-8) b))
         (la (length a)) (lb (length b))
         (acc (logxor la lb)))
    (dotimes (i (max la lb))
      (setf acc (logior acc (logxor (if (< i la) (aref a i) 0) (if (< i lb) (aref b i) 0)))))
    (zerop acc)))

#+win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-alien:load-shared-object "bcrypt.dll" :dont-save t))

(defun fill-random-octets (buf)
  "Fill BUF from the OS CSPRNG, or signal: tokens must never come from a predictable generator."
  #+win32
  (let ((status (sb-sys:with-pinned-objects (buf)
                  ;; BCRYPT_USE_SYSTEM_PREFERRED_RNG = 2, no algorithm handle
                  (sb-alien:alien-funcall
                   (sb-alien:extern-alien "BCryptGenRandom"
                                          (function sb-alien:long sb-alien:system-area-pointer
                                                    sb-alien:system-area-pointer sb-alien:unsigned-long
                                                    sb-alien:unsigned-long))
                   (sb-sys:int-sap 0) (sb-sys:vector-sap buf) (length buf) 2))))
    (unless (zerop status)
      (error "BCryptGenRandom failed with status ~X" (ldb (byte 32 0) status))))
  #-win32
  (with-open-file (s "/dev/urandom" :element-type '(unsigned-byte 8))
    (unless (= (read-sequence buf s) (length buf))
      (error "short read from /dev/urandom")))
  buf)

(defun random-token (&optional (bytes 32))
  "Cryptographically random hex token from the OS entropy source."
  (let ((buf (fill-random-octets (make-array bytes :element-type '(unsigned-byte 8)))))
    (with-output-to-string (out)
      (loop for b across buf do (format out "~(~2,'0X~)" b)))))

;;; ------------------------------------------------------------------ rate limiter (token bucket)

(defstruct (bucket (:constructor make-bucket (tokens updated)))
  (tokens 0d0 :type double-float)
  (updated 0d0 :type double-float))

(defstruct (rate-limiter (:constructor %make-rate-limiter))
  (capacity 60d0 :type double-float)      ; burst size
  (refill-per-second 10d0 :type double-float)
  (buckets (make-hash-table :test 'equal) :type hash-table)
  (max-keys 100000 :type fixnum)
  (lock (sb-thread:make-mutex :name "rate-limiter")))

(defun make-rate-limiter (&key (capacity 60) (refill-per-second 10) (max-keys 100000))
  (%make-rate-limiter :capacity (coerce capacity 'double-float)
                      :refill-per-second (coerce refill-per-second 'double-float)
                      :max-keys max-keys))

(defun rate-limit-allow-p (limiter key &optional (cost 1))
  "Consume COST tokens for KEY if available.  Returns T when the request may proceed."
  (let ((now (monotonic-ms)))
    (sb-thread:with-mutex ((rate-limiter-lock limiter))
      (let* ((table (rate-limiter-buckets limiter))
             (bucket (gethash key table)))
        (unless bucket
          ;; Bound memory: when too many distinct keys, evict stale ones (or refuse if still full).
          (when (>= (hash-table-count table) (rate-limiter-max-keys limiter))
            (%prune-locked limiter now)
            (when (>= (hash-table-count table) (rate-limiter-max-keys limiter))
              (return-from rate-limit-allow-p nil)))
          (setf bucket (make-bucket (rate-limiter-capacity limiter) now)
                (gethash key table) bucket))
        (let* ((elapsed (/ (- now (bucket-updated bucket)) 1000d0))
               (tokens (min (rate-limiter-capacity limiter)
                            (+ (bucket-tokens bucket) (* elapsed (rate-limiter-refill-per-second limiter))))))
          (setf (bucket-updated bucket) now)
          (cond ((>= tokens cost)
                 (setf (bucket-tokens bucket) (- tokens cost))
                 t)
                (t (setf (bucket-tokens bucket) tokens)
                   nil)))))))

(defun %prune-locked (limiter now)
  (let ((stale '())
        (full-after (/ (* 1000d0 (rate-limiter-capacity limiter)) (max 1d-9 (rate-limiter-refill-per-second limiter)))))
    (maphash (lambda (k b) (when (> (- now (bucket-updated b)) full-after) (push k stale)))
             (rate-limiter-buckets limiter))
    (dolist (k stale) (remhash k (rate-limiter-buckets limiter)))
    (length stale)))

(defun rate-limiter-prune (limiter)
  "Drop buckets that have fully refilled (no information lost).  Returns the number removed."
  (sb-thread:with-mutex ((rate-limiter-lock limiter))
    (%prune-locked limiter (monotonic-ms))))

(defun rate-limiter-count (limiter)
  (hash-table-count (rate-limiter-buckets limiter)))
