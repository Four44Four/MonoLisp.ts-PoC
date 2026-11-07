;;;; src/async/util.lisp -- small shared utilities

(in-package :monolisp.async)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline monotonic-ms now-ms))
(defun monotonic-ms ()
  "Monotonic milliseconds (double-float)."
  (/ (coerce (get-internal-real-time) 'double-float)
     (/ internal-time-units-per-second 1000d0)))

(defun now-ms ()
  "Wall clock milliseconds since the Unix epoch (approximate, second precision + monotonic offset)."
  (+ (* (- (get-universal-time) 2208988800) 1000d0)
     (mod (monotonic-ms) 1000d0)))

(defun string-to-octets (string &key (start 0) end)
  "Strict UTF-8 encoding."
  (sb-ext:string-to-octets string :external-format :utf-8 :start start :end end))

(defun octets-to-string (octets &key (start 0) end)
  "Strict UTF-8 decoding; signals on malformed input instead of substituting replacement chars."
  (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement nil) :start start :end end))

(defmacro with-mutex-held ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex) ,@body))

(defun make-octet-buffer (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun octets-concat (&rest parts)
  (let* ((total (reduce #'+ parts :key #'length))
         (out (make-octet-buffer total))
         (pos 0))
    (dolist (p parts out)
      (replace out p :start1 pos)
      (incf pos (length p)))))

(defun subseq-octets (octets start &optional end)
  (let* ((end (or end (length octets)))
         (out (make-octet-buffer (- end start))))
    (replace out octets :start2 start :end2 end)
    out))
