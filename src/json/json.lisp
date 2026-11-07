;;;; src/json/json.lisp -- strict, resource-bounded JSON (RFC 8259) parser and encoder
;;;;
;;;; Threat model: every byte comes from an attacker.  Therefore:
;;;;   * input is parsed as octets with strict UTF-8 validation (no overlongs, no encoded
;;;;     surrogates, no code points above U+10FFFF, no replacement-char substitution)
;;;;   * hard limits on total size, nesting depth, string length, number length, exponent size,
;;;;     array length, object key count and total value count -- each checked *before* allocating
;;;;   * duplicate object keys are rejected by default (they are a classic parser-differential vector)
;;;;   * no leading zeros, hex, NaN, Infinity, comments, trailing commas, single quotes, or BOM
;;;;   * exactly one top-level value; anything after it (other than JSON whitespace) is an error
;;;;   * error messages never echo payload bytes (only positions), so they can be returned to clients
;;;;   * strings are never interned as symbols; keys stay strings
;;;;
;;;; Value model: objects -> JSON-OBJECT (ordered, string keyed); arrays -> SIMPLE-VECTOR;
;;;; strings -> STRING; integers -> INTEGER; other numbers -> DOUBLE-FLOAT; literals -> :TRUE :FALSE :NULL.

(in-package :monolisp.json)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

;;; ------------------------------------------------------------------ limits

(defstruct (json-limits (:copier copy-json-limits))
  (max-depth 64 :type fixnum)
  (max-bytes (* 1024 1024) :type fixnum)
  (max-string-bytes (* 256 1024) :type fixnum)
  (max-number-chars 64 :type fixnum)
  (max-integer-digits 32 :type fixnum)
  (max-exponent 308 :type fixnum)
  (max-array-items 100000 :type fixnum)
  (max-object-keys 10000 :type fixnum)
  (max-total-values 500000 :type fixnum)
  (duplicate-keys :reject :type (member :reject :first :last)))

(defvar *default-limits* (make-json-limits))

;;; ------------------------------------------------------------------ conditions

(define-condition json-parse-error (parse-error)
  ((position :initarg :position :initform nil :reader json-parse-error-position)
   (message :initarg :message :reader json-parse-error-message))
  (:report (lambda (c s)
             (format s "JSON parse error~@[ at byte ~D~]: ~A"
                     (json-parse-error-position c) (json-parse-error-message c)))))

(define-condition json-encode-error (error)
  ((message :initarg :message :reader json-encode-error-message))
  (:report (lambda (c s) (format s "JSON encode error: ~A" (json-encode-error-message c)))))

;;; ------------------------------------------------------------------ json-object

(defstruct (json-object (:constructor %make-json-object ())
                        (:print-function print-json-object))
  (keys (make-array 4 :adjustable t :fill-pointer 0))
  (table (make-hash-table :test 'equal) :type hash-table))

(defun print-json-object (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream :type t)
    (format stream "~D key~:P" (length (json-object-keys obj)))))

(defun make-json-object (&rest keys-and-values)
  "Build an object from alternating string keys and values."
  (let ((obj (%make-json-object)))
    (loop for (k v) on keys-and-values by #'cddr
          do (json-set obj k v))
    obj))

(defun json-set (obj key value)
  (check-type key string)
  (unless (nth-value 1 (gethash key (json-object-table obj)))
    (vector-push-extend key (json-object-keys obj)))
  (setf (gethash key (json-object-table obj)) value)
  obj)

(defun json-get (obj key &optional default)
  "Value for KEY, or DEFAULT.  Second value is true if the key was present."
  (multiple-value-bind (v found) (gethash key (json-object-table obj))
    (if found (values v t) (values default nil))))

(defun json-has-key-p (obj key)
  (nth-value 1 (gethash key (json-object-table obj))))

(defun json-keys (obj) (coerce (json-object-keys obj) 'list))
(defun json-object-count (obj) (length (json-object-keys obj)))

(defun json-object-alist (obj)
  (loop for k across (json-object-keys obj)
        collect (cons k (gethash k (json-object-table obj)))))

(defun json-ref (value &rest path)
  "Walk PATH (strings for object keys, integers for array indices) into VALUE; NIL when absent."
  (dolist (step path value)
    (setf value
          (cond ((and (stringp step) (json-object-p value)) (json-get value step))
                ((and (integerp step) (simple-vector-p value) (< -1 step (length value))) (svref value step))
                (t (return nil))))))

(declaim (inline json-null-p json-true-p json-false-p json-bool))
(defun json-null-p (v) (eq v :null))
(defun json-true-p (v) (eq v :true))
(defun json-false-p (v) (eq v :false))
(defun json-bool (generalized-boolean) (if generalized-boolean :true :false))

;;; ------------------------------------------------------------------ parser state

(defstruct (parser (:constructor %make-parser (input end limits)))
  (input (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (depth 0 :type fixnum)
  (values 0 :type fixnum)
  (limits *default-limits* :type json-limits)
  (string-buffer (make-array 64 :element-type 'character :adjustable t :fill-pointer 0)))

(declaim (inline peek-byte fail))

(defun fail (parser message)
  (error 'json-parse-error :position (parser-pos parser) :message message))

(defun peek-byte (parser)
  (let ((pos (parser-pos parser)))
    (if (< pos (parser-end parser))
        (aref (parser-input parser) pos)
        -1)))

(defun skip-whitespace (parser)
  (declare (optimize (speed 3) (safety 1)))
  (let ((input (parser-input parser))
        (end (parser-end parser))
        (pos (parser-pos parser)))
    (declare (type octets input) (type fixnum pos end))
    (loop while (and (< pos end)
                     (let ((b (aref input pos)))
                       (or (= b 32) (= b 9) (= b 10) (= b 13))))
          do (incf pos))
    (setf (parser-pos parser) pos)))

(defun count-value (parser)
  (when (> (incf (parser-values parser)) (json-limits-max-total-values (parser-limits parser)))
    (fail parser "too many values")))

;;; ------------------------------------------------------------------ entry points

(defun json-parse-octets (octets &key (limits *default-limits*) (start 0) end)
  "Parse exactly one JSON text from OCTETS[start,end)."
  (check-type octets (vector (unsigned-byte 8)))
  (let* ((octets (if (typep octets 'octets) octets (coerce octets 'octets)))
         (end (or end (length octets))))
    (when (> (- end start) (json-limits-max-bytes limits))
      (error 'json-parse-error :position 0 :message "input exceeds maximum size"))
    (let ((parser (%make-parser octets end limits)))
      (setf (parser-pos parser) start)
      (skip-whitespace parser)
      (when (>= (parser-pos parser) end) (fail parser "empty input"))
      (let ((value (parse-value parser)))
        (skip-whitespace parser)
        (unless (= (parser-pos parser) end)
          (fail parser "unexpected trailing data after JSON value"))
        value))))

(defun json-parse (string-or-octets &key (limits *default-limits*))
  "Parse a JSON text from a string (encoded strictly as UTF-8 first) or an octet vector."
  (etypecase string-or-octets
    (string (json-parse-octets (sb-ext:string-to-octets string-or-octets :external-format :utf-8) :limits limits))
    ((vector (unsigned-byte 8)) (json-parse-octets string-or-octets :limits limits))))

;;; ------------------------------------------------------------------ values

(defun parse-value (parser)
  (count-value parser)
  (let ((b (peek-byte parser)))
    (case b
      (123 (parse-object parser))                        ; {
      (91 (parse-array parser))                          ; [
      (34 (parse-string parser))                         ; "
      (116 (parse-literal parser #(116 114 117 101) :true))        ; true
      (102 (parse-literal parser #(102 97 108 115 101) :false))    ; false
      (110 (parse-literal parser #(110 117 108 108) :null))        ; null
      (t (if (or (= b 45) (<= 48 b 57))
             (parse-number parser)
             (fail parser (if (= b -1) "unexpected end of input" "unexpected character")))))))

(defun parse-literal (parser bytes value)
  (let ((input (parser-input parser)) (pos (parser-pos parser)) (end (parser-end parser)))
    (loop for expected across bytes
          for i from pos
          do (unless (and (< i end) (= (aref input i) expected))
               (fail parser "invalid literal")))
    (setf (parser-pos parser) (+ pos (length bytes)))
    value))

(defun enter (parser)
  (when (> (incf (parser-depth parser)) (json-limits-max-depth (parser-limits parser)))
    (fail parser "nesting too deep")))

(defun leave (parser) (decf (parser-depth parser)))

(defun expect-byte (parser byte message)
  (skip-whitespace parser)
  (unless (= (peek-byte parser) byte) (fail parser message))
  (incf (parser-pos parser)))

(defun parse-object (parser)
  (enter parser)
  (incf (parser-pos parser))                             ; consume {
  (let ((obj (%make-json-object))
        (limits (parser-limits parser))
        (count 0))
    (skip-whitespace parser)
    (cond ((= (peek-byte parser) 125) (incf (parser-pos parser)))    ; empty {}
          (t
           (loop
             (skip-whitespace parser)
             (unless (= (peek-byte parser) 34) (fail parser "expected string key"))
             (when (>= count (json-limits-max-object-keys limits)) (fail parser "too many object keys"))
             (let ((key (parse-string parser)))
               (expect-byte parser 58 "expected ':' after key")
               (skip-whitespace parser)
               (let ((value (parse-value parser)))
                 (multiple-value-bind (old present) (gethash key (json-object-table obj))
                   (declare (ignore old))
                   (cond ((not present)
                          (vector-push-extend key (json-object-keys obj))
                          (setf (gethash key (json-object-table obj)) value))
                         (t (ecase (json-limits-duplicate-keys limits)
                              (:reject (fail parser "duplicate object key"))
                              (:first nil)
                              (:last (setf (gethash key (json-object-table obj)) value))))))
                 (incf count)))
             (skip-whitespace parser)
             (let ((b (peek-byte parser)))
               (cond ((= b 44) (incf (parser-pos parser)))           ; ,
                     ((= b 125) (incf (parser-pos parser)) (return)) ; }
                     (t (fail parser "expected ',' or '}' in object")))))))
    (leave parser)
    obj))

(defun parse-array (parser)
  (enter parser)
  (incf (parser-pos parser))                             ; consume [
  (let ((items '()) (count 0) (limits (parser-limits parser)))
    (skip-whitespace parser)
    (cond ((= (peek-byte parser) 93) (incf (parser-pos parser)))
          (t
           (loop
             (skip-whitespace parser)
             (when (>= count (json-limits-max-array-items limits)) (fail parser "too many array items"))
             (push (parse-value parser) items)
             (incf count)
             (skip-whitespace parser)
             (let ((b (peek-byte parser)))
               (cond ((= b 44) (incf (parser-pos parser)))
                     ((= b 93) (incf (parser-pos parser)) (return))
                     (t (fail parser "expected ',' or ']' in array")))))))
    (leave parser)
    (let ((vec (make-array count)))
      (loop for i from (1- count) downto 0
            for item in items
            do (setf (svref vec i) item))
      vec)))

;;; ------------------------------------------------------------------ strings

(defun hex-value (b)
  (cond ((<= 48 b 57) (- b 48))
        ((<= 97 b 102) (- b 87))
        ((<= 65 b 70) (- b 55))
        (t nil)))

(defun parse-hex4 (parser)
  (let ((input (parser-input parser)) (pos (parser-pos parser)) (end (parser-end parser)) (code 0))
    (when (> (+ pos 4) end) (fail parser "truncated \\u escape"))
    (dotimes (i 4)
      (let ((h (hex-value (aref input (+ pos i)))))
        (unless h (fail parser "invalid hex digit in \\u escape"))
        (setf code (+ (* code 16) h))))
    (setf (parser-pos parser) (+ pos 4))
    code))

(defun decode-utf8-char (parser input pos end)
  "Decode one strictly valid UTF-8 sequence starting at POS; returns (values code-point new-pos)."
  (declare (type octets input) (type fixnum pos end))
  (let ((b0 (aref input pos)))
    (flet ((cont (i)
             (let ((idx (+ pos i)))
               (when (>= idx end) (fail parser "truncated UTF-8 sequence"))
               (let ((b (aref input idx)))
                 (unless (= (logand b #xC0) #x80) (fail parser "invalid UTF-8 continuation byte"))
                 (logand b #x3F)))))
      (cond
        ((< b0 #x80) (values b0 (1+ pos)))
        ((< b0 #xC2) (fail parser "invalid UTF-8 lead byte"))              ; continuation or overlong C0/C1
        ((< b0 #xE0)
         (values (logior (ash (logand b0 #x1F) 6) (cont 1)) (+ pos 2)))
        ((< b0 #xF0)
         (let ((cp (logior (ash (logand b0 #x0F) 12) (ash (cont 1) 6) (cont 2))))
           (when (< cp #x800) (fail parser "overlong UTF-8 sequence"))
           (when (<= #xD800 cp #xDFFF) (fail parser "UTF-8 encoded surrogate"))
           (values cp (+ pos 3))))
        ((< b0 #xF5)
         (let ((cp (logior (ash (logand b0 #x07) 18) (ash (cont 1) 12) (ash (cont 2) 6) (cont 3))))
           (when (< cp #x10000) (fail parser "overlong UTF-8 sequence"))
           (when (> cp #x10FFFF) (fail parser "code point above U+10FFFF"))
           (values cp (+ pos 4))))
        (t (fail parser "invalid UTF-8 lead byte"))))))

(defun parse-string (parser)
  (declare (optimize (speed 2) (safety 1)))
  (let* ((input (parser-input parser))
         (end (parser-end parser))
         (start (1+ (parser-pos parser)))              ; after opening quote
         (max-bytes (json-limits-max-string-bytes (parser-limits parser)))
         (pos start))
    (declare (type octets input) (type fixnum pos end start))
    ;; Fast path: scan for the closing quote with no escapes and only ASCII printable bytes.
    (loop
      (when (>= pos end) (setf (parser-pos parser) pos) (fail parser "unterminated string"))
      (when (> (- pos start) max-bytes) (setf (parser-pos parser) pos) (fail parser "string too long"))
      (let ((b (aref input pos)))
        (cond ((= b 34)
               (setf (parser-pos parser) (1+ pos))
               (return-from parse-string
                 (let ((s (make-string (- pos start))))
                   (loop for i from start below pos
                         for j from 0
                         do (setf (schar s j) (code-char (aref input i))))
                   s)))
              ((or (= b 92) (>= b #x80)) (return))      ; escape or non-ASCII: slow path
              ((< b 32) (setf (parser-pos parser) pos) (fail parser "control character in string"))
              (t (incf pos)))))
    ;; Slow path: full decoding into the reusable buffer.
    (let ((buf (parser-string-buffer parser)))
      (setf (fill-pointer buf) 0)
      (loop for i from start below pos
            do (vector-push-extend (code-char (aref input i)) buf))
      (loop
        (when (>= pos end) (setf (parser-pos parser) pos) (fail parser "unterminated string"))
        (when (> (- pos start) max-bytes) (setf (parser-pos parser) pos) (fail parser "string too long"))
        (let ((b (aref input pos)))
          (cond
            ((= b 34)
             (setf (parser-pos parser) (1+ pos))
             (return (coerce buf 'simple-string)))
            ((= b 92)
             (incf pos)
             (when (>= pos end) (setf (parser-pos parser) pos) (fail parser "unterminated escape"))
             (let ((e (aref input pos)))
               (incf pos)
               (case e
                 (34 (vector-push-extend #\" buf))
                 (92 (vector-push-extend #\\ buf))
                 (47 (vector-push-extend #\/ buf))
                 (98 (vector-push-extend (code-char 8) buf))
                 (102 (vector-push-extend (code-char 12) buf))
                 (110 (vector-push-extend (code-char 10) buf))
                 (114 (vector-push-extend (code-char 13) buf))
                 (116 (vector-push-extend (code-char 9) buf))
                 (117
                  (setf (parser-pos parser) pos)
                  (let ((code (parse-hex4 parser)))
                    (setf pos (parser-pos parser))
                    (cond
                      ((<= #xD800 code #xDBFF)
                       ;; high surrogate: must be followed by \uDC00-\uDFFF
                       (unless (and (< (+ pos 1) end) (= (aref input pos) 92) (= (aref input (1+ pos)) 117))
                         (setf (parser-pos parser) pos) (fail parser "lone high surrogate"))
                       (setf (parser-pos parser) (+ pos 2))
                       (let ((low (parse-hex4 parser)))
                         (setf pos (parser-pos parser))
                         (unless (<= #xDC00 low #xDFFF)
                           (fail parser "invalid low surrogate"))
                         (vector-push-extend (code-char (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00))) buf)))
                      ((<= #xDC00 code #xDFFF)
                       (setf (parser-pos parser) pos) (fail parser "lone low surrogate"))
                      (t (vector-push-extend (code-char code) buf)))))
                 (t (setf (parser-pos parser) pos) (fail parser "invalid escape sequence")))))
            ((< b 32) (setf (parser-pos parser) pos) (fail parser "control character in string"))
            ((< b #x80) (vector-push-extend (code-char b) buf) (incf pos))
            (t (setf (parser-pos parser) pos)
               (multiple-value-bind (cp new-pos) (decode-utf8-char parser input pos end)
                 (vector-push-extend (code-char cp) buf)
                 (setf pos new-pos)))))))))

;;; ------------------------------------------------------------------ numbers

(defvar *powers-of-ten*
  (let ((v (make-array 23 :element-type 'double-float)))
    (dotimes (i 23 v) (setf (aref v i) (coerce (expt 10 i) 'double-float)))))

(defun parse-number (parser)
  (declare (optimize (speed 2) (safety 1)))
  (let* ((input (parser-input parser))
         (end (parser-end parser))
         (start (parser-pos parser))
         (pos start)
         (limits (parser-limits parser))
         (max-chars (json-limits-max-number-chars limits))
         (negative nil)
         (mantissa 0)
         (int-digits 0)
         (frac-digits 0)
         (exponent 0)
         (is-float nil))
    (declare (type octets input) (type fixnum pos end start int-digits frac-digits))
    (flet ((digit-at (p) (and (< p end) (let ((b (aref input p))) (and (<= 48 b 57) (- b 48)))))
           (check-len () (when (> (- pos start) max-chars) (setf (parser-pos parser) pos) (fail parser "number too long"))))
      (when (= (aref input pos) 45) (setf negative t) (incf pos))
      ;; integer part
      (let ((d (digit-at pos)))
        (unless d (setf (parser-pos parser) pos) (fail parser "invalid number"))
        (cond ((zerop d)
               (incf pos)
               (when (digit-at pos) (setf (parser-pos parser) pos) (fail parser "leading zero in number")))
              (t (loop for d = (digit-at pos)
                       while d
                       do (incf int-digits)
                          (when (> int-digits (json-limits-max-integer-digits limits))
                            (setf (parser-pos parser) pos) (fail parser "too many integer digits"))
                          (setf mantissa (+ (* mantissa 10) d))
                          (incf pos)
                          (check-len)))))
      ;; fraction
      (when (and (< pos end) (= (aref input pos) 46))
        (setf is-float t)
        (incf pos)
        (unless (digit-at pos) (setf (parser-pos parser) pos) (fail parser "digit expected after decimal point"))
        (loop for d = (digit-at pos)
              while d
              do (incf frac-digits)
                 (setf mantissa (+ (* mantissa 10) d))
                 (incf pos)
                 (check-len)))
      ;; exponent
      (when (and (< pos end) (or (= (aref input pos) 101) (= (aref input pos) 69)))
        (setf is-float t)
        (incf pos)
        (let ((exp-negative nil) (exp-digits 0))
          (when (and (< pos end) (or (= (aref input pos) 43) (= (aref input pos) 45)))
            (setf exp-negative (= (aref input pos) 45))
            (incf pos))
          (unless (digit-at pos) (setf (parser-pos parser) pos) (fail parser "digit expected in exponent"))
          (loop for d = (digit-at pos)
                while d
                do (incf exp-digits)
                   (when (> exp-digits 4) (setf (parser-pos parser) pos) (fail parser "exponent too large"))
                   (setf exponent (+ (* exponent 10) d))
                   (incf pos)
                   (check-len))
          (when exp-negative (setf exponent (- exponent)))))
      (setf (parser-pos parser) pos)
      (cond
        ((not is-float) (if negative (- mantissa) mantissa))
        (t
         (let ((effective-exp (- exponent frac-digits))
               (max-exp (json-limits-max-exponent limits)))
           ;; magnitude ~ 10^(int-digits + exponent - 1); reject before doing bignum arithmetic
           (when (> (+ int-digits exponent) (1+ max-exp))
             (fail parser "number magnitude too large"))
           (let ((value
                   (cond ((zerop mantissa) 0d0)
                         ((< (+ effective-exp int-digits frac-digits) -400) 0d0)   ; underflows to zero
                         ((and (< mantissa (expt 2 53)) (<= 0 effective-exp 22))
                          (* (coerce mantissa 'double-float) (aref *powers-of-ten* effective-exp)))
                         ((and (< mantissa (expt 2 53)) (<= -22 effective-exp -1))
                          (/ (coerce mantissa 'double-float) (aref *powers-of-ten* (- effective-exp))))
                         (t (let ((exact (* mantissa (expt 10 effective-exp))))
                              (when (> exact most-positive-double-float) (fail parser "number magnitude too large"))
                              (coerce exact 'double-float))))))
             (if negative (- value) value))))))))

;;; ------------------------------------------------------------------ encoder

(defvar *escape-html* t
  "When true, also escape < > & and U+2028/U+2029 so output is safe inside <script> blocks.")

(defparameter *max-encode-depth* 256)

(defgeneric encode-json-value (value stream depth)
  (:documentation "Write VALUE as JSON to STREAM.  Specialise for application types."))

(defgeneric json-encode-object-slots (value)
  (:documentation "Return an alist of (string-key . value) for structure-like VALUE, or :unsupported.")
  (:method (value) (declare (ignore value)) :unsupported))

(defun write-json-string (string stream)
  (declare (type string string))
  (write-char #\" stream)
  (loop for ch across string
        for code = (char-code ch)
        do (cond
             ((= code 34) (write-string "\\\"" stream))
             ((= code 92) (write-string "\\\\" stream))
             ((= code 10) (write-string "\\n" stream))
             ((= code 13) (write-string "\\r" stream))
             ((= code 9) (write-string "\\t" stream))
             ((= code 8) (write-string "\\b" stream))
             ((= code 12) (write-string "\\f" stream))
             ((< code 32) (format stream "\\u~4,'0X" code))
             ((and *escape-html* (or (= code 60) (= code 62) (= code 38)))
              (format stream "\\u~4,'0X" code))
             ((or (= code #x2028) (= code #x2029) (= code #x7F))
              (format stream "\\u~4,'0X" code))
             ((<= #xD800 code #xDFFF)
              (error 'json-encode-error :message "string contains a surrogate code point"))
             (t (write-char ch stream))))
  (write-char #\" stream))

(defun write-json-float (value stream)
  (let ((d (coerce value 'double-float)))
    (when (or (sb-ext:float-infinity-p d) (sb-ext:float-nan-p d))
      (error 'json-encode-error :message "cannot encode NaN or infinity"))
    (cond ((and (= d (ffloor d)) (< (abs d) 1d15))
           (format stream "~D" (truncate d)))
          (t
           ;; SBCL prints shortest round-tripping representation; normalise exponent markers.
           (let ((s (let ((*read-default-float-format* 'double-float)) (prin1-to-string d))))
             (write-string (substitute #\e #\d s) stream))))))

(defmethod encode-json-value ((value t) stream depth)
  (let ((slots (json-encode-object-slots value)))
    (if (eq slots :unsupported)
        (error 'json-encode-error :message (format nil "cannot encode value of type ~A" (type-of value)))
        (encode-alist slots stream depth))))

(defun encode-alist (alist stream depth)
  (write-char #\{ stream)
  (loop for (k . v) in alist
        for first = t then nil
        do (unless first (write-char #\, stream))
           (write-json-string (if (stringp k) k (string-downcase (string k))) stream)
           (write-char #\: stream)
           (encode-json-value v stream (1+ depth)))
  (write-char #\} stream))

(defmethod encode-json-value :around ((value t) stream depth)
  (when (> depth *max-encode-depth*)
    (error 'json-encode-error :message "value nesting too deep (cycle?)"))
  (call-next-method))

(defmethod encode-json-value ((value null) stream depth)
  (declare (ignore depth))
  (write-string "null" stream))

(defmethod encode-json-value ((value symbol) stream depth)
  (declare (ignore depth))
  (case value
    ((:true t) (write-string "true" stream))
    (:false (write-string "false" stream))
    (:null (write-string "null" stream))
    (t (write-json-string (string-downcase (symbol-name value)) stream))))

(defmethod encode-json-value ((value integer) stream depth)
  (declare (ignore depth))
  (format stream "~D" value))

(defmethod encode-json-value ((value real) stream depth)
  (declare (ignore depth))
  (write-json-float value stream))

(defmethod encode-json-value ((value string) stream depth)
  (declare (ignore depth))
  (write-json-string value stream))

(defmethod encode-json-value ((value character) stream depth)
  (declare (ignore depth))
  (write-json-string (string value) stream))

(defmethod encode-json-value ((value vector) stream depth)
  (write-char #\[ stream)
  (loop for item across value
        for first = t then nil
        do (unless first (write-char #\, stream))
           (encode-json-value item stream (1+ depth)))
  (write-char #\] stream))

(defmethod encode-json-value ((value cons) stream depth)
  (write-char #\[ stream)
  (loop for item in value
        for first = t then nil
        do (unless first (write-char #\, stream))
           (encode-json-value item stream (1+ depth)))
  (write-char #\] stream))

(defmethod encode-json-value ((value json-object) stream depth)
  (write-char #\{ stream)
  (loop for key across (json-object-keys value)
        for first = t then nil
        do (unless first (write-char #\, stream))
           (write-json-string key stream)
           (write-char #\: stream)
           (encode-json-value (gethash key (json-object-table value)) stream (1+ depth)))
  (write-char #\} stream))

(defmethod encode-json-value ((value hash-table) stream depth)
  (write-char #\{ stream)
  (let ((first t))
    (maphash (lambda (k v)
               (unless first (write-char #\, stream))
               (setf first nil)
               (write-json-string (if (stringp k) k (string-downcase (princ-to-string k))) stream)
               (write-char #\: stream)
               (encode-json-value v stream (1+ depth)))
             value))
  (write-char #\} stream))

(defun json-encode (value &key stream)
  "Encode VALUE as compact JSON.  Returns a string unless STREAM is given."
  (if stream
      (progn (encode-json-value value stream 0) nil)
      (with-output-to-string (s)
        (encode-json-value value s 0))))

(defun json-encode-to-octets (value)
  (sb-ext:string-to-octets (json-encode value) :external-format :utf-8))
