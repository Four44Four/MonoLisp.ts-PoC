;;;; src/transpiler/emit.lisp -- text output helpers

(in-package :monolisp.transpiler)

(defvar *out* nil "Stream receiving statements.")
(defvar *indent* 0)

(defun emit (fmt &rest args)
  "Emit one indented line."
  (loop repeat *indent* do (write-string "  " *out*))
  (apply #'format *out* fmt args)
  (terpri *out*))

(defmacro with-indent (&body body)
  `(let ((*indent* (1+ *indent*))) ,@body))

(defmacro with-indent-if (test &body body)
  `(if ,test (with-indent ,@body) (progn ,@body)))

(defmacro capture-statements (&body body)
  "Run BODY (which EMITs) and return the emitted text as a string."
  `(with-output-to-string (*out*) ,@body))

(defun js-string (string)
  "A double-quoted JS string literal; also escapes <, > and line separators so output is safe
inside <script> contexts."
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for ch across string
          for code = (char-code ch)
          do (case ch
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (#\Return (write-string "\\r" s))
               (#\Tab (write-string "\\t" s))
               (t (if (or (< code 32) (= code #x2028) (= code #x2029) (= code 127))
                      (format s "\\u~4,'0X" code)
                      (write-char ch s)))))
    (write-char #\" s)))

(defun js-number (n)
  (etypecase n
    (integer (if (> (abs n) (1- (expt 2 53)))
                 (format nil "~Dn" n)                   ; bigint literal
                 (format nil "~D" n)))
    (ratio (format nil "(~D / ~D)" (numerator n) (denominator n)))
    (float (let ((*read-default-float-format* 'double-float))
             (cond ((sb-ext:float-infinity-p n) (if (plusp n) "Infinity" "-Infinity"))
                   ((sb-ext:float-nan-p n) "NaN")
                   (t (let ((s (format nil "~F" (coerce n 'double-float))))
                        ;; "1.0" -> "1.0" is fine JS; strip exponent markers like d0
                        (substitute #\e #\d s))))))))

(defun paren (s) (format nil "(~A)" s))

(defun strip-parens (s)
  "Remove one pair of redundant outer parentheses from expression text S.  String-literal aware,
and refuses when the inner expression has a top-level comma (so `(a = 1, b = 2)` stays grouped)."
  (if (and (> (length s) 1) (char= (char s 0) #\() (char= (char s (1- (length s))) #\)))
      (let ((depth 0) (i 0) (n (length s)) (in-string nil) (ok t))
        (loop while (< i n)
              do (let ((c (char s i)))
                   (cond (in-string
                          (cond ((char= c #\\) (incf i))
                                ((char= c in-string) (setf in-string nil))))
                         ((member c '(#\" #\' #\`)) (setf in-string c))
                         ((member c '(#\( #\[ #\{)) (incf depth))
                         ((member c '(#\) #\] #\}))
                          (decf depth)
                          (when (and (zerop depth) (< i (1- n)))
                            (setf ok nil) (return)))
                         ((and (char= c #\,) (= depth 1))
                          (setf ok nil) (return))))
                 (incf i))
        (if ok (subseq s 1 (1- (length s))) s))
      s))

(defun strip-object-parens (s)
  "`({ ... })` -> `{ ... }` (safe inside argument lists); anything else unchanged."
  (if (and (> (length s) 3) (string= "({" s :end2 2))
      (let ((inner (strip-parens s)))
        (if (and (string/= inner s) (char= (char inner 0) #\{)) inner s))
      s))

(defun join (strings &optional (sep ", "))
  (format nil (concatenate 'string "~{~A~^" sep "~}") strings))

(defun simple-expression-p (s)
  "True when S is an identifier, member chain, or numeric/string literal (needs no parens)."
  (or (every (lambda (c) (or (alphanumericp c) (member c '(#\_ #\$ #\.)))) s)
      (and (> (length s) 1) (char= (char s 0) #\") (char= (char s (1- (length s))) #\")
           (not (find #\" s :start 1 :end (1- (length s)))))))
