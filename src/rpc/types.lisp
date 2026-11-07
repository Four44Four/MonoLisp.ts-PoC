;;;; src/rpc/types.lisp -- the shared type-schema language
;;;;
;;;; A schema is an s-expression understood by three consumers:
;;;;   1. the JSON-RPC boundary (VALIDATE-VALUE: untrusted JSON -> typed Lisp, ENCODE-VALUE: Lisp -> JSON)
;;;;   2. the transpiler (SCHEMA->TS: the exact TypeScript type the frontend must satisfy)
;;;;   3. the manifest (SCHEMA->JSON) consumed by build tooling
;;;;
;;;; Grammar:
;;;;   string | (string MAX-LENGTH) | integer | fixnum | (integer LOW HIGH) | number | float | boolean | null | any
;;;;   (or S...) | (member LITERAL...) | (array S) | (array S MAX-ITEMS) | (tuple S...) | (map S)
;;;;   (object (KEY S [:optional]) ...) | (nullable S) | STRUCT-NAME (defined by DEFINE-RPC-STRUCT)

(in-package :monolisp.rpc)

;;; ------------------------------------------------------------------ naming

(defun lisp-name->camel (name)
  "hello-world -> helloWorld, *foo-bar* -> FOO_BAR, is-ok-p -> isOkP"
  (let* ((s (string name))
         (s (if (and (> (length s) 2) (char= (char s 0) #\*) (char= (char s (1- (length s))) #\*))
                (return-from lisp-name->camel (substitute #\_ #\- (string-upcase (subseq s 1 (1- (length s))))))
                s))
         (s (if (and (> (length s) 2) (char= (char s 0) #\+) (char= (char s (1- (length s))) #\+))
                (return-from lisp-name->camel (substitute #\_ #\- (string-upcase (subseq s 1 (1- (length s))))))
                s))
         (lower (if (some #'lower-case-p s) s (string-downcase s))))
    (with-output-to-string (out)
      (let ((upcase-next nil))
        (loop for ch across lower
              do (cond ((char= ch #\-) (setf upcase-next t))
                       ((or (alphanumericp ch) (char= ch #\_) (char= ch #\$))
                        (write-char (if upcase-next (char-upcase ch) ch) out)
                        (setf upcase-next nil))
                       ((char= ch #\?) (write-char #\P out))
                       ((char= ch #\!) (write-string "Bang" out))
                       (t (write-char #\_ out))))))))

(defun lisp-name->pascal (name)
  (let ((camel (lisp-name->camel name)))
    (if (zerop (length camel)) camel
        (concatenate 'string (string (char-upcase (char camel 0))) (subseq camel 1)))))

(defun camel->lisp-name (camel)
  "helloWorld -> \"hello-world\""
  (with-output-to-string (out)
    (loop for ch across camel
          for i from 0
          do (cond ((and (upper-case-p ch) (plusp i)) (write-char #\- out) (write-char (char-downcase ch) out))
                   (t (write-char (char-downcase ch) out))))))

;;; ------------------------------------------------------------------ conditions

(define-condition schema-error (error)
  ((path :initarg :path :initform '() :reader schema-error-path)
   (expected :initarg :expected :reader schema-error-expected))
  (:report (lambda (c s)
             (format s "~A: expected ~A"
                     (if (schema-error-path c) (format-path (schema-error-path c)) "value")
                     (schema-error-expected c)))))

(defun format-path (path)
  "Render a reversed path list as $.a[0].b -- contains only our own names/indices, never payload data."
  (with-output-to-string (s)
    (write-string "$" s)
    (dolist (step (reverse path))
      (if (integerp step)
          (format s "[~D]" step)
          (format s ".~A" step)))))

(defun schema-fail (path expected)
  (error 'schema-error :path path :expected expected))

;;; ------------------------------------------------------------------ structs

(defstruct (rpc-struct-field (:conc-name field-))
  name json-name schema optional-p default)

(defstruct rpc-struct
  name ts-name constructor predicate fields)

(defvar *rpc-structs* (make-hash-table :test 'eq))

(defun find-rpc-struct (name) (gethash name *rpc-structs*))
(defun list-rpc-structs () (loop for s being the hash-values of *rpc-structs* collect s))
(defun clear-rpc-structs () (clrhash *rpc-structs*))

(defmacro define-rpc-struct (name &body field-specs)
  "Define a Lisp struct that can cross the RPC boundary.  Each field: (FIELD-NAME SCHEMA &key optional default)."
  (let* ((specs (mapcar (lambda (spec)
                          (destructuring-bind (fname schema &key optional default) spec
                            (list fname schema optional default)))
                        field-specs))
         (constructor (intern (format nil "MAKE-~A" (string name))))
         (predicate (intern (format nil "~A-P" (string name)))))
    `(progn
       (defstruct ,name
         ,@(mapcar (lambda (spec) (list (first spec) (fourth spec))) specs))
       (setf (gethash ',name *rpc-structs*)
             (make-rpc-struct
              :name ',name
              :ts-name ,(lisp-name->pascal name)
              :constructor ',constructor
              :predicate ',predicate
              :fields (list ,@(mapcar (lambda (spec)
                                        `(make-rpc-struct-field
                                          :name ',(first spec)
                                          :json-name ,(lisp-name->camel (first spec))
                                          :schema ',(second spec)
                                          :optional-p ,(third spec)
                                          :default ',(fourth spec)))
                                      specs))))
       ',name)))

;;; ------------------------------------------------------------------ normalisation / checking

(defparameter +js-max-safe-integer+ (1- (expt 2 53)))

(defun normalize-schema (schema)
  (cond ((member schema '(t any :any)) 'any)
        ((eq schema 'nil) 'null)
        ((and (symbolp schema) (not (keywordp schema))
              (member (symbol-name schema) '("STRING" "INTEGER" "FIXNUM" "NUMBER" "FLOAT" "BOOLEAN" "NULL" "ANY" "DOUBLE-FLOAT" "SINGLE-FLOAT")
                      :test #'string=))
         (let ((n (symbol-name schema)))
           (cond ((member n '("DOUBLE-FLOAT" "SINGLE-FLOAT") :test #'string=) 'float)
                 (t (intern n :monolisp.rpc)))))
        ((and (consp schema) (symbolp (car schema)))
         (let ((op (intern (symbol-name (car schema)) :monolisp.rpc)))
           (case op
             (nullable `(or ,(normalize-schema (second schema)) null))
             ((or and-not) (cons 'or (mapcar #'normalize-schema (cdr schema))))
             (member schema)
             ((array vector) (if (cdr schema)
                                 `(array ,(normalize-schema (second schema)) ,@(cddr schema))
                                 '(array)))
             (tuple (cons 'tuple (mapcar #'normalize-schema (cdr schema))))
             (map `(map ,(normalize-schema (second schema))))
             (object (cons 'object (mapcar (lambda (f) (list* (first f) (normalize-schema (second f)) (cddr f))) (cdr schema))))
             (string schema)
             (integer schema)
             (t schema))))
        (t schema)))

(defun schema-valid-p (schema)
  "True if SCHEMA is well formed and all struct references resolve."
  (let ((s (normalize-schema schema)))
    (cond ((member s '(string integer fixnum number float boolean null any)) t)
          ((and (symbolp s) (find-rpc-struct s)) t)
          ((consp s)
           (case (car s)
             (string (and (<= (length s) 2) (or (null (cdr s)) (typep (second s) '(integer 0)))))
             (integer (and (<= (length s) 3) (every (lambda (x) (or (eq x '*) (integerp x))) (cdr s))))
             (or (and (cdr s) (every #'schema-valid-p (cdr s))))
             (member (and (cdr s) (every (lambda (x) (or (stringp x) (integerp x) (keywordp x))) (cdr s))))
             (array (and (cdr s) (schema-valid-p (second s)) (or (null (cddr s)) (typep (third s) '(integer 0)))))
             (tuple (every #'schema-valid-p (cdr s)))
             (map (and (= (length s) 2) (schema-valid-p (second s))))
             (object (every (lambda (f) (and (consp f) (symbolp (first f)) (schema-valid-p (second f)))) (cdr s)))
             (t nil)))
          (t nil))))

;;; ------------------------------------------------------------------ validation: JSON -> Lisp

(defun validate-value (schema value &optional path)
  "Check untrusted JSON VALUE against SCHEMA; return the typed Lisp value or signal SCHEMA-ERROR."
  (let ((s (normalize-schema schema)))
    (cond
      ((eq s 'any) value)
      ((eq s 'string) (if (stringp value) value (schema-fail path "string")))
      ((eq s 'integer)
       (if (integerp value) value (schema-fail path "integer")))
      ((eq s 'fixnum)
       (if (and (integerp value) (<= (abs value) +js-max-safe-integer+))
           value
           (schema-fail path "safe integer")))
      ((member s '(number float))
       (cond ((integerp value) (if (eq s 'float) (coerce value 'double-float) value))
             ((floatp value) value)
             (t (schema-fail path "number"))))
      ((eq s 'boolean)
       (case value (:true t) (:false nil) (t (schema-fail path "boolean"))))
      ((eq s 'null)
       (if (eq value :null) nil (schema-fail path "null")))
      ((symbolp s)
       (let ((struct (find-rpc-struct s)))
         (unless struct (schema-fail path (format nil "known type (unknown schema ~A)" s)))
         (validate-struct struct value path)))
      ((consp s)
       (case (car s)
         (string
          (unless (stringp value) (schema-fail path "string"))
          (when (and (second s) (> (length value) (second s)))
            (schema-fail path (format nil "string of at most ~D characters" (second s))))
          value)
         (integer
          (unless (integerp value) (schema-fail path "integer"))
          (destructuring-bind (&optional (lo '*) (hi '*)) (cdr s)
            (when (or (and (integerp lo) (< value lo)) (and (integerp hi) (> value hi)))
              (schema-fail path (format nil "integer in [~A, ~A]" lo hi))))
          value)
         (or
          (dolist (alt (cdr s) (schema-fail path (format nil "one of ~{~A~^ | ~}" (mapcar #'schema->ts (cdr s)))))
            (handler-case (return (validate-value alt value path))
              (schema-error () nil))))
         (member
          (let ((lisp-value (cond ((stringp value) value)
                                  ((integerp value) value)
                                  (t nil))))
            (or (find-if (lambda (lit)
                           (cond ((stringp lit) (and (stringp lisp-value) (string= lit lisp-value)))
                                 ((integerp lit) (eql lit lisp-value))
                                 ((keywordp lit) (and (stringp lisp-value) (string= (string-downcase (symbol-name lit)) lisp-value)))))
                         (cdr s))
                (schema-fail path (format nil "one of ~{~S~^, ~}" (mapcar (lambda (x) (if (keywordp x) (string-downcase (symbol-name x)) x)) (cdr s)))))))
         (array
          (unless (simple-vector-p value) (schema-fail path "array"))
          (when (and (third s) (> (length value) (third s)))
            (schema-fail path (format nil "array of at most ~D items" (third s))))
          (let ((out (make-array (length value))))
            (loop for item across value
                  for i from 0
                  do (setf (svref out i) (validate-value (second s) item (cons i path))))
            out))
         (tuple
          (unless (and (simple-vector-p value) (= (length value) (length (cdr s))))
            (schema-fail path (format nil "tuple of ~D items" (length (cdr s)))))
          (let ((out (make-array (length value))))
            (loop for item across value
                  for sub in (cdr s)
                  for i from 0
                  do (setf (svref out i) (validate-value sub item (cons i path))))
            out))
         (map
          (unless (json-object-p value) (schema-fail path "object"))
          (let ((out (%make-ordered-object)))
            (dolist (key (json-keys value) out)
              (json-set out key (validate-value (second s) (json-get value key) (cons key path))))))
         (object
          (unless (json-object-p value) (schema-fail path "object"))
          (let ((out (%make-ordered-object))
                (allowed '()))
            (dolist (field (cdr s))
              (destructuring-bind (fname fschema &rest opts) field
                (let ((json-name (lisp-name->camel fname)))
                  (push json-name allowed)
                  (multiple-value-bind (v present) (json-get value json-name)
                    (cond (present (json-set out json-name (validate-value fschema v (cons json-name path))))
                          ((member :optional opts))
                          (t (schema-fail (cons json-name path) "required field")))))))
            (dolist (key (json-keys value))
              (unless (member key allowed :test #'string=)
                (schema-fail (cons key path) "no such field")))
            out))
         (t (schema-fail path (format nil "known type (unknown schema ~A)" (car s))))))
      (t (schema-fail path "known type")))))

(defun %make-ordered-object () (make-json-object))

(defun validate-struct (struct value path)
  (unless (json-object-p value) (schema-fail path (format nil "object (~A)" (rpc-struct-ts-name struct))))
  (let ((args '()) (allowed '()))
    (dolist (field (rpc-struct-fields struct))
      (push (field-json-name field) allowed)
      (multiple-value-bind (v present) (json-get value (field-json-name field))
        (cond (present
               (push (intern (symbol-name (field-name field)) :keyword) args)
               (push (validate-value (field-schema field) v (cons (field-json-name field) path)) args))
              ((field-optional-p field))
              (t (schema-fail (cons (field-json-name field) path) "required field")))))
    (dolist (key (json-keys value))
      (unless (member key allowed :test #'string=)
        (schema-fail (cons key path) "no such field")))
    (apply (rpc-struct-constructor struct) (nreverse args))))

;;; ------------------------------------------------------------------ encoding: Lisp -> JSON

(defun encode-value (schema value &optional path)
  "Convert a Lisp VALUE produced by server code into a JSON value according to SCHEMA.
Mismatches signal SCHEMA-ERROR (a server-side bug, reported as internal error to clients)."
  (let ((s (normalize-schema schema)))
    (cond
      ((eq s 'any) (encode-any value))
      ((eq s 'string) (if (stringp value) value (schema-fail path "string result")))
      ((member s '(integer fixnum))
       (if (integerp value) value (schema-fail path "integer result")))
      ((member s '(number float))
       (cond ((integerp value) value)
             ((realp value) (coerce value 'double-float))
             (t (schema-fail path "number result"))))
      ((eq s 'boolean) (if value :true :false))
      ((eq s 'null) (if (null value) :null (schema-fail path "null result")))
      ((symbolp s)
       (let ((struct (find-rpc-struct s)))
         (unless struct (schema-fail path "known type"))
         (unless (funcall (rpc-struct-predicate struct) value)
           (schema-fail path (format nil "~A instance" (rpc-struct-ts-name struct))))
         (let ((obj (make-json-object)))
           (dolist (field (rpc-struct-fields struct) obj)
             (let ((v (funcall (intern (format nil "~A-~A" (rpc-struct-name struct) (field-name field))
                                       (symbol-package (rpc-struct-name struct)))
                               value)))
               (unless (and (field-optional-p field) (null v))
                 (json-set obj (field-json-name field)
                           (encode-value (field-schema field) v (cons (field-json-name field) path)))))))))
      ((consp s)
       (case (car s)
         (string (if (stringp value) value (schema-fail path "string result")))
         (integer (if (integerp value) value (schema-fail path "integer result")))
         (or
          (dolist (alt (cdr s) (schema-fail path "value matching union"))
            (handler-case (return (encode-value alt value path))
              (schema-error () nil))))
         (member
          (cond ((stringp value) value)
                ((integerp value) value)
                ((keywordp value) (string-downcase (symbol-name value)))
                (t (schema-fail path "literal result"))))
         (array
          (let ((items (etypecase value (list value) (vector (coerce value 'list)))))
            (coerce (loop for item in items for i from 0
                          collect (encode-value (second s) item (cons i path)))
                    'simple-vector)))
         (tuple
          (let ((items (etypecase value (list value) (vector (coerce value 'list)))))
            (unless (= (length items) (length (cdr s))) (schema-fail path "tuple result"))
            (coerce (loop for item in items for sub in (cdr s) for i from 0
                          collect (encode-value sub item (cons i path)))
                    'simple-vector)))
         (map
          (let ((obj (make-json-object)))
            (etypecase value
              (json-object (dolist (k (json-keys value) obj)
                             (json-set obj k (encode-value (second s) (json-get value k) (cons k path)))))
              (hash-table (maphash (lambda (k v) (json-set obj (string k) (encode-value (second s) v (cons k path)))) value) obj)
              (list (loop for (k . v) in value do (json-set obj (string k) (encode-value (second s) v (cons k path)))) obj))))
         (object
          (let ((obj (make-json-object)))
            (dolist (field (cdr s) obj)
              (destructuring-bind (fname fschema &rest opts) field
                (let* ((json-name (lisp-name->camel fname))
                       (v (etypecase value
                            (json-object (json-get value json-name))
                            (hash-table (or (gethash json-name value) (gethash fname value)))
                            ;; (:total 3 :done 1) plist or ((total . 3) (done . 1)) alist
                            (list (if (keywordp (first value))
                                      (getf value (intern (symbol-name fname) :keyword))
                                      (cdr (assoc fname (remove-if-not #'consp value))))))))
                  (unless (and (null v) (member :optional opts))
                    (json-set obj json-name (encode-value fschema v (cons json-name path)))))))))
         (t (schema-fail path "known type"))))
      (t (schema-fail path "known type")))))

(defun encode-any (value)
  "Best-effort conversion of arbitrary Lisp data for an `any` schema."
  (typecase value
    ((or string integer json-object) value)
    (real (coerce value 'double-float))
    (null :null)
    ((eql t) :true)
    (keyword (case value ((:true :false :null) value) (t (string-downcase (symbol-name value)))))
    (vector (map 'simple-vector #'encode-any value))
    (cons (map 'simple-vector #'encode-any value))
    (hash-table (let ((obj (make-json-object))) (maphash (lambda (k v) (json-set obj (string k) (encode-any v))) value) obj))
    (t (let ((struct (find-if (lambda (s) (funcall (rpc-struct-predicate s) value)) (list-rpc-structs))))
         (if struct
             (encode-value (rpc-struct-name struct) value)
             (schema-fail nil (format nil "encodable value (got ~A)" (type-of value))))))))

;;; ------------------------------------------------------------------ TypeScript / JSON renderings

(defun schema->ts (schema)
  "TypeScript type text for SCHEMA."
  (let ((s (normalize-schema schema)))
    (cond
      ((eq s 'any) "unknown")
      ((eq s 'string) "string")
      ((member s '(integer fixnum number float)) "number")
      ((eq s 'boolean) "boolean")
      ((eq s 'null) "null")
      ((symbolp s)
       (let ((struct (find-rpc-struct s)))
         (if struct (rpc-struct-ts-name struct) (error "schema->ts: unknown schema ~S" s))))
      ((consp s)
       (case (car s)
         (string "string")
         (integer "number")
         (or (format nil "~{~A~^ | ~}" (mapcar (lambda (alt) (wrap-union-member (schema->ts alt))) (cdr s))))
         (member (format nil "~{~A~^ | ~}"
                         (mapcar (lambda (lit) (cond ((stringp lit) (ts-string-literal lit))
                                                     ((keywordp lit) (ts-string-literal (string-downcase (symbol-name lit))))
                                                     (t (format nil "~D" lit))))
                                 (cdr s))))
         (array (let ((inner (schema->ts (second s))))
                  (if (or (find #\| inner) (find #\Space inner))
                      (format nil "(~A)[]" inner)
                      (format nil "~A[]" inner))))
         (tuple (format nil "[~{~A~^, ~}]" (mapcar #'schema->ts (cdr s))))
         (map (format nil "Record<string, ~A>" (schema->ts (second s))))
         (object (format nil "{ ~{~A~^ ~} }"
                         (mapcar (lambda (f)
                                   (destructuring-bind (fname fschema &rest opts) f
                                     (format nil "~A~:[~;?~]: ~A;" (lisp-name->camel fname) (member :optional opts) (schema->ts fschema))))
                                 (cdr s))))
         (t (error "schema->ts: unknown schema ~S" s))))
      (t (error "schema->ts: unknown schema ~S" s)))))

(defun wrap-union-member (ts)
  (if (search " => " ts) (format nil "(~A)" ts) ts))

(defun ts-string-literal (string)
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for ch across string
          do (case ch
               (#\" (write-string "\\\"" s))
               (#\\ (write-string "\\\\" s))
               (#\Newline (write-string "\\n" s))
               (#\Return (write-string "\\r" s))
               (t (if (or (< (char-code ch) 32) (= (char-code ch) #x2028) (= (char-code ch) #x2029))
                      (format s "\\u~4,'0X" (char-code ch))
                      (write-char ch s)))))
    (write-char #\" s)))

(defun schema->json (schema)
  "JSON-friendly rendering of SCHEMA (strings and arrays) for the manifest."
  (let ((s (normalize-schema schema)))
    (cond ((symbolp s)
           (let ((struct (find-rpc-struct s)))
             (if struct (format nil "$~A" (rpc-struct-ts-name struct)) (string-downcase (symbol-name s)))))
          ((consp s)
           (coerce (cons (string-downcase (symbol-name (car s)))
                         (case (car s)
                           ((string integer array) (cons (if (symbolp (second s)) (schema->json (second s)) (second s))
                                                         (mapcar (lambda (x) (if (eq x '*) :null x)) (cddr s))))
                           (member (mapcar (lambda (x) (if (keywordp x) (string-downcase (symbol-name x)) x)) (cdr s)))
                           (object (mapcar (lambda (f) (coerce (list (lisp-name->camel (first f)) (schema->json (second f))
                                                                     (if (getf (cddr f) :optional) :true :false))
                                                               'simple-vector))
                                           (cdr s)))
                           (t (mapcar #'schema->json (cdr s)))))
                   'simple-vector))
          (t (princ-to-string s)))))
