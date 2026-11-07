;;;; src/transpiler/macros.lisp -- user macros (expanded in the host Lisp) and inline aliases

(in-package :monolisp.transpiler)

;;; ------------------------------------------------------------------ user macros

(defparameter *forbidden-macro-operators*
  '("EVAL" "COMPILE" "COMPILE-FILE" "LOAD" "REQUIRE" "PROVIDE"
    "OPEN" "WITH-OPEN-FILE" "WITH-OPEN-STREAM" "DELETE-FILE" "RENAME-FILE" "PROBE-FILE" "DIRECTORY"
    "ENSURE-DIRECTORIES-EXIST" "FILE-AUTHOR" "FILE-WRITE-DATE" "DRIBBLE" "ED" "INSPECT"
    "FIND-SYMBOL" "SYMBOL-FUNCTION" "FDEFINITION" "FUNCALL" "APPLY" "MULTIPLE-VALUE-CALL" "COMPILER-MACRO-FUNCTION"
    "MACRO-FUNCTION" "FIND-PACKAGE" "MAKE-PACKAGE" "DELETE-PACKAGE" "IN-PACKAGE" "USE-PACKAGE" "IMPORT" "EXPORT" "SHADOW"
    "SET" "SETF" "PSETF" "DEFVAR" "DEFPARAMETER" "DEFCONSTANT" "DEFUN" "DEFMACRO" "DEFINE-SYMBOL-MACRO" "DEFSETF"
    "DEFINE-COMPILER-MACRO" "DEFSTRUCT" "DEFCLASS" "DEFGENERIC" "DEFMETHOD" "MAKE-INSTANCE" "DEFINE-CONDITION"
    "READ" "READ-PRESERVING-WHITESPACE" "READ-FROM-STRING" "READ-LINE" "READ-CHAR" "READ-BYTE"
    "WRITE" "PRINT" "PRINC" "PRIN1" "WRITE-STRING" "WRITE-LINE" "WRITE-CHAR" "WRITE-BYTE" "TERPRI"
    "RPLACA" "RPLACD" "SLEEP" "ROOM" "GC" "SET-MACRO-CHARACTER" "SET-DISPATCH-MACRO-CHARACTER" "COPY-READTABLE" "INVOKE-DEBUGGER" "BREAK")
  "CL operators a frontend macro body may not use.  Frontend macros are pure syntax rewriters:
they take forms and return forms.  Anything that reaches the host image -- files, packages,
the reader/printer, global definitions, dynamic function lookup -- is refused up front, and the
expansion is re-checked for package-qualified symbols.  (This is a safety net for mistakes and
copy-pasted code, not a sandbox: do not transpile source you would not be willing to LOAD.)")

(defun check-macro-body (form body)
  "Refuse forbidden operators in the code a macro runs at build time.  The literal part of a
backquote template is only data that becomes frontend code, so `(setf ,place 1) is allowed;
everything under a comma runs in the host and is checked."
  (labels ((walk (x)
             (cond ((and (symbolp x) (not (keywordp x))
                         (eq (symbol-package x) (find-package :cl))
                         (member (symbol-name x) *forbidden-macro-operators* :test #'string=))
                    (tfail form "defmacro: ~A may not be used inside a frontend macro body" (string-downcase x)))
                   ((and (consp x) (eq (car x) 'sb-int:quasiquote)) (template (cdr x)))
                   ((consp x) (walk (car x)) (walk (cdr x)))
                   ((and (vectorp x) (not (stringp x))) (map nil #'walk x))
                   ((typep x 'sb-impl::comma) (walk (sb-impl::comma-expr x)))))
           (template (x)
             (cond ((typep x 'sb-impl::comma) (walk (sb-impl::comma-expr x)))
                   ((consp x) (template (car x)) (template (cdr x)))
                   ((and (vectorp x) (not (stringp x))) (map nil #'template x)))))
    (walk body)))

(defun define-client-macro (form module)
  "(defmacro name lambda-list body...) -- evaluated in the module's package at build time."
  (destructuring-bind (name lambda-list &rest body) (cdr form)
    (unless (and (symbolp name) (not (keywordp name)))
      (tfail form "defmacro: bad macro name"))
    (when (eq (symbol-package name) (find-package :cl))
      (tfail form "defmacro: cannot redefine the standard operator ~A" (string-downcase name)))
    (check-macro-body form body)
    (let ((*package* (module-package module))
          (*read-eval* nil))
      (handler-case (eval `(defmacro ,name ,lambda-list ,@body))
        (error (e) (tfail form "defmacro ~A failed: ~A" (string-downcase name) e))))
    (pushnew name (module-macros module))
    name))

(defun user-macro-p (op module)
  (and (symbolp op) (not (keywordp op)) module
       (member op (module-macros module))
       (macro-function op)))

(defun expand-user-macro (form module)
  (let ((*package* (module-package module)) (*read-eval* nil))
    (let ((expansion (handler-case (macroexpand-1 form)
                       (error (e) (tfail form "macro expansion of ~A failed: ~A" (string-downcase (car form)) e)))))
      ;; expansions are source too: no host-package symbols, no shared structure
      (check-form-symbols expansion (module-package module) *current-file* :allow-sharing t)
      expansion)))

;;; ------------------------------------------------------------------ inline aliases
;;;
;;; (declaim (inline console-log))
;;; (defun console-log (&rest args) (console.log (spread args)))
;;; (console-log "hi" x)  ==>  console.log("hi", x)
;;;
;;; Call sites are expanded by substitution (like a macro), so an alias costs nothing at runtime.
;;; A parameter used more than once with a non-trivial argument is bound with LET to preserve
;;; single evaluation.

(defun parse-inline-lambda-list (lambda-list)
  "Returns (values required optional rest) ; optional entries are (sym default)."
  (let ((required '()) (optional '()) (rest nil) (mode :required))
    (dolist (item lambda-list)
      (cond ((sym-is item "&OPTIONAL") (setf mode :optional))
            ((sym-is item "&REST") (setf mode :rest))
            ((sym-is item "&KEY") (tfail lambda-list "inline functions cannot take &key parameters"))
            (t (ecase mode
                 (:required (push item required))
                 ;; A missing optional becomes `undefined` (not null) so JS defaults still apply.
                 (:optional (push (if (consp item) (list (first item) (second item)) (list item :undefined)) optional))
                 (:rest (setf rest item))))))
    (values (nreverse required) (nreverse optional) rest)))

(defun count-occurrences (symbol form)
  (cond ((eq form symbol) 1)
        ((consp form) (+ (count-occurrences symbol (car form)) (count-occurrences symbol (cdr form))))
        (t 0)))

(defun trivial-arg-p (arg)
  (or (symbolp arg) (numberp arg) (stringp arg) (characterp arg)
      (and (consp arg) (sym-is (car arg) "QUOTE"))))

(defun substitute-spread (rest-symbol args form)
  "Replace (spread REST) with the literal ARGS and bare REST with (vector ARGS...)."
  (cond ((eq form rest-symbol) (cons (intern "VECTOR" :cl) args))
        ((consp form)
         (if (and (consp (car form)) (sym-is (caar form) "SPREAD") (eq (second (car form)) rest-symbol))
             (append args (substitute-spread rest-symbol args (cdr form)))
             (cons (substitute-spread rest-symbol args (car form))
                   (substitute-spread rest-symbol args (cdr form)))))
        (t form)))

(defun substitute-inline-params (subst form)
  "SUBLIS for inline bodies that leaves `..` property and method names alone: in
(.. el value (setAttribute name value)) only EL and the call arguments are variable references."
  (cond ((symbolp form) (let ((hit (assoc form subst))) (if hit (cdr hit) form)))
        ((atom form) form)
        ((sym-is (car form) "..")
         (list* (car form)
                (substitute-inline-params subst (second form))
                (mapcar (lambda (step)
                          (cond ((atom step) step)
                                ((and (eq (car step) :?) (consp (second step)))
                                 (list :? (cons (car (second step)) (substitute-inline-params subst (cdr (second step))))))
                                ((eq (car step) :?) step)
                                (t (cons (car step) (substitute-inline-params subst (cdr step))))))
                        (cddr form))))
        (t (cons (substitute-inline-params subst (car form)) (substitute-inline-params subst (cdr form))))))

(defun expand-inline (form definition)
  "DEFINITION is (lambda-list . body).  Returns the substituted body as a single form."
  (destructuring-bind (lambda-list . body) definition
    (multiple-value-bind (required optional rest) (parse-inline-lambda-list lambda-list)
      (let ((args (cdr form)))
        (when (< (length args) (length required))
          (tfail form "~A expects at least ~D argument~:P" (string-downcase (car form)) (length required)))
        (when (and (not rest) (> (length args) (+ (length required) (length optional))))
          (tfail form "~A expects at most ~D argument~:P" (string-downcase (car form)) (+ (length required) (length optional))))
        (multiple-value-bind (decls doc forms) (split-body-declarations body)
          (declare (ignore decls doc))
          (let* ((bindings '())
                 (subst '())
                 (remaining args))
            (dolist (p required)
              (let ((a (pop remaining)))
                (if (or (trivial-arg-p a) (<= (count-occurrences p forms) 1))
                    (push (cons p a) subst)
                    (let ((g (gensym (symbol-name p))))
                      (push (list g a) bindings)
                      (push (cons p g) subst)))))
            (dolist (o optional)
              (let ((a (if remaining (pop remaining) (second o))))
                (if (or (trivial-arg-p a) (<= (count-occurrences (first o) forms) 1))
                    (push (cons (first o) a) subst)
                    (let ((g (gensym (symbol-name (first o)))))
                      (push (list g a) bindings)
                      (push (cons (first o) g) subst)))))
            (let* ((body1 (if rest (substitute-spread rest remaining forms) forms))
                   (body2 (substitute-inline-params subst body1))
                   (result (if (= (length body2) 1) (first body2) (cons (intern "PROGN" :cl) body2))))
              (if bindings
                  (list (intern "LET*" :cl) (nreverse bindings) result)
                  result))))))))
