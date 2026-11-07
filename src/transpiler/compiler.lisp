;;;; src/transpiler/compiler.lisp -- forms -> TypeScript
;;;;
;;;; Two compilation modes:
;;;;   (cx form env)        -> expression text
;;;;   (cs form env dest)   -> emits statements; DEST is :void, :return or (:set . "jsName")
;;;; Statement-only constructs used in expression position become immediately-invoked arrow
;;;; functions (async ones when they contain AWAIT).

(in-package :monolisp.transpiler)

;;; ------------------------------------------------------------------ dispatch tables

(defvar *expr-forms* (make-hash-table :test 'equal))   ; NAME -> (lambda (form env))
(defvar *stmt-forms* (make-hash-table :test 'equal))   ; NAME -> (lambda (form env dest))

(defmacro defexpr (names (form env) &body body)
  (let ((names (if (listp names) names (list names))))
    `(let ((fn (lambda (,form ,env) (declare (ignorable ,form ,env)) ,@body)))
       (dolist (n ',names) (setf (gethash n *expr-forms*) fn)))))

(defmacro defstmt (names (form env dest) &body body)
  (let ((names (if (listp names) names (list names))))
    `(let ((fn (lambda (,form ,env ,dest) (declare (ignorable ,form ,env ,dest)) ,@body)))
       (dolist (n ',names) (setf (gethash n *stmt-forms*) fn)))))

(defparameter *statement-only*
  '("LET" "LET*" "PROGN" "LOCALLY" "LOOP" "DOLIST" "HANDLER-CASE" "UNWIND-PROTECT"
    "IGNORE-ERRORS" "BLOCK" "RETURN-FROM" "RETURN" "CASE" "ECASE" "%WHILE" "%FOR-OF" "%BREAK" "%CONTINUE"
    "DESTRUCTURING-BIND" "FLET" "LABELS" "DEFUN" "DEFVAR"
    "DEFPARAMETER" "DEFCONSTANT" "DEFSTRUCT" "ERROR" "SIGNAL" "PROG1" "TAGBODY" "GO" "DEFMACRO" "DECLAIM"
    "IMPORT" "EXPORT" "EXPORT-DEFAULT" "DEFTYPE" "SETF" "SETQ" "INCF" "DECF"))

;;; ------------------------------------------------------------------ helpers

(defun finish (dest expr)
  "Emit EXPR according to DEST."
  (cond ((eq dest :void)
         (let ((s (strip-parens expr)))
           (unless (or (string= s "null") (string= s "undefined") (string= s "true") (string= s "false")
                       (every (lambda (c) (or (alphanumericp c) (char= c #\_) (char= c #\$))) s))
             ;; an expression statement must not start with { (block) or "function"
             (emit "~A;" (if (or (char= (char s 0) #\{) (and (> (length s) 8) (string= "function" s :end2 8))) expr s)))))
        ((eq dest :return) (emit "return ~A;" (strip-parens expr)))
        ((and (consp dest) (eq (car dest) :set)) (emit "~A = ~A;" (cdr dest) (strip-parens expr)))
        (t (error "bad destination ~S" dest))))

(defun new-scope (env)
  "A new JS block scope: declarations inside it may reuse names from the enclosing block."
  (let ((e (copy-env env)))
    (setf (env-scope e) (list nil))
    e))

(defun contains-await-p (form)
  "Does FORM contain an AWAIT outside any nested lambda/defun?"
  (cond ((atom form) nil)
        ((or (sym-is (car form) "AWAIT") (sym-is (car form) "AWAIT-ALL")) t)
        ((member (sym-name (car form)) '("LAMBDA" "DEFUN" "ASYNC" "FLET" "LABELS") :test #'string=) nil)
        ((sym-is (car form) "QUOTE") nil)
        (t (or (contains-await-p (car form)) (contains-await-p (cdr form))))))

(defun iife (form env)
  "Wrap a statement-only FORM into an immediately invoked arrow function."
  (let* ((async (contains-await-p form))
         (inner (new-scope (copy-env env :async-p (or async (env-async-p env)) :blocks '())))
         (body (capture-statements (with-indent (cs form inner :return)))))
    (if async
        (if (env-async-p env)
            (format nil "(await (async () => {~%~A~A})())" body (indent-string))
            (format nil "(async () => {~%~A~A})()" body (indent-string)))
        (format nil "(() => {~%~A~A})()" body (indent-string)))))

(defun indent-string () (make-string (* 2 *indent*) :initial-element #\Space))

(defun macroexpand-client (form env)
  "Expand user macros / inline aliases at the head of FORM (repeatedly).  Returns the new form."
  (declare (ignorable env))
  (loop
    (if (and (consp form) (symbolp (car form)) (not (keywordp (car form))))
        (let ((op (car form)))
          (cond ((user-macro-p op *module*) (setf form (expand-user-macro form *module*)))
                ((and (not (gethash (symbol-name op) *expr-forms*))
                      (not (gethash (symbol-name op) *stmt-forms*))
                      (not (lookup-var op env))
                      (inline-definition op))
                 (setf form (expand-inline form (inline-definition op))))
                (t (return form))))
        (return form))))

(defun inline-definition (op)
  (or (and *module* (gethash (symbol-name op) (module-inlines *module*)))
      (gethash (symbol-name op) (prelude-inlines))))

(defun statement-form-p (form env)
  "Must FORM be compiled as statements?"
  (let ((form (macroexpand-client form env)))
    (and (consp form)
         (let ((name (sym-name (car form))))
           (cond ((null name) nil)
                 ((member name *statement-only* :test #'string=) t)
                 ((member name '("IF" "WHEN" "UNLESS" "COND") :test #'string=)
                  (some (lambda (f) (statement-form-p f env)) (if (string= name "COND") (apply #'append (mapcar #'cdr (cdr form))) (cddr form))))
                 ((string= name "FOR-EACH") nil)
                 (t nil))))))

(defun compile-args (args env)
  "Argument list text (spread-aware; trailing literal :undefined dropped)."
  (let ((args (let ((a (reverse args)))
                (loop while (and a (eq (car a) :undefined)) do (pop a))
                (nreverse a))))
    (join (mapcar (lambda (a)
                    (if (and (consp a) (sym-is (car a) "SPREAD"))
                        (format nil "...~A" (cx (second a) env))
                        (strip-parens (cx a env))))
                  args))))

(defun self-evaluating-p (form)
  (or (numberp form) (stringp form) (characterp form) (keywordp form) (member form '(t nil))
      (vectorp form)))

;;; ------------------------------------------------------------------ expression compiler

(defun cx (form env)
  (cond
    ((null form) "null")
    ((eq form t) "true")
    ((eq form :undefined) "undefined")
    ((eq form :true) "true")
    ((eq form :false) "false")
    ((eq form :null) "null")
    ((keywordp form) (js-string (format nil ":~A" (property-name form))))
    ((stringp form) (js-string form))
    ((characterp form) (js-string (string form)))
    ((complexp form) (tfail form "complex number literals are not supported on the frontend"))
    ((numberp form) (js-number form))
    ((and (vectorp form) (not (stringp form)))
     (format nil "[~A]" (join (map 'list (lambda (x) (cx (quote-form x) env)) form))))
    ((symbolp form) (compile-variable form env))
    ((consp form)
     (unless (listp (cdr (last form)))
       (tfail form "dotted pairs (cons cells) are not supported on the frontend"))
     (let ((op (car form)))
       (cond
         ((keywordp op)
          (if (env-jsx-p env)
              (compile-jsx form env)
              (tfail form "a keyword in function position is only meaningful inside as-html-string")))
         ((consp op)
          (if (sym-is (car op) "LAMBDA")
              (format nil "~A(~A)" (paren (compile-lambda op env)) (compile-args (cdr form) env))
              (tfail form "illegal function call")))
         ((not (symbolp op)) (tfail form "illegal function call"))
         ((user-macro-p op *module*) (cx (expand-user-macro form *module*) env))
         ((and (gethash (symbol-name op) *expr-forms*) (not (lookup-var op env)))
          (funcall (gethash (symbol-name op) *expr-forms*) form env))
         ((and (gethash (symbol-name op) *stmt-forms*) (not (lookup-var op env)))
          (iife form env))
         ((and (not (lookup-var op env)) (inline-definition op))
          (cx (expand-inline form (inline-definition op)) env))
         ;; struct constructors/accessors win over module globals (they may be imported names)
         ((and (not (assoc op (env-vars env))) (struct-accessor op))
          (compile-struct-accessor form env (struct-accessor op)))
         ((and (not (lookup-var op env)) (manifest-function op))
          (compile-server-call form env (manifest-function op)))
         (t (compile-call form env)))))
    (t (tfail form "cannot compile ~S" form))))

(defun quote-form (x) (list (cl-sym "QUOTE") x))

(defun compile-variable (symbol env)
  (let ((name (symbol-name symbol)))
    (cond ((string= name "PI") "Math.PI")
          ((string= name "THIS") "this")
          ((string= name "UNDEFINED") "undefined")
          (t (or (lookup-var symbol env)
                 (let ((f (manifest-function symbol)))
                   (when f
                     (module-note-server-call *module* (getf f :rpc-name) (getf f :ts-name))
                     (getf f :ts-name)))
                 (mangle-name symbol))))))

(defun compile-call (form env)
  (let* ((op (car form))
         (callee (or (lookup-var op env) (mangle-name op))))
    (format nil "~A(~A)" callee (compile-args (keyword-args-as-object form env) env))))

(defun keyword-args-as-object (form env)
  "Arguments of FORM, with trailing :key value pairs folded into the options object that the
compiled signature of a same-module &key function destructures: (f 1 :b 2) -> f(1, { b: 2 })."
  (let* ((op (car form))
         (args (cdr form))
         (lambda-list (and *module* (not (assoc op (env-vars env))) (not (assoc op (env-labels env)))
                           (gethash op (module-lambda-lists *module*))))
         (key-pos (position-if (lambda (p) (sym-is p "&KEY")) lambda-list)))
    (if (or (null key-pos) (find-if (lambda (p) (sym-is p "&REST")) lambda-list))
        args
        (let* ((before (subseq lambda-list 0 key-pos))
               (opt-pos (or (position-if (lambda (p) (sym-is p "&OPTIONAL")) before) (length before)))
               (required (length (subseq before 0 opt-pos)))
               (positional (+ required (max 0 (- (length before) opt-pos 1))))
               (key-names (mapcar (lambda (p) (symbol-name (cond ((symbolp p) p)
                                                                  ((consp (first p)) (second (first p)))
                                                                  (t (first p)))))
                                  (remove-if (lambda (p) (and (symbolp p) (char= (char (symbol-name p) 0) #\&)))
                                             (subseq lambda-list (1+ key-pos)))))
               (split (loop for a in (nthcdr required args)
                            for i from required
                            when (and (keywordp a) (member (symbol-name a) key-names :test #'string=)) return i)))
          (if (null split)
              args
              (let ((pairs (nthcdr split args)))
                (unless (and (evenp (length pairs))
                             (loop for (k) on pairs by #'cddr
                                   always (and (keywordp k) (member (symbol-name k) key-names :test #'string=))))
                  (tfail form "~A: keyword arguments must be :name value pairs naming its &key parameters (~{~(~A~)~^ ~})"
                         (string-downcase op) key-names))
                (append (subseq args 0 split)
                        (make-list (max 0 (- positional split)) :initial-element :undefined)
                        (list (cons (lang-sym "%OBJECT") pairs)))))))))

;;; ------------------------------------------------------------------ statement compiler

(defun cs (form env dest)
  (let ((form (if (and (consp form) (symbolp (car form)) (user-macro-p (car form) *module*))
                  (expand-user-macro form *module*)
                  form)))
    (cond
      ((and (consp form) (symbolp (car form)) (not (keywordp (car form)))
            (not (lookup-var (car form) env))
            (gethash (symbol-name (car form)) *stmt-forms*))
       (funcall (gethash (symbol-name (car form)) *stmt-forms*) form env dest))
      ((and (consp form) (symbolp (car form)) (not (keywordp (car form)))
            (not (lookup-var (car form) env))
            (not (gethash (symbol-name (car form)) *expr-forms*))
            (inline-definition (car form)))
       (cs (expand-inline form (inline-definition (car form))) env dest))
      (t (finish dest (cx form env))))))

(defun cbody (forms env dest)
  "Compile a body: all but the last form for effect."
  (multiple-value-bind (decls doc forms) (split-body-declarations forms)
    (declare (ignore decls doc))
    (cond ((null forms) (unless (eq dest :void) (finish dest "null")))
          (t (loop for (f . more) on forms
                   do (cs f env (if more :void dest))
                      ;; nothing after an unconditional jump is reachable
                      (when (and more (consp f) (member (sym-name (car f)) '("RETURN-FROM" "RETURN" "%BREAK" "%CONTINUE" "ERROR") :test #'string=))
                        (return)))))))

(defun compile-block-body (forms env dest)
  "Statements text for FORMS in a fresh scope, indented one level."
  (capture-statements (with-indent (cbody forms (new-scope env) dest))))

;;; ------------------------------------------------------------------ literals & data

(defexpr "QUOTE" (form env)
  (let ((x (second form)))
    (cond ((null x) "null")
          ((eq x t) "true")
          ((keywordp x) (js-string (format nil ":~A" (property-name x))))
          ((symbolp x) (js-string (string-downcase (symbol-name x))))
          ;; '{}(:a 1 :b x) -- the spec's object-literal syntax; values are evaluated
          ((and (consp x) (sym-is (car x) "%OBJECT")) (compile-object-literal x env))
          ((and (consp x) (sym-is (car x) "QUOTE")) (cx x env))
          ((and (consp x) (not (listp (cdr (last x)))))
           (tfail form "dotted pairs (cons cells) are not supported on the frontend"))
          ((consp x) (format nil "[~A]" (join (mapcar (lambda (e) (cx (quote-form e) env)) x))))
          ((and (vectorp x) (not (stringp x)))
           (format nil "[~A]" (join (map 'list (lambda (e) (cx (quote-form e) env)) x))))
          (t (cx x env)))))

(defun compile-object-literal (form env &optional quoted)
  "(%object :key value ...) -> ({key: value, ...})"
  (let ((plist (cdr form)) (parts '()))
    (loop while plist
          do (let ((key (pop plist)))
               (cond ((and (consp key) (sym-is (car key) "SPREAD"))
                      (push (format nil "...~A" (cx (second key) env)) parts))
                     ((null plist) (tfail form "object literal: missing value for ~S" key))
                     (t (let* ((value (pop plist))
                               (name (cond ((keywordp key) (property-name key))
                                           ((stringp key) key)
                                           ((symbolp key) (property-name key))
                                           (t (tfail form "object literal: bad key ~S" key))))
                               (name (if (js-identifier-p name) name (js-string name))))
                          (push (format nil "~A: ~A" name (strip-object-parens (if quoted (cx (quote-form value) env) (cx value env)))) parts))))))
    (if parts
        (format nil "({ ~A })" (join (nreverse parts)))
        "({})")))

(defexpr ("%OBJECT" "OBJ") (form env) (compile-object-literal form env))

(defexpr "VECTOR" (form env) (format nil "[~A]" (compile-args (cdr form) env)))

(defexpr "SPREAD" (form env)
  (tfail form "spread is only allowed inside a call, vector or object literal"))

(defexpr "FUNCTION" (form env)
  (let ((f (second form)))
    (cond ((and (consp f) (sym-is (car f) "LAMBDA")) (compile-lambda f env))
          ((and (symbolp f) (not (lookup-var f env))
                (member (symbol-name f) '("EVAL" "COMPILE" "LOAD" "READ-FROM-STRING" "INTERN" "FUNCTION") :test #'string=))
           (tfail form "~A is not available on the frontend" (symbol-name f)))
          ((and (symbolp f) (not (lookup-var f env))
                (or (gethash (symbol-name f) *expr-forms*) (gethash (symbol-name f) *stmt-forms*)))
           (compile-lambda (operator-as-lambda f form) env))
          ((symbolp f) (compile-variable f env))
          (t (tfail form "bad function designator")))))

(defparameter *unary-operators*
  '("NOT" "NULL" "IDENTITY" "1+" "1-" "ABS" "ZEROP" "PLUSP" "MINUSP" "EVENP" "ODDP" "LENGTH"
    "STRING-UPCASE" "STRING-DOWNCASE" "FLOOR" "CEILING" "ROUND" "TRUNCATE" "SQRT" "STRINGP" "NUMBERP"))

(defparameter *binary-operators*
  '("<" ">" "<=" ">=" "=" "/=" "+" "-" "*" "/" "MAX" "MIN" "MOD" "REM" "EXPT" "EQ" "EQL" "EQUAL"
    "STRING=" "STRING/=" "STRING<" "STRING>" "STRING<=" "STRING>=" "CHAR=" "CONCAT"))

(defun operator-as-lambda (op form)
  "#'< and friends: built-in operators have no runtime value, so wrap them in a lambda."
  (let ((name (symbol-name op))
        (a (make-symbol "A"))
        (b (make-symbol "B")))
    (cond ((member name *unary-operators* :test #'string=) `(,(cl-sym "LAMBDA") (,a) (,op ,a)))
          ((member name *binary-operators* :test #'string=) `(,(cl-sym "LAMBDA") (,a ,b) (,op ,a ,b)))
          (t (tfail form "#'~(~A~) is a built-in form with no function value; pass a lambda instead" name)))))

(defexpr "THE" (form env)
  (format nil "(~A as ~A)" (cx (third form) env) (type->ts (second form))))

(defexpr "%RAW-JS" (form env) (second form))
(defexpr "THIS" (form env) "this")

;;; ------------------------------------------------------------------ operators

(defun binary-chain (op args env &optional (identity nil))
  (cond ((null args) (or identity (tfail args "operator ~A needs arguments" op)))
        ((null (cdr args)) (cx (first args) env))
        (t (paren (join (mapcar (lambda (a) (cx a env)) args) (format nil " ~A " op))))))

(defexpr "+" (form env)
  (if (null (cdr form)) "0" (binary-chain "+" (cdr form) env)))
(defexpr "*" (form env)
  (if (null (cdr form)) "1" (binary-chain "*" (cdr form) env)))
(defexpr "-" (form env)
  (cond ((null (cdr form)) (tfail form "- needs arguments"))
        ((null (cddr form)) (format nil "(-~A)" (cx (second form) env)))
        (t (binary-chain "-" (cdr form) env))))
(defexpr "/" (form env)
  (cond ((null (cdr form)) (tfail form "/ needs arguments"))
        ((null (cddr form)) (format nil "(1 / ~A)" (cx (second form) env)))
        (t (binary-chain "/" (cdr form) env))))
(defexpr "REM" (form env) (binary-chain "%" (cdr form) env))
(defexpr "MOD" (form env)
  (let ((a (cx (second form) env)) (b (cx (third form) env)))
    (format nil "(((~A % ~A) + ~A) % ~A)" a b b b)))
(defexpr "EXPT" (form env) (format nil "Math.pow(~A, ~A)" (cx (second form) env) (cx (third form) env)))
(defexpr "1+" (form env) (format nil "(~A + 1)" (cx (second form) env)))
(defexpr "1-" (form env) (format nil "(~A - 1)" (cx (second form) env)))
(defexpr "LOGAND" (form env) (binary-chain "&" (cdr form) env "-1"))
(defexpr "LOGIOR" (form env) (binary-chain "|" (cdr form) env "0"))
(defexpr "LOGXOR" (form env) (binary-chain "^" (cdr form) env "0"))
(defexpr "LOGNOT" (form env) (format nil "(~~~A)" (cx (second form) env)))
(defexpr "ASH" (form env)
  (let ((n (third form)))
    (if (and (integerp n) (minusp n))
        (format nil "(~A >> ~D)" (cx (second form) env) (- n))
        (format nil "(~A << ~A)" (cx (second form) env) (cx n env)))))

(defun comparison-chain (op args env)
  (cond ((< (length args) 2) (tfail args "comparison needs two arguments"))
        ((= (length args) 2) (format nil "(~A ~A ~A)" (cx (first args) env) op (cx (second args) env)))
        (t (paren (join (loop for (a b) on args while b
                              collect (format nil "~A ~A ~A" (cx a env) op (cx b env)))
                        " && ")))))

(defexpr ("=" "EQ" "EQL" "EQUAL" "EQUALP" "STRING=" "CHAR=" "STRING-EQUAL") (form env) (comparison-chain "===" (cdr form) env))
(defexpr ("/=" "STRING/=" "CHAR/=") (form env) (comparison-chain "!==" (cdr form) env))
(defexpr ("<" "STRING<" "CHAR<") (form env) (comparison-chain "<" (cdr form) env))
(defexpr (">" "STRING>" "CHAR>") (form env) (comparison-chain ">" (cdr form) env))
(defexpr ("<=" "STRING<=") (form env) (comparison-chain "<=" (cdr form) env))
(defexpr (">=" "STRING>=") (form env) (comparison-chain ">=" (cdr form) env))

(defexpr "NOT" (form env) (format nil "(!~A)" (cx (second form) env)))
(defexpr "NULL" (form env) (format nil "(~A == null)" (cx (second form) env)))
(defexpr "AND" (form env)
  (cond ((null (cdr form)) "true")
        (t (binary-chain "&&" (cdr form) env))))
(defexpr "OR" (form env)
  (cond ((null (cdr form)) "null")
        (t (binary-chain "||" (cdr form) env))))
(defexpr "NIL-OR" (form env) (binary-chain "??" (cdr form) env "null"))

(defexpr "ZEROP" (form env) (format nil "(~A === 0)" (cx (second form) env)))
(defexpr "PLUSP" (form env) (format nil "(~A > 0)" (cx (second form) env)))
(defexpr "MINUSP" (form env) (format nil "(~A < 0)" (cx (second form) env)))
(defexpr "EVENP" (form env) (format nil "(~A % 2 === 0)" (cx (second form) env)))
(defexpr "ODDP" (form env) (format nil "(~A % 2 !== 0)" (cx (second form) env)))
(defexpr "INTEGERP" (form env) (format nil "Number.isInteger(~A)" (cx (second form) env)))
(defexpr ("NUMBERP" "REALP" "FLOATP") (form env) (format nil "(typeof ~A === \"number\")" (cx (second form) env)))
(defexpr "STRINGP" (form env) (format nil "(typeof ~A === \"string\")" (cx (second form) env)))
(defexpr "FUNCTIONP" (form env) (format nil "(typeof ~A === \"function\")" (cx (second form) env)))
(defexpr ("ARRAYP" "VECTORP") (form env) (format nil "Array.isArray(~A)" (cx (second form) env)))
(defexpr "HASH-TABLE-P" (form env) (format nil "(~A instanceof Map)" (cx (second form) env)))
(defexpr "BOOLEANP" (form env) (format nil "(typeof ~A === \"boolean\")" (cx (second form) env)))

(defexpr "TYPEOF" (form env)
  (if (cddr form)
      (format nil "(typeof ~A === ~A)" (cx (second form) env) (cx (third form) env))
      (format nil "(typeof ~A)" (cx (second form) env))))
(defexpr "INSTANCEOF" (form env)
  (format nil "(~A instanceof ~A)" (cx (second form) env) (cx (third form) env)))
(defexpr "TYPEP" (form env)
  (let ((type (let ((x (third form))) (if (and (consp x) (sym-is (car x) "QUOTE")) (second x) x))))
    (let ((name (sym-name type)))
      (cond ((member name '("STRING" "SYMBOL" "KEYWORD" "CHARACTER") :test #'string=)
             (format nil "(typeof ~A === \"string\")" (cx (second form) env)))
            ((member name '("NUMBER" "FIXNUM" "INTEGER" "FLOAT" "REAL") :test #'string=)
             (format nil "(typeof ~A === \"number\")" (cx (second form) env)))
            ((string= name "BOOLEAN") (format nil "(typeof ~A === \"boolean\")" (cx (second form) env)))
            ((string= name "FUNCTION") (format nil "(typeof ~A === \"function\")" (cx (second form) env)))
            ((member name '("ARRAY" "VECTOR") :test #'string=) (format nil "Array.isArray(~A)" (cx (second form) env)))
            ((string= name "HASH-TABLE") (format nil "(~A instanceof Map)" (cx (second form) env)))
            ((string= name "NULL") (format nil "(~A == null)" (cx (second form) env)))
            (t (format nil "(~A instanceof ~A)" (cx (second form) env) (mangle-type-name type)))))))

;;; math

(defun math-fn (name)
  (lambda (form env)
    (format nil "Math.~A(~A)" name (compile-args (cdr form) env))))

(loop for (lisp js) in '(("ABS" "abs") ("SQRT" "sqrt") ("EXP" "exp") ("SIN" "sin") ("COS" "cos") ("TAN" "tan")
                         ("ASIN" "asin") ("ACOS" "acos") ("ATAN" "atan") ("MIN" "min") ("MAX" "max")
                         ("SINH" "sinh") ("COSH" "cosh") ("TANH" "tanh") ("SIGNUM" "sign"))
      do (setf (gethash lisp *expr-forms*) (math-fn js)))

(defun division-rounding (js-name)
  (lambda (form env)
    (if (cddr form)
        (format nil "Math.~A(~A / ~A)" js-name (cx (second form) env) (cx (third form) env))
        (format nil "Math.~A(~A)" js-name (cx (second form) env)))))

(setf (gethash "FLOOR" *expr-forms*) (division-rounding "floor")
      (gethash "CEILING" *expr-forms*) (division-rounding "ceil")
      (gethash "TRUNCATE" *expr-forms*) (division-rounding "trunc")
      (gethash "ROUND" *expr-forms*) (division-rounding "round"))

(defexpr "LOG" (form env)
  (if (cddr form)
      (format nil "(Math.log(~A) / Math.log(~A))" (cx (second form) env) (cx (third form) env))
      (format nil "Math.log(~A)" (cx (second form) env))))

(defexpr "RANDOM" (form env)
  (cond ((null (cdr form)) "Math.random()")
        ((integerp (second form)) (format nil "Math.floor(Math.random() * ~D)" (second form)))
        (t (format nil "(Math.random() * ~A)" (cx (second form) env)))))

;;; ------------------------------------------------------------------ strings

(defun template-literal (pieces)
  "PIECES: list of strings (literal text) or (:expr . js)."
  (with-output-to-string (s)
    (write-char #\` s)
    (dolist (p pieces)
      (if (stringp p)
          (loop for ch across p
                do (case ch
                     (#\` (write-string "\\`" s))
                     (#\\ (write-string "\\\\" s))
                     (#\$ (write-string "\\$" s))
                     (#\Return (write-string "\\r" s))
                     (t (if (or (= (char-code ch) #x2028) (= (char-code ch) #x2029))
                            (format s "\\u~4,'0X" (char-code ch))
                            (write-char ch s)))))
          (format s "${~A}" (strip-parens (cdr p)))))
    (write-char #\` s)))

(defexpr ("CONCAT" "STRING-CONCAT") (form env)
  (cond ((null (cdr form)) "\"\"")
        (t (template-literal (mapcar (lambda (a) (if (stringp a) a (cons :expr (cx a env)))) (cdr form))))))

(defexpr "CONCATENATE" (form env)
  (let ((type (second form)))
    (cond ((and (consp type) (sym-is (second type) "STRING"))
           (cx (cons (lang-sym "CONCAT") (cddr form)) env))
          (t (format nil "[~A]" (join (mapcar (lambda (a) (format nil "...~A" (cx a env))) (cddr form))))))))

(defun compile-format-directive-string (control args env form)
  "Translate a FORMAT control string with ~A ~S ~D ~% ~~ into a template literal."
  (let ((pieces '()) (text (make-string-output-stream)) (i 0) (n (length control)))
    (flet ((flush () (let ((s (get-output-stream-string text))) (when (plusp (length s)) (push s pieces)))))
      (loop while (< i n)
            do (let ((ch (char control i)))
                 (cond ((char= ch #\~)
                        (incf i)
                        (when (>= i n) (tfail form "format: dangling ~~"))
                        ;; skip numeric/comma/colon/at prefix parameters
                        (loop while (and (< i n) (find (char control i) "0123456789,:@v#+-")) do (incf i))
                        (let ((d (char-upcase (char control i))))
                          (case d
                            ((#\A #\S #\D #\F #\B #\O #\X #\E #\G #\$)
                             (flush)
                             (unless args (tfail form "format: not enough arguments"))
                             (push (cons :expr (cx (pop args) env)) pieces))
                            (#\% (write-char #\Newline text))
                            (#\& nil)
                            (#\~ (write-char #\~ text))
                            (#\Newline (loop while (and (< (1+ i) n) (member (char control (1+ i)) '(#\Space #\Tab))) do (incf i)))
                            (t (tfail form "format: directive ~~~A is not supported on the frontend" d)))))
                       (t (write-char ch text)))
                 (incf i)))
      (flush))
    (when args (tfail form "format: too many arguments"))
    (template-literal (nreverse pieces))))

(defexpr "FORMAT" (form env)
  (destructuring-bind (dest control &rest args) (cdr form)
    (unless (stringp control) (tfail form "format: control string must be a literal"))
    (let ((text (compile-format-directive-string control args env form)))
      (cond ((null dest) text)
            ((eq dest t) (format nil "console.log(~A)" text))
            (t (tfail form "format: destination must be nil or t"))))))

(defexpr ("PRINC-TO-STRING" "WRITE-TO-STRING" "STRING") (form env) (format nil "String(~A)" (cx (second form) env)))
(defexpr "PRIN1-TO-STRING" (form env) (format nil "JSON.stringify(~A)" (cx (second form) env)))
(defexpr "PARSE-INTEGER" (form env)
  (let ((radix (getf (cddr form) :radix 10)))
    (format nil "parseInt(~A, ~A)" (cx (second form) env) (cx radix env))))
(defexpr "STRING-UPCASE" (form env) (format nil "~A.toUpperCase()" (cx (second form) env)))
(defexpr "STRING-DOWNCASE" (form env) (format nil "~A.toLowerCase()" (cx (second form) env)))
(defexpr "STRING-TRIM" (form env) (format nil "~A.trim()" (cx (third form) env)))
(defexpr "STRING-LEFT-TRIM" (form env) (format nil "~A.trimStart()" (cx (third form) env)))
(defexpr "STRING-RIGHT-TRIM" (form env) (format nil "~A.trimEnd()" (cx (third form) env)))
(defexpr "CHAR-CODE" (form env) (format nil "~A.charCodeAt(0)" (cx (second form) env)))
(defexpr "CODE-CHAR" (form env) (format nil "String.fromCharCode(~A)" (cx (second form) env)))
(defexpr ("CHAR" "SCHAR") (form env) (format nil "~A.charAt(~A)" (cx (second form) env) (cx (third form) env)))

;;; ------------------------------------------------------------------ sequences

(defexpr "SUBSEQ" (form env)
  (format nil "~A.slice(~A)" (cx (second form) env) (compile-args (cddr form) env)))
(defexpr ("SEARCH" "POSITION") (form env)
  (format nil "~A.indexOf(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "POSITION-IF" (form env) (format nil "~A.findIndex(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "FIND-IF" (form env) (format nil "~A.find(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "FIND" (form env)
  (let ((g (gensym-js env "e")))
    (format nil "~A.find((~A) => ~A === ~A)" (cx (third form) env) g g (cx (second form) env))))
(defexpr "REVERSE" (form env) (format nil "[...~A].reverse()" (cx (second form) env)))
(defexpr "NREVERSE" (form env) (format nil "~A.reverse()" (cx (second form) env)))
(defexpr ("COPY-SEQ" "COPY-LIST") (form env) (format nil "[...~A]" (cx (second form) env)))
(defexpr "APPEND" (form env)
  (format nil "[~A]" (join (mapcar (lambda (a) (format nil "...~A" (cx a env))) (cdr form)))))
(defexpr ("MAPCAR" "MAPC") (form env) (format nil "~A.map(~A)" (cx (third form) env) (cx (second form) env)))

;;; Iteration over arrays.  `for-each` is for side effects and always becomes `.forEach` (it yields
;;; undefined); `map` builds and returns a new array and always becomes `.map`.  Both share the
;;; binding syntax (var seq [index]).  dotimes / doarray / dolist do not exist on the frontend.

(defun check-iteration-binding (form what)
  (unless (iteration-binding-p form)
    (tfail form "~A expects (~A (var array [index]) body...)" what what)))

(defun compile-for-each (form env)
  "(for-each (var seq [index]) body...) -> seq.forEach((var: any[, index: number]) => { ... })"
  (check-iteration-binding form "for-each")
  (destructuring-bind ((var seq &optional index-var) &rest body) (cdr form)
    (multiple-value-bind (decls doc forms) (split-body-declarations body)
      (declare (ignore doc))
      (let* ((types (declared-types decls))
             (env1 (new-scope (copy-env env :blocks '() :function (or (env-function env) :lambda)))))
        (multiple-value-bind (js env2) (bind-var var env1)
          (multiple-value-bind (js-idx env3) (if index-var (bind-var index-var env2) (values nil env2))
            (let* ((env4 (reduce (lambda (e ty) (bind-type (car ty) (cdr ty) e)) types :initial-value env3))
                   (stmts (capture-statements (with-indent (cbody forms (new-scope env4) :void)))))
              (format nil "~A.forEach((~A: ~A~@[, ~A: number~]) => {~%~A~A})"
                      (cx seq env) js (if (lookup-type var env4) (type->ts (lookup-type var env4)) "any")
                      js-idx stmts (indent-string)))))))))

(defun compile-map (form env)
  "(map (var seq [index]) body...) -> seq.map((var[, index: number]) => body)"
  (destructuring-bind ((var seq &optional index-var) &rest body) (cdr form)
    (let ((lambda-form (if index-var
                           ;; the index is a number unless the body declares otherwise (later declarations win)
                           (list* (cl-sym "LAMBDA") (list var index-var)
                                  (list (cl-sym "DECLARE") (list (cl-sym "FIXNUM") index-var)) body)
                           (list* (cl-sym "LAMBDA") (list var) body))))
      (format nil "~A.map(~A)" (cx seq env) (compile-lambda lambda-form env)))))

(defexpr "MAP" (form env)
  (cond ((iteration-binding-p form) (compile-map form env))
        ;; Common Lisp style (map 'vector fn seq): the result type is irrelevant on the frontend
        ((and (consp (second form)) (sym-is (car (second form)) "QUOTE") (= (length form) 4))
         (format nil "~A.map(~A)" (cx (fourth form) env) (cx (third form) env)))
        (t (tfail form "map expects (map (var array [index]) body...) or (map 'vector fn array)"))))
(defexpr "REMOVE-IF-NOT" (form env) (format nil "~A.filter(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "REMOVE-IF" (form env)
  (let ((g (gensym-js env "e")))
    (format nil "~A.filter((~A) => !~A(~A))" (cx (third form) env) g (cx (second form) env) g)))
(defexpr "REMOVE" (form env)
  (let ((g (gensym-js env "e")))
    (format nil "~A.filter((~A) => ~A !== ~A)" (cx (third form) env) g g (cx (second form) env))))
(defexpr "REDUCE" (form env)
  (destructuring-bind (fn seq &key initial-value) (cdr form)
    (if initial-value
        (format nil "~A.reduce(~A, ~A)" (cx seq env) (cx fn env) (cx initial-value env))
        (format nil "~A.reduce(~A)" (cx seq env) (cx fn env)))))
(defexpr "SOME" (form env) (format nil "~A.some(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "EVERY" (form env) (format nil "~A.every(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "NOTANY" (form env) (format nil "(!~A.some(~A))" (cx (third form) env) (cx (second form) env)))
(defexpr "COUNT-IF" (form env) (format nil "~A.filter(~A).length" (cx (third form) env) (cx (second form) env)))
(defexpr "SORT" (form env)
  (let ((a (gensym-js env "a")) (b (gensym-js env "b")) (pred (cx (third form) env)))
    (format nil "[...~A].sort((~A, ~A) => ~A(~A, ~A) ? -1 : ~A(~A, ~A) ? 1 : 0)"
            (cx (second form) env) a b pred a b pred b a)))
(defexpr ("FIRST" "CAR") (form env) (format nil "~A[0]" (cx (second form) env)))
(defexpr "SECOND" (form env) (format nil "~A[1]" (cx (second form) env)))
(defexpr "THIRD" (form env) (format nil "~A[2]" (cx (second form) env)))
(defexpr "NTH" (form env) (format nil "~A[~A]" (cx (third form) env) (cx (second form) env)))
(defexpr ("REST" "CDR") (form env) (format nil "~A.slice(1)" (cx (second form) env)))
(defexpr "LAST" (form env) (let ((a (cx (second form) env))) (format nil "~A.slice(-1)" a)))
(defexpr ("AREF" "ELT" "SVREF") (form env)
  (format nil "~A~{[~A]~}" (cx (second form) env) (mapcar (lambda (i) (cx i env)) (cddr form))))
(defexpr ("LENGTH" "FILL-POINTER" "ARRAY-TOTAL-SIZE") (form env) (format nil "~A.length" (cx (second form) env)))
(defexpr ("VECTOR-PUSH-EXTEND" "VECTOR-PUSH") (form env)
  (format nil "~A.push(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "VECTOR-POP" (form env) (format nil "~A.pop()" (cx (second form) env)))
(defexpr "PUSH" (form env) (format nil "~A.unshift(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "POP" (form env) (format nil "~A.shift()" (cx (second form) env)))
(defexpr "MAKE-ARRAY" (form env)
  (destructuring-bind (&optional size &key initial-element initial-contents element-type adjustable fill-pointer) (cdr form)
    (declare (ignore element-type))
    (when adjustable (tfail form "make-array: :adjustable does not exist on the frontend (all arrays are adjustable)"))
    (cond (initial-contents (cx initial-contents env))
          ((or (null size) (eql size 0)) "[]")
          ((and (consp size) (sym-is (car size) "QUOTE")) (tfail form "make-array: multi-dimensional arrays are not supported"))
          (initial-element (format nil "new Array(~A).fill(~A)" (cx size env) (cx initial-element env)))
          (fill-pointer (format nil "new Array(~A)" (cx fill-pointer env)))
          (t (format nil "new Array(~A)" (cx size env))))))
(defexpr "MAKE-HASH-TABLE" (form env) "new Map()")
(defexpr "GETHASH" (form env)
  (destructuring-bind (key table &optional default) (cdr form)
    (if default
        (format nil "(~A.get(~A) ?? ~A)" (cx table env) (cx key env) (cx default env))
        (format nil "~A.get(~A)" (cx table env) (cx key env)))))
(defexpr "REMHASH" (form env) (format nil "~A.delete(~A)" (cx (third form) env) (cx (second form) env)))
(defexpr "CLRHASH" (form env) (format nil "~A.clear()" (cx (second form) env)))
(defexpr "HASH-TABLE-COUNT" (form env) (format nil "~A.size" (cx (second form) env)))
(defexpr "MAPHASH" (form env)
  (let ((k (gensym-js env "k")) (v (gensym-js env "v")))
    (format nil "~A.forEach((~A, ~A) => ~A(~A, ~A))" (cx (third form) env) v k (cx (second form) env) k v)))
(defexpr "FUNCALL" (form env) (format nil "~A(~A)" (cx (second form) env) (compile-args (cddr form) env)))
(defexpr "APPLY" (form env)
  (let ((args (butlast (cddr form))) (rest (car (last (cddr form)))))
    (format nil "~A(~A)" (cx (second form) env)
            (compile-args (append args (list (list (lang-sym "SPREAD") rest))) env))))
(defexpr "IDENTITY" (form env) (cx (second form) env))
;; multiple values: `(values a b)` is simply the array [a, b]; it is bound with a `:multi` let binding
;; (specification.txt), assigned with `(setf (values a b) form)` and indexed like any array.
(defexpr "VALUES" (form env) (format nil "[~A]" (compile-args (cdr form) env)))

;;; unsupported CL on the frontend -- clear messages instead of silently wrong code

(defun unsupported (message)
  (lambda (form env) (declare (ignore env)) (tfail form "~A" message)))

(loop for (names . msg) in
      '((("LIST" "CONS" "LIST*" "CAAR" "CADR" "CDDR" "NCONC" "ASSOC" "MEMBER" "RPLACA" "RPLACD" "MAPCAN" "GETF" "ACONS")
         . "lists and cons cells do not exist on the frontend; use arrays (vector, make-array, for-each, map)")
        (("MULTIPLE-VALUE-BIND" "MULTIPLE-VALUE-SETQ" "MULTIPLE-VALUE-LIST" "MULTIPLE-VALUE-CALL" "MULTIPLE-VALUE-PROG1" "NTH-VALUE")
         . "multiple-value-* forms do not exist on the frontend: (values ...) is an array; bind it with (let ((:multi (a b) form)) ...) or (let ((:const :multi (a b) form)) ...), assign it with (setf (values a b) form), index it with aref")
        (("DOLIST" "DOARRAY") . "dolist/doarray are not available on the frontend; use (for-each (x xs) ...) for side effects, (map (x xs) ...) to build a new array, or (loop for x across xs ...) for early return")
        (("DOTIMES") . "dotimes is not available on the frontend; use (loop for i below n ...) or (for-each (x xs i) ...)")
        (("TAGBODY" "GO") . "tagbody/go are not supported on the frontend; use loop, block/return-from or structured forms")
        (("EVAL" "COMPILE" "LOAD" "READ-FROM-STRING" "INTERN") . "eval and the reader are not available on the frontend")
        (("SLEEP") . "sleep is not available on the frontend; use (await (delay ms)) or setTimeout")
        (("DEFCLASS" "DEFMETHOD" "DEFGENERIC" "MAKE-INSTANCE" "SLOT-VALUE" "WITH-SLOTS" "DEFINE-CONDITION")
         . "CLOS is not supported on the frontend; use defstruct (interfaces) and plain functions"))
      do (dolist (n names) (setf (gethash n *expr-forms*) (unsupported msg))))

;;; ------------------------------------------------------------------ control flow (expressions)

(defexpr "IF" (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    (if (statement-form-p form env)
        (iife form env)
        (format nil "(~A ? ~A : ~A)" (cx test env) (cx then env) (cx else env)))))

(defexpr "WHEN" (form env)
  (if (statement-form-p form env)
      (iife form env)
      (format nil "(~A ? ~A : null)" (cx (second form) env) (cx (if (cdddr form) (cons (cl-sym "PROGN") (cddr form)) (third form)) env))))

(defexpr "UNLESS" (form env)
  (if (statement-form-p form env)
      (iife form env)
      (format nil "(~A ? null : ~A)" (cx (second form) env) (cx (if (cdddr form) (cons (cl-sym "PROGN") (cddr form)) (third form)) env))))

(defexpr "COND" (form env)
  (if (statement-form-p form env)
      (iife form env)
      (labels ((build (clauses)
                 (if (null clauses)
                     "null"
                     (destructuring-bind (test &rest body) (first clauses)
                       (cond ((or (eq test t) (sym-is test "OTHERWISE"))
                              (if body (cx (cons (cl-sym "PROGN") body) env) "true"))
                             ((null body)
                              (format nil "(~A || ~A)" (cx test env) (build (rest clauses))))
                             (t (format nil "(~A ? ~A : ~A)" (cx test env) (cx (cons (cl-sym "PROGN") body) env) (build (rest clauses)))))))))
        (build (cdr form)))))

(defexpr ("PROGN" "LOCALLY") (form env)
  (let ((forms (cdr form)))
    (cond ((null forms) "null")
          ((and (null (cdr forms)) (not (statement-form-p (first forms) env))) (cx (first forms) env))
          (t (iife form env)))))

(defexpr "SETQ" (form env) (cx (cons (cl-sym "SETF") (cdr form)) env))
(defexpr "SETF" (form env)
  (let ((pairs (cdr form)))
    (when (oddp (length pairs)) (tfail form "setf: odd number of arguments"))
    (cond ((null pairs) "null")
          ((= (length pairs) 2) (compile-assignment (first pairs) (second pairs) env))
          (t (paren (join (loop for (place value) on pairs by #'cddr collect (compile-assignment place value env)) ", "))))))

(defun compile-place (place env)
  "JS text for an assignable place."
  (cond
    ((symbolp place)
     (when (const-p place env) (tfail place "cannot assign to const ~A" (string-downcase place)))
     (compile-variable place env))
    ((consp place)
     (let ((op (sym-name (car place))))
       (cond
         ((user-macro-p (car place) *module*) (compile-place (expand-user-macro place *module*) env))
         ((member op '("AREF" "ELT" "SVREF" "NTH" "GETF") :test #'string=)
          (if (string= op "NTH")
              (format nil "~A[~A]" (cx (third place) env) (cx (second place) env))
              (cx place env)))
         ((member op '("FILL-POINTER" "LENGTH") :test #'string=) (cx place env))
         ((string= op "..") (cx place env))
         ((member op '("FIRST" "CAR") :test #'string=) (cx place env))
         ((string= op "THE") (compile-place (third place) env))
         ((let ((acc (struct-accessor (car place))))
            (and acc (consp (cdr acc)) (eq (cadr acc) :accessor)))
          (cx place env))
         ((and (not (lookup-var (car place) env)) (inline-definition (car place)))
          (compile-place (expand-inline place (inline-definition (car place))) env))
         (t (tfail place "unsupported setf place")))))
    (t (tfail place "unsupported setf place"))))

(defun compile-assignment (place value env)
  (cond
    ((and (consp place) (sym-is (car place) "GETHASH"))
     (format nil "~A.set(~A, ~A)" (cx (third place) env) (cx (second place) env) (cx value env)))
    ((and (consp place) (sym-is (car place) "VALUES"))
     (format nil "([~A] = ~A)" (join (mapcar (lambda (p) (compile-place p env)) (cdr place))) (cx value env)))
    (t (format nil "(~A = ~A)" (compile-place place env) (cx value env)))))

(defexpr "INCF" (form env)
  (format nil "(~A += ~A)" (compile-place (second form) env) (if (cddr form) (cx (third form) env) "1")))
(defexpr "DECF" (form env)
  (format nil "(~A -= ~A)" (compile-place (second form) env) (if (cddr form) (cx (third form) env) "1")))

;;; ------------------------------------------------------------------ chaining, new, async

(defexpr ".." (form env)
  (let ((text (cx (second form) env)))
    (when (and (> (length text) 0) (not (simple-expression-p text)) (not (char= (char text 0) #\()))
      ;; e.g. `a + b`.foo is a syntax error, but calls/indexing are fine
      (unless (or (find #\( text) (find #\[ text)) (setf text (paren text))))
    (dolist (step (cddr form) text)
      (setf text (compile-chain-step text step env nil)))))

(defun compile-chain-step (text step env optional)
  "One step of a `..` chain (specification.txt, dot chaining):
   symbol / keyword            -> .name            (property access)
   string / integer            -> [\"k\"] / [3]    (computed access)
   (aref i)                    -> [i]
   (:? step)                   -> optional (?.) version of STEP
   (name arg...)               -> .name(arg...)    (method call)
A list step is ALWAYS a call and its first element is ALWAYS the method name; there is no
form in which a list's first element is an argument."
  (cond
    ((and (consp step) (eq (car step) :?))
     (unless (consp (cdr step)) (tfail step ".. chain: (:? step) needs exactly one step"))
     (compile-chain-step text (second step) env t))
    ((null step) (tfail step ".. chain: a call step must name the method, e.g. (.. obj (method)) -- () is not allowed"))
    ((keywordp step) (format nil "~A~:[.~;?.~]~A" text optional (property-name step)))
    ((symbolp step) (format nil "~A~:[.~;?.~]~A" text optional (escape-reserved-property (property-name step))))
    ((stringp step) (format nil "~A~:[~;?.~][~A]" text optional (js-string step)))
    ((integerp step) (format nil "~A~:[~;?.~][~D]" text optional step))
    ((and (consp step) (sym-is (car step) "AREF"))
     (unless (and (consp (cdr step)) (null (cddr step))) (tfail step ".. chain: (aref index) takes exactly one index"))
     (format nil "~A~:[~;?.~][~A]" text optional (cx (second step) env)))
    ((consp step)
     ;; (.. obj (method a b)) -> obj.method(a, b): the first element is the method name, always.
     (let ((name (car step)))
       (unless (and (symbolp name) (not (keywordp name)) (not (null name)) (not (eq name t)))
         (tfail step ".. chain: the first element of a call step must be the method name (a symbol), got ~S" name))
       (unless (listp (cdr (last step))) (tfail step ".. chain: malformed call step"))
       (format nil "~A~:[.~;?.~]~A(~A)" text optional
               (escape-reserved-property (property-name name)) (compile-args (cdr step) env))))
    (t (tfail step "bad .. chain step ~S" step))))

(defun escape-reserved-property (name)
  ;; property names may be reserved words (obj.default is legal JS); keep verbatim
  name)

(defexpr "NEW" (form env)
  (let ((target (second form)))
    (if (and (consp target) (not (sym-is (car target) "..")))
        (format nil "new ~A(~A)" (cx (car target) env) (compile-args (cdr target) env))
        (format nil "new ~A(~A)" (cx target env) (compile-args (cddr form) env)))))

(defexpr "AWAIT" (form env)
  (unless (or (env-async-p env) (null (env-function env)))
    (tfail form "await used outside an async function"))
  (format nil "(await ~A)" (cx (second form) env)))

(defexpr "AWAIT-ALL" (form env)
  (unless (or (env-async-p env) (null (env-function env)))
    (tfail form "await-all used outside an async function"))
  (format nil "(await Promise.all([~A]))" (compile-args (cdr form) env)))

(defexpr "ASYNC" (form env)
  (let ((target (second form)))
    (cond ((and (consp target) (sym-is (car target) "LAMBDA") (null (cddr form)))
           (compile-lambda target env :async t))
          ((and (consp target) (sym-is (car target) "FUNCTION") (consp (second target)) (sym-is (car (second target)) "LAMBDA"))
           (compile-lambda (second target) env :async t))
          (t ;; (async body...) -> (async () => { body })()
           (let* ((inner (new-scope (copy-env env :async-p t :blocks '() :function (or (env-function env) :anonymous))))
                  (body (capture-statements (with-indent (cbody (cdr form) inner :return)))))
             (format nil "(async () => {~%~A~A})()" body (indent-string)))))))

(defstmt "ASYNC" (form env dest)
  (let ((target (second form)))
    (if (and (consp target) (sym-is (car target) "DEFUN") (null (cddr form)))
        (compile-defun target env dest :async t)
        (finish dest (cx form env)))))

;;; ------------------------------------------------------------------ lambda / defun

(defun parse-lambda-list (lambda-list env form)
  "Returns (values param-texts new-env).  Supports required, &optional, &rest, &key."
  (let ((mode :required) (texts '()) (key-parts '()) (key-types '()))
    (dolist (item lambda-list)
      (cond
        ((sym-is item "&OPTIONAL") (setf mode :optional))
        ((sym-is item "&REST") (setf mode :rest))
        ((sym-is item "&KEY") (setf mode :key))
        ((sym-is item "&ALLOW-OTHER-KEYS") nil)
        ((sym-is item "&AUX") (tfail form "&aux is not supported; use let"))
        (t
         (ecase mode
           (:required
            (if (consp item)
                (tfail form "destructuring lambda lists are not supported")
                (multiple-value-bind (js env2) (bind-var item env)
                  (setf env env2)
                  (push (list :required item js) texts))))
           (:optional
            (let ((sym (if (consp item) (first item) item))
                  (default (and (consp item) (cdr item) (second item))))
              (multiple-value-bind (js env2) (bind-var sym env)
                (setf env env2)
                (push (list :optional sym js default (and (consp item) (cdr item))) texts))))
           (:rest
            (multiple-value-bind (js env2) (bind-var item env)
              (setf env env2)
              (push (list :rest item js) texts)))
           (:key
            (let ((sym (if (consp item) (first item) item))
                  (default (and (consp item) (cdr item) (second item))))
              (multiple-value-bind (js env2) (bind-var sym env)
                (setf env env2)
                (push (list sym js default) key-parts))))))))
    (values (nreverse texts) (nreverse key-parts) env)))

(defun destructuring-pattern-entry (k env)
  "K is (sym js default-form-or-nil) -> `prop`, `prop: js`, `prop = d` or `prop: js = d`."
  (destructuring-bind (sym js default) k
    (let ((prop (property-name sym)))
      (concatenate 'string
                   prop
                   (if (string= prop js) "" (format nil ": ~A" js))
                   (if default (format nil " = ~A" (strip-parens (cx default env))) "")))))

(defun render-params (texts key-parts env types)
  "Parameter list text with type annotations from TYPES (alist symbol -> type)."
  (flet ((ann (sym) (let ((ty (or (cdr (assoc sym types)) (lookup-type sym env)))) (if ty (format nil ": ~A" (type->ts ty)) ": any"))))
    (let ((parts
            (mapcar (lambda (p)
                      (ecase (first p)
                        (:required (format nil "~A~A" (third p) (ann (second p))))
                        (:optional (destructuring-bind (sym js default has-default) (cdr p)
                                     (if has-default
                                         (format nil "~A~A = ~A" js (ann sym) (strip-parens (cx default env)))
                                         (format nil "~A?~A" js (ann sym)))))
                        (:rest (let ((ty (or (cdr (assoc (second p) types)) (lookup-type (second p) env))))
                                 (format nil "...~A: ~A" (third p) (if ty (type->ts ty) "any[]"))))))
                    texts)))
      (when key-parts
        ;; &key a (b 1) -> { a, b = 1 }: { a?: any; b?: any } = {}
        (setf parts
              (append parts
                      (list (format nil "{ ~A }: { ~A } = {}"
                                    (join (mapcar (lambda (k) (destructuring-pattern-entry k env)) key-parts))
                                    (join (mapcar (lambda (k) (format nil "~A?~A" (property-name (first k)) (ann (first k)))) key-parts) "; "))))))
      (join parts))))

(defun compile-function-body (forms env dest return-type)
  (declare (ignore return-type))
  (capture-statements (with-indent (cbody forms (new-scope env) dest))))

(defun compile-lambda (form env &key async)
  "(lambda (params) [decls] body...) -> arrow function text."
  (destructuring-bind (lambda-list &rest body) (cdr form)
    (multiple-value-bind (decls doc forms) (split-body-declarations body)
      (declare (ignore doc))
      (let* ((types (declared-types decls))
             (returns (declared-returns decls))
             (env1 (new-scope (copy-env env :async-p async :blocks '() :function :lambda))))
        (multiple-value-bind (texts key-parts env2) (parse-lambda-list lambda-list env1 form)
          (let* ((env3 (reduce (lambda (e ty) (bind-type (car ty) (cdr ty) e)) types :initial-value env2))
                 (params (render-params texts key-parts env3 types))
                 (ret (if returns (format nil ": ~A" (type->ts (if async (list (lang-sym "PROMISE") returns) returns))) ""))
                 (prefix (if async "async " "")))
            (if (and (= (length forms) 1) (not (statement-form-p (first forms) env3)))
                (format nil "(~A(~A)~A => ~A)" prefix params ret (cx (first forms) env3))
                (format nil "(~A(~A)~A => {~%~A~A})" prefix params ret
                        (compile-function-body forms env3 :return returns) (indent-string)))))))))

(defexpr "LAMBDA" (form env) (compile-lambda form env))

(defun compile-defun (form env dest &key async export)
  (destructuring-bind (name lambda-list &rest body) (cdr form)
    (unless (and (symbolp name) (not (keywordp name))) (tfail form "defun: bad function name"))
    ;; inline alias?
    (when (and *module* (member name (module-inline-names *module*)))
      (setf (gethash (symbol-name name) (module-inlines *module*)) (cons lambda-list body))
      (return-from compile-defun nil))
    (multiple-value-bind (decls doc forms) (split-body-declarations body)
      (let* ((ftype (and *module* (gethash name (module-ftypes *module*))))
             (types (append (declared-types decls)
                            (when (car ftype)
                              (loop for ty in (car ftype)
                                    for p in (remove-if (lambda (x) (and (symbolp x) (char= (char (symbol-name x) 0) #\&))) lambda-list)
                                    collect (cons (if (consp p) (first p) p) ty)))))
             (returns (or (declared-returns decls) (cdr ftype)))
             (js-name (mangle-name name))
             (toplevel (null (env-function env))))
        (when (and *module* toplevel)
          (setf (gethash name (module-functions *module*))
                (make-fn-info :name name :js-name js-name :params lambda-list :return-type returns :async-p async :exported-p export)))
        (let ((env1 (new-scope (copy-env env :async-p async :blocks (list (list name :function nil nil)) :function name))))
          (multiple-value-bind (texts key-parts env2) (parse-lambda-list lambda-list env1 form)
            (let* ((env3 (reduce (lambda (e ty) (bind-type (car ty) (cdr ty) e)) types :initial-value env2))
                   (params (render-params texts key-parts env3 types))
                   (ret (if returns (format nil ": ~A" (type->ts (if async (list (lang-sym "PROMISE") returns) returns))) "")))
              (when doc (emit "/** ~A */" (substitute #\Space #\Newline doc)))
              (emit "~:[~;export ~]~:[~;async ~]function ~A(~A)~A {" export async js-name params ret)
              (write-string (compile-function-body forms env3 :return returns) *out*)
              (emit "}")
              (when (and (consp dest) (eq (car dest) :set)) (finish dest js-name))
              (when (eq dest :return) (finish dest js-name)))))))))

(defstmt "DEFUN" (form env dest) (compile-defun form env dest))

;;; ------------------------------------------------------------------ bindings (statements)

(defun parse-let-binding (b)
  "Split one `let` binding into its parts.  Returns (values vars const multi has-init init) where
   VARS is a symbol, or a list of symbols for a `:multi` binding.  Accepted shapes:
     sym | (sym) | (sym init) | (:const sym init)
     (:multi (a b ...) init) | (:const :multi (a b ...) init)   ; modifiers in any order"
  (let ((const nil) (multi nil) (orig b))
    (loop while (and (consp b) (keywordp (first b)))
          do (case (first b)
               (:const (setf const t))
               (:multi (setf multi t))
               (t (tfail orig "let: unknown binding modifier ~S (expected :const or :multi)" (first b))))
             (setf b (rest b)))
    (let ((vars (if (consp b) (first b) b))
          (has-init (and (consp b) (cdr b)))
          (init (and (consp b) (second b))))
      (cond (multi
             (unless (and (consp vars) (every (lambda (v) (and (symbolp v) (not (keywordp v)))) vars))
               (tfail orig "let: :multi expects a list of variables, got ~S" vars))
             (unless has-init (tfail orig "let: a :multi binding needs a form that returns multiple values"))
             (when (cddr b) (tfail orig "let: too many forms in :multi binding ~S" orig)))
            (t
             (unless (and (symbolp vars) (not (keywordp vars))) (tfail orig "let: bad variable ~S" vars))
             (when (and (consp b) (cddr b)) (tfail orig "let: too many forms in binding ~S" orig))))
      (values vars const multi has-init init))))

(defun compile-let-bindings (bindings env sequential)
  "Emit let/const declarations.  Returns the new env.
   A `:multi` binding becomes an array destructuring declaration (`let [a, b] = f();`); when its
   value form is a literal `(values ...)` the variables are declared one by one instead."
  (let ((env-out env) (pending '()))
    (flet ((bind-one (sym const has-init init-text)
             (multiple-value-bind (js env2) (bind-var sym env-out :const const)
               (setf env-out env2)
               (push (list :single js const has-init init-text (lookup-type sym env2)) pending))))
      (dolist (b bindings)
        (multiple-value-bind (vars const multi has-init init) (parse-let-binding b)
          (let ((init-env (if sequential env-out env)))
            (cond ((not multi)
                   (bind-one vars const has-init (and has-init (cx init init-env))))
                  ((and (consp init) (sym-is (car init) "VALUES"))
                   ;; (:multi (a b) (values 1 2)) -> let a = 1; let b = 2;  (no array round trip)
                   (loop for sym in vars for i from 0
                         do (bind-one sym const t (if (nth i (cdr init)) (cx (nth i (cdr init)) init-env) "null"))))
                  (t
                   (let ((init-text (cx init init-env)) (js-names '()) (types '()))
                     (dolist (sym vars)
                       (multiple-value-bind (js env2) (bind-var sym env-out :const const)
                         (setf env-out env2)
                         (push js js-names)
                         (push (lookup-type sym env2) types)))
                     (push (list :multi (nreverse js-names) const init-text (nreverse types)) pending))))))))
    (dolist (p (nreverse pending))
      (if (eq (first p) :multi)
          (destructuring-bind (js-names const init-text types) (rest p)
            (emit "~A [~A]~A = ~A;" (if const "const" "let") (join js-names)
                  (if (some #'identity types)
                      (format nil ": [~{~A~^, ~}]" (mapcar (lambda (ty) (if ty (type->ts ty) "any")) types))
                      "")
                  (strip-parens init-text)))
          (destructuring-bind (js const has-init init-text ty) (rest p)
            (cond ((and (not has-init) (not ty)) (emit "let ~A: any = null;" js))
                  ((not has-init) (emit "let ~A: ~A | null = null;" js (type->ts ty)))
                  ;; `let x = null` would be typed `null` forever by TypeScript
                  ((and (not ty) (not const) (member init-text '("null" "undefined") :test #'string=))
                   (emit "let ~A: any = ~A;" js init-text))
                  (ty (emit "~A ~A: ~A = ~A;" (if const "const" "let") js (type->ts ty) (strip-parens init-text)))
                  (t (emit "~A ~A = ~A;" (if const "const" "let") js (strip-parens init-text)))))))
    env-out))

(defun compile-let (form env dest sequential)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (decls doc forms) (split-body-declarations body)
      (declare (ignore doc))
      (let* ((types (declared-types decls))
             (env1 (reduce (lambda (e ty) (bind-type (car ty) (cdr ty) e)) types :initial-value env))
             (env2 (compile-let-bindings bindings env1 sequential)))
        (cbody forms env2 dest)))))

(defstmt "LET" (form env dest) (compile-let form env dest nil))
(defstmt "LET*" (form env dest) (compile-let form env dest t))

(defstmt ("PROGN" "LOCALLY") (form env dest)
  (cond ((null (cdr form)) (finish dest "null"))
        ((null (cddr form)) (cs (second form) env dest))
        ((eq dest :void)
         (emit "{")
         (with-indent (cbody (cdr form) (new-scope env) dest))
         (emit "}"))
        (t (cbody (cdr form) env dest))))

(defstmt "PROG1" (form env dest)
  (let ((tmp (gensym-js env "first")))
    (emit "const ~A = ~A;" tmp (strip-parens (cx (second form) env)))
    (dolist (f (cddr form)) (cs f env :void))
    (finish dest tmp)))

(defstmt ("SETF" "SETQ" "INCF" "DECF") (form env dest)
  (if (and (sym-is (car form) "SETF") (> (length (cdr form)) 2))
      (loop for (place value . more) on (cdr form) by #'cddr
            do (if more
                   (emit "~A;" (strip-parens (compile-assignment place value env)))
                   (finish dest (compile-assignment place value env))))
      (finish dest (cx form env))))

(defstmt "DESTRUCTURING-BIND" (form env dest)
  (destructuring-bind (pattern value-form &rest body) (cdr form)
    (let ((env1 env) (parts '()) (mode :positional))
      (dolist (item pattern)
        (cond ((sym-is item "&REST") (setf mode :rest))
              ((sym-is item "&KEY") (setf mode :key))
              ((sym-is item "&OPTIONAL") nil)
              (t (let ((sym (if (consp item) (first item) item)))
                   (multiple-value-bind (js env2) (bind-var sym env1)
                     (setf env1 env2)
                     (push (ecase mode
                             (:positional (if (consp item) (format nil "~A = ~A" js (cx (second item) env)) js))
                             (:rest (format nil "...~A" js))
                             (:key (destructuring-pattern-entry (list sym js (and (consp item) (second item))) env)))
                           parts))))))
      (emit (if (eq mode :key) "const { ~A } = ~A;" "const [~A] = ~A;") (join (nreverse parts)) (strip-parens (cx value-form env)))
      (cbody body env1 dest))))

(defstmt ("FLET" "LABELS") (form env dest)
  (destructuring-bind (definitions &rest body) (cdr form)
    (let ((env1 env))
      ;; bind all names first (labels semantics; harmless for flet)
      (dolist (def definitions)
        (multiple-value-bind (js env2) (bind-var (first def) env1)
          (setf env1 (copy-env env2 :labels (acons (first def) js (env-labels env2))))))
      (dolist (def definitions)
        (destructuring-bind (name lambda-list &rest fbody) def
          (let ((js (lookup-var name env1)))
            (emit "const ~A = ~A;" js (strip-parens (compile-lambda (list* (cl-sym "LAMBDA") lambda-list fbody) env1))))))
      (cbody body env1 dest))))

;;; ------------------------------------------------------------------ control flow (statements)

(defun emit-branch (form env dest)
  (emit "{")
  (with-indent (cs form (new-scope env) dest))
  (emit "}"))

(defstmt "IF" (form env dest)
  (destructuring-bind (test then &optional (else nil else-p)) (cdr form)
    (emit "if (~A) {" (strip-parens (cx test env)))
    (with-indent (cs then (new-scope env) dest))
    (cond ((or else-p (and (consp dest) (eq (car dest) :set)))
           (emit "} else {")
           (with-indent (cs else (new-scope env) dest))
           (emit "}"))
          (t (emit "}")))))

(defstmt ("WHEN" "UNLESS") (form env dest)
  (let ((test (if (sym-is (car form) "WHEN") (cx (second form) env) (format nil "!~A" (cx (second form) env)))))
    (emit "if (~A) {" (if (sym-is (car form) "WHEN") (strip-parens test) test))
    (with-indent (cbody (cddr form) (new-scope env) dest))
    (cond ((and (consp dest) (eq (car dest) :set))
           (emit "} else {")
           (with-indent (finish dest "null"))
           (emit "}"))
          (t (emit "}")))))

(defstmt "COND" (form env dest)
  (let ((first t) (has-default nil))
    (dolist (clause (cdr form))
      (destructuring-bind (test &rest body) clause
        (cond ((or (eq test t) (sym-is test "OTHERWISE"))
               (setf has-default t)
               (if first
                   (cbody body env dest)
                   (progn (emit "} else {")
                          (with-indent (cbody body (new-scope env) dest))))
               (return))
              (t
               (emit "~:[} else if~;if~] (~A) {" first (strip-parens (cx test env)))
               (with-indent (if body (cbody body (new-scope env) dest) (finish dest (cx test env))))
               (setf first nil)))))
    (unless first
      (when (and (not has-default) (consp dest) (eq (car dest) :set))
        (emit "} else {")
        (with-indent (finish dest "null")))
      (emit "}"))))

(defun case-key-text (key env)
  (cond ((null key) "null")
        ((eq key t) "true")
        ((keywordp key) (cx key env))
        ((symbolp key) (js-string (string-downcase (symbol-name key))))
        (t (cx key env))))

(defstmt ("CASE" "ECASE") (form env dest)
  (destructuring-bind (keyform &rest clauses) (cdr form)
    (emit "switch (~A) {" (strip-parens (cx keyform env)))
    (with-indent
      (let ((has-default nil))
        (dolist (clause clauses)
          (destructuring-bind (keys &rest body) clause
            (cond ((or (eq keys t) (sym-is keys "OTHERWISE"))
                   (setf has-default t)
                   (emit "default: {")
                   (with-indent (cbody body (new-scope env) dest)
                     (unless (eq dest :return) (emit "break;")))
                   (emit "}"))
                  (t
                   (dolist (k (if (and (listp keys) (not (null keys))) keys (list keys)))
                     (emit "case ~A:" (case-key-text k env)))
                   (emit "{")
                   (with-indent (cbody body (new-scope env) dest)
                     (unless (eq dest :return) (emit "break;")))
                   (emit "}")))))
        (unless has-default
          (cond ((sym-is (car form) "ECASE")
                 (emit "default:")
                 (with-indent (emit "throw new Error(\"ecase: no matching clause\");")))
                ((and (consp dest) (eq (car dest) :set))
                 (emit "default:")
                 (with-indent (finish dest "null")))))))
    (emit "}")))

;;; blocks

(defun form-mentions-p (form names)
  (cond ((symbolp form) (and (not (keywordp form)) form (member (symbol-name form) names :test #'string=)))
        ((consp form) (or (form-mentions-p (car form) names) (form-mentions-p (cdr form) names)))
        (t nil)))

(defstmt "BLOCK" (form env dest)
  (destructuring-bind (name &rest body) (cdr form)
    (cond ((eq dest :return)
           (cbody body (push-block env name :return nil nil) dest))
          ((not (form-mentions-p body '("RETURN-FROM" "RETURN")))
           ;; nobody can exit this block early: no label needed
           (cbody body env dest))
          (t
           (let* ((label (gensym-js env "block"))
                  (result (and (consp dest) (eq (car dest) :set) (cdr dest)))
                  (env1 (push-block env name :block label result)))
             (emit "~A: {" label)
             (with-indent (cbody body (new-scope env1) dest))
             (emit "}"))))))

(defstmt "RETURN-FROM" (form env dest)
  (destructuring-bind (name &optional value) (cdr form)
    (let ((block (find-block env name)))
      (unless block (tfail form "return-from: no block named ~A" (if name (string-downcase name) "nil")))
      (destructuring-bind (bname kind label result-var) block
        (declare (ignore bname))
        (ecase kind
          ((:function :return) (emit "return ~A;" (strip-parens (cx value env))))
          ((:block :loop)
           (when result-var (emit "~A = ~A;" result-var (strip-parens (cx value env))))
           (when (and (not result-var) value)
             (cs value env :void))
           (emit "break ~A;" label)))))))

(defstmt "RETURN" (form env dest)
  (cs (list (cl-sym "RETURN-FROM") nil (second form)) env dest))

;;; loops

(defun loop-env (env label)
  (push-block env nil :loop label (and nil)))

(defstmt "%WHILE" (form env dest)
  (let* ((label (gensym-js env "loop"))
         (env1 (loop-env env label)))
    (emit "~A: while (~A) {" label (strip-parens (cx (second form) env)))
    (with-indent (cbody (cddr form) (new-scope env1) :void))
    (emit "}")
    (when (and (consp dest) (eq (car dest) :set)) (finish dest "null"))))

(defstmt "%FOR-OF" (form env dest)
  (destructuring-bind ((var seq) &rest body) (cdr form)
    (let* ((label (gensym-js env "loop"))
           (env1 (new-scope (loop-env env label)))
           (seq-text (strip-parens (cx seq env))))
      (if (consp var)
          (let ((names '()))
            (dolist (v var) (multiple-value-bind (js e) (bind-var v env1) (setf env1 e) (push js names)))
            (emit "~A: for (const [~A] of ~A) {" label (join (nreverse names)) seq-text))
          (multiple-value-bind (js e) (bind-var var env1)
            (setf env1 e)
            (emit "~A: for (const ~A of ~A) {" label js seq-text)))
      (with-indent (cbody body (new-scope env1) :void))
      (emit "}")
      (when (and (consp dest) (eq (car dest) :set)) (finish dest "null")))))

(defstmt "%BREAK" (form env dest)
  (let ((block (find-loop env)))
    (unless block (tfail form "break outside of a loop"))
    (emit "break ~A;" (third block))))

(defstmt "%CONTINUE" (form env dest)
  (let ((block (find-loop env)))
    (unless block (tfail form "continue outside of a loop"))
    (emit "continue ~A;" (third block))))

(defstmt "LOOP" (form env dest)
  (if (and (cdr form) (consp (second form)))
      ;; simple loop: (loop form...) == infinite loop
      (cs (list* (cl-sym "BLOCK") nil (list (list* (lang-sym "%WHILE") t (cdr form)))) env dest)
      (cs (expand-loop form) env dest)))

;; for-each is a side-effect loop: as a statement it is `xs.forEach(...)`; nothing is returned
;; (a function ending in for-each returns undefined, like one ending in `loop`).  Use `map` to
;; produce a value.
(defstmt "FOR-EACH" (form env dest)
  (emit "~A;" (compile-for-each form env))
  (when (and (consp dest) (eq (car dest) :set)) (finish dest "undefined")))

(defexpr "FOR-EACH" (form env) (compile-for-each form env))

;;; errors

(defun catch-all-type-p (type)
  (member (sym-name type) '("ERROR" "T" "CONDITION" "SERIOUS-CONDITION" "SIMPLE-ERROR" "WARNING") :test #'string=))

(defstmt "HANDLER-CASE" (form env dest)
  (destructuring-bind (protected &rest clauses) (cdr form)
    (let ((err (gensym-js env "err")))
      (emit "try {")
      (with-indent (cs protected (new-scope env) dest))
      (emit "} catch (~A) {" err)
      (with-indent
        (let ((first t) (catch-all nil))
          (dolist (clause clauses)
            (destructuring-bind (type (&optional var) &rest body) clause
              (let ((env1 (new-scope env)) (js nil))
                (when var
                  (multiple-value-setq (js env1) (bind-var var env1)))
                (cond ((catch-all-type-p type)
                       (setf catch-all t)
                       (unless first (emit "} else {"))
                       (with-indent-if (not first)
                         (when js (emit "const ~A: any = ~A;" js err))
                         (cbody body (new-scope env1) dest))
                       ;; the else branch is closed after the loop, together with the if chain
                       (return))
                      (t
                       (emit "~:[} else if~;if~] (~A instanceof ~A) {" first err (mangle-type-name type))
                       (with-indent
                         (when js (emit "const ~A = ~A;" js err))
                         (cbody body (new-scope env1) dest))
                       (setf first nil))))))
          (unless first
            (unless catch-all
              (emit "} else {")
              (with-indent (emit "throw ~A;" err)))
            (emit "}"))
          (when (and (not catch-all) first)
            (emit "throw ~A;" err))))
      (emit "}"))))

(defstmt "UNWIND-PROTECT" (form env dest)
  (emit "try {")
  (with-indent (cs (second form) (new-scope env) dest))
  (emit "} finally {")
  (with-indent (cbody (cddr form) (new-scope env) :void))
  (emit "}"))

(defstmt "IGNORE-ERRORS" (form env dest)
  (emit "try {")
  (with-indent (cbody (cdr form) (new-scope env) dest))
  (emit "} catch {")
  (with-indent (unless (eq dest :void) (finish dest "null")))
  (emit "}"))

(defun compile-throw-expression (form env)
  (destructuring-bind (what &rest args) (cdr form)
    (cond ((stringp what) (format nil "new Error(~A)" (compile-format-directive-string what args env form)))
          ((and (consp what) (sym-is (car what) "QUOTE"))
           (format nil "new ~A(~A)" (mangle-type-name (second what)) (compile-args args env)))
          (t (cx what env)))))

(defstmt ("ERROR" "SIGNAL") (form env dest)
  (emit "throw ~A;" (compile-throw-expression form env)))

(defexpr ("ERROR" "SIGNAL") (form env)
  (format nil "(() => { throw ~A; })()" (compile-throw-expression form env)))

;;; ------------------------------------------------------------------ definitions

(defun compile-global-definition (form env dest &key export keyword)
  (declare (ignore dest))
  (destructuring-bind (name &optional (init nil init-p) doc) (cdr form)
    (unless (and (symbolp name) (not (keywordp name))) (tfail form "bad variable name"))
    (let* ((js (mangle-name name))
           (ty (and *module* (gethash name (module-global-types *module*)))))
      (when *module* (setf (gethash name (module-globals *module*)) js))
      (when doc (emit "/** ~A */" (substitute #\Space #\Newline doc)))
      (cond ((not init-p) (emit "~:[~;export ~]~A ~A: ~A;" export keyword js (if ty (type->ts ty) "any")))
            (ty (emit "~:[~;export ~]~A ~A: ~A = ~A;" export keyword js (type->ts ty) (strip-parens (cx init env))))
            (t (emit "~:[~;export ~]~A ~A = ~A;" export keyword js (strip-parens (cx init env))))))))

(defstmt "DEFVAR" (form env dest) (compile-global-definition form env dest :keyword "var"))
(defstmt "DEFPARAMETER" (form env dest) (compile-global-definition form env dest :keyword "let"))
(defstmt "DEFCONSTANT" (form env dest) (compile-global-definition form env dest :keyword "const"))

(defstmt "DECLAIM" (form env dest)
  (dolist (decl (cdr form))
    (let ((kind (sym-name (car decl))))
      (cond ((string= kind "INLINE")
             (dolist (f (cdr decl)) (pushnew f (module-inline-names *module*))))
            ((string= kind "NOTINLINE") nil)
            ((string= kind "TYPE")
             (dolist (v (cddr decl)) (setf (gethash v (module-global-types *module*)) (second decl))))
            ((string= kind "FTYPE")
             (dolist (entry (declared-ftypes (list decl)))
               (setf (gethash (car entry) (module-ftypes *module*)) (cdr entry))))
            ((string= kind "OPTIMIZE") (tfail form "the optimize declaration is not supported on the frontend"))
            ((member kind '("SPECIAL" "IGNORE" "IGNORABLE") :test #'string=) nil)
            (t ;; (declaim (string *x*)) shorthand
             (dolist (v (cdr decl)) (setf (gethash v (module-global-types *module*)) (car decl))))))))

(defstmt "DEFMACRO" (form env dest) (define-client-macro form *module*))

(defstmt "DEFTYPE" (form env dest)
  (destructuring-bind (name lambda-list &rest body) (cdr form)
    (declare (ignore lambda-list))
    (multiple-value-bind (decls doc forms) (split-body-declarations body)
      (declare (ignore decls doc))
      (let ((spec (first forms)))
        (when (and (consp spec) (sym-is (car spec) "QUOTE")) (setf spec (second spec)))
        (emit "type ~A = ~A;" (mangle-type-name name) (type->ts spec))))))

;;; structs -> interfaces

(defvar *project-structs* (make-hash-table :test 'equal)
  "NAME -> struct-info, shared by all modules of a project so accessors work across imports.")

(defvar *project-struct-accessors* (make-hash-table :test 'equal)
  "Accessor NAME -> (struct-info . what), project wide.")

(defun struct-accessor (op)
  "(struct-info . :accessor/:make/:copy/:predicate) or NIL, keyed by the operator's name."
  (and *module* (symbolp op)
       (or (gethash (symbol-name op) (module-struct-accessors *module*))
           (gethash (symbol-name op) *project-struct-accessors*))))

(defun register-struct (info module)
  (let ((prefix (symbol-name (struct-info-name info))))
    (flet ((reg (name value)
             (setf (gethash name (module-struct-accessors module)) value
                   (gethash name *project-struct-accessors*) value)))
      (setf (gethash (struct-info-name info) (module-structs module)) info)
      (setf (gethash prefix *project-structs*) info)
      (reg (format nil "MAKE-~A" prefix) (cons info :make))
      (reg (format nil "COPY-~A" prefix) (cons info :copy))
      (reg (format nil "~A-P" prefix) (cons info :predicate))
      ;; accessors exist for inherited slots too: (point3-x p) when point3 includes point
      (dolist (f (all-struct-fields info))
        (reg (format nil "~A-~A" prefix (symbol-name (first f))) (cons info (list :accessor (first f))))))))

(defun coerce-nil-for-type (form type)
  "NIL given for a boolean-typed slot means false, not null."
  (if (and (null form) (sym-is type "BOOLEAN")) :false form))

(defun all-struct-fields (info)
  "Own fields plus inherited ones (parent first)."
  (let ((parent (and (struct-info-include info)
                     (or (gethash (struct-info-include info) (module-structs *module*))
                         (gethash (symbol-name (struct-info-include info)) *project-structs*)))))
    (append (and parent (all-struct-fields parent)) (struct-info-fields info))))

(defstmt "DEFSTRUCT" (form env dest)
  (compile-defstruct form env nil))

(defun compile-defstruct (form env export)
  (destructuring-bind (name-and-options &rest slots) (cdr form)
    (let* ((name (if (consp name-and-options) (first name-and-options) name-and-options))
           (options (if (consp name-and-options) (rest name-and-options) '()))
           (include nil))
      (when (stringp (first slots)) (pop slots))        ; docstring
      (dolist (opt options)
        (let ((key (if (consp opt) (first opt) opt)))
          (cond ((eq key :include)
                 (when include (tfail form "defstruct: only one :include is allowed"))
                 (setf include (second opt)))
                ((member key '(:conc-name :constructor :copier :predicate :print-function :print-object :type :named))
                 (twarn form "defstruct option ~S is ignored on the frontend" key))
                (t (tfail form "defstruct: unknown option ~S" key)))))
      (let* ((fields (mapcar (lambda (s)
                               (cond ((symbolp s) (list s nil nil))
                                     (t (destructuring-bind (sname &optional default &key type read-only) s
                                          (declare (ignore read-only))
                                          (list sname default type)))))
                             slots))
             (info (make-struct-info :name name :ts-name (mangle-type-name name) :include include :fields fields)))
        (when (and include (not (or (gethash include (module-structs *module*)) (gethash (symbol-name include) *project-structs*))))
          (tfail form "defstruct: unknown included struct ~A" (string-downcase include)))
        (register-struct info *module*)
        (emit "~:[~;export ~]interface ~A~@[ extends ~A~] {" export (struct-info-ts-name info)
              (and include (mangle-type-name include)))
        (with-indent
          (dolist (f fields)
            (emit "~A: ~A;" (property-name (first f)) (if (third f) (type->ts (third f)) "any"))))
        (emit "}")
        ;; constructor function (a real function so it can be exported/imported)
        (let* ((all (all-struct-fields info))
               (params (join (mapcar (lambda (f)
                                       (format nil "~A = ~A" (property-name (first f))
                                               (strip-parens (cx (coerce-nil-for-type (second f) (third f)) env))))
                                     all)))
               (types (join (mapcar (lambda (f) (format nil "~A?: ~A" (property-name (first f)) (if (third f) (type->ts (third f)) "any"))) all) "; ")))
          (emit "~:[~;export ~]function make~A({ ~A }: { ~A } = {}): ~A {" export (struct-info-ts-name info) params types (struct-info-ts-name info))
          (with-indent (emit "return { ~A };" (join (mapcar (lambda (f) (property-name (first f))) all))))
          (emit "}"))))))

(defun compile-struct-accessor (form env accessor)
  (destructuring-bind (info . what) accessor
    (cond ((eq what :make)
           (let ((plist (cdr form)) (parts '()))
             (loop while plist
                   do (let ((k (pop plist)) (v (if plist (pop plist) (tfail form "make-~A: missing value" (string-downcase (struct-info-name info))))))
                        (unless (keywordp k) (tfail form "make-~A: keyword argument expected" (string-downcase (struct-info-name info))))
                        (let ((field (find (symbol-name k) (all-struct-fields info) :key (lambda (f) (symbol-name (first f))) :test #'string=)))
                          (unless field
                            (tfail form "make-~A: no slot named ~A" (string-downcase (struct-info-name info)) (string-downcase k)))
                          (push (format nil "~A: ~A" (property-name k) (strip-parens (cx (coerce-nil-for-type v (third field)) env))) parts))))
             (format nil "make~A(~:[~;{ ~A }~])" (struct-info-ts-name info) parts (join (nreverse parts)))))
          ((eq what :copy) (format nil "({ ...~A })" (cx (second form) env)))
          ((eq what :predicate)
           (let ((x (cx (second form) env)))
             (format nil "(~A != null && typeof ~A === \"object\"~{ && ~A in ~A~})" x x
                     (loop for f in (struct-info-fields info) append (list (js-string (property-name (first f))) x)))))
          ((and (consp what) (eq (first what) :accessor))
           (unless (= (length form) 2) (tfail form "accessor takes exactly one argument"))
           (format nil "~A.~A" (cx (second form) env) (property-name (second what))))
          (t (tfail form "bad struct accessor")))))

;;; ------------------------------------------------------------------ modules: import / export

(defun sibling-module-name (from module-names)
  "The sibling module named by an import's :FROM (\"utils\", \"./utils\" or utils), or NIL."
  (let ((name (cond ((stringp from) (if (and (> (length from) 2) (string= "./" from :end2 2)) (subseq from 2) from))
                    ((and (symbolp from) (not (keywordp from))) (string-downcase (symbol-name from))))))
    (and name (find name module-names :test #'string=))))

(defun resolve-import-source (source form)
  (cond ((eq source :server) (config-stubs-module *config*))
        ((stringp source)
         (cond ((or (and (> (length source) 1) (string= "./" source :end2 2))
                    (and (> (length source) 2) (string= "../" source :end2 3)))
                source)
               ((member source (config-module-names *config*) :test #'string=) (format nil "./~A" source))
               (t source)))
        ((symbolp source)
         (let ((name (string-downcase (symbol-name source))))
           (if (member name (config-module-names *config*) :test #'string=) (format nil "./~A" name) name)))
        (t (tfail form "import: bad :from"))))

(defun import-binding (sym &optional source kind (original sym))
  "Register SYM as a module global (remembering where it was imported from); returns its JS name."
  (let ((js (mangle-name sym)))
    (setf (gethash sym (module-globals *module*)) js)
    (when source
      (setf (gethash sym (module-import-origins *module*))
            (list :source source :kind kind :js js :original (mangle-name original))))
    js))

(defun carried-import-p (js source)
  "True when JS was already imported from SOURCE on behalf of an imported inline alias."
  (let ((carried (find js (module-carried-imports *module*) :key #'car :test #'string=)))
    (and carried (string= (cdr carried) source))))

(defun register-imported-lambda-lists (sibling targets)
  "Calls to functions imported from SIBLING fold their &key arguments just like local calls."
  (dolist (tgt targets)
    (multiple-value-bind (name local)
        (cond ((and (symbolp tgt) tgt (not (keywordp tgt))) (values (symbol-name tgt) tgt))
              ((and (consp tgt) (eq (first tgt) :as)) (values (symbol-name (second tgt)) (third tgt))))
      (when name
        (loop for sym being the hash-keys of (module-lambda-lists sibling) using (hash-value lambda-list)
              when (string= (symbol-name sym) name)
                do (setf (gethash local (module-lambda-lists *module*)) lambda-list))))))

(defun import-line (kind js original source)
  (ecase kind
    (:namespace (format nil "import * as ~A from ~A;" js (js-string source)))
    (:default (format nil "import ~A from ~A;" js (js-string source)))
    (:named (format nil "import { ~A } from ~A;" js (js-string source)))
    (:renamed (format nil "import { ~A as ~A } from ~A;" original js (js-string source)))))

(defstmt "IMPORT" (form env dest)
  (block import-stmt-body
  (destructuring-bind (&key from targets as) (cdr form)
    (unless from (tfail form "import: :from is required"))
    (let* ((source (resolve-import-source from form))
           (sibling (let ((name (sibling-module-name from (config-module-names *config*))))
                      (and name (gethash name *project-modules*)))))
      (when (and sibling (consp targets))
        (setf targets (or (install-inline-imports sibling targets form)
                          (return-from import-stmt-body nil)))
        (register-imported-lambda-lists sibling targets))
      (cond
        ((eq targets :all)
         (unless as (tfail form "import: :targets :all needs :as"))
         (let ((js (import-binding as source :namespace)))
           (unless (carried-import-p js source)
             (emit "import * as ~A from ~A;" js (js-string source)))))
        ((and (symbolp targets) targets (not (keywordp targets)))
         (let ((js (import-binding targets source :default)))
           (unless (carried-import-p js source)
             (emit "import ~A from ~A;" js (js-string source)))))
        ((consp targets)
         (let ((names '()) (default nil))
           (dolist (tgt targets)
             (cond ((and (consp tgt) (eq (first tgt) :type))
                    (dolist (ty (rest tgt)) (push (format nil "type ~A" (mangle-type-name ty)) names)))
                   ((and (consp tgt) (eq (first tgt) :default))
                    (let ((js (import-binding (second tgt) source :default)))
                      (unless (carried-import-p js source) (setf default js))))
                   ((and (consp tgt) (eq (first tgt) :as))
                    (let ((js (import-binding (third tgt) source :renamed (second tgt))))
                      (unless (carried-import-p js source)
                        (push (format nil "~A as ~A" (mangle-name (second tgt)) js) names))))
                   ((and (symbolp tgt) (struct-accessor tgt)
                         (not (eq (cdr (struct-accessor tgt)) :make)))
                    ;; struct accessors/copiers/predicates compile inline: nothing to import
                    nil)
                   ((symbolp tgt)
                    (when (eq from :server) (pushnew tgt (module-server-imports *module*)))
                    (let ((js (import-binding tgt source :named)))
                      (unless (carried-import-p js source) (push js names))))
                   (t (tfail form "import: bad target ~S" tgt))))
           (cond (names (emit "import ~@[~A, ~]{ ~A } from ~A;" default (join (nreverse names)) (js-string source)))
                 (default (emit "import ~A from ~A;" default (js-string source))))))
        (t (tfail form "import: bad :targets")))))))

;;; ------------------------------------------------------------------ inline aliases across modules
;;;
;;; A module that declaims a function INLINE and exports it publishes a compile-time alias:
;;;
;;;   ;; chart-aliases.client.lisp
;;;   (import :from "chart.js" :targets (Chart))
;;;   (declaim (inline make-chart))
;;;   (export (defun make-chart (canvas config) (new Chart canvas config)))
;;;
;;;   ;; app.client.lisp
;;;   (import :from "chart-aliases" :targets (make-chart))
;;;   (make-chart el cfg)                   ==>  new Chart(el, cfg)
;;;
;;; The importer gets the definition (no runtime import of chart-aliases is emitted) and every
;;; import the alias body relies on (`import { Chart } from "chart.js"` above) is carried over.

(defun inline-lambda-list-symbols (lambda-list)
  (multiple-value-bind (required optional rest) (parse-inline-lambda-list lambda-list)
    (append required (mapcar #'first optional) (and rest (list rest)))))

(defun install-inline-imports (sibling targets form)
  "Install the exported inline aliases of SIBLING named in TARGETS; return the remaining targets."
  (remove-if (lambda (tgt)
               (multiple-value-bind (name local)
                   (cond ((and (symbolp tgt) tgt (not (keywordp tgt))) (values (symbol-name tgt) (symbol-name tgt)))
                         ((and (consp tgt) (eq (first tgt) :as) (symbolp (second tgt)) (symbolp (third tgt)))
                          (values (symbol-name (second tgt)) (symbol-name (third tgt)))))
                 (cond ((null name) nil)
                       ((member name (module-exported-inlines sibling) :test #'string=)
                        (install-inline sibling name local form)
                        t)
                       ((gethash name (module-inlines sibling))
                        (tfail form "import: ~A is an inline alias that module ~A does not export (wrap its defun in (export ...))"
                               (string-downcase name) (module-name sibling)))
                       (t nil))))
             targets))

(defun install-inline (sibling name local form)
  "Copy SIBLING's inline alias NAME into the current module as LOCAL, with its dependencies."
  (let ((definition (gethash name (module-inlines sibling)))
        (existing (gethash local (module-inlines *module*))))
    (cond ((eq existing definition) nil)
          (t (when (and existing (not (string= name local)))
               (tfail form "import: ~A is already an inline alias in this module" (string-downcase local)))
             (setf (gethash local (module-inlines *module*)) definition)
             (carry-inline-dependencies sibling name definition form)))))

(defun carry-inline-dependencies (sibling name definition form)
  "Make everything the alias body refers to in SIBLING available here: other inline aliases are
installed, the sibling's imports are re-imported and its exported definitions are imported from it."
  (destructuring-bind (lambda-list . body) definition
    (let ((params (inline-lambda-list-symbols lambda-list))
          (seen '()))
      (labels ((walk (x)
                 (cond ((and (symbolp x) x (not (eq x t)) (not (keywordp x)))
                        (unless (or (member x params) (member x seen))
                          (push x seen)
                          (carry-symbol sibling name x form)))
                       ((consp x)
                        ;; (.. obj prop (method args)): only OBJ and the call arguments are references
                        (if (sym-is (car x) "..")
                            (progn (walk (second x))
                                   (dolist (step (cddr x))
                                     (cond ((atom step))
                                           ((eq (car step) :?) (when (consp (second step)) (walk (cdr (second step)))))
                                           (t (walk (cdr step))))))
                            (progn (walk (car x)) (walk (cdr x)))))
                       ((and (vectorp x) (not (stringp x))) (map nil #'walk x)))))
        (walk body)))))

(defun carry-symbol (sibling alias sym form)
  ;; matched by JS name: `THREE` and `THREE.Vector3` read as differently-cased symbols
  ;; (readtable-case :invert) but refer to the same binding
  (let* ((name (symbol-name sym))
         (js (mangle-name sym))
         (dot (position #\. js))
         (head (if (and dot (> dot 0)) (subseq js 0 dot) js))
         (origin (loop for o being the hash-values of (module-import-origins sibling)
                       when (string= (getf o :js) head) return o)))
    (cond
      ((and (not dot) (string/= name alias) (gethash name (module-inlines sibling)))
       (install-inline sibling name name form))
      (origin
       (unless (string= (getf origin :source) (config-stubs-module *config*)) ; server calls import themselves
         (carry-import (getf origin :kind) (getf origin :js) (getf origin :original) (getf origin :source) form)))
      ((or (loop for v being the hash-values of (module-globals sibling) thereis (string= v head))
           (loop for f being the hash-values of (module-functions sibling) thereis (string= (fn-info-js-name f) head)))
       (if (member head (module-exports sibling) :test #'string=)
           (carry-import :named head head (format nil "./~A" (module-name sibling)) form)
           (twarn form "inline alias ~A uses ~A, which module ~A does not export"
                  (string-downcase alias) head (module-name sibling)))))))

(defun carry-import (kind js original source form)
  "Import JS from SOURCE into the current module unless it is already bound to the same thing."
  (let ((carried (find js (module-carried-imports *module*) :key #'car :test #'string=))
        (own (loop for o being the hash-values of (module-import-origins *module*)
                   when (string= (getf o :js) js) return o)))
    (cond (carried
           (unless (string= (cdr carried) source)
             (tfail form "import: ~A is needed from both ~S and ~S by imported inline aliases" js (cdr carried) source)))
          (own
           (unless (string= (getf own :source) source)
             (tfail form "import: an imported inline alias needs ~A from ~S, but this module imports ~A from ~S"
                    js source js (getf own :source))))
          ((or (loop for v being the hash-values of (module-globals *module*) thereis (string= v js))
               (loop for f being the hash-values of (module-functions *module*) thereis (string= (fn-info-js-name f) js)))
           (tfail form "import: an imported inline alias needs ~A from ~S, but this module defines its own ~A" js source js))
          (t
           (push (cons js source) (module-carried-imports *module*))
           (emit "~A" (import-line kind js original source))))))

(defun inline-declared-p (name)
  (and *module* (symbolp name) (member name (module-inline-names *module*))))

(defun compile-export (form env dest default)
  (let ((target (second form)))
    (cond
      ((and (consp target) (sym-is (car target) "DEFUN") (inline-declared-p (second target)))
       ;; an inline alias has no runtime value: exporting it lets sibling modules import the alias
       (when default (tfail form "export-default: inline alias ~A has no runtime value to export" (string-downcase (second target))))
       (compile-defun target env :void)
       (pushnew (symbol-name (second target)) (module-exported-inlines *module*) :test #'string=))
      ((and (consp target) (sym-is (car target) "DEFUN"))
       (if default
           (progn (compile-defun target env :void) (emit "export default ~A;" (mangle-name (second target))))
           (compile-defun target env :void :export t))
       (push (mangle-name (second target)) (module-exports *module*)))
      ((and (consp target) (sym-is (car target) "ASYNC") (consp (second target)) (sym-is (car (second target)) "DEFUN"))
       (if default
           (progn (compile-defun (second target) env :void :async t) (emit "export default ~A;" (mangle-name (second (second target)))))
           (compile-defun (second target) env :void :async t :export t))
       (push (mangle-name (second (second target))) (module-exports *module*)))
      ((and (consp target) (member (sym-name (car target)) '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT") :test #'string=))
       (compile-global-definition target env :void :export (not default)
                                  :keyword (cond ((sym-is (car target) "DEFVAR") "var")
                                                 ((sym-is (car target) "DEFPARAMETER") "let")
                                                 (t "const")))
       (when default (emit "export default ~A;" (mangle-name (second target))))
       (push (mangle-name (second target)) (module-exports *module*)))
      ((and (consp target) (sym-is (car target) "DEFSTRUCT") (not default))
       (compile-defstruct target env t)
       (push (mangle-type-name (if (consp (second target)) (first (second target)) (second target))) (module-exports *module*)))
      ((and (not default) (every #'symbolp (cdr form)))
       (let ((values '()))
         (dolist (s (cdr form))
           (if (gethash (symbol-name s) (module-inlines *module*))
               (pushnew (symbol-name s) (module-exported-inlines *module*) :test #'string=)
               (push s values)))
         (when values
           (setf values (nreverse values))
           (emit "export { ~A };" (join (mapcar (lambda (s) (or (lookup-var s env) (mangle-name s))) values)))
           (dolist (s values) (push (mangle-name s) (module-exports *module*))))))
      ((and default (symbolp target))
       (emit "export default ~A;" (or (lookup-var target env) (mangle-name target))))
      (t (tfail form "~A can only target defun, defvar/defparameter/defconstant~:[, defstruct~;~] forms or names"
                (if default "export-default" "export") default)))))

(defstmt "EXPORT" (form env dest) (compile-export form env dest nil))
(defstmt "EXPORT-DEFAULT" (form env dest) (compile-export form env dest t))

;;; ------------------------------------------------------------------ JSX

(defexpr "AS-HTML-STRING" (form env) (compile-as-html-string form env))
(defexpr "FRAGMENT" (form env)
  (compile-as-html-string (cons (car form) (list (cons :<> (cdr form)))) env))

;;; ------------------------------------------------------------------ server calls (alignment)

(defun manifest-function (symbol)
  "Manifest plist for a server function named like SYMBOL, or NIL."
  (let ((manifest (config-manifest *config*)))
    (and manifest (symbolp symbol) (not (keywordp symbol))
         (find (symbol-name symbol) (getf manifest :functions)
               :key (lambda (f) (symbol-name (getf f :name))) :test #'string=))))

(defun compile-server-call (form env fn)
  (check-server-call form fn env)
  (module-note-server-call *module* (getf fn :rpc-name) (getf fn :ts-name))
  (format nil "~A(~A)" (getf fn :ts-name) (compile-args (cdr form) env)))

(defexpr "SERVER-CALL" (form env)
  (destructuring-bind (name &rest args) (cdr form)
    (format nil "rpcCall(~A, [~A])" (cx name env) (compile-args args env))))
