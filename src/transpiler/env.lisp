;;;; src/transpiler/env.lisp -- module state and lexical environments

(in-package :monolisp.transpiler)

;;; ------------------------------------------------------------------ configuration

(defstruct (transpile-config (:conc-name config-))
  (react-p t)                       ; auto-import React in .clientx (tsx) modules
  (react-import "react")
  (manifest nil)                    ; server manifest plist (see SERVER-MANIFEST) or NIL
  (strict-server-calls t)           ; signal on arity/type mismatch against the manifest
  (stubs-module "./server-functions")
  (module-names '())                ; basenames of sibling client modules (for ./ import resolution)
  (indent-width 2))

(defvar *config* (make-transpile-config))

;;; ------------------------------------------------------------------ module

(defstruct struct-info
  name ts-name include fields)      ; fields: list of (name type default)

(defstruct fn-info
  name js-name params return-type async-p exported-p)

(defstruct module
  name
  (tsx-p nil)
  package
  (imports '())                     ; strings already emitted
  (exports '())                     ; js names exported
  (globals (make-hash-table :test 'eq))      ; symbol -> js-name (defvar/defparameter/import/defun)
  (global-types (make-hash-table :test 'eq)) ; symbol -> type spec (declaim type)
  (functions (make-hash-table :test 'eq))    ; symbol -> fn-info
  (lambda-lists (make-hash-table :test 'eq)) ; symbol -> lambda list of every top-level defun (pre-scanned)
  (ftypes (make-hash-table :test 'eq))       ; symbol -> (param-types . return-type)
  (inlines (make-hash-table :test 'equal))   ; NAME string -> (lambda-list . body)
  (inline-names '())                         ; symbols declaimed inline (awaiting a defun)
  (exported-inlines '())                     ; NAME strings of inline aliases other modules may import
  (import-origins (make-hash-table :test 'eq)) ; symbol -> (:source s :kind :named|:default|:namespace|:renamed :name orig)
  (carried-imports '())                      ; (js-name . source) imported on behalf of imported inline aliases
  (macros '())                               ; symbols with host macro definitions
  (structs (make-hash-table :test 'eq))      ; symbol -> struct-info
  (struct-accessors (make-hash-table :test 'equal)) ; NAME string -> (struct-info . field) | (struct-info . :make) ...
  (server-calls '())                         ; (rpc-name . js-name) referenced
  (server-imports '())                       ; symbols explicitly imported from :server
  (react-used nil)
  (warnings '())
  (body "")                                  ; compiled statements
  (output ""))

(defvar *module* nil)

(defvar *project-modules* (make-hash-table :test 'equal)
  "Module name -> compiled MODULE for the project being transpiled (dependencies compile first),
so importers can see a sibling's exported inline aliases.")

(defun module-note-server-call (module rpc-name js-name)
  (pushnew (cons rpc-name js-name) (module-server-calls module) :test #'equal))

;;; ------------------------------------------------------------------ lexical environment

(defstruct (env (:copier %copy-env))
  (vars '())        ; alist (symbol . js-name), innermost first
  (types '())       ; alist (symbol . type-spec)
  (consts '())      ; symbols bound with :const
  (blocks '())      ; list of (name kind label result-var)  kind: :function | :block | :loop
  (async-p nil)
  (function nil)    ; symbol of the enclosing defun/lambda
  (jsx-p nil)
  (labels '())      ; local functions (flet/labels): alist (symbol . js-name)
  (scope (list nil)) ; mutable cell: JS names declared in the current JS block (shared by siblings)
  (counter (list 0)))

(defun copy-env (env &rest overrides &key &allow-other-keys)
  (let ((new (%copy-env env)))
    (loop for (k v) on overrides by #'cddr
          do (ecase k
               (:vars (setf (env-vars new) v))
               (:types (setf (env-types new) v))
               (:consts (setf (env-consts new) v))
               (:blocks (setf (env-blocks new) v))
               (:async-p (setf (env-async-p new) v))
               (:function (setf (env-function new) v))
               (:jsx-p (setf (env-jsx-p new) v))
               (:labels (setf (env-labels new) v))))
    new))

(defun gensym-js (env &optional (base "tmp"))
  (format nil "$~A~D" base (incf (car (env-counter env)))))

(defun js-name-taken-p (js-name env)
  (or (rassoc js-name (env-vars env) :test #'string=)
      (rassoc js-name (env-labels env) :test #'string=)
      (member js-name (car (env-scope env)) :test #'string=)
      (and *module*
           (or (loop for v being the hash-values of (module-globals *module*) thereis (string= v js-name))
               (loop for f being the hash-values of (module-functions *module*) thereis (string= (fn-info-js-name f) js-name))))))

(defun bind-var (symbol env &key const)
  "Bind SYMBOL to a fresh JS identifier.  Returns (values js-name new-env)."
  (let* ((base (mangle-name symbol))
         (base (if (find #\. base) (substitute #\_ #\. base) base))
         (js base))
    (loop for i from 2 while (js-name-taken-p js env)
          do (setf js (format nil "~A_~D" base i)))
    (push js (car (env-scope env)))
    (values js
            (copy-env env
                      :vars (acons symbol js (env-vars env))
                      :consts (if const (cons symbol (env-consts env)) (env-consts env))))))

(defun bind-type (symbol type env)
  (copy-env env :types (acons symbol type (env-types env))))

(defun lookup-var (symbol env)
  "JS name for SYMBOL: lexical, local function, module global/function, else NIL."
  (or (cdr (assoc symbol (env-vars env)))
      (cdr (assoc symbol (env-labels env)))
      (and *module* (gethash symbol (module-globals *module*)))
      (and *module* (let ((f (gethash symbol (module-functions *module*)))) (and f (fn-info-js-name f))))))

(defun lookup-type (symbol env)
  (or (cdr (assoc symbol (env-types env)))
      (and *module* (gethash symbol (module-global-types *module*)))))

(defun const-p (symbol env) (member symbol (env-consts env)))

(defun push-block (env name kind &optional label result-var)
  (copy-env env :blocks (cons (list name kind label result-var) (env-blocks env))))

(defun find-block (env name)
  "Innermost BLOCK named NAME (symbols compared by name).  Loop records (kind :LOOP) are only
targets for %BREAK/%CONTINUE, never for RETURN-FROM."
  (find-if (lambda (b)
             (let ((bname (first b)))
               (and (not (eq (second b) :loop))
                    (if (null name)
                        (null bname)
                        (and bname (string= (symbol-name bname) (symbol-name name)))))))
           (env-blocks env)))

(defun find-loop (env)
  (find-if (lambda (b) (eq (second b) :loop)) (env-blocks env)))

;;; ------------------------------------------------------------------ declarations

(defun split-body-declarations (body)
  "Returns (values declarations docstring forms)."
  (let ((decls '()) (doc nil))
    (loop
      (cond ((and (consp (car body)) (sym-is (caar body) "DECLARE"))
             (setf decls (append decls (cdar body)))
             (pop body))
            ((and (stringp (car body)) (cdr body) (null doc))
             (setf doc (pop body)))
            (t (return))))
    (values decls doc body)))

(defun declared-types (declarations)
  "Alist (symbol . type) from (type T a b) / (T a b) / (string x) style declarations."
  (let ((out '()))
    (dolist (d declarations out)
      (when (consp d)
        (let ((head (car d)))
          (cond ((sym-is head "TYPE")
                 (dolist (v (cddr d)) (push (cons v (second d)) out)))
                ((member (sym-name head) '("IGNORE" "IGNORABLE" "OPTIMIZE" "DYNAMIC-EXTENT" "SPECIAL" "INLINE" "NOTINLINE" "FTYPE" "RETURNS" "VALUES") :test #'string=)
                 nil)
                (t ;; (string x y) shorthand
                 (dolist (v (cdr d)) (push (cons v head) out)))))))))

(defun declared-ftypes (declarations)
  "Alist (fname . (param-types . return-type)) from (ftype (function (A B) R) f ...)."
  (let ((out '()))
    (dolist (d declarations out)
      (when (and (consp d) (sym-is (car d) "FTYPE") (consp (second d)) (sym-is (car (second d)) "FUNCTION"))
        (let ((params (second (second d))) (ret (third (second d))))
          (dolist (f (cddr d)) (push (cons f (cons (if (eq params '*) nil params) ret)) out)))))))

(defun declared-returns (declarations)
  "(declare (returns T)) / (declare (values T)) -> T"
  (dolist (d declarations nil)
    (when (and (consp d) (or (sym-is (car d) "RETURNS") (sym-is (car d) "VALUES")))
      (return (if (and (sym-is (car d) "VALUES") (cddr d)) (cons (car d) (cdr d)) (second d))))))
