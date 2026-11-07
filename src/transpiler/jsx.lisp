;;;; src/transpiler/jsx.lisp -- Spinneret-style (:tag :attr value ... children) -> JSX
;;;;
;;;; (as-html-string (:div :class "x" (:h1 :id "big" "hi") (map (p items) (:p :key p p))))
;;;;   => (<div className="x"><h1 id="big">hi</h1>{items.map((p) => (<p key={p}>{p}</p>))}</div>)

(in-package :monolisp.transpiler)

(defparameter *jsx-attribute-renames*
  '(("class" . "className") ("for" . "htmlFor") ("tabindex" . "tabIndex") ("readonly" . "readOnly")
    ("maxlength" . "maxLength") ("colspan" . "colSpan") ("rowspan" . "rowSpan") ("onclick" . "onClick")
    ("onchange" . "onChange") ("oninput" . "onInput") ("onsubmit" . "onSubmit")))

(defun jsx-tag-name (key)
  (let ((name (property-name key)))
    (cond ((member name '("<>" "fragment" "Fragment") :test #'string=) "")
          (t name))))

(defun jsx-element-form-p (form env)
  (and (env-jsx-p env) (consp form) (keywordp (car form))
       (not (member (car form) '(:? :const)))))

(defun jsx-attribute-name (key)
  (let ((name (property-name key)))
    (or (cdr (assoc name *jsx-attribute-renames* :test #'string=)) name)))

(defun jsx-safe-text-p (text)
  (and (plusp (length text))
       (notany (lambda (c) (member c '(#\{ #\} #\< #\> #\& #\" #\\ #\Newline #\Return))) text)
       (not (member (char text 0) '(#\Space #\Tab)))
       (not (member (char text (1- (length text))) '(#\Space #\Tab)))))

(defun compile-jsx-attribute-value (value env)
  (cond ((and (stringp value) (notany (lambda (c) (member c '(#\" #\\ #\Newline #\Return #\{ #\}))) value))
         (format nil "\"~A\"" value))
        (t (format nil "{~A}" (strip-parens (cx value env))))))

(defun compile-jsx-child (form env)
  (cond ((stringp form)
         (if (jsx-safe-text-p form) form (format nil "{~A}" (js-string form))))
        ((jsx-element-form-p form env) (compile-jsx form env))
        ((and (consp form) (sym-is (car form) "FOR-EACH"))
         (tfail form "for-each is for side effects and yields no children; use (map (x xs) ...) to render a list"))
        ((and (consp form) (sym-is (car form) "MAP") (iteration-binding-p form))
         ;; (map (x xs [i]) body...) as a child means {xs.map((x, i) => body)}
         (destructuring-bind ((var seq &optional index-var) &rest body) (cdr form)
           (multiple-value-bind (js-var env2) (bind-var var env)
             (multiple-value-bind (js-idx env3) (if index-var (bind-var index-var env2) (values nil env2))
               (format nil "{~A.map((~A: ~A~@[, ~A: number~]) => ~A)}"
                       (cx seq env) js-var (if (lookup-type var env3) (type->ts (lookup-type var env3)) "any") js-idx
                       (if (= (length body) 1)
                           (paren (strip-parens (cx (first body) env3)))
                           (cx (cons (cl-sym "PROGN") body) env3)))))))
        ((and (consp form) (sym-is (car form) "PROGN"))
         (format nil "~{~A~}" (mapcar (lambda (f) (compile-jsx-child f env)) (cdr form))))
        (t (format nil "{~A}" (strip-parens (cx form env))))))

(defun compile-jsx (form env)
  "FORM is (:tag [:attr value]* child*)."
  (let* ((env (copy-env env :jsx-p t))
         (tag (jsx-tag-name (car form)))
         (rest (cdr form))
         (attrs '()))
    (loop while (and rest (keywordp (first rest)))
          do (let ((key (pop rest)))
               (cond (rest
                      (let ((value (pop rest)))
                        (push (cond ((eq value t) (jsx-attribute-name key))
                                    ((and (consp value) (sym-is (car value) "SPREAD"))
                                     (format nil "{...~A}" (cx (second value) env)))
                                    (t (format nil "~A=~A" (jsx-attribute-name key) (compile-jsx-attribute-value value env))))
                              attrs)))
                     (t (push (jsx-attribute-name key) attrs)))))
    (let ((children (mapcar (lambda (c) (compile-jsx-child c env)) rest)))
      (if (and (null children) (plusp (length tag)))
          (format nil "<~A~{ ~A~} />" tag (nreverse attrs))
          (format nil "<~A~{ ~A~}>~{~A~}</~A>" tag (nreverse attrs) children tag)))))

(defun compile-as-html-string (form env)
  "(as-html-string child*) -> a JSX expression (fragment when several children)."
  (let* ((env (copy-env env :jsx-p t))
         (children (cdr form)))
    (unless (module-tsx-p *module*)
      (tfail form "as-html-string requires a .clientx.lisp module (TSX output)"))
    (setf (module-react-used *module*) t)
    (paren (cond ((null children) "null")
                 ((and (= (length children) 1) (jsx-element-form-p (first children) env))
                  (compile-jsx (first children) env))
                 ((= (length children) 1) (strip-parens (cx (first children) env)))
                 (t (format nil "<>~{~A~}</>" (mapcar (lambda (c) (compile-jsx-child c env)) children)))))))
