;;;; src/async/async-macros.lisp -- `async` / `await` via a compile-time CPS transform,
;;;; plus cl-async style chaining macros (alet, alet*, chain, with-timeout, ...)
;;;;
;;;; (async BODY...) returns a promise.  Inside BODY, (await X) suspends until the promise X
;;;; settles; a rejection is re-signalled at the await point and can be caught with HANDLER-CASE.
;;;; Supported forms around an await: progn, let, let*, if, when, unless, cond, case, and, or,
;;;; setq, setf, the, multiple-value-bind, block/return-from, handler-case, unwind-protect,
;;;; dolist, dotimes, a `loop` subset, function calls (arguments are evaluated left to right),
;;;; flet/labels/locally (await only in the body) and any macro that expands into those.
;;;; Code without an await is emitted untouched, so there is no cost for synchronous sections.

(in-package :monolisp.async)

;;; ------------------------------------------------------------------ runtime support

(defun await (promise)
  "Only meaningful inside ASYNC; signals if evaluated as a normal function."
  (declare (ignore promise))
  (error "AWAIT used outside of an ASYNC body (or inside a form the CPS transform does not support)"))

(defmacro %guard (rk thunk)
  "Run THUNK; route errors to the rejection continuation RK."
  `(handler-case (funcall ,thunk)
     (error (e) (funcall ,rk e))))

(defvar *trampoline* nil)

(defun %bounce (thunk)
  "Run THUNK outside the current dynamic extent: on the loop's next tick when a loop is current,
otherwise through a trampoline so nested handler-case guards are unwound first."
  (declare (type function thunk))
  (cond (*loop* (loop-post *loop* thunk))
        (*trampoline* (sb-concurrency:enqueue thunk *trampoline*))
        (t (let ((*trampoline* (sb-concurrency:make-queue)))
             (funcall thunk)
             (loop
               (multiple-value-bind (next ok) (sb-concurrency:dequeue *trampoline*)
                 (unless ok (return))
                 (funcall next))))))
  nil)

;;; ------------------------------------------------------------------ cancellation

(define-condition cancelled-error (error)
  ((token :initarg :token :reader cancelled-error-token))
  (:report (lambda (c s) (declare (ignore c)) (format s "operation cancelled"))))

(defstruct (cancellation-token (:constructor %make-cancellation-token))
  (cancelled nil)
  (promise (%make-promise)))

(defun make-cancellation-token () (%make-cancellation-token))

(defun cancel (token)
  (unless (cancellation-token-cancelled token)
    (setf (cancellation-token-cancelled token) t)
    (reject (cancellation-token-promise token) (make-condition 'cancelled-error :token token))
    t))

(defun cancelled-p (token) (cancellation-token-cancelled token))

(defun check-cancelled (token)
  (when (cancellation-token-cancelled token)
    (error 'cancelled-error :token token)))

;;; ------------------------------------------------------------------ CPS transform

(defvar *async-rk* nil "Symbol naming the variable that holds the current rejection continuation.")
(defvar *async-blocks* '() "Alist of block-name -> (k-symbol rk-symbol) for RETURN-FROM.")

(defparameter *opaque-operators* '(quote function lambda async async-lambda define-async)
  "Forms whose interior is not scanned/transformed.")

(defun %op-is (form name)
  (and (consp form) (symbolp (car form)) (string= (symbol-name (car form)) (string name))))

(defun needs-cps-p (form)
  "Does FORM contain an AWAIT or a RETURN-FROM to an async block, outside opaque sub-forms?"
  (cond ((atom form) nil)
        ((member (car form) *opaque-operators*) nil)
        ((eq (car form) 'await) t)
        ((and (eq (car form) 'return-from) (assoc (second form) *async-blocks*)) t)
        ((and (eq (car form) 'return) (assoc nil *async-blocks*)) t)
        ((eq (car form) 'loop) (some #'needs-cps-p (cdr form)))
        ((and (symbolp (car form))
              (not (member (car form) '(progn let let* if when unless cond case and or setq setf the
                                        multiple-value-bind block return-from return handler-case
                                        unwind-protect dolist dotimes flet labels locally declare
                                        async-handler-case async-dolist async-dotimes async-loop)))
              (macro-function (car form)))
         (needs-cps-p (macroexpand-1 form)))
        (t (loop for sub on form
                 thereis (or (needs-cps-p (car sub))
                             ;; dotted tails
                             (and (atom (cdr sub)) nil))))))

(defun k-apply (k value-form)
  "Apply continuation K (a symbol or (lambda (v) ...) form) to VALUE-FORM."
  (cond ((symbolp k) `(funcall ,k ,value-form))
        ((and (consp k) (eq (car k) 'lambda))
         (destructuring-bind (lambda (var) &rest body) k
           (declare (ignore lambda))
           (if (and (symbolp value-form) (not (null value-form)))
               `(let ((,var ,value-form)) (declare (ignorable ,var)) ,@body)
               `(let ((,var ,value-form)) (declare (ignorable ,var)) ,@body))))
        (t (error "bad continuation ~S" k))))

(defmacro with-shared-k ((ks k) &body body)
  "Ensure K is a symbol so it can be referenced several times without duplicating code."
  (let ((code (gensym "CODE")))
    `(if (symbolp ,k)
         (let ((,ks ,k)) ,@body)
         (let ((,ks (gensym "K")))
           (let ((,code (progn ,@body)))
             `(let ((,,ks ,,k)) (declare (ignorable ,,ks)) ,,code))))))

(defun guarded (rk code)
  `(%guard ,rk (lambda () ,code)))

(defun bounce-to (rk code)
  `(%bounce (lambda () ,(guarded rk code))))

(defun cps (form k)
  (cond
    ((not (needs-cps-p form)) (k-apply k form))
    ((atom form) (k-apply k form))
    (t
     (let ((op (car form)))
       (case op
         (await (cps-await (second form) k))
         (progn (cps-progn (cdr form) k))
         (locally (cps-progn (cdr form) k))
         (let (cps-let form k))
         (let* (cps-let* form k))
         (if (cps-if (second form) (third form) (fourth form) k))
         (when (cps-if (second form) `(progn ,@(cddr form)) nil k))
         (unless (cps-if (second form) nil `(progn ,@(cddr form)) k))
         (cond (cps-cond (cdr form) k))
         (case (cps-case form k))
         (and (cps-and (cdr form) k))
         (or (cps-or (cdr form) k))
         (setq (cps-setq (cdr form) k))
         (setf (cps-setf (cdr form) k))
         (the (cps (third form) k))
         (multiple-value-bind (cps-mvb form k))
         (block (cps-block (second form) (cddr form) k))
         (return-from (cps-return-from (second form) (third form)))
         (return (cps-return-from nil (second form)))
         ((handler-case async-handler-case) (cps-handler-case (second form) (cddr form) k))
         (unwind-protect (cps-unwind-protect (second form) (cddr form) k))
         ((dolist async-dolist) (cps-dolist form k))
         ((dotimes async-dotimes) (cps-dotimes form k))
         ((loop async-loop) (cps-loop (cdr form) k))
         ((flet labels)
          (when (some #'needs-cps-p (mapcar #'cddr (second form)))
            (error "AWAIT inside a local function definition is not supported; use ASYNC-LAMBDA"))
          `(,op ,(second form) ,(cps-progn (cddr form) k)))
         (t
          (cond ((member op *opaque-operators*) (k-apply k form))
                ((and (symbolp op) (macro-function op)) (cps (macroexpand-1 form) k))
                ((and (symbolp op) (special-operator-p op))
                 (error "AWAIT is not supported inside ~S forms" op))
                (t (cps-call form k)))))))))

(defun cps-await (expr k)
  (let ((v (gensym "V")) (rk *async-rk*))
    (cps expr
         `(lambda (,v)
            (then (%as-promise ,v)
                  (lambda (,v) ,(guarded rk (k-apply k v)))
                  ,rk)))))

(defun split-declarations (body)
  (loop for rest on body
        while (and (consp (car rest)) (eq (caar rest) 'declare))
        collect (car rest) into decls
        finally (return (values decls rest))))

(defun cps-progn (forms k)
  (cond ((null forms) (k-apply k nil))
        ((null (cdr forms)) (cps (car forms) k))
        ((not (needs-cps-p (car forms)))
         `(progn ,(car forms) ,(cps-progn (cdr forms) k)))
        (t (let ((ignored (gensym "_")))
             (cps (car forms) `(lambda (,ignored) ,(cps-progn (cdr forms) k)))))))

(defun cps-sequence (forms kl)
  "Evaluate FORMS in order; call KL (a Lisp function) with a list of value expressions."
  (labels ((rec (forms acc)
             (if (null forms)
                 (funcall kl (reverse acc))
                 (let ((f (car forms)))
                   (cond ((or (constantp f) (keywordp f))
                          (rec (cdr forms) (cons f acc)))
                         ((needs-cps-p f)
                          (let ((tmp (gensym "T")))
                            (cps f `(lambda (,tmp) ,(rec (cdr forms) (cons tmp acc))))))
                         (t
                          (let ((tmp (gensym "T")))
                            `(let ((,tmp ,f)) ,(rec (cdr forms) (cons tmp acc))))))))))
    (rec forms '())))

(defun cps-call (form k)
  (cps-sequence (cdr form) (lambda (vals) (k-apply k `(,(car form) ,@vals)))))

(defun cps-let (form k)
  (destructuring-bind (bindings &rest body) (cdr form)
    (let* ((names (mapcar (lambda (b) (if (consp b) (first b) b)) bindings))
           (inits (mapcar (lambda (b) (if (consp b) (second b) nil)) bindings)))
      (multiple-value-bind (decls forms) (split-declarations body)
        (cps-sequence inits
                      (lambda (vals)
                        `(let ,(mapcar #'list names vals)
                           ,@decls
                           ,(cps-progn forms k))))))))

(defun cps-let* (form k)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (decls forms) (split-declarations body)
      (if (null bindings)
          (if decls `(locally ,@decls ,(cps-progn forms k)) (cps-progn forms k))
          (let* ((b (car bindings))
                 (name (if (consp b) (first b) b))
                 (init (if (consp b) (second b) nil))
                 (rest `(let* ,(cdr bindings) ,@decls ,@forms)))
            (if (needs-cps-p init)
                (cps init `(lambda (,name) ,(cps rest k)))
                `(let ((,name ,init)) ,(cps rest k))))))))

(defun cps-if (test then else k)
  (with-shared-k (ks k)
    (let ((tv (gensym "TEST")))
      (cps test `(lambda (,tv) (if ,tv ,(cps then ks) ,(cps else ks)))))))

(defun cps-cond (clauses k)
  (cond ((null clauses) (k-apply k nil))
        (t (let ((clause (car clauses)))
             (cond ((member (car clause) '(t otherwise))
                    (cps-progn (cdr clause) k))
                   ((null (cdr clause))
                    ;; (cond (test) ...) returns test value
                    (with-shared-k (ks k)
                      (let ((tv (gensym "TEST")))
                        (cps (car clause) `(lambda (,tv) (if ,tv ,(k-apply ks tv) ,(cps-cond (cdr clauses) ks)))))))
                   (t (cps-if (car clause) `(progn ,@(cdr clause)) `(cond ,@(cdr clauses)) k)))))))

(defun cps-case (form k)
  (destructuring-bind (keyform &rest clauses) (cdr form)
    (let ((kv (gensym "KEY")))
      (cps keyform
           `(lambda (,kv)
              ,(cps `(cond ,@(mapcar (lambda (clause)
                                       (destructuring-bind (keys &rest body) clause
                                         (cond ((member keys '(t otherwise)) `(t ,@body))
                                               ((listp keys) `((member ,kv ',keys) ,@body))
                                               (t `((eql ,kv ',keys) ,@body)))))
                                     clauses))
                    k))))))

(defun cps-and (forms k)
  (cond ((null forms) (k-apply k t))
        ((null (cdr forms)) (cps (car forms) k))
        (t (with-shared-k (ks k)
             (let ((v (gensym "V")))
               (cps (car forms) `(lambda (,v) (if ,v ,(cps-and (cdr forms) ks) ,(k-apply ks nil)))))))))

(defun cps-or (forms k)
  (cond ((null forms) (k-apply k nil))
        ((null (cdr forms)) (cps (car forms) k))
        (t (with-shared-k (ks k)
             (let ((v (gensym "V")))
               (cps (car forms) `(lambda (,v) (if ,v ,(k-apply ks v) ,(cps-or (cdr forms) ks)))))))))

(defun cps-setq (pairs k)
  (let ((vars (loop for (var nil) on pairs by #'cddr collect var))
        (vals (loop for (nil val) on pairs by #'cddr collect val)))
    (cps-sequence vals
                  (lambda (tmps)
                    (k-apply k `(progn ,@(mapcar (lambda (var tmp) `(setq ,var ,tmp)) vars tmps)))))))

(defun cps-setf (pairs k)
  (let ((places (loop for (place nil) on pairs by #'cddr collect place))
        (vals (loop for (nil val) on pairs by #'cddr collect val)))
    (when (some #'needs-cps-p places)
      (error "AWAIT inside a SETF place is not supported"))
    (cps-sequence vals
                  (lambda (tmps)
                    (k-apply k `(progn ,@(mapcar (lambda (place tmp) `(setf ,place ,tmp)) places tmps)))))))

(defun cps-mvb (form k)
  (destructuring-bind (vars values-form &rest body) (cdr form)
    (multiple-value-bind (decls forms) (split-declarations body)
      (if (needs-cps-p values-form)
          (let ((tmp (gensym "MV")))
            (cps values-form
                 `(lambda (,tmp)
                    (multiple-value-bind ,vars (values ,tmp)
                      ,@decls
                      (declare (ignorable ,@vars))
                      ,(cps-progn forms k)))))
          `(multiple-value-bind ,vars ,values-form ,@decls ,(cps-progn forms k))))))

(defun cps-block (name body k)
  (with-shared-k (ks k)
    (let ((*async-blocks* (acons name (list ks *async-rk*) *async-blocks*)))
      (cps-progn body ks))))

(defun cps-return-from (name value-form)
  (let ((entry (cdr (assoc name *async-blocks*))))
    (unless entry (error "RETURN-FROM ~S: no enclosing async block" name))
    (destructuring-bind (ks rk) entry
      (let ((v (gensym "RET")))
        (cps value-form `(lambda (,v) ,(bounce-to rk `(funcall ,ks ,v))))))))

(defun cps-handler-case (body clauses k)
  (with-shared-k (ks k)
    (let* ((outer-rk *async-rk*)
           (rk2 (gensym "RK"))
           (r (gensym "REASON"))
           (v (gensym "V")))
      (when (assoc :no-error clauses)
        (error ":NO-ERROR clauses are not supported in async HANDLER-CASE"))
      `(let ((,rk2 (lambda (,r)
                     ,(guarded outer-rk
                               `(typecase ,r
                                  ,@(mapcar (lambda (clause)
                                              (destructuring-bind (type lambda-list &rest cbody) clause
                                                `(,type ,(if lambda-list
                                                             `(let ((,(first lambda-list) ,r))
                                                                (declare (ignorable ,(first lambda-list)))
                                                                ,(cps-progn cbody ks))
                                                             (cps-progn cbody ks)))))
                                            clauses)
                                  (t (funcall ,outer-rk ,r)))))))
         ,(guarded rk2
                   (let ((*async-rk* rk2))
                     (cps body `(lambda (,v) ,(bounce-to outer-rk `(funcall ,ks ,v))))))))))

(defun cps-unwind-protect (protected cleanup k)
  (with-shared-k (ks k)
    (let* ((outer-rk *async-rk*)
           (rk2 (gensym "RK"))
           (r (gensym "REASON"))
           (v (gensym "V"))
           (ignored (gensym "_")))
      `(let ((,rk2 (lambda (,r)
                     ,(guarded outer-rk
                               (cps-progn cleanup `(lambda (,ignored) (funcall ,outer-rk ,r)))))))
         ,(guarded rk2
                   (let ((*async-rk* rk2))
                     (cps protected
                          `(lambda (,v)
                             ,(bounce-to outer-rk
                                         (cps-progn cleanup `(lambda (,ignored) (funcall ,ks ,v))))))))))))

(defun cps-dolist (form k)
  (destructuring-bind ((var list-form &optional result) &rest body) (cdr form)
    (multiple-value-bind (decls forms) (split-declarations body)
      (with-shared-k (ks k)
        (let ((iter (gensym "ITER")) (rest (gensym "REST")) (rk *async-rk*) (ignored (gensym "_")))
          (let ((*async-blocks* (acons nil (list ks rk) *async-blocks*)))
            `(labels ((,iter (,rest)
                        (if (endp ,rest)
                            (let ((,var nil)) (declare (ignorable ,var)) (funcall ,ks ,result))
                            (let ((,var (car ,rest)))
                              ,@decls
                              ,(cps-progn forms `(lambda (,ignored) ,(bounce-to rk `(,iter (cdr ,rest)))))))))
               (,iter ,list-form))))))))

(defun cps-dotimes (form k)
  (destructuring-bind ((var count-form &optional result) &rest body) (cdr form)
    (multiple-value-bind (decls forms) (split-declarations body)
      (with-shared-k (ks k)
        (let ((iter (gensym "ITER")) (n (gensym "N")) (rk *async-rk*) (ignored (gensym "_")))
          (let ((*async-blocks* (acons nil (list ks rk) *async-blocks*)))
            `(let ((,n ,count-form))
               (labels ((,iter (,var)
                          ,@decls
                          (if (>= ,var ,n)
                              (funcall ,ks ,result)
                              ,(cps-progn forms `(lambda (,ignored) ,(bounce-to rk `(,iter (1+ ,var))))))))
                 (,iter 0)))))))))

;;; A small `loop` subset for async bodies:
;;;   for VAR in LIST | across VECTOR | from A [to|below|downto B] [by S] | = INIT [then STEP]
;;;   repeat N | while TEST | until TEST | do FORMS... | collect FORM | sum FORM | count FORM
;;;   when/unless TEST (do|collect|sum|count) ... | finally (return FORM) | finally FORMS
(defun cps-loop (clauses k)
  (let ((bindings '()) (end-tests '()) (steps '()) (body '()) (result nil)
        (acc nil) (finally '()) (prebody '()))
    (labels ((kw (x name) (and (symbolp x) (string-equal (symbol-name x) name)))
             (ensure-acc () (or acc (setf acc (gensym "ACC"))))
             (parse-action (clauses)
               ;; returns (values form remaining)
               (let ((head (car clauses)))
                 (cond ((kw head "do")
                        (let ((forms '()) (rest (cdr clauses)))
                          (loop while (and rest (consp (car rest))) do (push (pop rest) forms))
                          (values `(progn ,@(reverse forms)) rest)))
                       ((or (kw head "collect") (kw head "collecting"))
                        (ensure-acc) (setf result `(nreverse ,acc))
                        (values `(push ,(second clauses) ,acc) (cddr clauses)))
                       ((or (kw head "sum") (kw head "summing"))
                        (ensure-acc) (setf result acc)
                        (values `(incf ,acc ,(second clauses)) (cddr clauses)))
                       ((or (kw head "count") (kw head "counting"))
                        (ensure-acc) (setf result acc)
                        (values `(when ,(second clauses) (incf ,acc)) (cddr clauses)))
                       ((kw head "return")
                        (values `(return ,(second clauses)) (cddr clauses)))
                       (t (error "async loop: unsupported clause ~S" head))))))
      (loop while clauses
            do (let ((head (pop clauses)))
                 (cond
                   ((consp head) (push head body))      ; simple (loop form...) body
                   ((kw head "for")
                    (let ((var (pop clauses)) (kind (pop clauses)))
                      (cond
                        ((kw kind "in")
                         (let ((rest (gensym "REST")))
                           (push `(,rest ,(pop clauses)) bindings)
                           (push `(,var nil) bindings)
                           (push `(endp ,rest) end-tests)
                           (push `(setq ,var (car ,rest)) prebody)
                           (push `(setq ,rest (cdr ,rest)) steps)))
                        ((kw kind "across")
                         (let ((vec (gensym "VEC")) (i (gensym "I")))
                           (push `(,vec ,(pop clauses)) bindings)
                           (push `(,i 0) bindings)
                           (push `(,var nil) bindings)
                           (push `(>= ,i (length ,vec)) end-tests)
                           (push `(setq ,var (aref ,vec ,i)) prebody)
                           (push `(incf ,i) steps)))
                        ((kw kind "from")
                         (let ((start (pop clauses)) (limit nil) (limit-kind nil) (by 1))
                           (loop while (and clauses (or (kw (car clauses) "to") (kw (car clauses) "below")
                                                        (kw (car clauses) "upto") (kw (car clauses) "downto")
                                                        (kw (car clauses) "by")))
                                 do (let ((w (pop clauses)))
                                      (cond ((kw w "by") (setf by (pop clauses)))
                                            (t (setf limit-kind (string-downcase (symbol-name w))
                                                     limit (pop clauses))))))
                           (push `(,var ,start) bindings)
                           (when limit
                             (let ((lim (gensym "LIM")))
                               (push `(,lim ,limit) bindings)
                               (push (cond ((string= limit-kind "below") `(>= ,var ,lim))
                                           ((string= limit-kind "downto") `(< ,var ,lim))
                                           (t `(> ,var ,lim)))
                                     end-tests)))
                           (push (if (and limit-kind (string= limit-kind "downto"))
                                     `(decf ,var ,by)
                                     `(incf ,var ,by))
                                 steps)))
                        ((kw kind "=")
                         (let ((init (pop clauses)) (first (gensym "FIRST")))
                           (push `(,first t) bindings)
                           (push `(,var nil) bindings)
                           (if (kw (car clauses) "then")
                               (progn (pop clauses)
                                      (let ((step (pop clauses)))
                                        (push `(setq ,var (if ,first (progn (setq ,first nil) ,init) ,step)) prebody)))
                               (push `(setq ,var ,init) prebody))))
                        (t (error "async loop: unsupported FOR kind ~S" kind)))))
                   ((kw head "repeat")
                    (let ((n (gensym "REPEAT")))
                      (push `(,n ,(pop clauses)) bindings)
                      (push `(<= ,n 0) end-tests)
                      (push `(decf ,n) steps)))
                   ((kw head "while") (push `(unless ,(pop clauses) (return :%loop-result)) body))
                   ((kw head "until") (push `(when ,(pop clauses) (return :%loop-result)) body))
                   ((or (kw head "when") (kw head "if") (kw head "unless"))
                    (let ((test (pop clauses)))
                      (multiple-value-bind (action rest) (parse-action clauses)
                        (setf clauses rest)
                        (push (if (kw head "unless") `(unless ,test ,action) `(when ,test ,action)) body))))
                   ((kw head "finally")
                    (loop while (and clauses (consp (car clauses))) do (push (pop clauses) finally)))
                   (t (multiple-value-bind (action rest) (parse-action (cons head clauses))
                        (setf clauses rest)
                        (push action body)))))))
    (when acc (push `(,acc ,(if (and (consp result) (eq (car result) 'nreverse)) nil 0)) bindings))
    ;; `while` clauses captured RESULT before it was known; patch them now.
    (setf body (sublis (list (cons :%loop-result result)) (reverse body)))
    (with-shared-k (ks k)
      (let ((iter (gensym "ITER")) (rk *async-rk*) (ignored (gensym "_"))
            (finally-code (reverse finally)))
        (let ((*async-blocks* (acons nil (list ks rk) *async-blocks*)))
          `(let ,(reverse bindings)
             (declare (ignorable ,@(mapcar #'car (reverse bindings))))
             (labels ((,iter ()
                        (if (or ,@(reverse end-tests))
                            ,(cps-progn (append finally-code (list result)) ks)
                            (progn
                              ,@(reverse prebody)
                              ,(cps-progn body
                                          `(lambda (,ignored)
                                             (progn ,@(reverse steps))
                                             ,(bounce-to rk `(,iter))))))))
               (,iter))))))))

;;; ------------------------------------------------------------------ user macros

(defmacro async (&body body)
  "Evaluate BODY asynchronously; returns a promise.  (await P) inside BODY suspends on promise P."
  (let ((result (gensym "RESULT")) (rk (gensym "RK")) (v (gensym "V")))
    `(let* ((,result (%make-promise))
            (,rk (lambda (reason) (reject ,result reason))))
       (declare (ignorable ,rk))
       ,(let ((*async-rk* rk) (*async-blocks* '()))
          (guarded rk (cps-progn body `(lambda (,v) (resolve ,result ,v)))))
       ,result)))

(defmacro async-lambda (lambda-list &body body)
  `(lambda ,lambda-list (async ,@body)))

(defmacro define-async (name lambda-list &body body)
  "Define NAME as a function returning a promise; (return-from NAME x) resolves it early."
  (multiple-value-bind (decls forms) (split-declarations body)
    (let ((doc (when (and (stringp (car forms)) (cdr forms)) (pop forms))))
      `(defun ,name ,lambda-list
         ,@(when doc (list doc))
         ,@decls
         (async (block ,name ,@forms))))))

(defmacro alet (bindings &body body)
  "Bind variables to the values of promises evaluated in parallel, then run BODY (which may return a promise)."
  (let ((vals (gensym "VALS")))
    `(then (all (list ,@(mapcar #'second bindings)))
           (lambda (,vals)
             (destructuring-bind ,(mapcar #'first bindings) ,vals
               (declare (ignorable ,@(mapcar #'first bindings)))
               ,@body)))))

(defmacro alet* (bindings &body body)
  "Sequential ALET."
  (if (null bindings)
      `(%as-promise (progn ,@body))
      (destructuring-bind ((var form) &rest rest) bindings
        `(then (%as-promise ,form)
               (lambda (,var)
                 (declare (ignorable ,var))
                 (alet* ,rest ,@body))))))

(defmacro chain (initial &rest steps)
  "Thread a promise through STEPS.  Each step (f a b) is called as (f value a b); a (lambda (v) ...)
step is funcalled; (:catch (e) body...) handles rejections; (:finally body...) always runs."
  (let ((form `(%as-promise ,initial)))
    (dolist (step steps form)
      (setf form
            (cond ((and (consp step) (eq (car step) :catch))
                   (destructuring-bind ((var) &rest body) (cdr step)
                     `(catch* ,form (lambda (,var) (declare (ignorable ,var)) ,@body))))
                  ((and (consp step) (eq (car step) :finally))
                   `(finally* ,form (lambda () ,@(cdr step))))
                  ((and (consp step) (eq (car step) 'lambda))
                   `(then ,form ,step))
                  ((consp step)
                   (let ((v (gensym "V")))
                     `(then ,form (lambda (,v) (,(car step) ,v ,@(cdr step))))))
                  (t `(then ,form (function ,step))))))))

(defun %with-timeout (ms thunk)
  (let* ((p (%as-promise (funcall thunk)))
         (result (%make-promise))
         (timer (set-timeout (lambda ()
                               (reject result (make-condition 'timeout-error :what "with-timeout"
                                                                             :seconds (/ ms 1000d0))))
                             ms)))
    (then p
          (lambda (v) (clear-timer timer) (resolve result v))
          (lambda (r) (clear-timer timer) (reject result r)))
    result))

(defmacro with-timeout ((ms) &body body)
  "Promise that rejects with TIMEOUT-ERROR unless BODY's promise settles within MS milliseconds."
  `(%with-timeout ,ms (lambda () ,@body)))

(defmacro wait-all (&rest promises)
  `(all (list ,@promises)))

(defmacro await-all (&rest promises)
  `(await (all (list ,@promises))))
