;;;; src/transpiler/reader.lisp -- reading frontend Lisp source
;;;;
;;;; * readtable-case :INVERT so that `fooBar`, `React`, `useState` round-trip verbatim while
;;;;   ordinary kebab-case names read as uppercase symbols (and are camelCased on output).
;;;; * `{}` object-literal syntax:  '{}(:a 1 :b 2)  reads as  (QUOTE (%OBJECT :a 1 :b 2))
;;;; * `..` dot-chaining symbol (normally a reader error in Common Lisp).
;;;; * *READ-EVAL* is NIL: no #. evaluation of source text, ever.

(in-package :monolisp.transpiler)

(defvar *client-lang-package* (find-package :monolisp.client-lang))

(defvar *client-readtable* nil)

(defun %brace-reader (stream char)
  (declare (ignore char))
  ;; Expect "{}" immediately, optionally followed (with no whitespace) by a plist in parens.
  (let ((next (read-char stream nil nil)))
    (unless (eql next #\})
      (error 'reader-error :stream stream :format-control "object literal must be written {}(:key value ...)")))
  (let ((peek (peek-char nil stream nil nil)))
    (if (eql peek #\()
        (let ((plist (read stream t nil t)))
          (cons (intern "%OBJECT" *client-lang-package*) plist))
        (list (intern "%OBJECT" *client-lang-package*)))))

(defun %close-brace-reader (stream char)
  (declare (ignore char))
  (error 'reader-error :stream stream :format-control "unmatched }"))

(defun %dot-reader (stream char)
  (declare (ignore char))
  (let ((peek (peek-char nil stream nil nil)))
    (cond ((eql peek #\.)
           ;; ".." (or more dots): the chaining operator
           (loop while (eql (peek-char nil stream nil nil) #\.) do (read-char stream))
           (intern ".." *client-lang-package*))
          ((or (null peek) (member peek '(#\Space #\Tab #\Newline #\Return #\) #\( #\" #\' #\;)))
           (error 'reader-error :stream stream :format-control "dotted pairs / cons cells are not supported in frontend Lisp"))
          (t
           ;; A token starting with "." such as .5 -- collect it and read with the standard readtable.
           (let ((chars (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)))
             (vector-push-extend #\. chars)
             (loop for c = (peek-char nil stream nil nil)
                   while (and c (not (member c '(#\Space #\Tab #\Newline #\Return #\) #\( #\" #\' #\; #\`))))
                   do (vector-push-extend (read-char stream) chars))
             (let ((*readtable* (copy-readtable nil)))
               (setf (readtable-case *readtable*) :invert)
               (read-from-string chars)))))))

(defun client-readtable ()
  (or *client-readtable*
      (setf *client-readtable*
            (let ((rt (copy-readtable nil)))
              (setf (readtable-case rt) :invert)
              (set-macro-character #\{ #'%brace-reader nil rt)
              (set-macro-character #\} #'%close-brace-reader nil rt)
              (set-macro-character #\. #'%dot-reader t rt)
              rt))))

(defvar *module-package-counter* 0)

(defun make-client-package (name)
  "A fresh package for one module's symbols.  Uses CL (so LET, IF, LOOP... are the standard symbols
and host macros can be defined with DEFMACRO) and the client-lang additions."
  (let ((pkg-name (format nil "MONOLISP.CLIENT.~A.~D" (string-upcase name) (incf *module-package-counter*))))
    (when (find-package pkg-name) (delete-package pkg-name))
    (make-package pkg-name :use (list :cl *client-lang-package*))))

(defparameter *max-source-bytes* (* 4 1024 1024)
  "Largest frontend source file we will read.")

(defparameter *max-nesting-depth* 400
  "Deepest parenthesis nesting accepted in frontend source.  The Lisp reader recurses once per
level, so unbounded nesting is a stack-exhaustion attack on the build machine.")

(defun check-source-shape (text file)
  "Linear pre-scan: bounds the size and nesting depth of TEXT before the recursive reader sees it.
Strings, `;` comments, `#| |#` comments and `#\\x` character literals are skipped."
  (when (> (length text) *max-source-bytes*)
    (error 'transpile-error :message (format nil "source exceeds ~D characters" *max-source-bytes*) :file file))
  (let ((depth 0) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char= c #\")
                      (incf i)
                      (loop while (and (< i n) (char/= (char text i) #\"))
                            do (when (char= (char text i) #\\) (incf i)) (incf i)))
                     ((char= c #\;)
                      (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
                     ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                      (incf i 2)
                      (loop while (and (< (1+ i) n) (not (and (char= (char text i) #\|) (char= (char text (1+ i)) #\#))))
                            do (incf i))
                      (incf i))
                     ((and (char= c #\#) (< (+ i 2) n) (char= (char text (1+ i)) #\\))
                      (incf i 2))
                     ((member c '(#\( #\{ #\[))
                      (incf depth)
                      (when (> depth *max-nesting-depth*)
                        (error 'transpile-error :message (format nil "source nesting deeper than ~D levels" *max-nesting-depth*) :file file)))
                     ((member c '(#\) #\} #\]))
                      (setf depth (max 0 (1- depth)))))
               (incf i)))))

(defun allowed-symbol-package-p (symbol package)
  (let ((home (symbol-package symbol)))
    (or (null home)                                   ; uninterned (gensyms from macros)
        (eq home package)
        (eq home (find-package :keyword))
        (eq home (find-package :cl))
        (eq home *client-lang-package*)
        ;; SBCL reads `form as (SB-INT:QUASIQUOTE form); the commas are structs, not symbols
        (and (eq home (find-package :sb-int)) (string= (symbol-name symbol) "QUASIQUOTE")))))

(defun check-form-symbols (form package file &key allow-sharing)
  "Frontend source may only name symbols of its own module, CL, KEYWORD and the client language.
Package-qualified references into the host image (sb-ext:quit, asdf:load-system, cl-user::foo)
are rejected before anything is compiled or macroexpanded.  Source read from text must also be
a tree: #n= sharing / #n# cycles are refused (ALLOW-SHARING is for macro expansions, which may
legitimately reuse argument forms)."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (x)
               (cond ((symbolp x)
                      (unless (allowed-symbol-package-p x package)
                        (error 'transpile-error
                               :message (format nil "package-qualified symbol ~S is not allowed in frontend source" x)
                               :form x :file file)))
                     ((consp x)
                      ;; every cons of a tree is reached exactly once; a repeat means #n= sharing
                      ;; or a #n# cycle, which would hang the compiler's list walks
                      (cond ((not (gethash x seen))
                             (setf (gethash x seen) t)
                             (walk (car x))
                             (walk (cdr x)))
                            ((not allow-sharing)
                             (error 'transpile-error
                                    :message "circular or shared structure (#n= / #n#) is not supported in frontend source"
                                    :file file))))
                     ((and (vectorp x) (not (stringp x)))
                      (map nil #'walk x))
                     ;; ,form inside a backquote: the wrapped expression is evaluated at
                     ;; macroexpansion time, so it must obey the same rules
                     ((typep x 'sb-impl::comma)
                      (walk (sb-impl::comma-expr x))))))
      (walk form))))

(defun read-client-source (text &key (package (make-client-package "ANON")) (file nil))
  "Read every top-level form of TEXT.  Returns a list of forms.  Reader errors become TRANSPILE-ERRORs."
  (check-source-shape text file)
  (let ((*readtable* (client-readtable))
        (*package* package)
        (*read-eval* nil)
        (*read-default-float-format* 'double-float)
        (*current-file* file)
        (forms '()))
    (with-input-from-string (in text)
      (handler-case
          (loop
            (let ((form (read in nil in)))
              (when (eq form in) (return))
              (check-form-symbols form package file)
              (push form forms)))
        (reader-error (e)
          (error 'transpile-error :message (format nil "read error: ~A" e) :file file))
        (end-of-file ()
          (error 'transpile-error :message "unexpected end of file (unbalanced parentheses?)" :file file))))
    (nreverse forms)))
