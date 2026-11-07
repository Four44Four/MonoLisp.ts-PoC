;;;; src/transpiler/names.lisp -- Lisp symbol -> JavaScript identifier

(in-package :monolisp.transpiler)

(defparameter *js-reserved-words*
  '("break" "case" "catch" "class" "const" "continue" "debugger" "default" "delete" "do" "else"
    "enum" "export" "extends" "false" "finally" "for" "function" "if" "import" "in" "instanceof"
    "new" "null" "return" "super" "switch" "this" "throw" "true" "try" "typeof" "var" "void"
    "while" "with" "yield" "let" "static" "implements" "interface" "package" "private" "protected"
    "public" "await" "arguments" "eval"))

(defun js-reserved-p (name) (member name *js-reserved-words* :test #'string=))

(defun js-identifier-p (name)
  (and (plusp (length name))
       (let ((c (char name 0))) (or (alpha-char-p c) (char= c #\_) (char= c #\$)))
       (every (lambda (c) (or (alphanumericp c) (char= c #\_) (char= c #\$))) name)
       (not (js-reserved-p name))))

(defun mangle-part (name)
  "NAME is a symbol-name as read with readtable-case :invert.
  all-uppercase (kebab-case in source)  -> camelCase
  all-lowercase (UPPERCASE in source)   -> UPPERCASE kept
  mixed case (written camelCase/Pascal) -> verbatim"
  (let ((has-upper (some #'upper-case-p name))
        (has-lower (some #'lower-case-p name)))
    (cond ((and has-upper (not has-lower)) (lisp-name->camel name))
          ((and has-lower (not has-upper)) (substitute #\_ #\- (string-upcase name)))
          ((not (or has-upper has-lower)) (lisp-name->camel name))
          (t name))))

(defun escape-reserved (name)
  (if (js-reserved-p name) (concatenate 'string name "_") name))

(defun mangle-name (symbol-or-string)
  "JavaScript identifier (possibly dotted, e.g. console.log) for a Lisp symbol."
  (let ((name (if (symbolp symbol-or-string) (symbol-name symbol-or-string) symbol-or-string)))
    (cond ((find #\. name)
           ;; The case rule applies to the token as a whole: `console.log` (read as CONSOLE.LOG)
           ;; camelCases each part, `document.getElementById` (mixed) is kept verbatim.
           (let ((has-upper (some #'upper-case-p name)) (has-lower (some #'lower-case-p name)))
             (cond ((and has-upper has-lower) name)
                   (t (format nil "~{~A~^.~}" (mapcar (lambda (p) (escape-reserved (mangle-part p))) (split-string name #\.)))))))
          (t (escape-reserved (mangle-part name))))))

(defun mangle-type-name (symbol)
  "Type/interface names: kebab-case -> PascalCase; written-case preserved otherwise."
  (let ((name (symbol-name symbol)))
    (if (and (some #'upper-case-p name) (notany #'lower-case-p name))
        (lisp-name->pascal name)
        (mangle-part name))))

(defun property-name (key)
  "Object property / JSX attribute name for a keyword, symbol or string."
  (etypecase key
    (string key)
    (symbol (mangle-part (symbol-name key)))))

(defun split-string (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun sym-name (x)
  "Symbol name used for dispatch (symbols compare by name, since every module has its own package)."
  (and (symbolp x) (not (keywordp x)) (symbol-name x)))

(defun sym-is (x name) (and (symbolp x) (not (keywordp x)) (string= (symbol-name x) name)))

(defun iteration-binding-p (form)
  "Is (second FORM) a (var array [index]) binding list, as used by for-each and map, rather than
a quoted type or a function?"
  (let ((b (second form)))
    (and (consp b) (symbolp (car b)) (not (keywordp (car b))) (not (sym-is (car b) "QUOTE"))
         (consp (cdr b)) (<= 2 (length b) 3)
         (or (null (third b)) (and (symbolp (third b)) (not (keywordp (third b))))))))
