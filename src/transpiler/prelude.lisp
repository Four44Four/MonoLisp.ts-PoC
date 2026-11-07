;;;; src/transpiler/prelude.lisp -- inline aliases available to every frontend module
;;;;
;;;; These are ordinary frontend-Lisp definitions declaimed INLINE, so each call site expands
;;;; to the aliased JS expression and nothing is emitted for the definitions themselves.

(in-package :monolisp.transpiler)

(defparameter *prelude-source* "
(declaim (inline console-log console-error console-warn console-info console-debug
                 doc-create-element doc-create-text-node doc-body-append-child append-child remove-child
                 add-event-listener remove-event-listener stringify-json parse-json slice
                 doc-get-element-by-id doc-query-selector doc-query-selector-all
                 set-inner-text set-inner-html get-attribute set-attribute remove-attribute
                 add-class remove-class toggle-class has-class
                 set-timeout set-interval clear-timeout clear-interval
                 alert prompt confirm
                 join split trim index-of includes starts-with ends-with to-upper-case to-lower-case
                 replace-all pad-start pad-end char-at
                 array-push array-pop array-shift array-unshift array-concat array-includes array-index-of
                 array-find array-find-index array-filter array-map array-reduce array-some array-every
                 array-sort array-reverse array-join array-flat array-from array-is-array fill-array
                 math-floor math-ceil math-round math-trunc math-abs math-min math-max math-sqrt math-pow math-random
                 number-to-string parse-float number-is-nan number-is-integer
                 object-keys object-values object-entries object-assign object-freeze
                 promise-resolve promise-reject promise-all promise-race
                 json-fetch now date-now
                 prevent-default stop-propagation
                 undefined-p defined-p))

(defun console-log (&rest args) (console.log (spread args)))
(defun console-error (&rest args) (console.error (spread args)))
(defun console-warn (&rest args) (console.warn (spread args)))
(defun console-info (&rest args) (console.info (spread args)))
(defun console-debug (&rest args) (console.debug (spread args)))

(defun doc-create-element (tag) (document.createElement tag))
(defun doc-create-text-node (text) (document.createTextNode text))
(defun doc-body-append-child (el) (document.body.appendChild el))
(defun append-child (parent child) (.. parent (appendChild child)))
(defun remove-child (parent child) (.. parent (removeChild child)))
(defun add-event-listener (target event handler) (.. target (addEventListener event handler)))
(defun remove-event-listener (target event handler) (.. target (removeEventListener event handler)))
(defun doc-get-element-by-id (id) (document.getElementById id))
(defun doc-query-selector (sel) (document.querySelector sel))
(defun doc-query-selector-all (sel) (document.querySelectorAll sel))
(defun set-inner-text (el text) (setf (.. el innerText) text))
(defun set-inner-html (el html) (setf (.. el innerHTML) html))
(defun get-attribute (el name) (.. el (getAttribute name)))
(defun set-attribute (el name value) (.. el (setAttribute name value)))
(defun remove-attribute (el name) (.. el (removeAttribute name)))
(defun add-class (el &rest names) (.. el classList (add (spread names))))
(defun remove-class (el &rest names) (.. el classList (remove (spread names))))
(defun toggle-class (el name) (.. el classList (toggle name)))
(defun has-class (el name) (.. el classList (contains name)))
(defun prevent-default (event) (.. event (preventDefault)))
(defun stop-propagation (event) (.. event (stopPropagation)))

(defun stringify-json (value &optional replacer indent) (JSON.stringify value replacer indent))
(defun parse-json (text) (JSON.parse text))

(defun set-timeout (fn ms &rest args) (setTimeout fn ms (spread args)))
(defun set-interval (fn ms &rest args) (setInterval fn ms (spread args)))
(defun clear-timeout (id) (clearTimeout id))
(defun clear-interval (id) (clearInterval id))
(defun alert (message) (window.alert message))
(defun prompt (message &optional default) (window.prompt message default))
(defun confirm (message) (window.confirm message))

(defun slice (seq start &optional end) (.. seq (slice start end)))
(defun join (array &optional (sep \",\")) (.. array (join sep)))
(defun split (string sep &optional limit) (.. string (split sep limit)))
(defun trim (string) (.. string (trim)))
(defun index-of (seq item &optional from) (.. seq (indexOf item from)))
(defun includes (seq item) (.. seq (includes item)))
(defun starts-with (string prefix) (.. string (startsWith prefix)))
(defun ends-with (string suffix) (.. string (endsWith suffix)))
(defun to-upper-case (string) (.. string (toUpperCase)))
(defun to-lower-case (string) (.. string (toLowerCase)))
(defun replace-all (string from to) (.. string (replaceAll from to)))
(defun pad-start (string n &optional pad) (.. string (padStart n pad)))
(defun pad-end (string n &optional pad) (.. string (padEnd n pad)))
(defun char-at (string i) (.. string (charAt i)))

(defun array-push (array &rest items) (.. array (push (spread items))))
(defun array-pop (array) (.. array (pop)))
(defun array-shift (array) (.. array (shift)))
(defun array-unshift (array &rest items) (.. array (unshift (spread items))))
(defun array-concat (array &rest others) (.. array (concat (spread others))))
(defun array-includes (array item) (.. array (includes item)))
(defun array-index-of (array item) (.. array (indexOf item)))
(defun array-find (array pred) (.. array (find pred)))
(defun array-find-index (array pred) (.. array (findIndex pred)))
(defun array-filter (array pred) (.. array (filter pred)))
(defun array-map (array fn) (.. array (map fn)))
(defun array-reduce (array fn &optional initial) (.. array (reduce fn initial)))
(defun array-some (array pred) (.. array (some pred)))
(defun array-every (array pred) (.. array (every pred)))
(defun array-sort (array &optional cmp) (.. array (sort cmp)))
(defun array-reverse (array) (.. array (reverse)))
(defun array-join (array &optional (sep \",\")) (.. array (join sep)))
(defun array-flat (array &optional depth) (.. array (flat depth)))
(defun array-from (iterable &optional map-fn) (Array.from iterable map-fn))
(defun array-is-array (x) (Array.isArray x))
(defun fill-array (array value) (.. array (fill value)))

(defun math-floor (x) (Math.floor x))
(defun math-ceil (x) (Math.ceil x))
(defun math-round (x) (Math.round x))
(defun math-trunc (x) (Math.trunc x))
(defun math-abs (x) (Math.abs x))
(defun math-min (&rest xs) (Math.min (spread xs)))
(defun math-max (&rest xs) (Math.max (spread xs)))
(defun math-sqrt (x) (Math.sqrt x))
(defun math-pow (x y) (Math.pow x y))
(defun math-random () (Math.random))
(defun number-to-string (n &optional radix) (.. n (toString radix)))
(defun parse-float (s) (parseFloat s))
(defun number-is-nan (x) (Number.isNaN x))
(defun number-is-integer (x) (Number.isInteger x))

(defun object-keys (o) (Object.keys o))
(defun object-values (o) (Object.values o))
(defun object-entries (o) (Object.entries o))
(defun object-assign (target &rest sources) (Object.assign target (spread sources)))
(defun object-freeze (o) (Object.freeze o))

(defun promise-resolve (v) (Promise.resolve v))
(defun promise-reject (v) (Promise.reject v))
(defun promise-all (promises) (Promise.all promises))
(defun promise-race (promises) (Promise.race promises))

(defun json-fetch (url &optional init) (.. (await (fetch url init)) (json)))
(defun now () (performance.now))
(defun date-now () (Date.now))

(defun undefined-p (x) (eq x :undefined))
(defun defined-p (x) (not (eq x :undefined)))
")

(defvar *prelude-inlines* nil)

(defun prelude-inlines ()
  "Hash table NAME -> (lambda-list . body) for the prelude aliases (read once)."
  (or *prelude-inlines*
      (setf *prelude-inlines*
            (let ((table (make-hash-table :test 'equal))
                  (forms (read-client-source *prelude-source* :package (make-client-package "PRELUDE") :file "<prelude>")))
              (dolist (form forms table)
                (when (and (consp form) (sym-is (car form) "DEFUN"))
                  (setf (gethash (symbol-name (second form)) table)
                        (cons (third form) (cdddr form)))))))))
