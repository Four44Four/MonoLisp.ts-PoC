;;;; examples/todo/utils.client.lisp -- a plain (non-JSX) ES module

(export (defstruct filter-state
  (query "" :type string)
  (show-done t :type boolean)))

(export (defun format-count (n)
  (declare (fixnum n) (returns string))
  (format nil "~A item~A" n (if (= n 1) "" "s"))))

(export (defun matches-filter (title done state)
  (declare (string title) (boolean done) (filter-state state) (returns boolean))
  (and (or (filter-state-show-done state) (not done))
       (includes (to-lower-case title) (to-lower-case (filter-state-query state))))))

(export (defun percent (ratio)
  (declare (float ratio) (returns string))
  (concat (math-round (* ratio 100)) "%")))

(export (defun debounce (fn ms)
  (declare (function fn) (fixnum ms))
  (let ((timer nil))
    (lambda (&rest args)
      (when timer (clear-timeout timer))
      (setf timer (set-timeout (lambda () (apply fn args)) ms))))))

(export (defun clamp (x lo hi)
  (declare (real x lo hi) (returns real))
  (cond ((< x lo) lo)
        ((> x hi) hi)
        (t x))))

(export (defun range (n)
  (declare (fixnum n) (returns (array fixnum)))
  (loop for i from 0 below n collect i)))

(export (defun word-frequencies (words)
  (declare ((array string) words) (returns (hash-table string fixnum)))
  (let ((table (make-hash-table)))
    (for-each (w words)
      (setf (gethash w table) (1+ (gethash w table 0))))
    table)))

(export (defun shout (words)
  (declare ((array string) words) (returns (array string)))
  (map (w words i)
    (concat i ":" (to-upper-case w)))))

(export (defun summarize (table)
  (declare ((hash-table string fixnum) table) (returns string))
  (loop for k being the hash-keys of table using (hash-value v)
        collect (concat k "=" v) into parts
        finally (return (join parts ", ")))))

(export (defun safe-parse (text)
  (declare (string text))
  (handler-case (parse-json text)
    (error (e) (console-warn "bad json" e) nil))))

(export (defun classify-number (n)
  (declare (real n) (returns string))
  (case (clamp n -1 1)
    (-1 "negative")
    (0 "zero")
    (1 "positive")
    (otherwise "nan"))))

(export (defun first-long-word (words)
  (declare ((array string) words) (returns (or string null)))
  (loop for w across words
        when (> (length w) 5) return w)))

(export (defun pairs (xs)
  (declare ((array fixnum) xs) (returns (array (tuple fixnum fixnum))))
  (loop for x across xs
        for i from 0
        collect (values i x))))

(export (defun stats-tuple (xs)
  (declare ((array fixnum) xs) (returns (values fixnum fixnum)))
  (loop for x across xs
        maximize x into hi
        minimize x into lo
        finally (return (values (nil-or lo 0) (nil-or hi 0))))))

(export (defun use-stats (xs)
  (declare ((array fixnum) xs) (returns string))
  (let ((:multi (lo hi) (stats-tuple xs)))
    (declare (fixnum lo hi))
    (concat lo ".." hi))))

(export (defstruct (point (:include filter-state)) (x 0 :type real) (y 0 :type real)))

(export (defun move-point (p dx dy)
  (declare (point p) (real dx dy) (returns point))
  (let ((q (copy-point p)))
    (incf (point-x q) dx)
    (incf (point-y q) dy)
    q)))
