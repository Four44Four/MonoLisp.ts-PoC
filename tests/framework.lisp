;;;; tests/framework.lisp -- tiny dependency-free test framework

(defpackage :monolisp.test
  (:use :cl)
  (:export #:deftest #:is #:is-equal #:is-true #:is-false #:signals #:finishes #:fail
           #:run-all-tests #:run-suite #:run-tests-named #:*verbose* #:with-timeout-guard #:*backtrace-on-timeout*
           #:bench #:*bench-results*))

(in-package :monolisp.test)

(defvar *tests* (make-hash-table :test 'equal)
  "suite-name -> list of (test-name . function)")
(defvar *suite-order* '())
(defvar *verbose* nil)
(defvar *current-failures* 0)
(defvar *current-assertions* 0)
(defvar *bench-results* '())

(defmacro deftest (name (&key (suite "default")) &body body)
  (let ((fname (intern (format nil "TEST/~A" (string name)))))
    `(progn
       (defun ,fname () ,@body)
       (let ((suite (string ',suite)))
         (unless (gethash suite *tests*)
           (setf *suite-order* (append *suite-order* (list suite))))
         (setf (gethash suite *tests*)
               (append (remove ',name (gethash suite *tests*) :key #'car :test #'string=)
                       (list (cons ',name #',fname)))))
       ',name)))

(defun report-failure (fmt &rest args)
  (incf *current-failures*)
  (format t "~&    FAIL: ~?~%" fmt args))

(defmacro is (form &optional (msg nil msg-p) &rest args)
  `(progn
     (incf *current-assertions*)
     (unless (handler-case ,form
               (error (e)
                 (report-failure "~S signalled ~A: ~A" ',form (type-of e) e)
                 :errored))
       ,(if msg-p
            `(report-failure ,msg ,@args)
            `(report-failure "~S was false" ',form)))))

(defmacro is-equal (expected form &optional (test ''equal))
  (let ((e (gensym)) (v (gensym)))
    `(let ((,e ,expected))
       (incf *current-assertions*)
       (handler-case
           (let ((,v ,form))
             (unless (funcall ,test ,e ,v)
               (report-failure "~S~%          expected: ~S~%          got:      ~S" ',form ,e ,v)))
         (error (c) (report-failure "~S signalled ~A: ~A" ',form (type-of c) c))))))

(defmacro is-true (form &rest msg) `(is ,form ,@msg))
(defmacro is-false (form &rest msg) `(is (not ,form) ,@msg))

(defmacro signals (condition-type form)
  `(progn
     (incf *current-assertions*)
     (handler-case (progn ,form
                          (report-failure "~S did not signal ~S" ',form ',condition-type))
       (,condition-type () t)
       (error (e) (report-failure "~S signalled ~A instead of ~S: ~A" ',form (type-of e) ',condition-type e)))))

(defmacro finishes (form)
  `(progn
     (incf *current-assertions*)
     (handler-case (progn ,form t)
       (error (e) (report-failure "~S signalled ~A: ~A" ',form (type-of e) e)))))

(defun fail (fmt &rest args)
  (incf *current-assertions*)
  (apply #'report-failure fmt args))

(defvar *backtrace-on-timeout* nil "Print a backtrace when a guarded test hangs.")

(defmacro with-timeout-guard ((seconds) &body body)
  "Fail the test instead of hanging forever."
  `(handler-case
       (handler-bind ((sb-ext:timeout (lambda (c) (declare (ignore c))
                                        (when *backtrace-on-timeout*
                                          (format t "~&    --- backtrace at timeout ---~%")
                                          (sb-debug:print-backtrace :count 30)))))
         (sb-ext:with-timeout ,seconds ,@body))
     (sb-ext:timeout () (report-failure "timed out after ~A seconds" ,seconds))))

(defmacro bench ((name iterations) &body body)
  "Run BODY ITERATIONS times, record ops/sec."
  (let ((start (gensym)) (end (gensym)) (i (gensym)) (n (gensym)))
    `(let* ((,n ,iterations)
            (,start (get-internal-real-time)))
       (dotimes (,i ,n) ,@body)
       (let* ((,end (get-internal-real-time))
              (secs (max 1d-9 (/ (- ,end ,start) internal-time-units-per-second)))
              (ops (/ ,n secs)))
         (push (list ,name ops) *bench-results*)
         (format t "~&    BENCH ~A: ~,0F ops/sec (~A iterations in ~,3Fs)~%" ,name ops ,n secs)
         ops))))

(defun run-suite (suite)
  (let ((tests (gethash (string suite) *tests*))
        (suite-failures 0))
    (format t "~&== Suite ~A (~D tests)~%" suite (length tests))
    (dolist (entry tests)
      (destructuring-bind (name . fn) entry
        (let ((*current-failures* 0)
              (*current-assertions* 0))
          (when *verbose* (format t "~&  - ~A~%" name))
          (handler-case (funcall fn)
            (error (e)
              (report-failure "unhandled ~A in test ~A: ~A" (type-of e) name e)))
          (when (plusp *current-failures*)
            (format t "~&  x ~A: ~D failure(s) / ~D assertion(s)~%" name *current-failures* *current-assertions*)
            (incf suite-failures *current-failures*)))))
    suite-failures))

(defun run-tests-named (names)
  "Run only the tests whose names (strings/symbols) are in NAMES, in the given order."
  (let ((*verbose* t) (failures 0))
    (dolist (name names failures)
      (let ((entry (loop for suite being the hash-values of *tests*
                         thereis (find (string name) suite :key (lambda (e) (string (car e))) :test #'string-equal))))
        (if (null entry)
            (format t "~&  ? no such test ~A~%" name)
            (let ((*current-failures* 0) (*current-assertions* 0))
              (format t "~&  - ~A~%" (car entry))
              (handler-case (funcall (cdr entry))
                (error (e) (report-failure "unhandled ~A: ~A" (type-of e) e)))
              (incf failures *current-failures*)))))))

(defun run-all-tests (&key (suites *suite-order*))
  (let ((total 0)
        (start (get-internal-real-time)))
    (dolist (s suites)
      (incf total (run-suite s)))
    (format t "~&~%==== ~A: ~D failure(s) in ~,2Fs ====~%"
            (if (zerop total) "ALL TESTS PASSED" "TESTS FAILED")
            total
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (zerop total)))
