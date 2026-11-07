;;;; scripts/run-tests.lisp -- load the framework and run the test suite (Linux, macOS, Windows).
;;;;
;;;;   sbcl --script scripts/run-tests.lisp                       run everything
;;;;   sbcl --script scripts/run-tests.lisp --suite json          run one suite (repeatable)
;;;;   sbcl --script scripts/run-tests.lisp --test NAME           run one test by name (repeatable)
;;;;   sbcl --script scripts/run-tests.lisp --verbose             print each test name
;;;;   sbcl --script scripts/run-tests.lisp --list                list the suites and exit
;;;;
;;;; Suites: async json types rpc http-parser transpiler examples attack integration perf
;;;; Exits 0 when every selected test passed, 1 when any failed, 2 on usage errors.
;;;; `npm test -- --suite json` is equivalent (see package.json).

(load (merge-pathnames "cli.lisp" (or *load-truename* *compile-file-truename*)))

(in-package :monolisp.cli)

(check-options '("--suite" "--test" "--verbose" "--list" "--help"))

(when (flag-p "--help")
  (note "usage: sbcl --script scripts/run-tests.lisp [--suite NAME]... [--test NAME]... [--verbose] [--list]")
  (sb-ext:exit :code 0))

(quiet-load-system "monolisp/tests")

;; The forms below are read only after the system is loaded, so the test package exists.
(let* ((known monolisp.test::*suite-order*)
       (suites (options "--suite"))
       (tests (options "--test")))
  (when (flag-p "--list")
    (note "~{~A~%~}" known)
    (sb-ext:exit :code 0))
  (dolist (s suites)
    (unless (member s known :test #'string=)
      (die "unknown suite ~A (known: ~{~A~^ ~})" s known)))
  (when (flag-p "--verbose")
    (setf monolisp.test:*verbose* t))
  (setf monolisp.test:*backtrace-on-timeout* t)
  (let ((ok (with-interrupt-exit
              (cond (tests (zerop (monolisp.test:run-tests-named tests)))
                    (suites (monolisp.test:run-all-tests :suites suites))
                    (t (monolisp.test:run-all-tests))))))
    (finish-output)
    (sb-ext:exit :code (if ok 0 1))))
