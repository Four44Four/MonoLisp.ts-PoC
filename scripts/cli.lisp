;;;; scripts/cli.lisp -- shared helpers for the `sbcl --script` entry points in this directory.
;;;;
;;;; The framework's utility scripts are plain Lisp files run with `sbcl --script`, so the same
;;;; command works on Linux, macOS and Windows with no shell (sh/PowerShell) in between: no quoting
;;;; layer, no line-ending sensitivity, no temp files.  Each script does
;;;;
;;;;   (load (merge-pathnames "cli.lisp" *load-truename*))
;;;;
;;;; which registers the project with ASDF (via load.lisp) and provides the helpers below.
;;;; Command-line arguments after the script name are read from SB-EXT:*POSIX-ARGV*.

(defpackage :monolisp.cli
  (:use :cl)
  (:export #:*root* #:args #:flag-p #:option #:options #:check-options #:die #:note
           #:quiet-load-system #:tsc-path #:run-tsc #:with-interrupt-exit))

(in-package :monolisp.cli)

(load (merge-pathnames "load.lisp" (or *load-truename* *compile-file-truename*)))

(defvar *root* (truename cl-user::*monolisp-root*)
  "Repository root directory.")

;;; ------------------------------------------------------------------ arguments

(defun args ()
  "User arguments: everything after the script path.  SBCL strips its own options."
  (rest sb-ext:*posix-argv*))

(defun flag-p (name)
  "True when --NAME was given."
  (and (member name (args) :test #'string=) t))

(defun options (name)
  "All values given for a repeatable --NAME VALUE option, in order."
  (loop for (arg value) on (args)
        when (string= arg name)
          collect (or value (die "~A requires a value" name))))

(defun option (name &optional default)
  "The value of the last --NAME VALUE option, or DEFAULT."
  (let ((values (options name)))
    (if values (car (last values)) default)))

(defun check-options (allowed)
  "Fail on any --option that is not in ALLOWED (a list of strings).  Positional arguments pass."
  (dolist (arg (args))
    (when (and (> (length arg) 2) (string= "--" arg :end2 2)
               (not (member arg allowed :test #'string=)))
      (die "unknown option ~A (expected one of ~{~A~^ ~})" arg allowed))))

;;; ------------------------------------------------------------------ output

(defun note (fmt &rest fmt-args)
  (format t "~&~?~%" fmt fmt-args)
  (finish-output))

(defun die (fmt &rest fmt-args)
  "Print an error to stderr and exit with status 2."
  (format *error-output* "~&error: ~?~%" fmt fmt-args)
  (finish-output *error-output*)
  (sb-ext:exit :code 2 :abort t))

;;; ------------------------------------------------------------------ loading

(defun quiet-load-system (name)
  "ASDF-load system NAME without compiler chatter (notes, style warnings, file-by-file progress).
Real errors still propagate; under --script they terminate the process with a non-zero status."
  (let ((*compile-verbose* nil) (*compile-print* nil)
        (*load-verbose* nil) (*load-print* nil))
    (handler-bind ((warning #'muffle-warning)
                   (sb-ext:compiler-note #'muffle-warning))
      (asdf:load-system name))))

;;; ------------------------------------------------------------------ node / tsc

(defun tsc-path ()
  "The TypeScript compiler installed by `npm install`, run through node (portable: no npx/.cmd shims)."
  (merge-pathnames "node_modules/typescript/bin/tsc" *root*))

(defun run-tsc (tsconfig)
  "Type-check the project described by TSCONFIG with the repository's tsc.  Output is passed
through; returns the exit code (0 = no type errors)."
  (unless (probe-file (tsc-path))
    (die "~A not found; run `npm install` in ~A first" (enough-namestring (tsc-path) *root*) *root*))
  (let ((proc (handler-case
                  (sb-ext:run-program "node"
                                      (list (namestring (tsc-path)) "--noEmit" "-p" (namestring tsconfig))
                                      :search t :output t :error t :directory (namestring *root*))
                (error (e) (die "could not start node (is it on PATH?): ~A" e)))))
    (sb-ext:process-exit-code proc)))

;;; ------------------------------------------------------------------ interrupts

(defmacro with-interrupt-exit (&body body)
  "Run BODY; on Ctrl-C print a short note and exit 0 instead of dumping a backtrace."
  `(handler-case (progn ,@body)
     (sb-sys:interactive-interrupt ()
       (note "~%interrupted")
       (sb-ext:exit :code 0 :abort t))))
