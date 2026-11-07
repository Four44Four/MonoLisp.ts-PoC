;;;; scripts/build-example.lisp -- build an example under examples/: load the backend, transpile the
;;;; frontend, type-check the output with tsc against the generated server stubs (Linux, macOS, Windows).
;;;;
;;;;   sbcl --script scripts/build-example.lisp                       build examples/todo + tsc
;;;;   sbcl --script scripts/build-example.lisp --example guestbook   build examples/guestbook
;;;;   sbcl --script scripts/build-example.lisp --serve               ... then serve JSON-RPC on :8080
;;;;   sbcl --script scripts/build-example.lisp --port 9000 --serve
;;;;   sbcl --script scripts/build-example.lisp --skip-tsc            transpile only (no node needed)
;;;;   sbcl --script scripts/build-example.lisp --list                list the examples
;;;;
;;;; Exits 0 on success, 1 when transpilation or tsc fails, 2 on usage errors.
;;;; `npm run build:example` / `npm run serve:example` (and the per-example aliases) are equivalent
;;;; (see package.json).

(load (merge-pathnames "cli.lisp" (or *load-truename* *compile-file-truename*)))

(in-package :monolisp.cli)

(check-options '("--example" "--serve" "--port" "--skip-tsc" "--list" "--help"))

(when (flag-p "--help")
  (note "usage: sbcl --script scripts/build-example.lisp [--example NAME] [--serve] [--port N] [--skip-tsc] [--list]")
  (sb-ext:exit :code 0))

(defun example-names ()
  "Directories under examples/ that contain a tsconfig.json."
  (sort (loop for dir in (uiop:subdirectories (merge-pathnames "examples/" *root*))
              when (probe-file (merge-pathnames "tsconfig.json" dir))
                collect (car (last (pathname-directory dir))))
        #'string<))

(when (flag-p "--list")
  (note "~{~A~%~}" (example-names))
  (sb-ext:exit :code 0))

(defparameter *example-name*
  (let ((name (option "--example" "todo")))
    (unless (member name (example-names) :test #'string=)
      (die "unknown example ~A (expected one of ~{~A~^ ~})" name (example-names)))
    name))
(defparameter *example* (merge-pathnames (format nil "examples/~A/" *example-name*) *root*))
(defparameter *out* (merge-pathnames "build/" *example*))
(defparameter *port*
  (let ((p (option "--port" "8080")))
    (or (parse-integer p :junk-allowed t) (die "--port expects an integer, got ~A" p))))

(quiet-load-system "monolisp")

;; Read only after the system is loaded, so the MONOLISP package exists.
(progn
  (monolisp:load-server-files *example*)
  (let ((result (handler-case (monolisp:build-frontend *example* *out*)
                  (monolisp:transpile-error (e)
                    (note "transpile error: ~A" e)
                    (sb-ext:exit :code 1 :abort t)))))
    (note "Transpiled:~%~{  ~A~%~}" (mapcar (lambda (p) (enough-namestring p *root*)) (getf result :outputs)))
    (when (getf result :warnings)
      (note "Warnings:~%~{  ~A~%~}" (getf result :warnings))))

  (if (flag-p "--skip-tsc")
      (note "tsc: skipped (--skip-tsc).")
      (let ((code (run-tsc (merge-pathnames "tsconfig.json" *example*))))
        (unless (zerop code)
          (note "tsc reported type errors (exit ~D)." code)
          (sb-ext:exit :code 1 :abort t))
        (note "tsc: frontend type-checks against the backend manifest.")))

  (when (flag-p "--serve")
    (note "Serving examples/~A on http://127.0.0.1:~D/rpc (Ctrl-C to stop)" *example-name* *port*)
    (with-interrupt-exit
      (monolisp:start :port *port* :cors-origins :any))))
