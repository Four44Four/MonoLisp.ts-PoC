;;;; tests/example-tests.lisp -- every project under examples/ builds, type-checks and behaves
;;;;
;;;; Each example's backend is loaded into an empty server-function registry (the fixtures of the
;;;; rpc suite are restored afterwards), its frontend is transpiled against that manifest, and the
;;;; output is run through `tsc --strict` when node is available.

(defpackage :monolisp.test.examples
  (:use :cl :monolisp.test :monolisp.json :monolisp.rpc :monolisp.async))

(in-package :monolisp.test.examples)

(defun repo-root () (uiop:ensure-directory-pathname cl-user::*monolisp-root*))

(defun example-dir (name) (merge-pathnames (format nil "examples/~A/" name) (repo-root)))

(defun example-names ()
  (sort (loop for dir in (uiop:subdirectories (merge-pathnames "examples/" (repo-root)))
              when (probe-file (merge-pathnames "tsconfig.json" dir))
                collect (car (last (pathname-directory dir))))
        #'string<))

(defun tsc-available-p ()
  (and (probe-file (merge-pathnames "node_modules/typescript/bin/tsc" (repo-root)))
       (ignore-errors
        (zerop (sb-ext:process-exit-code
                (sb-ext:run-program "node" '("--version") :search t :output nil :error nil))))))

(defun copy-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun restore-table (table saved)
  (clrhash table)
  (maphash (lambda (k v) (setf (gethash k table) v)) saved))

(defmacro with-example-backend ((name) &body body)
  "Run BODY with only examples/NAME's server functions registered."
  `(let ((saved-functions (copy-table monolisp.rpc::*server-functions*))
         (saved-structs (copy-table monolisp.rpc::*rpc-structs*))
         (*authenticator* *authenticator*))
     (unwind-protect
          (progn
            (clear-server-functions)
            (clear-rpc-structs)
            (monolisp:load-server-files (example-dir ,name))
            ,@body)
       (restore-table monolisp.rpc::*server-functions* saved-functions)
       (restore-table monolisp.rpc::*rpc-structs* saved-structs))))

(defun run-tsc (dir)
  "Type-check every .ts/.tsx file in DIR with the repository's tsconfig.  Returns (values ok output)."
  (let ((tsconfig (merge-pathnames "tsconfig.json" dir))
        (root (namestring (repo-root))))
    (with-open-file (s tsconfig :direction :output :if-exists :supersede)
      (format s "{ \"extends\": ~S, \"compilerOptions\": { \"typeRoots\": [~S] }, \"include\": [\"*.ts\", \"*.tsx\"] }"
              (concatenate 'string root "tsconfig.json")
              (concatenate 'string root "node_modules/@types")))
    (let* ((output (make-string-output-stream))
           (proc (sb-ext:run-program "node"
                                     (list (namestring (merge-pathnames "node_modules/typescript/bin/tsc" (repo-root)))
                                           "--noEmit" "-p" (namestring tsconfig))
                                     :search t :output output :error output :directory root)))
      (values (zerop (sb-ext:process-exit-code proc)) (get-output-stream-string output)))))

(deftest examples-transpile-and-type-check (:suite "examples")
  (is (subsetp '("dashboard" "guestbook" "todo") (example-names) :test #'string=))
  (dolist (name (example-names))
    (let ((out-dir (merge-pathnames (format nil "monolisp-example-~A-~D/" name (random 1000000)) (uiop:temporary-directory))))
      (unwind-protect
           (with-example-backend (name)
             (let ((result (monolisp:build-frontend (example-dir name) out-dir)))
               (is (find "server-functions.ts" (getf result :outputs) :key #'file-namestring :test #'string=) "~A" name)
               (is (find "monolisp-runtime.ts" (getf result :outputs) :key #'file-namestring :test #'string=) "~A" name)
               (is (null (getf result :warnings)) "~A warnings: ~S" name (getf result :warnings))
               (if (tsc-available-p)
                   (multiple-value-bind (ok output) (run-tsc out-dir)
                     (is ok "~A: tsc errors:~%~A" name output))
                   (format t "~&    (tsc not available; skipping TypeScript verification of ~A)~%" name))))
        (ignore-errors (uiop:delete-directory-tree out-dir :validate t :if-does-not-exist :ignore))))))

;;; ------------------------------------------------------------------ backend behaviour

(defun call (method params &key headers)
  "One JSON-RPC call; returns the parsed response."
  (let ((*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (json-parse (handle-rpc-sync (json-encode (make-json-object "jsonrpc" "2.0" "method" method "params" params "id" 1))
                                 :context (make-rpc-context :headers headers)))))

(defun result (response) (json-get response "result"))
(defun err-code (response) (json-ref response "error" "code"))

(deftest guestbook-backend (:suite "examples")
  (with-example-backend ("guestbook")
    (let* ((session (result (call "sign-in" #("Ada"))))
           (token (json-get session "token"))
           (auth (list (cons "authorization" (concatenate 'string "Bearer " token)))))
      (is-equal 64 (length token))
      (is-equal "Ada" (json-get session "name"))
      (is-equal -32010 (err-code (call "sign-in" #("<script>"))))
      ;; guarded functions need the bearer token
      (is-equal +unauthorized+ (err-code (call "post-entry" #("hello"))))
      (is-equal :null (result (call "whoami" #())))
      (is-equal "Ada" (result (call "whoami" #() :headers auth)))
      (let ((entry (result (call "post-entry" #("hello" "happy") :headers auth))))
        (is-equal "happy" (json-get entry "mood"))
        (is-equal "Ada" (json-get entry "author"))
        ;; application errors carry their code, message and data
        (let ((dup (call "post-entry" #("hello") :headers auth)))
          (is-equal -32012 (err-code dup))
          (is-equal "You already posted that." (json-ref dup "error" "message"))
          (is-equal (json-get entry "id") (json-ref dup "error" "data")))
        (is-equal -32011 (err-code (call "post-entry" #("   ") :headers auth)))
        ;; the enum is enforced at the boundary
        (is-equal +invalid-params+ (err-code (call "post-entry" #("hi" "angry") :headers auth)))
        ;; pagination: newest first, cursor = id of the last entry on the page
        (dotimes (i 4) (call "post-entry" (vector (format nil "note ~D" i)) :headers auth))
        (let ((page (result (call "list-entries" #(2)))))
          (is-equal 2 (length (json-get page "entries")))
          (is-equal "note 3" (json-get (aref (json-get page "entries") 0) "message"))
          (let ((rest (result (call "list-entries" (vector 10 (json-get page "nextCursor"))))))
            (is-equal 3 (length (json-get rest "entries")))
            (is-equal :null (json-get rest "nextCursor"))))
        ;; someone else cannot delete Ada's entry
        (let* ((bob (json-get (result (call "sign-in" #("Bob"))) "token"))
               (bob-auth (list (cons "authorization" (concatenate 'string "Bearer " bob)))))
          (is-equal -32013 (err-code (call "delete-entry" (vector (json-get entry "id")) :headers bob-auth))))
        (is-equal :true (result (call "delete-entry" (vector (json-get entry "id")) :headers auth))))
      ;; moderator-only: no key configured, so it fails closed
      (is-equal +unauthorized+ (err-code (call "clear-guestbook" #() :headers auth)))
      (is-equal :true (result (call "sign-out" #() :headers auth)))
      (is-equal :null (result (call "whoami" #() :headers auth))))))

(deftest dashboard-backend (:suite "examples")
  (with-example-backend ("dashboard")
    (let ((series (result (call "get-series" #("cpu" 30)))))
      (is-equal "cpu" (json-get series "name"))
      (is-equal "ratio" (json-get series "unit"))
      (is-equal 30 (length (json-get series "samples"))))
    (is-equal +invalid-params+ (err-code (call "get-series" #("disk"))))
    (is-equal +invalid-params+ (err-code (call "get-series" #("cpu" 500))))
    ;; four probes of 30..200ms run concurrently: well under their 360ms+ sum
    (let* ((start (get-internal-real-time))
           (health (result (call "check-services" #())))
           (elapsed-ms (/ (- (get-internal-real-time) start) (/ internal-time-units-per-second 1000))))
      (is-equal 4 (length health))
      (is (< elapsed-ms 350) "check-services took ~Dms" (round elapsed-ms))
      (is (every (lambda (h) (member (json-get h "status") '("up" "degraded" "down") :test #'string=)) health)))
    (let ((p (result (call "latency-percentiles" (vector (coerce (loop for i from 1 to 100 collect (float i 1d0)) 'simple-vector))))))
      (is-equal 100 (json-get p "count"))
      (is (<= 50 (json-get p "p50") 51))
      (is (<= 99 (json-get p "p99") 100)))
    (let ((latest (result (call "latest-values" #()))))
      (is (json-object-p latest))
      (is-equal '("cpu" "latency" "memory") (sort (copy-list (json-keys latest)) #'string<)))))
