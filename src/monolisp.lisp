;;;; src/monolisp.lisp -- the umbrella package: one entry point for applications
;;;;
;;;; A Monolisp application is:
;;;;   * backend  .server.lisp files: DEFINE-RPC-STRUCT / DEFINE-SERVER-FUNCTION forms (package MONOLISP.RPC)
;;;;   * frontend .client.lisp / .clientx.lisp files transpiled to TS/TSX (package MONOLISP.TRANSPILER)
;;;;   * (MONOLISP:BUILD-FRONTEND src out)  -> TypeScript + typed server stubs + runtime
;;;;   * (MONOLISP:START ...)               -> the JSON-RPC-only HTTP server on the async loop

(defpackage :monolisp
  (:use :cl)
  (:import-from :monolisp.rpc #:define-rpc-struct #:define-server-function #:rpc-fail #:*request-context*
                #:context-principal #:context-header #:context-remote-address #:*authenticator* #:*debug-mode*
                #:server-manifest #:server-manifest-json #:list-server-functions #:make-rate-limiter)
  (:import-from :monolisp.http #:start-server #:stop-server #:run-server #:server-port #:server-stats #:*server*)
  (:import-from :monolisp.transpiler #:transpile-project #:transpile-file #:transpile-string
                #:write-alignment-files #:alignment-stale-p #:transpile-error)
  (:import-from :monolisp.async #:with-event-loop #:run-loop #:stop-loop #:async #:await #:define-async
                #:delay #:offload #:then #:fetch)
  (:export #:define-rpc-struct #:define-server-function #:rpc-fail #:*request-context* #:context-principal
           #:context-header #:context-remote-address #:*authenticator* #:*debug-mode*
           #:server-manifest #:server-manifest-json #:list-server-functions #:make-rate-limiter
           #:start-server #:stop-server #:run-server #:server-port #:server-stats #:*server*
           #:transpile-project #:transpile-file #:transpile-string #:write-alignment-files #:alignment-stale-p
           #:transpile-error
           #:with-event-loop #:run-loop #:stop-loop #:async #:await #:define-async #:delay #:offload #:then #:fetch
           #:load-server-files #:build-frontend #:start #:version))

(in-package :monolisp)

(defparameter *version* "0.1.0")
(defun version () *version*)

(defun load-server-files (directory)
  "Load every *.server.lisp file in DIRECTORY (with *READ-EVAL* disabled) into MONOLISP.RPC's
user package so their DEFINE-SERVER-FUNCTION forms register.  Returns the loaded pathnames."
  (let ((files (remove-if-not (lambda (p) (search ".server" (pathname-name p)))
                              (uiop:directory-files (uiop:ensure-directory-pathname directory) "*.lisp")))
        (*read-eval* nil)
        (*package* (or (find-package :monolisp-user)
                       (make-package :monolisp-user :use '(:cl :monolisp :monolisp.rpc :monolisp.async :monolisp.json)))))
    (dolist (f files files)
      (load f))))

(defun build-frontend (src-dir out-dir &key (react-p t) (strict-server-calls t))
  "Transpile all client modules in SRC-DIR to OUT-DIR and write the typed server stubs/runtime
generated from the currently registered server functions."
  (transpile-project src-dir out-dir :manifest (server-manifest) :react-p react-p
                                     :strict-server-calls strict-server-calls))

(defun start (&rest args &key (port 8080) &allow-other-keys)
  "Run the JSON-RPC server (blocking) with the currently registered server functions."
  (apply #'run-server :port port args))
