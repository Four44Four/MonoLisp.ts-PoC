;;;; monolisp.asd -- Monolisp.ts: monolithic full-stack application server in Common Lisp
;;;;
;;;; Systems:
;;;;   monolisp/json        hardened JSON parser + serializer
;;;;   monolisp/rpc         type schemas, server-function registry, strict JSON-RPC 2.0 dispatcher
;;;;   monolisp/http        strict HTTP/1.1 JSON-RPC-only server on the async reactor
;;;;   monolisp/transpiler  frontend Lisp dialect -> TypeScript / TSX transpiler
;;;;   monolisp             everything above (plus the standalone monolisp-async toolkit)
;;;;   monolisp/tests       unit, integration, attack-vector and performance tests
;;;;
;;;; The asynchronous toolkit lives in its own system (src/async/monolisp-async.asd) so that it
;;;; can be split into a separate project without touching the rest of the framework.

(defsystem "monolisp/json"
  :description "Strict, resource-bounded JSON parser and serializer (RFC 8259)."
  :pathname "src/json/"
  :serial t
  :components ((:file "package")
               (:file "json")))

(defsystem "monolisp/rpc"
  :description "Server-function registry and hardened JSON-RPC 2.0 boundary."
  :depends-on ("monolisp/json" "monolisp-async")
  :pathname "src/rpc/"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "security")
               (:file "registry")
               (:file "jsonrpc")))

(defsystem "monolisp/http"
  :description "Minimal strict HTTP/1.1 server exposing only the JSON-RPC endpoint."
  :depends-on ("monolisp/rpc" "monolisp-async")
  :pathname "src/http/"
  :serial t
  :components ((:file "package")
               (:file "parser")
               (:file "server")))

(defsystem "monolisp/transpiler"
  :description "Transpiler from the Monolisp frontend Lisp dialect to TypeScript/TSX."
  :depends-on ("monolisp/rpc" "monolisp/json")
  :pathname "src/transpiler/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "reader")
               (:file "names")
               (:file "emit")
               (:file "types")
               (:file "env")
               (:file "macros")
               (:file "loop")
               (:file "prelude")
               (:file "jsx")
               (:file "compiler")
               (:file "align")
               (:file "modules")))

(defsystem "monolisp"
  :description "Monolisp.ts -- monolithic full-stack application server written in Common Lisp."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("monolisp-async" "monolisp/json" "monolisp/rpc" "monolisp/http" "monolisp/transpiler")
  :pathname "src/"
  :components ((:file "monolisp"))
  :in-order-to ((test-op (test-op "monolisp/tests"))))

(defsystem "monolisp/tests"
  :depends-on ("monolisp")
  :pathname "tests/"
  :serial t
  :components ((:file "framework")
               (:file "async-tests")
               (:file "json-tests")
               (:file "types-tests")
               (:file "rpc-tests")
               (:file "http-parser-tests")
               (:file "transpiler-tests")
               (:file "example-tests")
               (:file "attack-tests")
               (:file "integration-tests")
               (:file "perf-tests"))
  :perform (test-op (o c)
             (unless (symbol-call :monolisp.test :run-all-tests)
               (error "Monolisp test suite failed"))))
