;;;; monolisp-async.asd -- standalone asynchronous toolkit used by Monolisp.ts
;;;;
;;;; This system has no dependencies outside of SBCL contribs so it can be split into
;;;; its own project.  It provides:
;;;;
;;;;   promise.lisp      thread-safe promises (then / catch / finally / all / race / any / all-settled)
;;;;   loop.lisp         single-threaded event loop: task queue, timer heap, cross-thread wakeups
;;;;   reactor.lisp      select()-based readiness reactor over non-blocking BSD sockets
;;;;   thread-pool.lisp  bounded worker pool for blocking / CPU work -> promises
;;;;   channel.lisp      async channels and semaphores
;;;;   async-macros.lisp CPS `async` / `await`, `alet`, `alet*`, `chain`, `with-timeout`, ...
;;;;   tcp.lisp          async TCP server/client streams on the reactor
;;;;   http-client.lisp  minimal async HTTP/1.1 client (`fetch`) for calling slow backend APIs

(defsystem "monolisp-async"
  :description "libuv-style asynchronous toolkit for SBCL: event loop, promises, async/await macros."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("sb-bsd-sockets" "sb-concurrency")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "promise")
               (:file "loop")
               (:file "reactor")
               (:file "thread-pool")
               (:file "channel")
               (:file "async-macros")
               (:file "tcp")
               (:file "http-client")))
