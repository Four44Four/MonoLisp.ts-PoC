---
name: monolisp-server-functions
description: Builds Monolisp.ts backend APIs in *.server.lisp files with define-server-function and define-rpc-struct, including schemas, authentication (*authenticator*, :auth guards), application errors (rpc-fail), async work (define-async, await, all, offload) and backend tests. Use when adding or changing server functions, RPC structs, auth, error codes, or JSON-RPC behaviour, or when the user mentions endpoints, API routes, procedures or server actions in this repository.
---

# Monolisp server functions

Backend files (`*.server.lisp`) are ordinary Common Lisp, loaded into the `monolisp-user`
package (it uses `cl`, `monolisp`, `monolisp.rpc`, `monolisp.async` and `monolisp.json`). Each
`define-server-function` becomes a JSON-RPC method at `POST /rpc`, a typed TS stub in
`build/server-functions.ts`, and a build-time check on every frontend call.

## Template

```lisp
(in-package :monolisp-user)

(define-rpc-struct guest-entry                 ; -> export interface GuestEntry { ... }
  (id fixnum)
  (author (string 40))                         ; bounded string: longer input is rejected
  (mood (member "happy" "neutral" "sad"))      ; enum -> "happy" | "neutral" | "sad"
  (tags (array (string 32) 16) :optional t))   ; tags?: string[] (max 16)

(define-server-function post-entry ((message (string 280))
                                    &optional ((mood (member "happy" "neutral" "sad")) "neutral"))
    (:returns guest-entry :auth :authenticated :timeout-ms 2000 :doc "Shown as JSDoc on the stub.")
  (when (zerop (length (string-trim " " message)))
    (rpc-fail "Say something first." :code -32011))
  (make-guest-entry :id 1 :author (context-principal *request-context*) :message message :mood mood))
```

Parameters are `(name schema)`; optional ones are `((name schema) default)` after `&optional`.
`:returns` is required.

## Schemas

`fixnum integer float number string boolean null keyword`, plus `(string max)`,
`(integer lo hi)`, `(array elem max)`, `(tuple a b)`, `(or a b)`, `(nullable a)`,
`(member "a" "b")`, `(object (field type [:optional t]) ...)`, `(map value-type)` and any
`define-rpc-struct` name.

Return values:

- **object**: a property list `(list :entries page :next-cursor nil)` or an alist;
- **map**: a hash table;
- **array**: a list or vector;
- **struct**: `make-<struct>`;
- **null**: `nil`, which is also `false` for `boolean`.

A result that does not match `:returns` is logged and becomes `-32603`; it never leaks.

## Auth

```lisp
(setf *authenticator*                          ; runs once per request; return a principal or NIL; must not signal
      (lambda (context) (gethash (bearer-token context) *sessions*)))
;; :auth :public (default) | :authenticated | a predicate symbol called with the context
(defun moderator-p (context)
  (let ((key (context-header context "x-moderator-key")))
    (and *moderator-key* key (constant-time-equal key *moderator-key*))))   ; fail closed
```

Helpers: `(context-principal *request-context*)`, `(context-header ctx "name")` (lowercase
lookup), `(context-remote-address ctx)`, `(random-token 32)` (OS CSPRNG, hex),
`(constant-time-equal a b)`.

## Errors

`(rpc-fail "human message" :code -32010 :data id)`. Codes must be in -32000..-32099 or positive;
the message and `:data` reach the client as `RpcError.message` / `.data`. Treat the codes as API
contract and document them. Reserved codes: -32001 unauthorized, -32002 rate limited, -32003
timeout, -32602 invalid params, -32603 internal. Any other condition becomes an opaque `-32603`;
set `*debug-mode*` to `t` locally to see its text.

## Async: never block the event loop

Server functions run on the loop thread.

```lisp
(define-async probe (name) (await (delay 50)) name)                     ; waits without blocking
(define-server-function check () (:returns (array string) :timeout-ms 1000)
  (all (list (probe "a") (probe "b"))))                                 ; parallel, Promise.all
(define-server-function crunch ((xs (array float 10000))) (:returns float)
  (offload (lambda () (reduce #'+ xs))))                                ; CPU work on the thread pool
```

Returning a promise is fine; the dispatcher awaits it. Special variables do not survive an
`await`. Use `fetch` for outbound HTTP.

## Verify

1. Build and type-check the frontend against the new manifest:
   `sbcl --script scripts/build-example.lisp --example <name>`. Stale frontend calls now fail
   with messages like `argument x of server function f must be ...`.
2. Call it directly: `sbcl --script scripts/build-example.lisp --example <name> --serve`, then
   ```sh
   curl -s localhost:8080/rpc -H 'Content-Type: application/json' \
     -d '{"jsonrpc":"2.0","method":"post-entry","params":["hi"],"id":1}'
   ```
   Params may be positional or named (camelCase keys).
3. Add backend tests to `tests/example-tests.lisp` with `with-example-backend` (isolates the
   registry) and `monolisp.rpc::handle-rpc-sync`; follow `guestbook-backend` there. Run
   `sbcl --script scripts/run-tests.lisp --suite examples`.

## References

- `examples/guestbook/api.server.lisp`: sessions, guards, error codes, pagination
- `examples/dashboard/api.server.lisp`: `define-async`, `all`, `offload`, `map` / `object` results
- `docs/architecture.md`: the request lifecycle step by step
- `README.md`: the security model of the boundary
