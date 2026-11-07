# Architecture

A map of the codebase for people who will change the framework itself. It covers how the pieces
fit, how a request and a build flow through them, where the extension points are, and which
invariants you must not break. For using the framework, start with the
[quick start](quick-start.md).

## The big picture

```
                     build time                                           run time
 ┌──────────────────────────────────────────────────────┐   ┌──────────────────────────────────────┐
 │ *.server.lisp ──load──▶ registry (monolisp/rpc)      │   │  browser                             │
 │                           │  server-function structs │   │    build/*.ts ──rpcCall──┐           │
 │                           ▼                          │   │                          │ POST /rpc │
 │                      server-manifest ───────────┐    │   │                          ▼           │
 │                                                 ▼    │   │  monolisp/http (strict HTTP/1.1)     │
 │ *.client(x).lisp ──read──▶ transpiler ──▶ build/*.ts │   │    └▶ monolisp/rpc (JSON-RPC 2.0)    │
 │      (monolisp/transpiler)   │ checks calls          │   │         └▶ your server function      │
 │                              ▼ against manifest      │   │              (on the async loop or   │
 │                 server-functions.ts, runtime,        │   │               the thread pool)       │
 │                 manifest.json ──▶ tsc --strict       │   └──────────────────────────────────────┘
 └──────────────────────────────────────────────────────┘
```

The same Lisp image that defines the server functions also produces the frontend's types. That
single source of truth is the point of the framework: the client stubs are generated from the
live registry, and the transpiler refuses calls that the registry would reject.

## Systems and packages

| ASDF system | Package | Directory | Role |
|---|---|---|---|
| `monolisp-async` (standalone) | `monolisp.async` | `src/async/` | promises, event loop, `select()` reactor, timers, thread pool, channels, CPS `async`/`await`, TCP, HTTP client |
| `monolisp/json` | `monolisp.json` | `src/json/` | octet-level JSON parser/encoder with hard resource limits |
| `monolisp/rpc` | `monolisp.rpc` | `src/rpc/` | schemas (`types.lisp`), limits/auth/rate limiting (`security.lisp`), registry + manifest (`registry.lisp`), dispatcher (`jsonrpc.lisp`) |
| `monolisp/http` | `monolisp.http` | `src/http/` | strict request-head parser and the JSON-RPC-only server |
| `monolisp/transpiler` | `monolisp.transpiler` | `src/transpiler/` | frontend dialect → TS/TSX, alignment files |
| `monolisp` | `monolisp` | `src/monolisp.lisp` | umbrella: re-exports the public API, `load-server-files`, `build-frontend`, `start` |
| `monolisp/tests` | `monolisp.test` | `tests/` | ~150-line test framework and all suites |

Dependencies point one way: `json` ← `rpc` ← `http`, and `rpc` ← `transpiler` (for the
manifest and name mangling). `async` is used by `rpc` and `http` but knows nothing about them.
Keep it that way: `monolisp-async` is designed to be split out as its own project.

Backend code (`*.server.lisp`) is loaded into the `monolisp-user` package, which uses `cl`,
`monolisp`, `monolisp.rpc`, `monolisp.async` and `monolisp.json`. Frontend code never touches
a host package: every client module is read into its own fresh package (see
[Transpiler internals](#transpiler-internals)).

## Request lifecycle

What happens to `POST /rpc` with `{"jsonrpc":"2.0","method":"post-entry","params":["hi"],"id":1}`:

1. **Accept** (`async/tcp.lisp`). The reactor accepts the connection, up to `:max-connections`,
   and `handle-connection` (`http/server.lisp`) starts reading into a growable buffer with a
   header timeout.
2. **Parse the head** (`http/parser.lisp`). `parse-request-head` either returns `:incomplete`
   (read more) or a request, or signals `http-parse-error` with a status: 400, 411, 413, 414, 415,
   431 or 501. It enforces a strict subset of HTTP/1.1, so there is no ambiguity to smuggle
   through.
3. **Route** (`dispatch-request`). There are exactly three routes: `GET /health`, `OPTIONS`
   preflight, and `POST /rpc`. Before reading the body, the server checks CORS origin, rate limit
   (token bucket per remote address), `Content-Type`, `Content-Length` and payload size.
4. **Read the body** under a body timeout, then build an `rpc-context` (remote address, headers,
   random request id).
5. **Decode** (`rpc/jsonrpc.lisp`, `handle-rpc-octets`). Bounded JSON parse, then a batch or a
   single request.
6. **Validate the envelope** (`validate-envelope`). It requires the exact member set and version
   `"2.0"`, a token-shaped method name, and an id that is a safe integer, string or null.
7. **Look up** the method by string hash (`find-server-function`, never `intern`).
8. **Authenticate** (`authenticate`). `*authenticator*` runs once per request; a non-nil
   return becomes `context-principal`, and any error means anonymous.
9. **Authorize** (`check-auth`). `:public`, `:authenticated`, or a predicate symbol called with
   the context. A failure gives `-32001`.
10. **Bind and validate params** (`bind-params`, `validate-value` in `types.lisp`). Positional or
    named params are checked against each parameter's schema. Error data names the parameter
    path, never the input.
11. **Invoke** (`invoke-server-function`). The call runs with `*request-context*` bound. The
    result may be a plain value or a promise; a timeout comes from `:timeout-ms` or the limits.
    `rpc-error` (from `rpc-fail`) goes to the client verbatim. Any other condition is logged via
    `*error-logger*` and reported as `-32603` (with the condition text only under `*debug-mode*`).
12. **Encode the result** (`encode-value`) against `:returns`. A mismatch is logged and becomes
    `-32603`, so a bug cannot leak data of the wrong shape.
13. **Respond** with security headers on every response, then keep the connection alive up to
    `:max-requests-per-connection`.

Server functions run **on the event loop thread**. Blocking there blocks every client: use
promises (`delay`, `fetch`, `all`) or `offload` for CPU-bound or blocking work. Special variables
bound around an `await` do not survive it; bind them inside the continuation instead.

## Build pipeline

`(monolisp:build-frontend src out)` is `transpile-project` (`transpiler/modules.lisp`) with the
live manifest:

1. **Collect** `*.client.lisp` and `*.clientx.lisp` in `src`. Each is read
   (`read-client-source`) into a fresh package `monolisp.client.<name>.<n>`.
2. **Order** modules topologically by their sibling imports (`order-modules`); cycles are an
   error.
3. **Compile** each module (`compile-module`) with `*project-modules*` holding the already
   compiled ones, so importers can see a sibling's exported inline aliases, carried imports and
   lambda lists.
4. **Write** `out/<name>.ts` or `.tsx`. A module with no import or export line gets `export {};`,
   so every output is an ES module.
5. **Write the alignment files** (`align.lisp`, `write-alignment-files`):
   * `server-functions.ts`: an interface per struct and an async stub per function, plus a
     manifest digest in the header (`alignment-stale-p` compares it with the live registry).
   * `monolisp-runtime.ts`: `rpcCall`, `rpcNotify`, `configureRpc`, `RpcError`.
   * `manifest.json`.
6. `scripts/build-example.lisp` then runs `tsc -p <example>/tsconfig.json`.

## Transpiler internals

Files, in load order (`monolisp.asd`):

| File | Responsibility |
|---|---|
| `conditions.lisp` | `transpile-error` / `transpile-warning`, `tfail` / `twarn` with file and form context |
| `reader.lisp` | the client readtable (`'{}` objects, `..` handling, readtable-case `:invert`), source size and depth checks before reading, per-module packages, symbol policing (`check-form-symbols`) |
| `names.lisp` | `mangle-name` (kebab → camel, `*x*` / `+x+` → `X`, reserved words), `property-name`, `sym-is` / `sym-name` |
| `emit.lisp` | the statement output stream (`emit`, `with-indent`, `capture-statements`), `js-string`, `js-number`, parenthesis helpers |
| `types.lisp` | `type->ts`: Lisp type specs to TypeScript type text |
| `env.lisp` | `transpile-config`, the `module` struct (per-module state), lexical `env` (variables, types, labels, scope, async-p, jsx-p) |
| `macros.lisp` | user `defmacro` (host-evaluated, body-checked) and inline-alias expansion (`expand-inline`, single-evaluation via `let`) |
| `loop.lisp` | the `loop` macro → `%while` / `%for-of` driver forms |
| `prelude.lisp` | ~100 built-in inline aliases, compiled once from Lisp source |
| `jsx.lisp` | `as-html-string` → JSX |
| `compiler.lisp` | the core: `cx` / `cs`, special forms, functions, `let`, conditionals, `handler-case`, structs, imports/exports, inline aliases across modules |
| `align.lisp` | type inference for server-call checks (`infer-type`, `schema-accepts-type-p`, `check-server-call`), stub and runtime generation |
| `modules.lisp` | `compile-module`, `transpile-string`, `transpile-file`, `transpile-project` |

### Two compilation modes

`compiler.lisp` compiles every form in one of two modes:

* `(cx form env)` returns **expression** text.
* `(cs form env dest)` **emits statements**. `dest` says what to do with the value:
  `:void` (discard), `:return`, or `(:set . "jsName")` (assign).

Statement-only constructs (`let`, `loop`, `handler-case`, `case`, ...) used in expression
position become IIFEs: `iife`, which is async if the body contains `await`. That is why the
output sometimes has `(() => { ... })()`. Lowering a form to statement position, usually by
binding it with `let` first, avoids it.

### Dispatch order in `cx`

For a call `(op args...)`:

1. a keyword `op` inside `as-html-string`: JSX;
2. a user macro: expand and recompile;
3. `*expr-forms*` (registered with `defexpr`), unless `op` is lexically bound;
4. `*stmt-forms*` (`defstmt`): compiled through an IIFE;
5. an inline alias (module or prelude): substitute and recompile;
6. a struct constructor or accessor;
7. a server function from the manifest: `compile-server-call`, which checks the call and notes
   the auto-import;
8. otherwise a plain call (`compile-call`), which also folds `:key value` arguments into the
   options object for known `&key` functions.

`cs` checks `*stmt-forms*` first, then inline aliases, then falls back to `(finish dest (cx ...))`.

### Per-module state

`*module*` holds everything one file accumulates:

* globals and their JS names;
* function signatures, plus pre-scanned lambda lists;
* structs and their accessors;
* inline aliases, both own and imported, with the import origins used to carry imports along
  with them;
* user macros;
* the server functions it calls (for the auto-import line).

Project-wide state lives in `*project-structs*`, `*project-struct-accessors*` and
`*project-modules*`. All three are rebound per `transpile-project` / `transpile-string` call.

## Extension recipes

### Add a special form

Register it in `compiler.lisp`:

```lisp
(defexpr "CLAMP" (form env)                        ; (clamp x lo hi) -> Math.min(Math.max(x, lo), hi)
  (destructuring-bind (x lo hi) (cdr form)
    (format nil "Math.min(Math.max(~A, ~A), ~A)" (cx x env) (cx lo env) (cx hi env))))
```

Use `defstmt` with `(form env dest)` when it needs statements, and finish with
`(finish dest expr)`. Names are matched by symbol name, so `clamp` works from every module
package. Add an `emits` test in `tests/transpiler-tests.lisp`. If the form is statement-only,
add it to `*statement-only*`.

Prefer an inline alias over a special form when the output is a single JS expression with the
arguments substituted in. That covers most API shorthands, and it costs one line of Lisp in
`prelude.lisp` or an alias file ([guide](inline-aliases.md)).

### Add a prelude alias

Add the name to the `declaim` and a `defun` to `*prelude-source*` in `prelude.lisp`. Every
module gets it, and a module's own definitions shadow it.

### Add a schema type

All in `rpc/types.lisp`:

1. `normalize-schema` and `schema-valid-p` (accept the new form);
2. `validate-value` (decoded JSON → Lisp value, or `schema-fail` with the path);
3. `encode-value` (Lisp value → JSON value);
4. `schema->ts` (TypeScript text) and `schema->json` (manifest).

Then teach `schema-accepts-type-p` in `transpiler/align.lisp` which frontend types it accepts.
Add tests in `types-tests.lisp`, hostile inputs in `attack-tests.lisp`, and a call-site check in
`transpiler-tests.lisp`.

### Change what the HTTP server serves

`dispatch-request` in `http/server.lisp`. The server is deliberately JSON-RPC only: static
files belong to a CDN or reverse proxy. If you add a route, add its parser and limit cases to
`http-parser-tests.lisp` / `attack-tests.lisp` and an end-to-end case to
`integration-tests.lisp`.

### Add an example

Create `examples/<name>/` with `*.server.lisp`, client modules, a `tsconfig.json` (extends
`../../tsconfig.json`, includes `build/**`), an `index.html` and a `vite.config.js` proxying
`/rpc`. `build-example.lisp --example <name>` and the `examples` test suite pick it up
automatically. Backend behaviour tests go in `tests/example-tests.lisp`, using
`with-example-backend`, which isolates the global registry.

## Invariants: do not break these

* **Hostile input never reaches the host Lisp unchecked.**
  * JSON is parsed from octets with limits.
  * Methods are looked up by string and never interned.
  * Params are validated before any user code runs.
  * Error responses never echo input.
  * Only `json-parse-error`, `http-parse-error` and `rpc-error` may escape the parsers. The
    fuzzers in `attack-tests.lisp` assert this.
* **Results are encoded against `:returns`.** Never write a raw Lisp value to the wire.
* **Auth fails closed.** An authenticator or predicate that signals means "no".
* **The transpiler never evaluates source**, apart from the bodies of user `defmacro`s:
  * reading uses `*read-eval*` nil, with source size and depth bounded first;
  * `#n=` and dotted pairs are refused;
  * package-qualified symbols outside the module, CL and client-lang packages are refused,
    expansions included;
  * macro bodies cannot use host-affecting operators. The literal part of a backquote template
    is exempt because it only becomes frontend code; everything under a comma is checked.

  This is a guard against mistakes, not a sandbox: only transpile source you would `load`.
* **Generated code must pass `tsc --strict`.** The `examples` suite type-checks every example;
  keep it green.
* **Output stays reproducible.** It has no timestamps and auto-imports are sorted, so the
  same sources give the same files and diffs of `build/` are meaningful.

## Testing strategy

| Suite | Use it for |
|---|---|
| `async` | promise semantics, loop/timer/pool behaviour, CPS macro coverage |
| `json`, `types`, `rpc`, `http-parser` | unit behaviour of each boundary layer |
| `transpiler` | one `emits` / `rejects` assertion per dialect feature; multi-module projects via `transpile-modules` |
| `examples` | every example transpiles and passes `tsc`; example backends behave (auth, errors, pagination, async) |
| `attack` | hostile inputs and fuzzing for every parser, including the transpiler reader and macro checks |
| `integration` | real sockets: keep-alive, CORS, limits, slowloris, caps |
| `perf` | throughput floors for the hot paths, so regressions show up as failures |

Run one suite with `--suite name` and one test with `--test name`. A framework change is not
done until `npm test` passes and all three examples still build.

## Known limitations and ideas

* **Imports carried by inline aliases** are emitted as one line per name
  (`import { useState } from "react"; import { useEffect } from "react";`). This is correct but
  verbose; merging lines with the same source would be a nice cleanup in `compile-module`.
* **No generic type parameters** in the dialect; users cast with `the`. Supporting
  `(declare (type-params T))` on `defun` would be the natural extension in
  `compile-defun` / `render-params`.
* **Macros are file-local** and cannot be exported. Inline aliases are the cross-module tool.
* **`transpile-file` compiles one file in isolation**, so it cannot see siblings' aliases or
  structs. Use `transpile-project` / `build-frontend`.
* **No watch mode.** `build-example.lisp` is one-shot; a watcher that reloads changed
  `*.server.lisp` into the running image and re-transpiles changed client modules would make the
  dev loop instant.
* **No static file serving** (by design). In development, Vite serves `build/` and proxies `/rpc`.
* **Keyword arguments** are folded only for functions whose lambda list the transpiler knows
  (same module, or imported by name from a sibling). Calls through variables or default imports
  pass options objects explicitly: `(f '{}(:retries 3))`.
