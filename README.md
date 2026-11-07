# Monolisp.ts

A monolithic full-stack application server written in Common Lisp (SBCL), built from `specification.txt`.

* **Backend**: server functions written in Lisp, exposed as a strict JSON-RPC 2.0 endpoint behind a hand-written, asynchronous HTTP/1.1 server. Every byte that crosses the (de)serialization boundary is treated as hostile.
* **Frontend**: a Lisp dialect transpiled to TypeScript / TSX (React), with typed stubs for every server function generated from the backend's own type schemas, so the two halves cannot drift apart silently.
* **Async toolkit** (`monolisp-async`, its own ASDF system in `src/async/`): promises, a `select()` reactor, timers, a thread pool, channels, and compile-time `async`/`await` macros in the style of cl-async / libuv.

Zero external Lisp dependencies: only SBCL contribs (`sb-bsd-sockets`, `sb-alien`, `sb-concurrency`). `node` + `typescript` are used to type-check the generated frontend, and Vite serves it during development.

## Documentation

| Read this | When |
|---|---|
| [Quick start](docs/quick-start.md) | first hour: install, run an example, add a feature end to end |
| [TypeScript developer's guide](docs/typescript-guide.md) | you know TS/React and want the dialect spelled out, with the gotchas |
| [Writing an inline alias file](docs/inline-aliases.md) | giving a JS/TS library short Lisp names at zero runtime cost |
| [Architecture](docs/architecture.md) | before changing the framework: system map, request lifecycle, extension recipes, invariants |
| Example READMEs: [todo](examples/todo/README.md), [guestbook](examples/guestbook/README.md), [dashboard](examples/dashboard/README.md) | how each app works and which features it uses |

Agent skills for Cursor live in `.cursor/skills/` (`monolisp-frontend`, `monolisp-server-functions`,
`monolisp-inline-aliases`, `monolisp-framework-internals`) and are picked up automatically when
an agent works on the matching files.

```
monolisp.asd                 systems: monolisp/json, /rpc, /http, /transpiler, monolisp, monolisp/tests
src/async/                   monolisp-async (standalone system)
src/json/                    hardened JSON parser + encoder
src/rpc/                     type schemas, server-function registry, JSON-RPC dispatcher, security
src/http/                    strict HTTP/1.1 parser + JSON-RPC-only server
src/transpiler/              frontend Lisp -> TS/TSX, alignment (stubs, runtime, manifest)
src/monolisp.lisp            umbrella package: load-server-files, build-frontend, start
examples/todo/               starter: CRUD server functions + a React list
examples/guestbook/          vanilla DOM: bearer-token auth, guards, typed error codes, pagination, alias file
examples/dashboard/          React: async fan-out, thread-pool offload, alias files for React/Canvas/Intl
docs/                        quick start, TypeScript guide, inline alias guide, architecture
.cursor/skills/              agent skills for working in this repository
tests/                       unit, integration, attack-vector and performance suites
scripts/                     load.lisp (ASDF registration), run-tests.lisp, build-example.lisp (sbcl --script entry points)
```

## Quick start (Linux, macOS, Windows; SBCL and node on PATH)

All framework utilities are Lisp files run with `sbcl --script`, so the same commands work in bash,
zsh, cmd and PowerShell with no shell scripts in between:

```sh
npm install                                             # typescript, react, react-dom, vite (dev only)
sbcl --script scripts/run-tests.lisp                    # all 10 suites (~30s); --suite transpiler for one
sbcl --script scripts/build-example.lisp                # load backend, transpile frontend, run tsc against the manifest
sbcl --script scripts/build-example.lisp --serve        # ... then serve JSON-RPC on http://127.0.0.1:8080/rpc
sbcl --script scripts/build-example.lisp --example guestbook --serve   # another example (--list shows them)
npx vite examples/guestbook                             # in a second terminal: the page, with /rpc proxied to :8080
```

Each example has `npm run build:<name>`, `serve:<name>` and `dev:<name>` scripts (`example` is
the todo app), so `npm run serve:dashboard` + `npm run dev:dashboard` runs the React dashboard.
`npm test`, `npm run build:example` and `npm run serve:example` are aliases for the commands above
(`npm test -- --suite json` forwards arguments; PowerShell swallows a bare `--`, so quote it there:
`npm test '--' --suite json`). If `sbcl` is not on PATH, give the full path to the
binary instead (`"C:\Program Files\Steel Bank Common Lisp\sbcl.exe" --script scripts/run-tests.lisp`).

From a REPL (`sbcl --load scripts/load.lisp`):

```lisp
(load "scripts/load.lisp")
(asdf:load-system :monolisp)
(monolisp:load-server-files "examples/todo/")        ; registers the server functions
(monolisp:build-frontend "examples/todo/" "examples/todo/build/")
(monolisp:start :port 8080 :cors-origins '("http://localhost:5173"))
```

## Backend: server functions

```lisp
(define-rpc-struct todo
  (id fixnum)
  (title (string 200))
  (done boolean)
  (tags (array (string 32) 16) :optional t))

(define-server-function add-todo ((title (string 200)) &optional ((tags (array (string 32) 16)) nil))
    (:returns todo :doc "Create a todo.")
  (let ((todo (make-todo :id (incf *next-id*) :title title :done nil :tags (and tags (coerce tags 'list)))))
    (vector-push-extend todo *todos*)
    todo))

(define-server-function admin-reset ()
    (:returns boolean :auth :authenticated :timeout-ms 2000)
  (setf (fill-pointer *todos*) 0) t)
```

Each parameter and the return value carry a **schema**. The same schema is used for

1. validating the decoded JSON (`validate-value`) before the function body runs,
2. encoding the result (`encode-value`) so a wrong return type is an internal error, never a leak,
3. rendering the TypeScript signature (`schema->ts`) and the JSON manifest,
4. compile-time checking of frontend call sites.

Schema language: `fixnum integer float number string boolean null keyword` plus bounded forms `(string max)`, `(integer lo hi)`, `(array elem max)`, `(tuple a b)`, `(or a b)`, `(nullable a)`, `(member "a" "b")`, `(object (field type [:optional t]) ...)`, `(map value-type)` and any `define-rpc-struct` name. Server functions may return a promise (they run on the event loop; use `then`, `delay`, `async`/`await`, or `offload` for blocking work).

Calls arrive at `POST /rpc` as JSON-RPC 2.0 (single or batch, positional or named params, notifications supported). `*request-context*` gives access to the principal, headers and remote address; `*authenticator*` turns a context into a principal; `:auth :authenticated` or `:auth predicate-fn` guards a function. `rpc-fail` returns an application error with a code in the -32000..-32099 range; every other condition is logged server-side and reported as an opaque `-32603 Internal error` unless `*debug-mode*` is set.

## Security model of the boundary

The JSON parser (`src/json/json.lisp`) works on octets and enforces: strict UTF-8 (no overlongs, no encoded surrogates, nothing above U+10FFFF), maximum size / nesting depth / string length / number length / exponent / array items / object keys / total values, no duplicate keys (configurable), no control characters, correct surrogate pairs in `\u` escapes, no trailing garbage, no comments, no `NaN`/`Infinity`. The encoder escapes `<`, `>`, `&`, U+2028/9 and control characters, refuses non-finite floats and surrogates, and detects cycles.

The JSON-RPC dispatcher (`src/rpc/jsonrpc.lisp`) refuses unknown envelope members, non-`"2.0"` versions, non-string or non-token method names (so no `rpc.` prefix, no unicode, no path tricks), oversized or non-scalar ids, params that are neither array nor object, over-cap batches, oversized payloads; it validates params against the schema with error data that never reflects input, applies per-function timeouts, a token-bucket rate limiter per remote address, and auth checks that fail closed.

The HTTP server (`src/http/`) only knows `POST /rpc`, `GET /health` and `OPTIONS` preflight (CORS allow-list, no wildcard unless `:cors-origins :any`). The parser is a strict subset of HTTP/1.1: single spaces in the request line, origin-form targets with RFC 3986 characters only, token header names, no obsolete folding, exactly one well-formed `Host`, a single unambiguous `Content-Length`, `Transfer-Encoding` answered with 501 (no chunked, hence no CL/TE smuggling), 411/413/414/415/431 limits, header and body timeouts (slowloris), a cap on requests per keep-alive connection and on concurrent connections, and `nosniff` / `no-store` / `DENY` / CSP headers on every response.

The transpiler reads source with `*read-eval*` bound to `nil`, bounds source size and nesting depth before the recursive reader runs, refuses `#n=`/`#n#` structure and dotted pairs, rejects any package-qualified symbol that reaches outside the module / CL / client language (also inside backquote commas and in macro expansions), and forbids host-affecting operators (files, packages, `eval`, `funcall`/`apply`, global definitions, I/O) in the code a `defmacro` runs (the literal part of a backquote template is only output, so `` `(setf ,place 1) `` is fine; everything under a comma is checked). Macro bodies still run in the host image at build time: this is a safety net against mistakes, not a sandbox for untrusted source.

`tests/attack-tests.lisp` exercises each of these classes plus randomized fuzzing (JSON, JSON-RPC, HTTP) asserting that only the designated condition types ever escape a parser. `tests/integration-tests.lisp` repeats the socket-level ones (smuggling, slowloris, connection/request caps, CORS injection, 429) against a real server over loopback.

## Async toolkit (`monolisp-async`)

```lisp
(with-event-loop ()
  (define-async fetch-both (a b)
    (let ((ra (await (fetch a)))
          (rb (await (fetch b))))
      (values (http-response-status ra) (http-response-status rb))))
  (then (fetch-both "http://127.0.0.1:8080/health" "http://127.0.0.1:8080/health")
        (lambda (s) (format t "~A~%" s))))
```

* `promise`, `resolve`/`reject`, `then`/`catch*`/`finally*`, `all`/`race`/`any`/`all-settled`, `promise-wait` (blocking helper for tests), unhandled-rejection hook.
* Event loop with a `select()`-based reactor (`sb-alien`, no FFI libraries), timers (`set-timeout`, `set-interval`, `delay`), `post`/`next-tick`, cross-thread wakeups, per-callback error isolation, ref/unref.
* `async`/`await`/`define-async`/`async-lambda`: a compile-time CPS transform supporting `let`, `if`/`cond`/`case`, `and`/`or`, `progn`, `block`/`return-from`, `dolist`/`dotimes`/`loop` (subset), `handler-case`, `unwind-protect`, `setf`, `multiple-value-bind`. Deep iteration does not grow the stack. `alet`/`alet*`, `chain`, `with-timeout`, cancellation tokens, `wait-all`/`await-all`.
* Thread pool `offload` for blocking work with saturation back-pressure; bounded `channel`s and async semaphores.
* Non-blocking TCP (`tcp-listen`, `tcp-connect`, `conn-read`/`conn-write`, idle timeouts) and an HTTP `fetch` client (content-length and chunked bodies, header-injection guard).

Special bindings do not survive an `await` (the continuation runs from the loop thread later); bind specials around the `async` form, not inside it.

## Frontend dialect

Files: `name.client.lisp` -> `name.ts`, `name.clientx.lisp` -> `name.tsx`. One file is one ES module. Source is read with readtable-case `:invert`, so `fooBar` and `React` round-trip verbatim while `foo-bar` becomes `fooBar` and `*max-size*`/`MAX-SIZE` become `MAX_SIZE`. Reserved words get a trailing underscore (`else` -> `else_`).

| Lisp | TypeScript |
|---|---|
| `(defvar x 1)` / `(defparameter x 1)` / `(defconstant +x+ 1)` | `var x = 1;` / `let x = 1;` / `const X = 1;` |
| `(let ((a 1) (:const b 2)) ...)`, `let*` | `let a = 1; const b = 2;` (collision-free renaming on shadowing) |
| `(let ((:multi (q r) (divmod 7 2)) (:const :multi (state set-state) (useState ""))) ...)` | `let [q, r] = divmod(7, 2); const [state, setState] = useState("");` (declared types become a tuple annotation; a literal `(values ...)` initializer binds one variable at a time) |
| `(defun f (a &optional (b 2) &key k) ...)`, `(f 1 :k 3)` | `function f(a: any, b: any = 2, { k }: { k?: any } = {})`, `f(1, undefined, { k: 3 })` (keyword arguments become the options object, also for functions imported from sibling modules) |
| `(sort xs #'<)`, `(reduce #'+ xs :initial-value 0)`, `(remove-if-not #'evenp xs)` | `#'` on a built-in operator becomes an arrow function: `(a, b) => a < b`, `(a) => a % 2 === 0` |
| `(declare (string a) (fixnum b) (returns boolean))`, `(declaim (ftype ...))`, `(declaim (type string *x*))` | TS annotations; missing declarations become `any` |
| `(lambda (e) ...)` | `(e: any) => ...` |
| `(async (defun f () (await p)))`, `(async (lambda ...))`, `(async body...)` | `async function`, `async () =>`, async IIFE |
| `'{}(:a 1 :b x)`, `'{}((spread o) :c 3)`, `'{}` | `{ a: 1, b: x }`, `{ ...o, c: 3 }`, `{}` |
| `#(1 2)`, `(vector 1 2)` | `[1, 2]` (no lists: `list`, `cons`, `dolist` are compile errors) |
| `:keyword` | `":keyword"` (keywords are strings prefixed with `:`) |
| `(.. obj prop (method a b))`, `(.. React (createElement "h1" props "hi"))`, `(.. a (:? b) (:? (c 1)))`, `(.. obj "weird key")`, `(.. xs (aref i))` | `obj.prop.method(a, b)`, `React.createElement("h1", props, "hi")`, `a?.b?.c(1)`, `obj["weird key"]`, `xs[i]`. A list step is always a call and its first element is always the method name; `(.. obj method (args))` is an error |
| `(nil-or a b 0)` | `a ?? b ?? 0` |
| `(new Map)`, `(new Foo 1 2)` | `new Map()`, `new Foo(1, 2)` |
| `(spread xs)` in call/array/object position | `...xs` |
| `(progn ...)` | `{ ... }` or an IIFE in expression position |
| `(if c a b)`, `(when c ...)`, `(unless c ...)`, `(cond ...)` | ternary / `if` / `else if` chains |
| `(case x ((1 2) ...) (:big ...) (otherwise ...))` | `switch` with `case 1: case 2:`, `case ":big":`, `default:` |
| `(loop for x across xs ... collect/sum/count/maximize/when/unless/while/until/return/always/never/thereis/finally ...)`, `for i from/to/below/downto/above/by`, `for x = init then next`, `for k being the hash-keys ... using (hash-value v)`, `repeat`, `with` | `for (const x of xs)` or a `while (true)` driver loop with labeled breaks |
| `(for-each (x xs [i]) ...)` | `xs.forEach((x, i) => { ... })` -- side effects only, yields nothing |
| `(map (x xs [i]) ...)`, `(map 'vector f xs)` | `xs.map((x, i) => ...)`, `xs.map(f)` -- builds and returns a new array (no `dotimes`/`doarray`/`dolist`: use `loop for i below n` / `loop for x across xs ... return` for counted loops and early exit) |
| `(block b ... (return-from b v))`, `(return v)` inside `loop` | labeled blocks / early `return` |
| `(handler-case form (TypeError (e) ...) (error (e) ...))` | `try { } catch ($err) { if ($err instanceof TypeError) ... }` |
| `(unwind-protect f cleanup)`, `(ignore-errors f)`, `(error "bad ~A" x)`, `(error 'type-error "no")` | `try/finally`, `try/catch {}`, `throw new Error(\`bad ${x}\`)`, `throw new TypeError("no")` |
| `(values a b)`, `(setf (values a b) (f))`, `(destructuring-bind (x &rest r) xs ...)` | `return [a, b]` (multiple values are an array; bind them with a `:multi` `let` binding), `[a, b] = f()`, `const [x, ...r] = xs` |
| `(concat "a" x "b")`, `(format nil "~A items~%" n)` | template literals |
| `(defstruct point (x 0 :type fixnum) (y 0 :type fixnum))`, `(defstruct (p3 (:include point)) z)` | `interface Point { x: number; y: number }`, `makePoint({x = 0, y = 0} = {}): Point`, `p.x`, `(setf (point-x p) 1)` -> `p.x = 1`, `copy-point`, inherited accessors `p3-x`; accessors work across module imports |
| `(deftype id () '(or string fixnum))` | `type Id = string \| number;` |
| `(import :from "three" :targets (WebGLRenderer (:type Vector3Like) MathUtils))`, `:targets :all :as THREE`, `:targets React`, `(import :from :server :targets (list-todos))` | `import { WebGLRenderer, type Vector3Like, MathUtils } from "three"`, `import * as THREE`, default import, typed server stubs |
| `(export (defun ...))`, `(export a b)`, `(export-default (defun App () ...))` | `export function`, `export { a, b }`, `export default App` |
| `(as-html-string (:div :class "x" :onClick (lambda (e) ...) (:h1 "hi") (map (t items) (:li :key t t))))` (only in `.clientx.lisp`) | JSX with `className`, `htmlFor`, event handlers, `{items.map(...)}` (`for-each` is rejected there: it yields no children), fragments for multiple roots; `React` is auto-imported (configurable) |
| `(declaim (inline console-log)) (defun console-log (&rest a) (console.log (spread a)))` | call sites are substituted (single evaluation preserved); a prelude of ~100 aliases (`console-log`, `doc-get-element-by-id`, `add-event-listener`, `stringify-json`, `json-fetch`, `array-push`, `slice`, ...) is built in |
| `(declaim (inline vec3)) (export (defun vec3 (x y z) (new THREE.Vector3 x y z)))` in one module, `(import :from "three-aliases" :targets (vec3))` in another | the importer expands `(vec3 1 2 3)` in place and receives the library imports the alias body uses; no runtime import of the alias module. See [docs/inline-aliases.md](docs/inline-aliases.md) |
| `(defmacro twice (f) \`(progn ,f ,f))` | expanded at build time (file-local) |

Type mapping: `nil` -> `null`, `:undefined` -> `undefined`, `keyword`/`symbol`/`character` -> `string`, `number` -> `number | bigint`, `fixnum`/`float`/`real` -> `number`, `bignum` -> `bigint`, `ratio` -> `{ numerator: bigint; denominator: bigint }`, `complex` -> `{ real: number; imag: number }`, `array`/`vector` -> `any[]` or `T[]`, `bit-vector` -> `Uint8Array`, `hash-table` -> `Map<K, V>`, `(or a b)`, `(member ...)`, `(function (a b) r)`, `(promise t)`, `(values a b)` -> tuple, struct names -> PascalCase interfaces, anything else verbatim (`|Vector3Like|`, `(|Foo| string)` -> `Foo<string>`).

Not supported on purpose (clear compile errors): `optimize` declarations, lists and cons cells, `dolist`/`dotimes`/`doarray` (use `for-each`, `map` or `loop`), `multiple-value-bind`/`multiple-value-setq`/`multiple-value-list`/`nth-value` (use `:multi` `let` bindings, `(setf (values ...))` and `aref`), `tagbody`/`go`, `eval`/the reader, CLOS, `sleep`, `defstruct` with more than one `:include`, `await` outside `async`.

## Backend <-> frontend alignment

`(monolisp:build-frontend src out)` (or `transpile-project`) writes, next to the modules:

* `monolisp-runtime.ts`: a strict JSON-RPC client (`rpcCall`, `rpcNotify`, `configureRpc`, `RpcError`) that validates every response envelope, matches ids, times out via `AbortController` and sends credentials only when configured.
* `server-functions.ts`: `export interface Todo {...}` for every `define-rpc-struct` and `export function addTodo(title: string, tags?: string[]): Promise<Todo>` for every server function, plus `serverFunctionNames`. The header carries a FNV-1a digest of the manifest; `alignment-stale-p` compares it with the live registry (use it in CI or a dev-server hook).
* `manifest.json`: the machine-readable API description (`server-manifest-json`).

While compiling, every call to a server function (auto-imported, or imported explicitly with `(import :from :server ...)`) is checked against the manifest: arity, literal and declared argument types (`(greet 42)` when `name` is `(string 64)` is a build error; `:strict-server-calls nil` downgrades it to a warning). `tsc --strict` on the output then verifies everything the Lisp side could not infer. `scripts/build-example.lisp` performs the complete loop for any example (`--example NAME`, default `todo`); the `examples` test suite performs it for every example as a test (skipping `tsc` when node is unavailable).

## Tests

```sh
sbcl --script scripts/run-tests.lisp                          # everything
sbcl --script scripts/run-tests.lisp --suite attack           # one suite (repeatable)
sbcl --script scripts/run-tests.lisp --test parse-numbers     # one test by name (repeatable)
sbcl --script scripts/run-tests.lisp --verbose                # print test names
sbcl --script scripts/run-tests.lisp --list                   # list suites
```

Exit status is 0 when everything passed, 1 on failures, 2 on usage errors, so the command can be
used directly in CI on any platform.

| Suite | File | What |
|---|---|---|
| async | `tests/async-tests.lisp` | promises, loop, timers, pool, channels, CPS macros, TCP, fetch |
| json | `tests/json-tests.lisp` | parser/encoder semantics |
| types | `tests/types-tests.lisp` | schema validation, encoding, TS rendering, name mangling |
| rpc | `tests/rpc-tests.lisp` | registry, dispatcher, auth, timeouts, batches, rate limiter |
| http-parser | `tests/http-parser-tests.lisp` | strict request-head parsing |
| transpiler | `tests/transpiler-tests.lisp` | every dialect feature, multi-module projects, alignment |
| examples | `tests/example-tests.lisp` | every example through `tsc`; guestbook and dashboard backend behaviour |
| attack | `tests/attack-tests.lisp` | hostile inputs for JSON, JSON-RPC, HTTP, schemas, the transpiler reader; fuzzing |
| integration | `tests/integration-tests.lisp` | real sockets: end-to-end calls, keep-alive, CORS, limits, slowloris, caps |
| perf | `tests/perf-tests.lisp` | ops/sec floors for the hot paths (parse, encode, dispatch, validate, HTTP, transpile, e2e) |

The test framework (`tests/framework.lisp`) is ~150 lines with no dependencies: `deftest`, `is`, `is-equal`, `signals`, `finishes`, `bench`, `with-timeout-guard`.
