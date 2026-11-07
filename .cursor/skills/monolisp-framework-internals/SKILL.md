---
name: monolisp-framework-internals
description: Guides changes to the Monolisp.ts framework itself, covering the transpiler (src/transpiler), the JSON-RPC layer and schemas (src/rpc), the HTTP server (src/http), the JSON codec (src/json) and the async toolkit (src/async), plus the test suites and security invariants. Use when modifying files under src/ or tests/, adding a dialect special form, prelude alias, schema type or route, fixing a transpiler bug, or reviewing framework changes.
---

# Working on Monolisp.ts internals

Read `docs/architecture.md` before a non-trivial change. It has the system map, request
lifecycle, build pipeline and the `cx` dispatch order.

## Where things live

| Change | File(s) |
|---|---|
| New dialect form (expression) | `src/transpiler/compiler.lisp`: `(defexpr "NAME" (form env) ...)` returns TS text |
| New dialect form (statements) | `compiler.lisp`: `(defstmt "NAME" (form env dest) ...)`, end with `(finish dest expr)`; add to `*statement-only*` if needed |
| Built-in JS shorthand | `src/transpiler/prelude.lisp`: add to the `declaim` and add a `defun` (prefer this over a special form) |
| Name mangling / reserved words | `src/transpiler/names.lisp` |
| Lisp type → TS type | `src/transpiler/types.lisp` (`type->ts`) |
| JSX | `src/transpiler/jsx.lisp` |
| `loop` | `src/transpiler/loop.lisp` |
| Imports/exports, inline aliases across modules | `compiler.lisp` (`IMPORT` defstmt, `compile-export`, `install-inline-imports`, `carry-*`) |
| Module ordering, project build | `src/transpiler/modules.lisp` |
| Server-call checking, stubs, runtime TS | `src/transpiler/align.lisp` |
| Schema type | `src/rpc/types.lisp`: `normalize-schema`, `schema-valid-p`, `validate-value`, `encode-value`, `schema->ts`, `schema->json`; then `schema-accepts-type-p` in `align.lisp` |
| Dispatcher, error codes | `src/rpc/jsonrpc.lisp` |
| Auth context, limits, tokens, rate limiter | `src/rpc/security.lisp` |
| HTTP parsing / routes / CORS | `src/http/parser.lisp`, `src/http/server.lisp` (`dispatch-request`) |

`cx` compiles a form to expression text; `cs` emits statements into a `dest` (`:void`,
`:return` or `(:set . "js")`). Forms dispatch by **symbol name** (each client module has its own
package), via `sym-is` / `sym-name`. Report user errors with `(tfail form "message ~A" x)`.

## Workflow

```
- [ ] 1. Write a failing test first (see table below)
- [ ] 2. Make the change
- [ ] 3. sbcl --script scripts/run-tests.lisp --suite <suite>
- [ ] 4. All three examples still build: sbcl --script scripts/run-tests.lisp --suite examples
- [ ] 5. Full run before finishing: npm test
```

Windows: `$env:Path = "C:\Program Files\Steel Bank Common Lisp;" + $env:Path` if `sbcl` is
missing. Run a single test with `--test name`.

| Change type | Test file / helper |
|---|---|
| Dialect output | `tests/transpiler-tests.lisp`: `(emits "expected TS fragment" "lisp source")`, `(rejects "source" "error fragment")` (whitespace is collapsed) |
| Multi-module behaviour | `transpile-modules` + `module-output-named` in the same file |
| Schemas / encoding | `tests/types-tests.lisp` |
| Dispatcher / auth / limits | `tests/rpc-tests.lisp` (`handle-rpc-sync`) |
| Parser hardening | `tests/attack-tests.lisp` (hostile inputs, fuzzers) |
| Socket-level behaviour | `tests/integration-tests.lisp` (`with-test-server`) |
| Example backends | `tests/example-tests.lisp` (`with-example-backend`) |

To see real output quickly, write a scratch script outside the repo:

```lisp
(load "<repo>/scripts/cli.lisp")
(monolisp.cli:quiet-load-system "monolisp")
(format t "~A" (monolisp:transpile-string "(defun f (xs) (sort xs #'<))" :name "t"))
```

Pass `:config (monolisp.transpiler::make-transpile-config :manifest (monolisp:server-manifest))`
after `load-server-files` to exercise server-call checks.

## Invariants (never break)

- Only `json-parse-error`, `http-parse-error` and `rpc-error` may escape the parsers. Error
  responses never echo input. Methods are looked up by string, never interned.
- Params are validated before user code runs, and results are encoded against `:returns`.
- Auth fails closed: an authenticator or predicate that signals means "no".
- `random-token` uses the OS CSPRNG (BCryptGenRandom / `/dev/urandom`) and errors rather than
  falling back to a weak generator.
- The transpiler reads with `*read-eval*` nil, bounds size and depth first, refuses foreign
  package-qualified symbols (including in macro expansions), and checks `defmacro` bodies for
  host-affecting operators. Backquote template literals are exempt; comma expressions are not.
- Generated TS must pass `tsc --strict` (the examples suite enforces this). Output must stay
  reproducible: no timestamps, sorted auto-imports.
- `monolisp-async` must not depend on any other monolisp system.

## Known gaps (good first projects)

Imports carried by inline aliases are emitted one line per name. There are no generic type
parameters, no watch mode, and macros cannot be exported. `transpile-file` cannot see sibling
modules. See "Known limitations" in `docs/architecture.md`.
