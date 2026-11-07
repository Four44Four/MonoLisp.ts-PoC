---
name: monolisp-frontend
description: Writes and debugs Monolisp.ts frontend code: the Lisp dialect in *.client.lisp / *.clientx.lisp files that transpiles to TypeScript/TSX (React via as-html-string). Use when editing or creating .client.lisp or .clientx.lisp files, translating TypeScript or React code into the dialect, calling server functions from the frontend, or fixing transpile and tsc errors in examples/*/build output.
---

# Monolisp frontend dialect

One file is one ES module: `foo.client.lisp` → `build/foo.ts`, and `foo.clientx.lisp` →
`build/foo.tsx` (JSX allowed). The generated code is plain TypeScript, so **always read the
output** in `build/` when unsure what a form compiles to.

## Verify loop (run after every change)

```sh
sbcl --script scripts/build-example.lisp --example <name>   # transpile + tsc; prints errors with the offending form
```

On Windows, if `sbcl` is not found: `$env:Path = "C:\Program Files\Steel Bank Common Lisp;" + $env:Path`.
For a quick look at one form, `monolisp:transpile-string` returns the TS text (see `tests/transpiler-tests.lisp` for `emits`-style assertions).

## Core mapping

```lisp
(defconstant +max+ 3) (defparameter cur nil) (defvar *n* 0)   ; const MAX = 3; let cur = null; var N = 0;
(let ((:const x 1) (y 2)) ...)                                 ; const x = 1; let y = 2;
(let ((:const :multi (a set-a) (useState 0))) ...)             ; const [a, setA] = useState(0);
(defun f (a &optional (b 2) &key k) (declare (string a) (returns string)) ...)
(f "x" 3 :k 1)                                                 ; f("x", 3, { k: 1 })
(lambda (e) ...)  (async (lambda () (await p)))                ; (e: any) => ...   async () => await p
(.. obj a (method 1) (:? b))  (.. o "key")  (aref xs i)        ; obj.a.method(1)?.b   o["key"]   xs[i]
(setf (.. el hidden) :false)                                   ; el.hidden = false
'{}(:a 1 (spread o))  (vector 1 2)  (new Map)                  ; { a: 1, ...o }   [1, 2]   new Map()
(nil-or a b)  (and a b)  (or a b)  (not a)  (null x)           ; a ?? b   a && b   a || b   !a   x == null
(= a b) / (equal a b) / (string= a b)                          ; a === b
(format nil "~A items" n) / (concat "a" x)                     ; template literals
(map (x xs i) ...)  (for-each (x xs) ...)  (loop for x across xs sum x)
(handler-case (risky) (TypeError (e) ...) (error (e) ...))     ; try/catch with instanceof
(the |HTMLInputElement| x)                                     ; x as HTMLInputElement
```

Names: `kebab-case` → `camelCase`, `*x*` / `+x+` → `X`, mixed-case symbols (`useState`,
`React`, `|HTMLElement|`) stay verbatim. Struct fields from the server arrive camelCase, and
`(.. e created-at)` → `e.createdAt`.

## Rules that bite

- **No lists**: `list`, `cons`, `car`, `dolist` and `dotimes` are compile errors. Use `vector`, `for-each`, `map`, `loop`.
- `nil` is `null`; use `:false` for a literal false and `:undefined` for undefined. Keywords are strings (`:a` → `":a"`).
- Truthiness is JavaScript's (`0` and `""` are falsy).
- `(push x xs)` is `unshift`. To append, use `(vector-push-extend x xs)` or `(array-push xs x)`.
- `sort` takes a less-than predicate and copies (`(sort xs #'<)`); `array-sort` is the in-place JS comparator.
- No generics: cast initial values, e.g. `(useState (the (array todo) (vector)))` or `(use-ref (the (or |HTMLCanvasElement| null) nil))`.
- A React effect cleanup must end with `:undefined` (void).
- Statement-only forms (`let`, `loop`, `case`, `handler-case`) in expression position become IIFEs. Bind with `let` first for cleaner output.
- `defmacro` is file-local. Its own code (outside the backquote template) cannot use `setf`, `eval`, files or packages.

## Modules and imports

```lisp
(import :from "react" :targets (useState useEffect))             ; named
(import :from "three" :targets :all :as THREE)                   ; namespace
(import :from "app" :targets App)                                ; default import of sibling ./app
(import :from "utils" :targets (format-count (:as clamp clamp-n)))
(import :from :server :targets ((:type guest-entry)))            ; generated interface
(export (defun f () ...))  (export a b)  (export-default (defun App () ...))
```

Sibling modules are named without `./` or an extension; cycles are rejected.

## Server calls

Every `define-server-function` is callable directly, e.g. `(await (list-entries 10))` →
`await listEntries(10)`, and its import is added automatically. The build checks arity, literal
types and enum values against the backend. Failures throw `RpcError` (import it from
`"./monolisp-runtime"`) with a `code`: -32001 unauthorized, -32002 rate limited, -32003 timeout,
-32602 invalid params, -32603 internal, -32000 network/HTTP, or app codes from `rpc-fail`. Use
`(configureRpc '{}(:headers (lambda () ...)))` for auth headers.

## React (`.clientx.lisp`)

```lisp
(as-html-string
  (:div :class "card" :onClick (lambda () (set-n (+ n 1)))      ; className, event handlers
    (:h3 title)
    (when err (:p :class "error" err))                         ; {err ? <p/> : null}
    (map (s items) (:SeriesCard :key (.. s id) :series s))))   ; components keep their case
```

Type props with `(declare ((object (series metric-series)) props))`. Mount from an entry module:
`(.. (createRoot (the |HTMLElement| (doc-get-element-by-id "root"))) (render (as-html-string (:App))))`.

## Built-in shorthands

About 100 prelude aliases: `console-log`, `doc-get-element-by-id`, `add-event-listener`,
`set-inner-text`, `add-class`, `set-timeout`, `set-interval`, `array-push`, `array-sort`,
`object-keys`, `promise-all`, `stringify-json`, `date-now`, ... Full list:
`src/transpiler/prelude.lisp`. For a library's API, write an alias file (see the
monolisp-inline-aliases skill).

## References

- `docs/typescript-guide.md`: the full TS → dialect translation and gotchas
- `README.md` "Frontend dialect": the reference table
- `examples/*/README.md`: worked examples (DOM + auth, React + async)
