# Monolisp.ts for TypeScript developers

You already know the runtime: the frontend dialect compiles to plain, readable TypeScript, and
the browser runs that. What changes is the syntax and a handful of semantics that come from
Common Lisp. This guide is a translation table plus the gotchas. Every Lisp → TS pair on this
page was produced by the transpiler; when in doubt, build and read the generated `build/*.ts`.

Contents: [reading Lisp](#reading-lisp-in-two-minutes) ·
[values](#values-and-literals) · [variables](#variables) · [functions](#functions) ·
[objects and arrays](#objects-arrays-and-property-access) · [control flow](#control-flow) ·
[types](#types) · [async](#async-and-errors) · [modules](#modules) · [React](#react) ·
[server functions](#talking-to-the-server) · [macros](#macros-your-new-superpower) ·
[gotchas](#gotchas-checklist)

## Reading Lisp in two minutes

* A call is a list with the function first: `(f a b)` is `f(a, b)`. Operators are calls too:
  `(+ a b c)` is `a + b + c`, and `(< a b c)` is `a < b && b < c`.
* Everything is an expression, so `if` returns a value (it becomes a ternary in expression
  position). The last form in a function body is its return value.
* `;` starts a comment. `;;;` is a section comment by convention.
* Names are kebab-case and become camelCase: `load-entries` → `loadEntries`. `*global*` →
  `GLOBAL`, `+constant+` → `CONSTANT`. Mixed-case names are kept verbatim, so `React`,
  `useState` and `HTMLElement` round-trip unchanged.
* `(.. obj a (b 1))` is the property and method chain `obj.a.b(1)`. A dotted name like
  `document.getElementById` also works.
* Indentation is just convention; parentheses carry the structure. Let your editor balance them
  (VS Code/Cursor: a Lisp extension with rainbow brackets helps a lot).

## Values and literals

| TypeScript | Monolisp | Notes |
|---|---|---|
| `null` | `nil` | `nil` is also the "nothing" return value |
| `true` / `false` | `t` / `:false` | **`nil` is `null`, not `false`**. Use `:false` where a real boolean is required (`el.hidden = false`) |
| `undefined` | `:undefined` | trailing `:undefined` arguments are dropped from calls |
| `"text"` | `"text"` | |
| `` `${n} items` `` | `(format nil "~A items" n)` or `(concat n " items")` | both become template literals |
| `42`, `3.14` | `42`, `3.14` | |
| `[1, 2]` | `(vector 1 2)` or `#(1 2)` | |
| `{ a: 1, b: x }` | `'{}(:a 1 :b x)` | |
| `{ ...opts, a: 1 }` | `'{}((spread opts) :a 1)` | |
| `":my-key"`?! | `:my-key` | keywords are **strings** in the output: `":myKey"`. Use real strings for data |

## Variables

```lisp
(defconstant +max-retries+ 3)       ; const MAX_RETRIES = 3;
(defparameter current-user nil)     ; let currentUser = null;
(defvar *count* 0)                  ; var COUNT = 0;

(let ((:const total (length xs))    ; const total = xs.length;
      (attempts 0))                 ; let attempts = 0;
  (incf attempts)                   ; attempts += 1  (also (incf x 5), (decf x))
  (setf attempts 10))               ; attempts = 10

(let ((:const :multi (q r) (divmod 7 2))) ...)   ; const [q, r] = divmod(7, 2);
(destructuring-bind (&key a (b 2)) obj ...)      ; const { a, b = 2 } = obj;
```

`let` bindings are evaluated in parallel (none can see the others); `let*` is sequential, like
consecutive `const`s. Shadowing is safe: the compiler renames as needed.

## Functions

```lisp
(defun greet (name &optional (greeting "Hello"))
  (declare (string name greeting) (returns string))
  (concat greeting ", " name "!"))
```
```ts
function greet(name: string, greeting: string = "Hello"): string {
  return `${greeting}, ${name}!`;
}
```

| TypeScript | Monolisp |
|---|---|
| `(a, b) => a + b` | `(lambda (a b) (+ a b))` |
| `async () => { ... }` | `(async (lambda () ...))` |
| `function f(...rest: any[])` | `(defun f (&rest rest) ...)` |
| options object `f(url, { retries: 3 })` | `(defun f (url &key (retries 3) verbose) ...)` called as `(f url :retries 3)` → `f(url, { retries: 3 })` |
| `cb(1)` where `cb` is a parameter | `(funcall cb 1)`, or `(cb 1)`, since local variables can be called directly |
| `fn(...args)` | `(apply fn args)` or `(fn (spread args))` |
| `xs.filter(isEven)` with a built-in predicate | `(remove-if-not #'evenp xs)`; `#'op` on a built-in becomes `(a) => ...` |

Keyword arguments are the idiomatic options object: the definition destructures one, and calls
from the same module or from importing modules fold `:name value` pairs into it.

## Objects, arrays and property access

```lisp
(.. user (:? profile) (:? name))          ; user?.profile?.name
(setf (.. obj count) (+ (.. obj count) 1)); obj.count = (obj.count + 1)
(.. obj "weird key")                      ; obj["weird key"]
(.. xs (aref 0))  /  (aref xs i)          ; xs[0] / xs[i]
(nil-or a b 0)                            ; a ?? b ?? 0
(new Map)  /  (new Date 0)                ; new Map() / new Date(0)
```

Arrays (no lists! see gotchas):

| TypeScript | Monolisp |
|---|---|
| `xs.length` | `(length xs)` |
| `xs.push(x)` | `(vector-push-extend x xs)` or `(array-push xs x)` |
| `xs.unshift(x)` | `(push x xs)`, the Lisp meaning of push (front) |
| `xs.pop()` | `(vector-pop xs)` |
| `xs.map(x => x * 2)` | `(map (x xs) (* x 2))`, with index: `(map (x xs i) ...)` |
| `xs.forEach((x, i) => ...)` | `(for-each (x xs i) ...)` |
| `xs.filter(p)` | `(remove-if-not p xs)` or `(array-filter xs p)` |
| `xs.find(p)` | `(find-if p xs)`; `(find 3 xs)` for a value |
| `xs.reduce((a, x) => a + x, 0)` | `(reduce #'+ xs :initial-value 0)` or `(array-reduce xs (lambda (a x) (+ a x)) 0)` |
| `[...xs].sort((a, b) => a - b)` | `(sort xs #'<)`: Lisp `sort` takes a *less-than predicate* and does not mutate |
| `xs.sort((a, b) => a - b)` | `(array-sort xs (lambda (a b) (- a b)))`: a JS comparator, in place |
| `xs.slice(1, 3)` / `s.toUpperCase()` / `s.trim()` | `(subseq xs 1 3)` / `(string-upcase s)` / `(string-trim " " s)` |

Maps: `(make-hash-table)` → `new Map()`, `(gethash k m)` → `m.get(k)`,
`(setf (gethash k m) v)` → `m.set(k, v)`, `(remhash k m)` → `m.delete(k)`.

About 100 more JS APIs have ready-made short names (`console-log`, `doc-get-element-by-id`,
`add-event-listener`, `set-timeout`, `stringify-json`, `object-keys`, `promise-all`, ...). See
[`src/transpiler/prelude.lisp`](../src/transpiler/prelude.lisp). Anything else is reachable
through `..` or a dotted name, and you can add your own names with an
[inline alias file](inline-aliases.md).

## Control flow

```lisp
(if c a b)                    ; c ? a : b   (or if/else as a statement)
(when c ...) / (unless c ...) ; if (c) {...} / if (!c) {...}
(cond ((< n 0) "neg")         ; if / else if / else
      ((= n 0) "zero")
      (t "pos"))
(case mood                    ; switch (mood)
  ("happy" "😊")              ;   case "happy":
  ((1 2) "small")             ;   case 1: case 2:
  (otherwise "😐"))           ;   default:
(and a b) (or a b) (not a)    ; a && b, a || b, !a
(= a b) (equal a b) (string= a b) (eq a b)   ; all ===
(null x)                      ; x == null  (null or undefined)
(typeof x) (instanceof x Date); typeof x, x instanceof Date
```

Loops: `(for-each (x xs) ...)` for side effects; `(map (x xs) ...)` to build an array; and
`loop` for everything else:

```lisp
(loop for x across xs sum x)                       ; for (const x of xs) acc += x
(loop for i from 0 below n collect (* i i))        ; builds an array
(loop for x across xs when (> x 10) return x)      ; early exit
(loop for k being the hash-keys of m using (hash-value v) do (console-log k v))
```

## Types

Types are optional, as in TS. Undeclared parameters become `any`. Declare them with `declare` at
the top of a function body:

```lisp
(defun total (xs)
  (declare ((array fixnum) xs) (returns fixnum))
  (loop for x across xs sum x))                    ; function total(xs: number[]): number
```

| TypeScript | Monolisp type |
|---|---|
| `string` / `number` / `boolean` | `string` / `fixnum`, `float` or `number` / `boolean` |
| `string[]` | `(array string)` |
| `string \| null` | `(or string null)` |
| `"a" \| "b"` | `(member "a" "b")` |
| `{ x: number; y?: string }` | `(object (x fixnum) (y string :optional t))` |
| `Record<string, string>` | `(record string string)` |
| `Map<string, number>` | `(hash-table string fixnum)` |
| `(a: number) => string` | `(function (fixnum) string)` |
| `Promise<T>` | `(promise T)` |
| `HTMLInputElement`, `Foo<string>` | `\|HTMLInputElement\|`, `(\|Foo\| string)`. Pipes keep the exact case |
| `type Status = ...` | `(deftype status () '(member "active" "banned"))` |
| `interface User {...}` + factory | `(defstruct user (id 0 :type fixnum) (name "" :type string))`, which also gives `make-user`, `user-name`, ... |
| `x as T` | `(the T x)` |

**There are no generic type parameters.** Where TypeScript would infer from a type argument, cast
the initial value instead. TS then infers the generic from it:

```lisp
(use-ref (the (or |HTMLCanvasElement| null) nil))   ; useRef(null as HTMLCanvasElement | null)
(useState (the (array todo) (vector)))              ; useState([] as Todo[])
```

## Async and errors

```lisp
(async (defun load-user (id)
  (declare (fixnum id))
  (let ((:const res (await (fetch (concat "/api/users/" id)))))
    (await (.. res (json))))))
```
```ts
async function loadUser(id: number) {
  const res = await fetch(`/api/users/${id}`);
  return await res.json();
}
```

```lisp
(handler-case (risky)
  (TypeError (e) (console-error e) nil)     ; catch: if ($err instanceof TypeError) ...
  (error (e) (console-error "other" e) nil)); catch-all
(unwind-protect (work) (cleanup))           ; try { work() } finally { cleanup() }
(error "bad ~A" x)                          ; throw new Error(`bad ${x}`)
```

## Modules

One file is one ES module. `foo.client.lisp` becomes `foo.ts` and `foo.clientx.lisp` (JSX
allowed) becomes `foo.tsx`.

```lisp
(import :from "three" :targets (WebGLRenderer (:type Vector3Like)))  ; import { WebGLRenderer, type Vector3Like } from "three"
(import :from "three" :targets :all :as THREE)                       ; import * as THREE from "three"
(import :from "react" :targets React)                                ; default import
(import :from "app" :targets Dashboard)                              ; default import of a sibling module (./app)
(import :from "utils" :targets ((:as format-date fmt)))              ; import { formatDate as fmt } from "./utils"
(import :from :server :targets ((:type todo)))                       ; the interface generated for a server struct

(export (defun helper () ...))      ; export function helper()
(export a b)                        ; export { a, b }
(export-default (defun App () ...)) ; export default App
```

A sibling module's name is written without `./` or an extension. The transpiler orders modules by
their imports and rejects cycles.

## React

`.clientx.lisp` files can use `as-html-string`, a Lisp spelling of JSX:

```lisp
(import :from "react" :targets (useState))

(export-default (defun Counter (props)
  (declare ((object (start fixnum)) props))
  (let ((:const :multi (count set-count) (useState (.. props start))))
    (as-html-string
      (:button :onClick (lambda () (set-count (+ count 1)))
        "Clicked " count " times")))))
```
```tsx
import React from "react";
import { useState } from "react";
function Counter(props: { start: number; }) {
  const [count, setCount] = useState(props.start);
  return <button onClick={() => setCount(count + 1)}>{"Clicked "}{count}{" times"}</button>;
}
export default Counter;
```

* `(:tag :attr value children...)` is an element; `:class` → `className`, `:for` → `htmlFor`.
* `(:MyComponent :prop x)` is a component (the capitalized name is kept).
* Lists of children: `(map (item items i) (:li :key i item))` → `{items.map(...)}`.
* `(when cond (:p ...))` renders `{cond ? <p/> : null}`.
* `React` is imported automatically.
* The [dashboard example](../examples/dashboard/README.md) shows a React alias file
  (`use-state`, `use-effect` with dependency arrays) and a reusable effect macro.

## Talking to the server

Every `define-server-function` is an async function in the frontend. You never write `fetch`,
serialization or a client:

```lisp
(let ((:const page (await (list-entries 10))))      ; const page = await listEntries(10);
  (.. page next-cursor))                            ; page.nextCursor  (fields are camelCase)
```

* Calls are checked against the backend at build time (arity, literal types, enum values), then
  `tsc --strict` checks the rest against the generated `server-functions.ts`.
* Struct types come from the server: `(import :from :server :targets ((:type guest-entry)))`
  then `(declare (guest-entry e))`.
* Failures throw `RpcError` (from `./monolisp-runtime`) with a numeric `code`, a `message` and
  optional `data`:

| code | meaning |
|---|---|
| -32001 | unauthorized (an `:auth` guard refused the call) |
| -32002 | rate limited |
| -32003 | server-side timeout (`:timeout-ms`) |
| -32602 | invalid params (schema validation failed) |
| -32603 | internal error (your function signalled an error, or returned data that does not match `:returns`) |
| -32000 | network error / HTTP error / client timeout |
| -32000..-32099, or a positive code | your own `rpc-fail` codes; the message is shown to users |

* Auth headers: `(configureRpc '{}(:headers (lambda () ...)))`. The function runs before every
  call. See the [guestbook](../examples/guestbook/README.md).

### Server side, for the TS developer

```lisp
(define-rpc-struct todo                        ; interface Todo { id: number; title: string; ... }
  (id fixnum)
  (title (string 200))                         ; max length is enforced on input
  (tags (array (string 32) 16) :optional t))   ; tags?: string[] (at most 16)

(define-server-function add-todo ((title (string 200)))
    (:returns todo :auth :authenticated :timeout-ms 2000 :doc "Create a todo.")
  (make-todo :id (incf *next-id*) :title title))
```

The server is Common Lisp proper (full language, lists allowed). Useful helpers:

* `(rpc-fail "message" :code -32010 :data id)` returns an application error to the caller.
* `(context-principal *request-context*)` is whatever your `*authenticator*` returned.
* `(context-header *request-context* "x-api-key")` reads a request header.
* `(constant-time-equal a b)` and `(random-token 32)` are for secrets.
* `(define-async f ...)`, `(await p)`, `(delay ms)`, `(all (list p1 p2))` run work on the event
  loop without blocking it. `(offload (lambda () ...))` moves CPU-heavy work to the thread pool.

## Macros: your new superpower

A `defmacro` is a build-time function from code to code, roughly a Babel plugin you can write
inline. Use one when a function cannot do the job: wrapping a body, defining things, or avoiding
repeated boilerplate.

```lisp
(defmacro reporting-errors (&body body)
  `(handler-case (progn ,@body)
     (RpcError (e) (show-error (explain e)))
     (error (e) (show-error (concat "Unexpected error: " e)))))

(defmacro toggle (place) `(setf ,place (not ,place)))
```

The backquote is a template, and `,x` / `,@body` splice arguments into it. Macros run inside
the build, so their *own* code (outside the template) may not touch files, packages, `eval`,
`setf` or global definitions. Macros are file-local: copy them into each file that needs them.

## Gotchas checklist

1. **No lists.** `list`, `cons`, `car` and `dolist` are compile errors on the frontend; JS has
   arrays. Use `(vector ...)`, `for-each`, `map` and `loop ... across`.
2. **`push` adds to the front** (`unshift`). Use `vector-push-extend` / `array-push` to append.
3. **`nil` is `null`, not `false`.** Write `:false` when an API needs a boolean, e.g.
   `(setf (.. el hidden) :false)`.
4. **Truthiness is JavaScript's.** `(if 0 a b)` takes the `b` branch and `""` is falsy, unlike
   real Common Lisp where only `nil` is false.
5. **Keywords are strings with a colon.** `(case x (:a ...))` compares with `":a"`. Use string
   keys for data that crosses into JS APIs or the server.
6. **No generics.** Cast the initial value with `the` (see Types).
7. **`=` is `===`**, and there is no deep `equal` on objects.
8. **`sort` takes a predicate, not a comparator**, and returns a new array. `array-sort` is the
   JS method.
9. **Integer division:** `(/ a b)` is JS division (`7 / 2 = 3.5`). Use `(floor a b)` for integers.
10. **Server structs arrive with camelCase fields** (`created-at` → `createdAt`). The `..`
    accessor converts: `(.. entry created-at)` → `entry.createdAt`.
11. **One server function per network round trip.** Batch by returning an object, or run
    calls in parallel with `promise-all` (see the dashboard example).
12. **Read the output.** Every module becomes a readable `.ts` file in `build/`; that is the fastest
    way to answer "what does this compile to?".
