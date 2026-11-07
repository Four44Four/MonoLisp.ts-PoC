---
name: monolisp-inline-aliases
description: Creates Monolisp.ts inline alias files, zero-runtime-cost Lisp shorthand names for any JS/TS library (React hooks, three.js, Canvas, Intl, DOM) built from exported inline defuns and the .. property/method chain macro. Use when the user wants shorthand or wrapper functions for a JavaScript or TypeScript library, a *-aliases.client.lisp file, fewer (.. obj (method ...)) chains, or asks about declaim inline across modules.
---

# Inline alias files

An alias file is a frontend module of exported `inline` defuns. The transpiler pastes each body
into the caller at build time, so no wrapper function exists at runtime. Importers also receive
the library imports the bodies use.

## Recipe

```lisp
;;;; three-aliases.client.lisp -- Lisp names for the three.js APIs this app uses

(import :from "three" :targets :all :as THREE)        ; however the bodies need the library

(declaim (inline vec3 scene-add set-position mesh-name deg->rad))   ; BEFORE the defuns

(export (defun vec3 (&optional (x 0) (y 0) (z 0)) (new THREE.Vector3 x y z)))
(export (defun scene-add (scene &rest objects) (.. scene (add (spread objects)))))
(export (defun set-position (obj x y z) (.. obj position (set x y z))))
(export (defun mesh-name (obj) (nil-or (.. obj (:? userData) (:? name)) "unnamed")))
(export (defun deg->rad (degrees) (THREE.MathUtils.degToRad degrees)))
```

Use it from a sibling module:

```lisp
(import :from "three-aliases" :targets (vec3 scene-add (:as set-position place)))
(scene-add scene (vec3 1))       ;; scene.add(new THREE.Vector3(1, 0, 0)) + import * as THREE from "three"
```

## Translating a TS call into an alias body

| TypeScript | Alias body |
|---|---|
| `obj.method(a, b)` | `(.. obj (method a b))` |
| `obj.a.b.method(x)` | `(.. obj a b (method x))` |
| `obj?.a?.b` | `(.. obj (:? a) (:? b))` |
| `obj.prop = v` | `(setf (.. obj prop) v)` |
| `new Lib.Thing(x)` | `(new Lib.Thing x)` |
| `Lib.fn(x)` (static) | `(Lib.fn x)` |
| `fn(...rest)` | `&rest rest` + `(fn (spread rest))` |
| `fn(f, [a, b])` deps array | `&rest deps` + `(fn f (vector (spread deps)))` |
| `x as T` | `(the \|T\| x)` |
| `{ a: 1, b }` options | `'{}(:a 1 :b b)` |
| default arg `x = 0` | `&optional (x 0)` |

## Rules

- `declaim inline` must come **before** the `defun`; otherwise it is an ordinary function.
- `export` each alias; unexported aliases are module-private.
- Lambda lists support only required, `&optional` (with defaults) and `&rest`. **No `&key`**:
  take an options object built with `'{}(...)`.
- A parameter used more than once is bound to a temporary when the argument is non-trivial,
  so it is still evaluated once.
- Property and method names inside `..` are never substituted, so `(defun set-value (el value) (setf (.. el value) value))` is safe.
- Multi-statement bodies are allowed, but use them only in statement position (in an expression
  they become an IIFE).
- An alias may call other aliases, transitively.
- Expensive objects (an `Intl.NumberFormat`, a config) should be a real exported `defconstant`
  that the alias refers to. Importers then get `import { NAME } from "./alias-file"` automatically.
- `export-default` of an inline is an error (it has no runtime value). Macros cannot be
  exported; use aliases.
- An import conflict (the same JS name from two sources, or a local definition with the alias's
  dependency name) fails the build with a message naming both.

## Verify

Build (`sbcl --script scripts/build-example.lisp --example <name>`), then read `build/<importer>.ts`:
the call site should contain the library call, the needed import should be present, and the
alias module should compile to just `export {};` (or its exported constants). Multi-module
tests live in `tests/transpiler-tests.lisp` (`transpile-modules`, `inline-aliases-*`).

## References

- `docs/inline-aliases.md`: the full guide, template and troubleshooting table
- `examples/dashboard/react-aliases.client.lisp`, `canvas-aliases.client.lisp`, `intl-aliases.client.lisp`
- `examples/guestbook/browser-aliases.client.lisp`: DOM and `localStorage` aliases using `the` casts
- `src/transpiler/prelude.lisp`: the ~100 built-in aliases (available everywhere, no import)
