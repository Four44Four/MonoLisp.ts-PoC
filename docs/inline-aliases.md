# Writing a `..` inline alias file for a JS/TS library

The `..` macro can reach any JavaScript API, but code that leans on one library quickly turns
into a wall of `(.. scene (add mesh))`, `(.. camera position (set 0 2 5))` and
`(new (.. THREE Vector3) x y z)`. An **alias file** fixes that. It is one frontend module that
gives a library short Lisp-style names, and it costs nothing at runtime:

```lisp
(scene-add scene cube light)     ;; scene.add(cube, light)
(set-position camera 0 2 5)      ;; camera.position.set(0, 2, 5)
(vec3 1)                         ;; new THREE.Vector3(1, 0, 0)
```

Every alias is an `inline` function. The transpiler pastes its body into each call site, much
like a macro, so no wrapper function exists in the JavaScript and importers never load the alias
file. The built-in shorthands (`console-log`, `doc-get-element-by-id`, `add-event-listener`, and
about 100 more) are an alias file too: see
[`src/transpiler/prelude.lisp`](../src/transpiler/prelude.lisp).

If you know TypeScript, think of it as a `.d.ts`-sized module of one-line helper functions that
the compiler guarantees to inline, plus automatic import forwarding.

## The recipe

### 1. Create the file

Put it next to your other frontend modules, named `<library>-aliases.client.lisp`. It must be
part of the project you build with `monolisp:build-frontend` (or `sbcl --script
scripts/build-example.lisp`), because importers can only see aliases from sibling modules in the
same build.

### 2. Import the library the way the aliases need it

```lisp
;;;; three-aliases.client.lisp -- Lisp names for the three.js APIs this app uses

(import :from "three" :targets :all :as THREE)     ;; import * as THREE from "three"
```

Any import form works: namespace (`:targets :all :as X`), named (`:targets (WebGLRenderer Color)`),
default (`:targets React`) or renamed (`:targets ((:as Color ThreeColor))`). Importers inherit
exactly the imports their aliases use, so they never import the library themselves.

### 3. Declare every alias `inline`, before its `defun`

```lisp
(declaim (inline vec3 vec3-add deg->rad scene-add set-position mesh-name look-at-origin))
```

A `defun` whose name was not declaimed first becomes an ordinary function.

### 4. Write each alias as an exported one-expression `defun`

```lisp
(export (defun vec3 (&optional (x 0) (y 0) (z 0)) (new THREE.Vector3 x y z)))
(export (defun vec3-add (a b) (.. a (clone) (add b))))
(export (defun deg->rad (degrees) (THREE.MathUtils.degToRad degrees)))
(export (defun scene-add (scene &rest objects) (.. scene (add (spread objects)))))
(export (defun set-position (obj x y z) (.. obj position (set x y z))))
(export (defun mesh-name (obj) (nil-or (.. obj (:? userData) (:? name)) "unnamed")))
(export (defun look-at-origin (camera) (.. camera (lookAt (vec3)))))   ; aliases may use aliases
```

`export` is what makes an alias importable. Unexported inline functions remain private
shorthands for the file itself.

### 5. Import the aliases where you use them

```lisp
;;;; app.client.lisp
(import :from "three-aliases" :targets (vec3 vec3-add deg->rad scene-add set-position mesh-name look-at-origin))

(export (defun setup (scene camera cube light)
  (scene-add scene cube light)
  (set-position camera 0 2 5)
  (look-at-origin camera)
  (setf (.. cube rotation y) (deg->rad 45))
  (console-log (mesh-name cube) (vec3-add (vec3 1) (vec3 0 1)))))
```

### 6. Build and read the output

This is the real output for the two files above:

```typescript
// app.ts
import * as THREE from "three";
export function setup(scene: any, camera: any, cube: any, light: any) {
  scene.add(cube, light);
  camera.position.set(0, 2, 5);
  camera.lookAt(new THREE.Vector3(0, 0, 0));
  cube.rotation.y = THREE.MathUtils.degToRad(45);
  return console.log(cube?.userData?.name ?? "unnamed", new THREE.Vector3(1, 0, 0).clone().add(new THREE.Vector3(0, 1, 0)));
}
```

```typescript
// three-aliases.ts  (nothing imports this at runtime)
import * as THREE from "three";
```

Notice three things. There is no `import ... from "./three-aliases"`. The `THREE` import was
carried over. `look-at-origin` was expanded through `vec3`. Run `tsc` on the output (the example
build script does this for you); it checks every expanded call against the library's own type
definitions (`npm install three @types/three` for this example).

## Translating TypeScript call shapes into alias bodies

| You would write in TS | Alias body |
|---|---|
| `obj.method(a, b)` | `(.. obj (method a b))` |
| `obj.a.b.method(x)` | `(.. obj a b (method x))` |
| `obj.prop` | `(.. obj prop)` |
| `obj.prop = v` | `(setf (.. obj prop) v)` |
| `obj?.a?.b` | `(.. obj (:? a) (:? b))` |
| `obj[key]` / `xs[i]` | `(.. obj (aref key))` |
| `new Lib.Thing(a)` | `(new Lib.Thing a)` or `(new (.. Lib Thing) a)` |
| `Lib.helpers.fn(x)` (static) | `(Lib.helpers.fn x)` |
| `fn(...items)` | `(defun f (&rest items) (fn (spread items)))` |
| `fn(x, [a, b])` from `(f x a b)` | `(defun f (x &rest deps) (fn x (vector (spread deps))))` |
| optional argument | `(defun f (x &optional y) ...)`: a missing `y` becomes `undefined`, and trailing `undefined`s are dropped from the call |
| default argument | `(defun f (&optional (x 0)) ...)` |
| `x as HTMLInputElement` | `(the \|HTMLInputElement\| x)` |
| `useRef<T \| null>(null)` (generic argument) | `(useRef (the (or \|T\| null) nil))` |
| `{ a: 1, ...opts }` | `'{}(:a 1 (spread opts))` |

Dotted symbols such as `THREE.MathUtils.degToRad` or `localStorage.getItem` compile verbatim, so
use them for static paths. Use `..` whenever part of the chain is a variable or a call.

## What an importer receives

When module B runs `(import :from "three-aliases" :targets (look-at-origin))`, the transpiler:

1. Copies the alias definition into B. Calls in B expand at build time, and no runtime import of
   the alias file is emitted.
2. Installs any other aliases the body uses (`vec3` here), transitively.
3. Re-imports what the body references from the alias file's scope:
   - a library binding the alias file imported (`THREE`) gets the same import line in B;
   - an *exported* `defvar` / `defconstant` / `defun` of the alias file gets
     `import { NAME } from "./three-aliases"`. That's the right tool for objects you want to
     create only once; see the `intl-aliases` file in the dashboard example.
4. Skips imports B already has from the same source, and fails the build if B binds the same
   name to something else.

You can rename an alias on import with `(:as original local)`:
`(import :from "three-aliases" :targets ((:as vec3 v)))`.

## Rules and limits

- **Declaim first, export always.** `(declaim (inline name ...))` must come before the `defun`,
  and only exported aliases can be imported. Importing an unexported alias is a build error that
  tells you so.
- **No `&key`.** Use `&optional`, or take an options object built with `'{}(...)`.
- **Single evaluation is preserved.** If a parameter appears more than once in the body and the
  caller passes a non-trivial expression, the argument is bound to a temporary first. That keeps
  side effects correct, but in expression position it produces an immediately invoked function.
  Use each parameter once when you can (`clear-canvas` in the dashboard example uses `ctx` three
  times, which is fine because callers pass a plain variable).
- **Property names are never parameters.** In `(defun set-value (el value) (setf (.. el value)
  value))`, only the last `value` is substituted. `..` steps are always property or method names.
- **Prefer one expression.** A multi-statement body works, but it becomes an IIFE when used as
  an expression. Write it so callers use it as a statement (like `stroke-path` in the dashboard).
- **Keep the body self-contained.** It can use its parameters, other aliases of the same file,
  the file's imports, the file's *exported* definitions, and browser globals (`document`,
  `window`, `Math`, ...). A reference to an unexported definition produces a build warning, and
  `tsc` will report the missing name in the importer.
- **Pick distinctive binding names.** A carried import cannot share its JavaScript name with a
  different binding in the importer (a local `defvar THREE`, or an import of `THREE` from
  another package). The build reports the conflict instead of guessing.
- **Project builds only.** `transpile-file` and `transpile-string` compile one module in
  isolation and cannot see siblings. Use `build-frontend` / `transpile-project`.
- **Macros stay file-local.** `defmacro` cannot be exported. If you need to share a shorthand,
  write an inline alias; if you need real syntax (new binding forms, `every-ms` style wrappers),
  copy the macro into each file that needs it.

## Inline alias or regular exported function?

| Use an inline alias when... | Use a regular `(export (defun ...))` when... |
|---|---|
| the body is one call, property access or constructor | there is real logic: branches, loops, several statements |
| you want zero runtime cost and the library's own types at every call site | you want one place to put a breakpoint, or a declared signature (`(declare (string x) (returns number))`) |
| it is a thin rename of an API | it holds state (caches, singletons); export the state and alias around it |

Both can live in the same file. The guestbook example's `browser-aliases` is all aliases; the
dashboard's `intl-aliases` mixes exported constants with aliases that use them.

## Template

```lisp
;;;; <lib>-aliases.client.lisp -- Lisp names for the <lib> APIs this app uses
;;;;
;;;; Inline aliases: every call site is replaced by the JS expression at build time.

(import :from "<npm-package>" :targets :all :as Lib)

(declaim (inline lib-make lib-get lib-set lib-run))

(export (defun lib-make (options) (new Lib.Thing options)))
(export (defun lib-get (thing key) (.. thing (get key))))
(export (defun lib-set (thing key value) (.. thing (set key value))))
(export (defun lib-run (thing &rest args) (.. thing (run (spread args)))))
```

## Troubleshooting

| Build message | Meaning and fix |
|---|---|
| `import: X is an inline alias that module M does not export` | Wrap the alias `defun` in `(export ...)`. |
| `an imported inline alias needs X from "a", but this module imports X from "b"` | Two libraries are bound to the same name. Rename one import with `(:as orig local)`, or change the namespace name in the alias file. |
| `... but this module defines its own X` | The importer has a `defvar`/`defun` with the JS name of a carried import. Rename it. |
| `inline alias A uses X, which module M does not export` (warning) | Export `X` from the alias file, or inline it into the alias body. |
| `X expects at least N arguments` | Inline calls are arity-checked against the alias lambda list. |
| `tsc` errors on an expanded call | Read the generated `.ts`: the error is in the pasted expression, which `tsc` checks against the library's real types. Add a `the` cast in the alias if the library's type is wider than you need. |

## Working examples in this repository

- [`examples/guestbook/browser-aliases.client.lisp`](../examples/guestbook/browser-aliases.client.lisp): DOM, `localStorage` and `Intl` aliases with `the` casts.
- [`examples/dashboard/react-aliases.client.lisp`](../examples/dashboard/react-aliases.client.lisp): React hooks; `&rest` + `spread` builds dependency arrays.
- [`examples/dashboard/canvas-aliases.client.lisp`](../examples/dashboard/canvas-aliases.client.lisp): Canvas 2D, including a multi-statement alias.
- [`examples/dashboard/intl-aliases.client.lisp`](../examples/dashboard/intl-aliases.client.lisp): aliases around exported singleton formatters.
- The `inline-aliases-*` tests in [`tests/transpiler-tests.lisp`](../tests/transpiler-tests.lisp) pin down the exact behaviour.
