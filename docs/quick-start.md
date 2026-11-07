# Quick start

This page gets you from a fresh clone to a running full-stack app, then walks you through adding
your first feature end to end. It assumes you are comfortable with TypeScript, React and
Node tooling, but have not written Lisp before. You will not need to learn much Lisp to be
productive: the frontend dialect maps almost one-to-one onto TypeScript (see the
[TypeScript developer's guide](typescript-guide.md)).

## The mental model in one table

Monolisp.ts is a monolith: one Lisp image holds your API, and the same build produces a typed
TypeScript frontend that cannot drift from that API.

| You know this from a TS stack | Monolisp.ts equivalent |
|---|---|
| tRPC procedures / Next.js server actions | `define-server-function` in a `*.server.lisp` file |
| zod schemas / `interface` shared between client and server | the schema on each parameter and `define-rpc-struct` |
| generated API client (tRPC client, OpenAPI codegen) | `build/server-functions.ts`, regenerated on every build |
| `.ts` / `.tsx` source files | `*.client.lisp` / `*.clientx.lisp` (one file = one ES module) |
| Express/Fastify server | the built-in async HTTP server: `POST /rpc`, `GET /health`, CORS preflight |
| middleware that sets `req.user` | `*authenticator*` (bearer tokens, API keys, ...) |
| `tsc --noEmit` in CI | runs on the generated output as part of every example build |

The flow:

```
api.server.lisp ──load──▶ server-function registry ──▶ JSON-RPC endpoint  (POST /rpc)
                                     │
                                     └──manifest──▶ server-functions.ts (typed stubs)
                                                            ▲
app.clientx.lisp ──transpile (calls checked against manifest)──▶ app.tsx ──▶ tsc --strict ──▶ Vite / your bundler
```

## 1. Install

You need:

* **SBCL** (Steel Bank Common Lisp) 2.x: [sbcl.org](https://www.sbcl.org/platform-table.html). On
  Windows use the MSI installer. On macOS use `brew install sbcl`. On Linux use your package
  manager.
* **Node.js 20+** and npm: used to type-check the generated TypeScript and to run Vite.

No Lisp package manager (Quicklisp) is needed: the framework has zero external Lisp
dependencies.

```sh
git clone <this-repo-url> monolispdotts && cd monolispdotts
npm install                # typescript, react, react-dom, vite (dev dependencies only)
sbcl --version             # check that SBCL is on PATH
```

> **Windows:** the SBCL installer does not always add itself to `PATH`. Either add
> `C:\Program Files\Steel Bank Common Lisp` to your user `PATH`, or run this once per PowerShell
> session: `$env:Path = "C:\Program Files\Steel Bank Common Lisp;" + $env:Path`.

## 2. Run the tests

```sh
npm test                                  # all suites, ~30 s
npm test -- --suite transpiler            # just one suite (in PowerShell: npm test '--' --suite transpiler)
```

Each framework command is a Lisp file run with `sbcl --script`, so the same commands work in bash,
zsh, cmd and PowerShell. `npm test` is just an alias for `sbcl --script scripts/run-tests.lisp`.

## 3. Run an example app

Use two terminals. The first holds the Lisp backend, the second holds Vite serving the generated
frontend:

```sh
# terminal 1: load the backend, transpile the frontend, type-check it, then serve /rpc on :8080
npm run serve:guestbook

# terminal 2: Vite on http://localhost:5173, proxying /rpc to :8080
npm run dev:guestbook
```

Open <http://localhost:5173>, sign in with any name and post something. Then try the React
example the same way: `npm run serve:dashboard` and `npm run dev:dashboard` (on port 5174).

| Example | Shows | README |
|---|---|---|
| `examples/todo` | the basics: structs, CRUD server functions, a React component | [todo](../examples/todo/README.md) |
| `examples/guestbook` | vanilla DOM, bearer-token auth, auth guards, typed error codes, pagination, an inline alias file | [guestbook](../examples/guestbook/README.md) |
| `examples/dashboard` | React hooks, async fan-out on the server, thread-pool offload, alias files for React / Canvas / Intl, a mount entry point | [dashboard](../examples/dashboard/README.md) |

`sbcl --script scripts/build-example.lisp --list` lists the examples. Add `--skip-tsc` to
transpile without Node, or `--port 9000` to serve somewhere else.

## 4. Your first feature, end to end

We will add a "word count" server function to the guestbook and call it from the frontend.

### Backend: define the function

Append to `examples/guestbook/api.server.lisp`:

```lisp
(define-server-function word-count ((text (string 280)))
    (:returns (object (words fixnum) (chars fixnum))
     :doc "Count the words and characters in TEXT.")
  (list :words (length (remove "" (uiop:split-string text :separator " ") :test #'string=))
        :chars (length text)))
```

Read it as TypeScript:

```ts
// what you'd write with tRPC + zod
wordCount: publicProcedure
  .input(z.string().max(280))
  .output(z.object({ words: z.number().int(), chars: z.number().int() }))
  .query(({ input }) => ({ words: ..., chars: input.length }));
```

* `(text (string 280))` is a parameter **with its schema**. The JSON-RPC layer rejects anything
  that is not a string of at most 280 characters before your code runs.
* `:returns` is the result schema. A result that does not match it becomes an opaque internal
  error rather than leaking malformed data.
* The body is ordinary Common Lisp. For objects, return a property list (`:key value ...`).

### Frontend: call it

In `examples/guestbook/main.client.lisp`, anywhere inside an `async` function:

```lisp
(let ((:const counts (await (word-count (input-value "message")))))
  (console-log (.. counts words) "words"))
```

You do not import `word-count` yourself. Every server function is visible to every frontend
module, and the transpiler adds `import { wordCount } from "./server-functions"` when you use one.

### Build and look at the output

```sh
npm run build:guestbook
```

`examples/guestbook/build/server-functions.ts` now contains:

```ts
/** Count the words and characters in TEXT. */
export function wordCount(text: string): Promise<{ words: number; chars: number; }> {
  return rpcCall<{ words: number; chars: number; }>("word-count", [text]);
}
```

and `build/main.ts` contains `const counts = await wordCount(...)`. The output is plain,
readable TypeScript, so read it whenever you are unsure what a form does.

### Break it on purpose

Change the call to `(word-count 42)` and rebuild:

```
argument text of server function word-count must be string, but 42 was given
```

The build fails before `tsc` even runs. Mistakes the Lisp side cannot infer (a variable of the
wrong type, for instance) are then caught by `tsc --strict` on the generated code. Arity is
checked the same way (`server function list-entries takes at most 2 arguments, got 3`), and so
are enum values (`must be "happy" | "neutral" | "sad", but "angry" was given`).

## 5. The development loop

1. Edit `*.server.lisp` or `*.client.lisp` files.
2. Re-run `npm run serve:<example>` (or `build:<example>` if only the frontend changed and the
   server is not running). The build takes a few seconds and includes `tsc`.
3. Vite notices the new files in `build/` and reloads the page.

There is no file watcher yet. Backend changes need a server restart, because the build script
loads the server files into a fresh image. If you want hot redefinition, use the REPL (below):
re-evaluating a `define-server-function` replaces it in the running server.

## 6. Starting your own app

The quickest path is to copy an example folder:

```sh
cp -r examples/guestbook examples/my-app    # or Copy-Item -Recurse in PowerShell
rm -rf examples/my-app/build
sbcl --script scripts/build-example.lisp --example my-app --serve
npx vite examples/my-app
```

Any directory under `examples/` that has a `tsconfig.json` counts as an example. For a project
somewhere else, drive the same pipeline from a REPL or your own script:

```lisp
;; sbcl --load scripts/load.lisp
(asdf:load-system :monolisp)
(monolisp:load-server-files "my-app/")                  ; registers every *.server.lisp
(monolisp:build-frontend "my-app/" "my-app/build/")     ; writes .ts/.tsx + stubs + runtime
(monolisp:start :port 8080 :cors-origins '("http://localhost:5173"))   ; blocks; Ctrl-C to stop
```

A project looks like:

```
my-app/
  api.server.lisp            backend: structs + server functions (as many *.server.lisp files as you like)
  lib-aliases.client.lisp    optional: inline shorthands for a JS library (docs/inline-aliases.md)
  view.client.lisp           frontend modules -> build/view.ts
  app.clientx.lisp           React modules    -> build/app.tsx
  main.clientx.lisp          entry point that mounts the app
  index.html                 <script type="module" src="/build/main.tsx">
  vite.config.js             proxy /rpc -> http://127.0.0.1:8080
  tsconfig.json              extends ../../tsconfig.json, includes build/**
  build/                     generated, git-ignored
```

In production, serve the Vite build output from any static host or CDN and put the Lisp server
behind a reverse proxy at `/rpc`. The Lisp server deliberately serves nothing except
`POST /rpc`, `GET /health` and CORS preflight.

## 7. When something goes wrong

| Symptom | Cause / fix |
|---|---|
| `sbcl: command not found` / not recognized | SBCL is not on `PATH` (see Install) |
| `tsc reported type errors` | Read the `.ts` file named in the error: the generated code is meant to be read. Usually a missing `(declare ...)` type or a `null` that needs a check. Add `--skip-tsc` to look at the output first |
| `... is not available on the frontend` / `lists ... are not supported` | You used a Common Lisp feature with no JS equivalent. See the [TypeScript guide](typescript-guide.md) for the replacement |
| `http proxy error: /rpc ... ECONNREFUSED` in the Vite terminal | The backend is not running (terminal 1) or uses another port |
| Browser shows `HTTP 403` from `/rpc` | Origin not allowed: pass `:cors-origins '("http://your-origin")` to `start` (the example script uses `:any`) |
| Calls fail with `-32001 Unauthorized` | The function has `:auth :authenticated` and no principal was found. Check your `*authenticator*` and the `Authorization` header |
| Server returns `-32603 Internal error` with no detail | Your function signalled an error or returned data that does not match `:returns`. Set `monolisp:*debug-mode*` to `t` while developing to see the condition text |

## Next steps

* [TypeScript developer's guide](typescript-guide.md): every TS idiom and its Lisp spelling, plus the gotchas.
* [Writing an inline alias file](inline-aliases.md): give any JS/TS library short Lisp-style names at zero runtime cost.
* [Architecture](architecture.md): how the pieces fit, where to extend the framework, and the invariants to keep.
* The [README](../README.md) is the reference: the full dialect table, schema language and security model.
