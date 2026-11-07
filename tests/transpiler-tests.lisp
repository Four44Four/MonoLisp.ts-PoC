;;;; tests/transpiler-tests.lisp -- frontend Lisp -> TypeScript, and backend/frontend alignment

(defpackage :monolisp.test.transpiler
  (:use :cl :monolisp.test :monolisp.transpiler :monolisp.rpc))

(in-package :monolisp.test.transpiler)

(defun ts (source &key tsx manifest (strict t))
  "Transpile SOURCE and return the body (header stripped) with runs of whitespace collapsed."
  (let* ((config (make-transpile-config :manifest manifest :strict-server-calls strict))
         (out (transpile-string source :name "t" :tsx-p tsx :config config)))
    (collapse-ws out)))

(defun collapse-ws (text)
  (with-output-to-string (s)
    (let ((prev-space nil))
      (loop for ch across text
            do (if (member ch '(#\Space #\Tab #\Newline #\Return))
                   (unless prev-space (write-char #\Space s) (setf prev-space t))
                   (progn (write-char ch s) (setf prev-space nil)))))))

(defmacro emits (expected source &rest args)
  `(let ((out (ts ,source ,@args)))
     (is (search ,expected out) "expected ~S in output:~%~A" ,expected out)))

(defmacro rejects (source &optional (pattern nil) &rest args)
  `(let ((msg (handler-case (progn (ts ,source ,@args) nil)
                (transpile-error (e) (format nil "~A" e)))))
     (is msg "expected a transpile error for ~S" ,source)
     ,@(when pattern `((is (and msg (search ,pattern msg)) "expected ~S in error message ~S" ,pattern msg)))))

;;; ------------------------------------------------------------------ names and literals

(deftest names-camel-case (:suite "transpiler")
  (emits "var helloWorld = \"hiii\";" "(defvar hello-world \"hiii\")")
  (emits "var MAX_SIZE = 3;" "(defvar MAX-SIZE 3)")
  (emits "var useState2 = 1;" "(defvar useState2 1)")
  (emits "var COUNTER = 0;" "(defvar *counter* 0)")
  (emits "var else_ = 1;" "(defvar else 1)"))

(deftest literals (:suite "transpiler")
  (emits "var a = 1.5;" "(defvar a 1.5)")
  (emits "var b = \"x\\ny\";" (format nil "(defvar b \"x~Cy\")" #\Newline))
  (emits "var c = \":key\";" "(defvar c :key)")
  (emits "var d = null;" "(defvar d nil)")
  (emits "var e = true;" "(defvar e t)")
  (emits "var f = undefined;" "(defvar f :undefined)")
  (emits "var g = [1, 2, 3];" "(defvar g #(1 2 3))")
  (emits "var h = [1, 2, 3];" "(defvar h (vector 1 2 3))")
  (emits "var i = \"c\";" "(defvar i #\\c)")
  (emits "var j = 9007199254740993n;" "(defvar j 9007199254740993)")
  (emits "var k = 1 / 3;" "(defvar k 1/3)"))

(deftest object-literals (:suite "transpiler")
  (emits "{ className: \"hello\", count: 3 }" "(defvar o '{}(:className \"hello\" :count 3))")
  (emits "{ fooBar: 1 }" "(defvar o '{}(:foo-bar 1))")
  (emits "{ \"a-b\": 1 }" "(defvar o '{}(\"a-b\" 1))")
  (emits "{ ...base, x: 1 }" "(defvar o '{}((spread base) :x 1))")
  (emits "var o = {};" "(defvar o '{})")
  (emits "nested: { a: 1 }" "(defvar o '{}(:nested '{}(:a 1)))"))

;;; ------------------------------------------------------------------ core forms

(deftest let-and-const (:suite "transpiler")
  (let ((out (ts "(let ((thingy \"hi\") (something \"um\") (:const else \"world\")) (console-log thingy something else))")))
    (is (search "let thingy = \"hi\";" out))
    (is (search "let something = \"um\";" out))
    (is (search "const else_ = \"world\";" out))
    (is (search "console.log(thingy, something, else_);" out)))
  (rejects "(let ((:const x 1)) (setf x 2))" "const"))

(deftest let-shadowing-renames (:suite "transpiler")
  (let ((out (ts "(let ((x 1)) (let ((x (+ x 1))) (console-log x)))")))
    (is (search "let x = 1;" out))
    (is (search "let x_2 = x + 1;" out))
    (is (search "console.log(x_2);" out)))
  ;; sequential lets in one block must not redeclare
  (let ((out (ts "(progn (let ((x 1)) (console-log x)) (let ((x 2)) (console-log x)))")))
    (is (search "let x = 1;" out))
    (is (search "let x_2 = 2;" out))))

(deftest typed-let (:suite "transpiler")
  (emits "let n: number = 3;" "(let ((n 3)) (declare (fixnum n)) n)")
  (emits "let s: string | null = null;" "(let (s) (declare (string s)) s)")
  (emits "let u: any = null;" "(let (u) u)"))

(deftest setf-places (:suite "transpiler")
  (emits "x = 67;" "(defvar x 0) (setf x 67)")
  (emits "a[i] = v;" "(setf (aref a i) v)")
  (emits "h.set(k, v);" "(setf (gethash k h) v)")
  (emits "a.length = 0;" "(setf (fill-pointer a) 0)")
  (emits "obj.inner.value = 1;" "(setf (.. obj inner value) 1)")
  (emits "a = 1; b = 2;" "(setf a 1 b 2)")
  (emits "x += 2;" "(incf x 2)")
  (emits "x -= 1;" "(decf x)"))

(deftest arithmetic-and-comparison (:suite "transpiler")
  (emits "var a = 1 + 2 + 3;" "(defvar a (+ 1 2 3))")
  (emits "var b = -x;" "(defvar b (- x))")
  (emits "var c = ((a % b) + b) % b;" "(defvar c (mod a b))")
  (emits "var d = a === b;" "(defvar d (= a b))")
  (emits "var e = a < b && b < c;" "(defvar e (< a b c))")
  (emits "var f = !x;" "(defvar f (not x))")
  (emits "var g = (a && b) || c;" "(defvar g (or (and a b) c))")
  (emits "var h = x ?? y ?? 42;" "(defvar h (nil-or x y 42))")
  (emits "Math.floor(a / b)" "(defvar i (floor a b))")
  (emits "Math.pow(2, 10)" "(defvar j (expt 2 10))"))

(deftest if-when-cond-case (:suite "transpiler")
  (emits "var a = x ? 1 : 2;" "(defvar a (if x 1 2))")
  (emits "var b = x ? 1 : null;" "(defvar b (when x 1))")
  (emits "if (x) { console.log(1); }" "(when x (console-log 1))")
  (emits "if (x) { console.log(1); } else { console.log(2); }" "(if x (console-log 1) (console-log 2))")
  (emits "if (a) { console.log(1); } else if (b) { console.log(2); } else { console.log(3); }"
         "(cond (a (console-log 1)) (b (console-log 2)) (t (console-log 3)))")
  (let ((out (ts "(defun f (x) (case x ((1 2) \"small\") (:big \"big\") (otherwise \"other\")))")))
    (is (search "switch (x) {" out))
    (is (search "case 1: case 2: { return \"small\"; }" out))
    (is (search "case \":big\": { return \"big\"; }" out))
    (is (search "default: { return \"other\"; }" out)))
  (emits "throw new Error(\"ecase" "(defun f (x) (ecase x (1 2)))"))

(deftest progn-is-a-block (:suite "transpiler")
  (emits "{ console.log(1); console.log(2); }" "(progn (console-log 1) (console-log 2))")
  ;; expression position -> IIFE
  (emits "(() => { console.log(1); return 2; })()" "(defvar x (progn (console-log 1) 2))"))

(deftest functions-and-lambdas (:suite "transpiler")
  (emits "function doThing() {" "(defun do-thing ())")
  (emits "export function doThing() {" "(export (defun do-thing ()))")
  (emits "export default doThing;" "(export-default (defun do-thing ()))")
  (emits "function f(a: string, b: number = 2, ...rest: any[]) {"
         "(defun f (a &optional (b 2) &rest rest) (declare (string a) (fixnum b)) a)")
  (emits "function g(x: any, { loud, mode = \"a\" }: { loud?: any; mode?: any } = {}) {"
         "(defun g (x &key loud (mode \"a\")) x)")
  (emits "function h(): string {" "(defun h () (declare (returns string)) \"x\")")
  ;; #'operator becomes a lambda, so it can be passed like any function
  (emits "xs.reduce(((a: any, b: any) => (a + b)), 0)" "(defun f (xs) (reduce #'+ xs :initial-value 0))")
  (emits "xs.filter(((a: any) => (a % 2 === 0)))" "(defun f (xs) (remove-if-not #'evenp xs))")
  (emits "(a: any, b: any) => (a < b)" "(defun f (xs) (sort xs #'<))")
  (rejects "(defun f (xs) (map 'vector #'when xs))" "pass a lambda instead")
  ;; &key call sites pass the options object the signature destructures (even before the defun)
  (emits "f(1, { b: 2 });" "(defun f (a &key (b 1) c) (+ a b)) (f 1 :b 2)")
  (emits "return f(1, { c: 3, b: 4 });" "(defun g () (f 1 :c 3 :b 4)) (defun f (a &key b c) c)")
  (emits "f(1, undefined, { k: 2 });" "(defun f (a &optional o &key k) k) (f 1 :k 2)")
  (emits "f(1);" "(defun f (a &key k) k) (f 1)")
  (rejects "(defun f (a &key k) k) (f 1 :k)" "keyword arguments")
  (emits "var f = (e: any) => window.alert(\"hi\");" "(defvar f (lambda (e) (alert \"hi\")))")
  (emits "(x: any) => { console.log(x); return x + 1; }" "(defvar f (lambda (x) (console-log x) (+ x 1)))")
  (emits "function f(a: string, b: number): boolean {"
         "(declaim (ftype (function (string fixnum) boolean) f)) (defun f (a b) t)")
  (emits "var r = (((x: any) => x))(3);" "(defvar r ((lambda (x) x) 3))"))

(deftest flet-labels-and-recursion (:suite "transpiler")
  (let ((out (ts "(defun f () (labels ((fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5)))")))
    (is (search "const fact = (n: any) => ((n <= 1) ? 1 : (n * fact(n - 1)));" out))
    (is (search "return fact(5);" out))))

(deftest multiple-values (:suite "transpiler")
  ;; returning: (values ...) is an array
  (emits "return [a, b];" "(defun f (a b) (values a b))")
  ;; binding: a :multi let binding (specification.txt), optionally :const
  (emits "let [q, r] = divmod(7, 2);" "(let ((:multi (q r) (divmod 7 2))) (console-log q r))")
  (emits "const [state, setState] = useState(\"\");" "(let ((:const :multi (state set-state) (useState \"\"))) state)")
  (emits "const [a, b] = f();" "(let ((:multi :const (a b) (f))) a)")
  (emits "let something = \"hi\"; const [state, setState] = useState(\"\"); let [things, otherThings] = g();"
         "(let ((something \"hi\") (:const :multi (state set-state) (useState \"\")) (:multi (things other-things) (g))) things)")
  ;; a literal (values ...) initializer binds the variables one by one (no array round trip)
  (emits "let a = 1; let b = 2;" "(let ((:multi (a b) (values 1 2))) (console-log a b))")
  (emits "const a = 1; const b = null;" "(let ((:const :multi (a b) (values 1))) (console-log a b))")
  (emits "let a = 1; let b: any = null;" "(let ((:multi (a b) (values 1))) (console-log a b))")
  ;; declared types become a tuple annotation
  (emits "let [lo, hi]: [number, number] = stats(xs);"
         "(let ((:multi (lo hi) (stats xs))) (declare (fixnum lo hi)) (console-log lo hi))")
  (emits "const [n, s]: [number, any] = f();" "(let ((:const :multi (n s) (f))) (declare (fixnum n)) n)")
  ;; let* sees earlier bindings; shadowing is renamed
  (emits "let [a, b] = pair(); let c = a + b;" "(let* ((:multi (a b) (pair)) (c (+ a b))) c)")
  (let ((out (ts "(let ((a 1)) (let ((:multi (a b) (f a))) (console-log a b)))")))
    (is (search "let a = 1;" out))
    (is (search ", b] = f(a);" out))
    (is (search "console.log(a_" out)))
  ;; assignment to existing variables
  (emits "[a, b] = f();" "(setf (values a b) (f))")
  (emits "[k, v] = seq" "(loop for x across xs for (k v) across ys do (console-log x k v))")
  ;; the CL multiple-value-* forms are errors pointing at the let syntax
  (rejects "(multiple-value-bind (q r) (divmod 7 2) q)" ":multi")
  (rejects "(multiple-value-setq (a b) (f))" "(setf (values a b) form)")
  (rejects "(defvar x (multiple-value-list (f)))" "is an array")
  (rejects "(defvar x (nth-value 1 (f)))" "aref")
  ;; malformed :multi bindings
  (rejects "(let ((:multi (a b))) a)" "needs a form")
  (rejects "(let ((:multi a (f))) a)" "list of variables")
  (rejects "(let ((:multi (a :b) (f))) a)" "list of variables")
  (rejects "(let ((:frozen a 1)) a)" "unknown binding modifier")
  ;; destructuring-bind is unrelated to multiple values and unchanged
  (emits "const [x, y, ...more] = xs;" "(destructuring-bind (x y &rest more) xs x)")
  (emits "const { a, b = 2 } = obj;" "(destructuring-bind (&key a (b 2)) obj a)"))

;;; ------------------------------------------------------------------ loops

(deftest loop-across-collect (:suite "transpiler")
  (let ((out (ts "(defun f (xs) (loop for x across xs collect (* x 2)))")))
    (is (search "for (const x of xs) {" out))
    (is (search ".push(x * 2);" out))
    (is (search "return acc" out))))

(deftest loop-arithmetic-drivers (:suite "transpiler")
  (let ((out (ts "(defun f (n) (loop for i from 1 to n by 2 sum i))")))
    (is (search "let i = 1;" out))
    (is (search "while (true) {" out))
    (is (search "if (!(i <= n)) { break" out))
    (is (search "+= i;" out))
    (is (search "i += 2;" out)))
  (emits "if (!(i > 0)) { break" "(loop for i from 10 above 0 do (console-log i))")
  (emits "i -= 1;" "(loop for i from 10 downto 0 do (console-log i))")
  (emits "if (!(i < 10)) { break" "(loop for i below 10 do (console-log i))"))

(deftest loop-clauses (:suite "transpiler")
  (emits "if (!(x > 0)) { break" "(loop for x across xs while (> x 0) do (console-log x))")
  (emits "if (x > 100) { break" "(loop for x across xs until (> x 100) do (console-log x))")
  (emits "repeat" "(loop repeat 3 do (console-log 1))")
  (emits "for (const [k, v] of table) {" "(loop for k being the hash-keys of table using (hash-value v) do (console-log k v))")
  (emits "for (const k of table.keys()) {" "(loop for k being the hash-keys of table do (console-log k))")
  (emits "for (const v of table.values()) {" "(loop for v being the hash-values of table do (console-log v))")
  (let ((out (ts "(defun f (xs) (loop for x across xs when (evenp x) collect x into evens else collect x into odds finally (return (values evens odds))))")))
    (is (search "return [evens, odds];" out))
    (is (search "if (x % 2 === 0) { evens.push(x); } else { odds.push(x); }" out)))
  (let ((out (ts "(defun f (xs) (loop for x across xs maximize x))")))
    (is (search "null as number | null" out))
    (is (search "Math.max(" out)))
  (emits "return x;" "(defun f (xs) (loop for x across xs when (> x 1) return x))")
  (let ((out (ts "(defun f (xs) (loop for x across xs always (> x 0)))")))
    (is (search "return false;" out))
    (is (search "return true;" out)))
  (emits "let x = init; $loop" "(loop for x = init then (next x) while x do (console-log x))")
  (emits "x = next(x);" "(loop for x = init then (next x) while x do (console-log x))"))

(deftest loop-with-and-initially-finally (:suite "transpiler")
  (let ((out (ts "(defun f () (loop with total = 0 for i from 0 below 3 initially (console-log \"start\") do (incf total i) finally (console-log total) (return total)))")))
    (is (search "let total = 0;" out))
    (is (search "console.log(\"start\");" out))
    (is (search "total += i;" out))
    (is (search "console.log(total); return total;" out))))

(deftest loop-rejects-lists (:suite "transpiler")
  (rejects "(loop for x on xs do (console-log x))" "ON")
  (rejects "(dolist (x xs) (console-log x))" "for-each")
  (rejects "(defvar l (list 1 2))" "lists"))

(deftest dotimes-and-doarray-are-removed (:suite "transpiler")
  (rejects "(dotimes (i 3) (console-log i))" "dotimes is not available")
  (rejects "(dotimes (i 3) (console-log i))" "loop for i below")
  (rejects "(doarray (x xs) (console-log x))" "for-each")
  (rejects "(defun f (xs) (doarray (x xs nil) (when (> x 1) (return x))))" "map")
  ;; the idioms that replace them
  (emits "for (const x of xs) {" "(defun f (xs) (loop for x across xs when (> x 1) return x))")
  (emits "if (!(i < 3)) { break" "(loop for i below 3 do (console-log i))"))

(deftest for-each-is-for-side-effects (:suite "transpiler")
  (emits "xs.forEach((e: any) => { console.log(e); console.log(\"hi\"); });"
         "(for-each (e xs) (console-log e) (console-log \"hi\"))")
  (emits "xs.forEach((e: any, i: number) => {" "(for-each (e xs i) (console-log e i))")
  (emits "xs.forEach((e: string) => {" "(defun f (xs) (declare ((array string) xs)) (for-each (e xs) (declare (string e)) (console-log e)))")
  ;; never a .map, whatever the position; a function ending in for-each returns nothing
  (let ((out (ts "(defun f (xs) (for-each (e xs) (console-log e)))")))
    (is (search "xs.forEach(" out))
    (is-false (search ".map(" out))
    (is-false (search "return xs" out)))
  (let ((out (ts "(defvar r (for-each (e xs) (console-log e)))")))
    (is (search "xs.forEach(" out))
    (is-false (search ".map(" out)))
  (emits "f(xs.forEach((e: any) => {" "(f (for-each (e xs) (console-log e)))")
  (rejects "(for-each xs (console-log xs))" "for-each expects"))

(deftest map-builds-a-new-array (:suite "transpiler")
  (emits "var ys = xs.map(((e: any) => (e * 2)));" "(defvar ys (map (e xs) (* e 2)))")
  (emits "xs.map(((e: any, i: number) => (e * i)))" "(defvar ys (map (e xs i) (* e i)))")
  (emits "xs.map(((e: string): number => e.length))"
         "(defvar ys (map (e xs) (declare (string e) (returns fixnum)) (length e)))")
  (emits "return xs.map(" "(defun f (xs) (map (e xs) (* e 2)))")
  (let ((out (ts "(defvar ys (map (e xs) (console-log e) (* e 2)))")))
    (is (search "console.log(e);" out))
    (is (search "return e * 2;" out)))
  ;; Common Lisp (map 'vector fn seq) still works; the result type is irrelevant
  (emits "xs.map(f)" "(defvar ys (map 'vector f xs))")
  (rejects "(map xs f)" "map expects"))

(deftest block-and-return-from (:suite "transpiler")
  (let ((out (ts "(defun f (x) (block outer (when x (return-from outer 1)) 2))")))
    (is (search "if (x) { return 1; }" out))
    (is (search "return 2;" out)))
  (let ((out (ts "(block b (console-log 1) (return-from b) (console-log 2))")))
    (is (search "$block1: {" out))
    (is (search "break $block1;" out))
    (is-false (search "console.log(2)" out)))
  (rejects "(return-from nowhere 1)" "no block"))

;;; ------------------------------------------------------------------ errors, async, chaining

(deftest handler-case-and-unwind-protect (:suite "transpiler")
  (let ((out (ts "(defun f () (handler-case (risky) (error (e) (console-log e) nil)))")))
    (is (search "try { return risky(); } catch ($err1) { const e: any = $err1; console.log(e); return null; }" out)))
  (let ((out (ts "(defun f () (handler-case (risky) (TypeError (e) 1) (RangeError () 2)))")))
    (is (search "if ($err1 instanceof TypeError) {" out))
    (is (search "} else if ($err1 instanceof RangeError) {" out))
    (is (search "} else { throw $err1; }" out)))
  ;; a typed clause followed by a catch-all closes the if/else chain exactly once
  (let ((out (ts "(defun f () (handler-case (risky) (TypeError (e) 1) (error (e) 2)))")))
    (is (search "} else { const e: any = $err1; return 2; } } }" out) "~A" out)
    (is (= (count #\{ out) (count #\} out)) "unbalanced braces: ~A" out))
  (emits "try { risky(); } finally { cleanup(); }" "(unwind-protect (risky) (cleanup))")
  (emits "throw new Error(`bad ${x}`);" "(defun f (x) (error \"bad ~A\" x))")
  (emits "throw new TypeError(\"no\");" "(defun f () (error 'type-error \"no\"))")
  (emits "try { risky(); } catch { }" "(ignore-errors (risky))"))

(deftest async-await (:suite "transpiler")
  (emits "async function f(url: any) {" "(async (defun f (url) (await (fetch url))))")
  (emits "export async function f() {" "(export (async (defun f () 1)))")
  (emits "var g = async (x: any) => (await x);" "(defvar g (async (lambda (x) (await x))))")
  (emits "await Promise.all([a, b])" "(async (defun f () (await-all a b)))")
  (emits "(async () => { return await x; })()" "(defvar p (async (await x)))")
  (rejects "(defun f () (await x))" "outside an async")
  ;; top-level await is fine in ES modules
  (emits "await x;" "(await x)")
  ;; an IIFE containing await inside an async function awaits itself
  (emits "await (async () => {" "(async (defun f () (let ((v (progn (await a) (await b)))) v)))"))

(deftest dot-chaining (:suite "transpiler")
  ;; spec examples: the first element of a list step is always the method name
  (emits "React.createElement(\"h1\", { className: \"hello-message\" }, \"HIII\")"
         "(.. React (createElement \"h1\" '{}(:className \"hello-message\") \"HIII\"))")
  (emits "Something.doSomething()" "(.. Something (do-something))")
  (emits "React.Fragment" "(defvar f (.. React Fragment))")
  (emits "data?.user?.preferences?.theme?.color" "(defvar c (.. data (:? user) (:? preferences) (:? theme) (:? color)))")
  (emits "el.innerText" "(defvar t (.. el inner-text))")
  (emits "a.b()?.c(1)" "(defvar x (.. a (b) (:? (c 1))))")
  (emits "obj[\"weird key\"]" "(defvar x (.. obj \"weird key\"))")
  (emits "xs[i]" "(defvar x (.. xs (aref i)))")
  (emits "(a + b).toString()" "(defvar x (.. (+ a b) (toString)))")
  ;; a method named like a reserved word or with a name that mangles
  (emits "promise.then((v: any) => v)" "(defvar x (.. promise (then (lambda (v) v))))")
  (emits "el.classList.add(\"x\")" "(defvar x (.. el class-list (add \"x\")))")
  ;; the old `name (args...)` split form is NOT a call: the list is a call whose head must be a symbol
  (rejects "(.. React createElement (\"h1\" \"HIII\"))" "first element of a call step")
  (rejects "(.. obj method ())" "() is not allowed")
  (rejects "(.. fn (:? (1)))" "first element of a call step")
  (rejects "(.. obj (:x 1))" "first element of a call step")
  (rejects "(.. obj ((lambda (x) x) 1))" "first element of a call step"))

(deftest new-spread-typeof (:suite "transpiler")
  (emits "new Map()" "(defvar m (new Map))")
  (emits "new WebGLRenderer({ antialias: true })" "(defvar r (new WebGLRenderer '{}(:antialias t)))")
  (emits "f(1, ...rest)" "(f 1 (spread rest))")
  (emits "[...a, ...b]" "(defvar c (append a b))")
  (emits "typeof x === \"string\"" "(defvar s (typep x 'string))")
  (emits "x instanceof Foo" "(defvar s (instanceof x Foo))")
  (emits "var n = x as number;" "(defvar n (the fixnum x))")
  (rejects "(defvar s (spread x))" "spread"))

;;; ------------------------------------------------------------------ inline aliases & macros

(deftest prelude-inline-aliases (:suite "transpiler")
  (emits "console.log(\"hi\");" "(console-log \"hi\")")
  (emits "document.createElement(\"img\")" "(defvar i (doc-create-element \"img\"))")
  (emits "document.body.appendChild(el);" "(doc-body-append-child el)")
  (emits "document.getElementById(\"my-id\")" "(defvar e (doc-get-element-by-id \"my-id\"))")
  (emits "document.querySelectorAll(\".c\")" "(defvar e (doc-query-selector-all \".c\"))")
  (emits "a.appendChild(b);" "(append-child a b)")
  (emits "el.addEventListener(\"click\", (e: any) => window.alert(\"hi\"));" "(add-event-listener el \"click\" (lambda (e) (alert \"hi\")))")
  (emits "JSON.stringify({ something: \"hi\", \"else\": \"world\" })" "(defvar s (stringify-json '{}(:something \"hi\" :else \"world\")))")
  (emits "arr.slice(6, 7)" "(defvar s (slice arr 6 7))")
  ;; missing optional -> dropped, not null
  (emits "arr.slice(6)" "(defvar s (slice arr 6))"))

(deftest user-inline-aliases (:suite "transpiler")
  (let ((out (ts "(declaim (inline twice-log)) (defun twice-log (x) (console-log x) (console-log x)) (twice-log (compute))")))
    (is-false (search "function twiceLog" out))
    ;; non-trivial argument used twice is bound once
    (is (search "compute();" out))
    (is (search "console.log(x" out)))
  (emits "console.log(1, 2, 3);" "(declaim (inline log3)) (defun log3 (&rest xs) (console.log (spread xs))) (log3 1 2 3)"))

(deftest user-macros (:suite "transpiler")
  (emits "{ console.log(1); console.log(1); }" "(defmacro twice (form) `(progn ,form ,form)) (twice (console-log 1))")
  (emits "(x === 1) ? \"one\" : \"many\"" "(defmacro one-or-many (n) `(if (= ,n 1) \"one\" \"many\")) (defvar s (one-or-many x))")
  (rejects "(defmacro when (a) a)" "standard operator")
  ;; the template is frontend code, so setf is fine there; code under a comma is still checked
  (emits "x = (!x);" "(defvar x t) (defmacro toggle (place) `(setf ,place (not ,place))) (toggle x)")
  (rejects "(defmacro bad (place) `(setf ,place ,(setf *x* 1))) (bad y)" "may not be used"))

;;; ------------------------------------------------------------------ structs & types

(deftest defstruct-to-interface (:suite "transpiler")
  (let ((out (ts "(defstruct point (x 0 :type fixnum) (y 0 :type fixnum))
                  (defstruct (point3 (:include point)) (z 0 :type fixnum))
                  (defun f (p) (declare (point3 p)) (+ (point-x p) (point3-z p)))
                  (defvar q (make-point3 :x 1 :z 3))
                  (setf (point3-y q) 7)")))
    (is (search "interface Point { x: number; y: number; }" out))
    (is (search "interface Point3 extends Point { z: number; }" out))
    (is (search "function makePoint3({ x = 0, y = 0, z = 0 }: { x?: number; y?: number; z?: number } = {}): Point3 { return { x, y, z }; }" out))
    (is (search "function f(p: Point3) { return p.x + p.z; }" out))
    (is (search "var q = makePoint3({ x: 1, z: 3 });" out))
    (is (search "q.y = 7;" out)))
  (rejects "(defstruct (a (:include b) (:include c)) x)" "one :include")
  (rejects "(defstruct p x) (defvar q (make-p :nope 1))" "no slot")
  (emits "var b = { ...p };" "(defstruct p x) (defvar b (copy-p p))")
  (emits "makeS({ done: false })" "(defstruct s (done nil :type boolean)) (defvar o (make-s :done nil))"))

(deftest type-mapping (:suite "transpiler")
  (is-equal "null" (type->ts nil))
  (is-equal "undefined" (type->ts :undefined))
  (is-equal "true" (type->ts :true))
  (is-equal "string" (type->ts 'symbol))
  (is-equal "string" (type->ts 'keyword))
  (is-equal "string" (type->ts 'character))
  (is-equal "number | bigint" (type->ts 'number))
  (is-equal "number" (type->ts 'float))
  (is-equal "number" (type->ts 'fixnum))
  (is-equal "bigint" (type->ts 'bignum))
  (is-equal "{ numerator: bigint; denominator: bigint }" (type->ts 'ratio))
  (is-equal "{ real: number; imag: number }" (type->ts 'complex))
  (is-equal "any[]" (type->ts 'array))
  (is-equal "any[]" (type->ts 'vector))
  (is-equal "Uint8Array" (type->ts 'bit-vector))
  (is-equal "Map<any, any>" (type->ts 'hash-table))
  (is-equal "string[]" (type->ts '(array string)))
  (is-equal "(string | null)[]" (type->ts '(array (or string null))))
  (is-equal "Map<string, number>" (type->ts '(hash-table string fixnum)))
  (is-equal "string | number" (type->ts '(or string fixnum)))
  (is-equal "\"a\" | 1" (type->ts '(member "a" 1)))
  (is-equal "(arg0: string, arg1: number) => boolean" (type->ts '(function (string fixnum) boolean)))
  (is-equal "Promise<string>" (type->ts '(promise string)))
  (is-equal "[number, string]" (type->ts '(values fixnum string)))
  (is-equal "MyType" (type->ts 'my-type))
  (is-equal "Vector3Like" (type->ts '|Vector3Like|))
  (is-equal "Foo<string>" (type->ts '(|Foo| string))))

(deftest declarations (:suite "transpiler")
  (emits "var TITLE: string = \"hi\";" "(declaim (type string *title*)) (defvar *title* \"hi\")")
  (emits "function f(x: any) {" "(defun f (x) x)")
  (emits "type Id = string | number;" "(deftype id () '(or string fixnum))")
  (rejects "(declaim (optimize (speed 3)))" "optimize"))

;;; ------------------------------------------------------------------ modules

(deftest imports (:suite "transpiler")
  (emits "import * as THREE from \"three\";" "(import :from \"three\" :targets :all :as THREE)")
  (emits "import { WebGLRenderer, type Vector3Like, type Intersection, MathUtils, Color, type Material } from \"three\";"
         "(import :from \"three\" :targets (WebGLRenderer (:type Vector3Like Intersection) MathUtils Color (:type Material)))")
  (emits "import React from \"react\";" "(import :from \"react\" :targets React)")
  (emits "import { doThing } from \"./utils\";"
         "(import :from \"./utils\" :targets (do-thing))")
  (rejects "(import :targets (x))" ":from"))

(deftest exports (:suite "transpiler")
  (emits "export var helloWorld = \"hiii\";" "(export (defvar hello-world \"hiii\"))")
  (emits "export const LIMIT = 3;" "(export (defconstant +limit+ 3))")
  (emits "export interface P { x: any; }" "(export (defstruct p x))")
  (emits "export { a, b };" "(defvar a 1) (defvar b 2) (export a b)")
  (rejects "(export (console-log 1))" "export can only"))

;;; ------------------------------------------------------------------ inline aliases across modules

(defun transpile-modules (&rest name-source-pairs)
  "Transpile NAME-SOURCE-PAIRS (\"name.client.lisp\" source ...) as one project.
Returns an alist (module-name . collapsed output); signals TRANSPILE-ERROR like the real build."
  (let ((dir (merge-pathnames (format nil "monolisp-modules-~D/" (random 1000000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (loop for (file source) on name-source-pairs by #'cddr
                 do (let ((path (merge-pathnames file dir)))
                      (ensure-directories-exist path)
                      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
                        (write-string source s))))
           (let ((result (transpile-project dir (merge-pathnames "out/" dir) :manifest nil)))
             (mapcar (lambda (m) (cons (monolisp.transpiler::module-name m)
                                       (collapse-ws (monolisp.transpiler::module-output m))))
                     (getf result :modules))))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defparameter *three-aliases*
  "(import :from \"three\" :targets :all :as THREE)
   (declaim (inline vec3 v3-zero vec3-add scene-add deg->rad))
   (export (defun vec3 (x y z) (new (.. THREE Vector3) x y z)))
   (export (defun deg->rad (degrees) (THREE.MathUtils.degToRad degrees)))
   (export (defun v3-zero () (vec3 0 0 0)))
   (export (defun vec3-add (a b) (.. a (clone) (add b))))
   (defun scene-add (scene obj) (.. scene (add obj)))
   (export scene-add)")

(defun module-output-named (name outputs) (cdr (assoc name outputs :test #'string=)))

(deftest inline-aliases-cross-modules (:suite "transpiler")
  (let* ((outputs (transpile-modules
                   "three-aliases.client.lisp" *three-aliases*
                   "app.client.lisp" "(import :from \"three-aliases\" :targets (v3-zero vec3-add scene-add))
                                      (defun build (scene) (scene-add scene (vec3-add (v3-zero) (v3-zero))))"))
         (app (module-output-named "app" outputs)))
    ;; the alias expands at the call site, transitively (v3-zero -> vec3), and pulls in THREE
    (is (search "scene.add(new THREE.Vector3(0, 0, 0).clone().add(new THREE.Vector3(0, 0, 0)));" app) "~A" app)
    (is (search "import * as THREE from \"three\";" app) "~A" app)
    ;; nothing is imported from the alias module at runtime
    (is-false (search "three-aliases" app) "~A" app)
    ;; the alias module itself is still a valid ES module
    (is (search "import * as THREE from \"three\";" (module-output-named "three-aliases" outputs))))
  ;; a dotted symbol (THREE.MathUtils.degToRad) needs the THREE binding just the same
  (let ((app (module-output-named "app" (transpile-modules
                                          "three-aliases.client.lisp" *three-aliases*
                                          "app.client.lisp" "(import :from \"three-aliases\" :targets (deg->rad))
                                                             (defun f () (deg->rad 90))"))))
    (is (search "import * as THREE from \"three\";" app) "~A" app)
    (is (search "return THREE.MathUtils.degToRad(90);" app) "~A" app)))

(deftest keyword-arguments-across-modules (:suite "transpiler")
  (let ((app (module-output-named "app" (transpile-modules
                                          "http.client.lisp" "(export (defun request (url &key (method \"GET\") body) url))"
                                          "app.client.lisp" "(import :from \"http\" :targets (request (:as request req)))
                                                             (request \"/a\" :method \"POST\")
                                                             (req \"/b\" :body 1)"))))
    (is (search "request(\"/a\", { method: \"POST\" });" app) "~A" app)
    (is (search "req(\"/b\", { body: 1 });" app) "~A" app)))

(deftest inline-aliases-renamed-and-deduplicated (:suite "transpiler")
  (let* ((outputs (transpile-modules
                   "three-aliases.client.lisp" *three-aliases*
                   "app.client.lisp" "(import :from \"./three-aliases\" :targets ((:as vec3 v)))
                                      (import :from \"three\" :targets :all :as THREE)
                                      (defun f () (v 1 2 3))"))
         (app (module-output-named "app" outputs)))
    (is (search "return new THREE.Vector3(1, 2, 3);" app) "~A" app)
    (is (= 1 (count-matches "from \"three\"" app)) "~A" app))
  ;; an import the consumer already has is not repeated either
  (let ((app (module-output-named "app" (transpile-modules
                                          "three-aliases.client.lisp" *three-aliases*
                                          "app.client.lisp" "(import :from \"three\" :targets :all :as THREE)
                                                             (import :from \"three-aliases\" :targets (vec3))
                                                             (defun f () (vec3 1 2 3))"))))
    (is (= 1 (count-matches "from \"three\"" app)) "~A" app)))

(deftest inline-aliases-carry-module-exports (:suite "transpiler")
  (let* ((outputs (transpile-modules
                   "fmt.client.lisp" "(export (defvar *locale* \"en-US\"))
                                      (declaim (inline fmt-number))
                                      (export (defun fmt-number (n) (.. n (toLocaleString *locale*))))"
                   "app.client.lisp" "(import :from \"fmt\" :targets (fmt-number)) (defun f (n) (fmt-number n))"))
         (app (module-output-named "app" outputs)))
    (is (search "import { LOCALE } from \"./fmt\";" app) "~A" app)
    (is (search "return n.toLocaleString(LOCALE);" app) "~A" app)
    (is (search "export {};" (module-output-named "app" (transpile-modules
                                                          "a.client.lisp" "(declaim (inline f)) (export (defun f (x) x))"
                                                          "app.client.lisp" "(import :from \"a\" :targets (f)) (console-log (f 1))")))
        "a module with no imports/exports still emits export {}")))

(deftest inline-alias-parameters-do-not-rename-properties (:suite "transpiler")
  ;; `value` is both a parameter and a property/method name: only the variable is substituted
  (emits "el.value = \"x\";"
         "(declaim (inline set-value)) (defun set-value (el value) (setf (.. el value) value)) (set-value el \"x\")")
  (emits "el.setAttribute(\"name\", 1)?.name;"
         "(declaim (inline f)) (defun f (el name) (.. el (setAttribute \"name\" name) (:? name))) (f el 1)"))

(deftest inline-aliases-errors (:suite "transpiler")
  (flet ((error-text (&rest pairs)
           (handler-case (progn (apply #'transpile-modules pairs) nil)
             (transpile-error (e) (format nil "~A" e)))))
    (let ((msg (error-text "a.client.lisp" "(declaim (inline f)) (defun f (x) x)"
                           "app.client.lisp" "(import :from \"a\" :targets (f))")))
      (is (and msg (search "does not export" msg)) "~S" msg))
    (let ((msg (error-text "three-aliases.client.lisp" *three-aliases*
                           "app.client.lisp" "(defvar THREE 1) (import :from \"three-aliases\" :targets (vec3))")))
      (is (and msg (search "defines its own THREE" msg)) "~S" msg))
    (let ((msg (error-text "three-aliases.client.lisp" *three-aliases*
                           "app.client.lisp" "(import :from \"other\" :targets :all :as THREE)
                                              (import :from \"three-aliases\" :targets (vec3))")))
      (is (and msg (search "this module imports THREE from \"other\"" msg)) "~S" msg)))
  (rejects "(declaim (inline f)) (export-default (defun f (x) x))" "no runtime value"))

;;; ------------------------------------------------------------------ JSX

(deftest jsx-basic (:suite "transpiler")
  (let ((out (ts "(defun main-component (prop-1 prop-2)
                    (as-html-string
                      (:div :style '{}(:backgroundColor \"dodgerblue\" :color \"white\"))
                      (:div :class \"test-class\"
                        (:h1 :id \"big-header\" \"hi\")
                        (:p :class \"body-text\" \"hello\"))
                      (:div :class \"other-class\"
                        (:div :class \"next-class\"
                          (map (cur-thing prop-1)
                            (:p :key cur-thing (concat cur-thing prop-2)))))))"
                 :tsx t)))
    (is (search "import React from \"react\";" out))
    (is (search "return <><div style={{ backgroundColor: \"dodgerblue\", color: \"white\" }} />" out))
    (is (search "<div className=\"test-class\"><h1 id=\"big-header\">hi</h1><p className=\"body-text\">hello</p></div>" out))
    (is (search "{prop1.map((curThing: any) => (<p key={curThing}>{`${curThing}${prop2}`}</p>))}" out)))
  (emits "{items.map((x: any, i: number) => (<li key={i}>{x}</li>))}"
         "(defun f (items) (as-html-string (:ul (map (x items i) (:li :key i x)))))" :tsx t)
  ;; for-each yields nothing, so it cannot render children
  (rejects "(defun f (items) (as-html-string (:ul (for-each (x items) (:li x)))))" "use (map" :tsx t))

(deftest jsx-expressions-and-components (:suite "transpiler")
  (emits "<MyWidget title=\"x\" count={3} />" "(defun f () (as-html-string (:MyWidget :title \"x\" :count 3)))" :tsx t)
  (emits "<input type=\"checkbox\" checked={done} onChange={(e: any) => toggle(e)} />"
         "(defun f (done) (as-html-string (:input :type \"checkbox\" :checked done :onChange (lambda (e) (toggle e)))))" :tsx t)
  (emits "{err ? <p className=\"error\">{err}</p> : null}" "(defun f (err) (as-html-string (:div (when err (:p :class \"error\" err)))))" :tsx t)
  (emits "<label htmlFor=\"id\">{\"a < b\"}</label>" "(defun f () (as-html-string (:label :for \"id\" \"a < b\")))" :tsx t)
  (emits "<div {...props} />" "(defun f (props) (as-html-string (:div :spread (spread props))))" :tsx t)
  (rejects "(defun f () (as-html-string (:div \"x\")))" ".clientx")
  (rejects "(defvar x (:div \"x\"))" "as-html-string" :tsx t))

(deftest react-auto-import-can-be-disabled (:suite "transpiler")
  (let* ((config (make-transpile-config :react-p nil))
         (out (transpile-string "(defun f () (as-html-string (:div \"x\")))" :name "t" :tsx-p t :config config)))
    (is-false (search "import React" out))))

;;; ------------------------------------------------------------------ reader

(deftest reader-rules (:suite "transpiler")
  (rejects "(defvar x #.(+ 1 2))" "read error")
  (rejects "(defvar x '(a . b))" "cons")
  (rejects "(defvar x (foo . bar))" "cons")
  (rejects "(defvar x (foo" "end of file")
  (emits "var x = 0.5;" "(defvar x .5)")
  (emits "var y = 15000000000.0;" "(defvar y 1.5e10)"))

;;; ------------------------------------------------------------------ alignment with the backend

(defun manifest () (server-manifest))

(deftest server-call-compiles-to-typed-stub (:suite "transpiler")
  (let ((out (ts "(async (defun go () (await (add-numbers 1 2))))" :manifest (manifest))))
    (is (search "import { addNumbers } from \"./server-functions\";" out))
    (is (search "await addNumbers(1, 2)" out)))
  (let ((out (ts "(import :from :server :targets (greet)) (async (defun go () (await (greet \"Ada\"))))" :manifest (manifest))))
    (is (search "import { greet } from \"./server-functions\";" out))
    ;; not imported twice
    (is (= 1 (count-matches "from \"./server-functions\"" out)))))

(defun count-matches (needle text)
  (loop with start = 0 for pos = (search needle text :start2 start)
        while pos count t do (setf start (1+ pos))))

(deftest server-call-arity-checked (:suite "transpiler")
  (rejects "(async (defun go () (await (add-numbers 1))))" "at least 2" :manifest (manifest))
  (rejects "(async (defun go () (await (add-numbers 1 2 3 4))))" "at most 3" :manifest (manifest))
  (rejects "(async (defun go () (await (greet))))" "at least 1" :manifest (manifest)))

(deftest server-call-types-checked (:suite "transpiler")
  (rejects "(async (defun go () (await (greet 42))))" "must be string" :manifest (manifest))
  (rejects "(async (defun go () (await (add-numbers \"1\" 2))))" "must be number" :manifest (manifest))
  (rejects "(async (defun go () (await (add-numbers 1.5 2))))" "must be number" :manifest (manifest))
  ;; declared variable types are used too
  (rejects "(async (defun go (n) (declare (fixnum n)) (await (greet n))))" "must be string" :manifest (manifest))
  ;; unknown types pass (tsc is the second line of defence)
  (finishes (ts "(async (defun go (n) (await (greet n))))" :manifest (manifest)))
  ;; string literal too long for (string 64)
  (rejects "(async (defun go () (await (greet \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"))))"
           "must be string" :manifest (manifest))
  ;; non-strict mode only warns
  (finishes (ts "(async (defun go () (await (greet 42))))" :manifest (manifest) :strict nil))
  ;; a (member ...) type is checked literal by literal
  (finishes (ts "(async (defun go (x) (await (greet (the (member \"Ada\" \"Bob\") x)))))" :manifest (manifest)))
  (rejects "(async (defun go (x) (await (add-numbers (the (member \"a\") x) 2))))" "must be number" :manifest (manifest))
  (is (monolisp.transpiler::schema-accepts-type-p '(member "happy" "sad") '(member "happy" "sad")))
  (is-false (monolisp.transpiler::schema-accepts-type-p '(member "happy" "sad") '(member "happy" "angry")))
  (is-false (monolisp.transpiler::schema-accepts-type-p '(string 3) '(member "abcd"))))

(deftest server-stubs-generation (:suite "transpiler")
  (let ((stubs (generate-server-stubs (manifest))))
    (is (search "export interface Greeting {" stubs))
    (is (search "text: string;" stubs))
    (is (search "tags?: string[];" stubs))
    (is (search "export function addNumbers(a: number, b: number, scale?: number): Promise<number> {" stubs))
    (is (search "return rpcCall<number>(\"add-numbers\", trimArgs([a, b, scale]));" stubs))
    (is (search "export function greet(name: string): Promise<Greeting> {" stubs))
    (is (search "export function whoami(): Promise<string | null> {" stubs))
    (is (search "manifest-digest: " stubs))
    (is (search "serverFunctionNames = [" stubs))))

(deftest manifest-digest-stability (:suite "transpiler")
  (is-equal (manifest-digest (manifest)) (manifest-digest (manifest)))
  (let ((other (list :version 1
                     :functions (list (list :name 'x :rpc-name "x" :ts-name "x" :params '() :returns 'string))
                     :structs '())))
    (is (string/= (manifest-digest (manifest)) (manifest-digest other)))))

(deftest alignment-staleness-detection (:suite "transpiler")
  (let ((path (merge-pathnames (format nil "monolisp-stubs-~D.ts" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (is (alignment-stale-p path :manifest (manifest)))
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string (generate-server-stubs (manifest)) out))
           (is-false (alignment-stale-p path :manifest (manifest)))
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string "// manifest-digest: 0000000000000000" out))
           (is (alignment-stale-p path :manifest (manifest))))
      (ignore-errors (delete-file path)))))

(deftest runtime-source-is-strict (:suite "transpiler")
  (let ((rt (generate-runtime)))
    (is (search "json.jsonrpc !== \"2.0\" || json.id !== id" rt))
    (is (search "class RpcError" rt))
    (is (search "AbortController" rt))
    (is (search "credentials: options.credentials" rt))))

;;; The example projects are built through tsc by the "examples" suite (tests/example-tests.lisp).
