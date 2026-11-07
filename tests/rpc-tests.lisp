;;;; tests/rpc-tests.lisp -- server-function registry and JSON-RPC dispatcher behaviour

(defpackage :monolisp.test.rpc
  (:use :cl :monolisp.test :monolisp.json :monolisp.rpc :monolisp.async))

(in-package :monolisp.test.rpc)

;;; A small API used by these tests and by the integration / attack suites.

(define-rpc-struct greeting
  (text string)
  (length fixnum)
  (tags (array string) :optional t))

(define-server-function add-numbers ((a fixnum) (b fixnum) &optional ((scale fixnum) 1))
    (:returns fixnum :doc "Adds two numbers, optionally scaled.")
  (* scale (+ a b)))

(define-server-function greet ((name (string 64)))
    (:returns greeting :rpc-name "greet")
  (make-greeting :text (format nil "Hello, ~A!" name) :length (length name)))

(define-server-function whoami ()
    (:returns (nullable string) :auth :authenticated)
  (context-principal *request-context*))

(define-server-function admin-only ()
    (:returns boolean :auth is-admin-p)
  t)

(defun is-admin-p (context) (equal (context-principal context) "admin"))

(define-server-function slow-echo ((value string) (delay-ms fixnum))
    (:returns string :timeout-ms 200)
  (then (delay delay-ms) (lambda (v) (declare (ignore v)) value)))

(define-server-function async-sum ((xs (array fixnum)))
    (:returns fixnum)
  (async (let ((total 0))
           (loop for x across xs do (incf total (await (resolved x))))
           total)))

(define-server-function explode ()
    (:returns string)
  (error "secret internal detail: database password is hunter2"))

(define-server-function fail-nicely ((code fixnum))
    (:returns string)
  (rpc-fail "nope" :code (- -32000 code) :data (make-json-object "hint" "try again")))

(define-server-function bad-return ()
    (:returns fixnum)
  "not a number")

(defvar *last-notification* nil)

(define-server-function notify-sink ((x fixnum))
    (:returns null)
  (setf *last-notification* x)
  nil)

;;; helpers

(defun rpc (text &key context)
  "Send TEXT; returns parsed response (json-object / vector) or NIL."
  (let ((*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (let ((out (handle-rpc-sync text :context (or context (make-rpc-context)))))
      (and out (json-parse out)))))

(defun call (method params &key (id 1) context)
  (rpc (json-encode (make-json-object "jsonrpc" "2.0" "method" method "params" params "id" id)) :context context))

(defun err-code (response) (json-ref response "error" "code"))

(deftest registry-registers-and-describes (:suite "rpc")
  (let ((sf (find-server-function "add-numbers")))
    (is sf)
    (is-equal 'add-numbers (server-function-name sf))
    (is-equal 3 (length (server-function-params sf)))
    (is (param-optional-p (third (server-function-params sf))))
    (is-equal "addNumbers(a: number, b: number, scale?: number): Promise<number>" (server-function-ts-signature sf)))
  (is (find-server-function "greet"))
  (is-false (find-server-function "ADD-NUMBERS"))
  (is-false (find-server-function "is-admin-p"))
  (is-equal 5 (add-numbers 2 3))
  (is-equal 10 (add-numbers 2 3 2)))

(deftest registry-rejects-bad-definitions (:suite "rpc")
  (signals error (macroexpand-1 '(define-server-function no-returns ((a fixnum)) () a)))
  (signals error (macroexpand-1 '(define-server-function rest-args ((a fixnum) &rest r) (:returns fixnum) a)))
  (signals error (macroexpand-1 '(define-server-function bad-name ((a fixnum)) (:returns fixnum :rpc-name "rpc.evil") a)))
  (signals error (macroexpand-1 '(define-server-function bad-name2 ((a fixnum)) (:returns fixnum :rpc-name "has space") a)))
  (signals error (eval '(define-server-function bad-schema ((a frobnicator)) (:returns fixnum) a))))

(deftest positional-call (:suite "rpc")
  (let ((r (call "add-numbers" #(2 3))))
    (is-equal "2.0" (json-get r "jsonrpc"))
    (is-equal 5 (json-get r "result"))
    (is-equal 1 (json-get r "id"))
    (is-false (json-has-key-p r "error"))))

(deftest optional-positional (:suite "rpc")
  (is-equal 10 (json-get (call "add-numbers" #(2 3 2)) "result")))

(deftest named-call (:suite "rpc")
  (is-equal 5 (json-get (call "add-numbers" (make-json-object "a" 2 "b" 3)) "result"))
  (is-equal 10 (json-get (call "add-numbers" (make-json-object "b" 3 "a" 2 "scale" 2)) "result")))

(deftest struct-result (:suite "rpc")
  (let ((r (call "greet" #("Ada"))))
    (is-equal "Hello, Ada!" (json-ref r "result" "text"))
    (is-equal 3 (json-ref r "result" "length"))
    (is-false (json-has-key-p (json-get r "result") "tags"))))

(deftest string-id-and-null-id (:suite "rpc")
  (is-equal "abc" (json-get (call "add-numbers" #(1 1) :id "abc") "id"))
  (is-equal :null (json-get (call "add-numbers" #(1 1) :id :null) "id")))

(deftest notification-gets-no-response (:suite "rpc")
  (setf *last-notification* nil)
  (is-equal nil (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"notify-sink\",\"params\":[7]}"))
  (is-equal 7 *last-notification*)
  ;; even errors are silent for notifications
  (is-equal nil (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"does-not-exist\"}")))

(deftest method-not-found (:suite "rpc")
  (is-equal +method-not-found+ (err-code (call "nope" #()))))

(deftest invalid-params-errors (:suite "rpc")
  (is-equal +invalid-params+ (err-code (call "add-numbers" #(1))))
  (is-equal +invalid-params+ (err-code (call "add-numbers" #(1 2 3 4))))
  (is-equal +invalid-params+ (err-code (call "add-numbers" #("1" 2))))
  (is-equal +invalid-params+ (err-code (call "add-numbers" #(1.5 2))))
  (is-equal +invalid-params+ (err-code (call "add-numbers" (make-json-object "a" 1))))
  (is-equal +invalid-params+ (err-code (call "add-numbers" (make-json-object "a" 1 "b" 2 "c" 3))))
  (is-equal +invalid-params+ (err-code (call "greet" #("this name is far too long to be accepted by the sixty four limit!!"))))
  (let ((r (call "add-numbers" #(1 "x"))))
    (is-equal "Invalid params" (json-ref r "error" "message"))
    (is (search "$[1]" (json-ref r "error" "data")))))

(deftest missing-params-for-no-arg-function (:suite "rpc")
  (is-equal t (json-true-p (json-get (call "admin-only" #() :context (let ((c (make-rpc-context))) (setf (context-principal c) "admin") c)) "result"))))

(deftest auth-public-vs-authenticated (:suite "rpc")
  (is-equal +unauthorized+ (err-code (call "whoami" #())))
  (let ((ctx (make-rpc-context)))
    (setf (context-principal ctx) "user-1")
    (is-equal "user-1" (json-get (call "whoami" #() :context ctx) "result"))))

(deftest auth-custom-predicate (:suite "rpc")
  (let ((ctx (make-rpc-context)))
    (setf (context-principal ctx) "user-1")
    (is-equal +unauthorized+ (err-code (call "admin-only" #() :context ctx)))
    (setf (context-principal ctx) "admin")
    (is-equal :true (json-get (call "admin-only" #() :context ctx) "result"))))

(deftest authenticator-hook (:suite "rpc")
  (let ((*authenticator* (lambda (ctx) (let ((tok (context-header ctx "authorization")))
                                         (and tok (constant-time-equal tok "Bearer secret") "token-user")))))
    (is-equal +unauthorized+ (err-code (call "whoami" #())))
    (is-equal "token-user"
              (json-get (call "whoami" #() :context (make-rpc-context :headers '(("authorization" . "Bearer secret")))) "result"))
    (is-equal +unauthorized+
              (err-code (call "whoami" #() :context (make-rpc-context :headers '(("authorization" . "Bearer wrong"))))))))

(deftest async-server-functions (:suite "rpc")
  (is-equal 6 (json-get (call "async-sum" #(#(1 2 3))) "result"))
  (is-equal "hi" (json-get (call "slow-echo" #("hi" 10)) "result")))

(deftest per-function-timeout (:suite "rpc")
  (is-equal +timeout+ (err-code (call "slow-echo" #("hi" 1000)))))

(deftest internal-errors-are-opaque (:suite "rpc")
  (let* ((logged nil)
         (r (let ((*error-logger* (lambda (c ctx) (declare (ignore ctx)) (setf logged c))))
              (handle-rpc-sync "{\"jsonrpc\":\"2.0\",\"method\":\"explode\",\"id\":1}"))))
    (is-equal +internal-error+ (json-ref (json-parse r) "error" "code"))
    (is-false (search "hunter2" r) "internal details leaked to client")
    (is logged "error was not logged server side")
    (is (search "hunter2" (princ-to-string logged)))))

(deftest debug-mode-includes-details (:suite "rpc")
  (let ((*debug-mode* t))
    (is (search "hunter2" (json-encode (call "explode" #()))))))

(deftest bad-return-value-is-internal-error (:suite "rpc")
  (is-equal +internal-error+ (err-code (call "bad-return" #()))))

(deftest application-errors-pass-through (:suite "rpc")
  (let ((r (call "fail-nicely" #(5))))
    (is-equal -32005 (err-code r))
    (is-equal "nope" (json-ref r "error" "message"))
    (is-equal "try again" (json-ref r "error" "data" "hint"))))

(deftest batch-requests (:suite "rpc")
  (let ((r (rpc "[{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[1,2],\"id\":1},
                  {\"jsonrpc\":\"2.0\",\"method\":\"notify-sink\",\"params\":[3]},
                  {\"jsonrpc\":\"2.0\",\"method\":\"missing\",\"id\":2},
                  {\"foo\":\"bar\"}]")))
    (is (simple-vector-p r))
    (is-equal 3 (length r))
    (let ((by-id (lambda (id) (find id r :key (lambda (x) (json-get x "id")) :test #'equal))))
      (is-equal 3 (json-get (funcall by-id 1) "result"))
      (is-equal +method-not-found+ (err-code (funcall by-id 2)))
      (is-equal +invalid-request+ (err-code (funcall by-id :null))))))

(deftest batch-of-only-notifications-is-silent (:suite "rpc")
  (is-equal nil (rpc "[{\"jsonrpc\":\"2.0\",\"method\":\"notify-sink\",\"params\":[1]}]")))

(deftest manifest-describes-api (:suite "rpc")
  (let* ((m (server-manifest))
         (add (find "add-numbers" (getf m :functions) :key (lambda (f) (getf f :rpc-name)) :test #'string=)))
    (is add)
    (is-equal "addNumbers" (getf add :ts-name))
    (is-equal 'fixnum (getf add :returns))
    (is (find 'greeting (getf m :structs) :key (lambda (s) (getf s :name))))
    (let ((json (json-encode (server-manifest-json))))
      (is (search "\"rpcName\":\"add-numbers\"" json))
      (is (search "\"tsName\":\"Greeting\"" json)))))

(deftest rate-limiter-token-bucket (:suite "rpc")
  (let ((rl (make-rate-limiter :capacity 3 :refill-per-second 1000)))
    (is (rate-limit-allow-p rl "ip1"))
    (is (rate-limit-allow-p rl "ip1"))
    (is (rate-limit-allow-p rl "ip1"))
    (is-false (rate-limit-allow-p rl "ip1"))
    (is (rate-limit-allow-p rl "ip2") "limits are per key")
    (sleep 0.01)
    (is (rate-limit-allow-p rl "ip1") "bucket refills")))

(deftest rate-limiter-bounds-memory (:suite "rpc")
  (let ((rl (make-rate-limiter :capacity 1 :refill-per-second 1 :max-keys 10)))
    (dotimes (i 10) (rate-limit-allow-p rl (format nil "k~D" i)))
    (is-equal 10 (rate-limiter-count rl))
    ;; all buckets are drained and not refilled -> cannot evict -> refuse new keys
    (is-false (rate-limit-allow-p rl "k-new"))))

(deftest constant-time-equal-works (:suite "rpc")
  (is (constant-time-equal "abc" "abc"))
  (is-false (constant-time-equal "abc" "abd"))
  (is-false (constant-time-equal "abc" "abcd"))
  (is-false (constant-time-equal "" "a"))
  (is (constant-time-equal "" "")))

(deftest random-token-is-random (:suite "rpc")
  (let ((a (random-token)) (b (random-token)))
    (is-equal 64 (length a))
    (is-false (string= a b))
    (is (every (lambda (c) (digit-char-p c 16)) a)))
  ;; the OS generator actually fills the buffer: 64 KiB hits every octet value
  (let ((seen (make-array 256 :element-type 'bit :initial-element 0)))
    (loop for b across (monolisp.rpc::fill-random-octets (make-array 65536 :element-type '(unsigned-byte 8)))
          do (setf (sbit seen b) 1))
    (is (every #'plusp seen))))
