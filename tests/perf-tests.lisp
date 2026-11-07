;;;; tests/perf-tests.lisp -- throughput of the hot paths
;;;;
;;;; These are regression guards rather than benchmarks: each test asserts a floor that is an
;;;; order of magnitude below what a debug build on a laptop reaches, so a quadratic slip or an
;;;; accidental full-copy shows up as a failure instead of as a slow server.

(defpackage :monolisp.test.perf
  (:use :cl :monolisp.test :monolisp.json :monolisp.rpc :monolisp.http :monolisp.async))

(in-package :monolisp.test.perf)

(defvar *sample-json*
  "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":{\"a\":12345,\"b\":67890,\"scale\":2},\"id\":\"req-0001\",\"meta\":{\"tags\":[\"alpha\",\"beta\",\"gamma\"],\"ratio\":0.3333,\"flags\":[true,false,null],\"text\":\"The quick brown fox \\u00e9\\n jumps over the lazy dog\"}}")

(defvar *sample-octets* (sb-ext:string-to-octets *sample-json* :external-format :utf-8))

(defun floor-ok (ops floor what)
  (is (> ops floor) "~A: ~,0F ops/sec is below the floor of ~D" what ops floor))

(deftest perf-json-parse (:suite "perf")
  (let ((ops (bench ("json-parse 300B document" 20000) (json-parse-octets *sample-octets*))))
    (floor-ok ops 5000 "json-parse")))

(deftest perf-json-encode (:suite "perf")
  (let* ((value (json-parse-octets *sample-octets*))
         (ops (bench ("json-encode 300B document" 20000) (json-encode value))))
    (floor-ok ops 5000 "json-encode")))

(deftest perf-json-large-array (:suite "perf")
  (let* ((text (format nil "[~{~D~^,~}]" (loop for i below 10000 collect i)))
         (octets (sb-ext:string-to-octets text :external-format :utf-8))
         (ops (bench ("json-parse 10k-int array" 200) (json-parse-octets octets))))
    (floor-ok ops 20 "json-parse large array")))

(deftest perf-json-string-heavy (:suite "perf")
  (let* ((text (json-encode (make-array 500 :initial-element "héllo wörld, this is a fairly ordinary string \"with\" escapes\n")))
         (octets (sb-ext:string-to-octets text :external-format :utf-8))
         (ops (bench ("json-parse 500 escaped strings" 500) (json-parse-octets octets))))
    (floor-ok ops 50 "json-parse string heavy")))

(deftest perf-rpc-dispatch (:suite "perf")
  ;; full boundary: parse -> validate envelope -> validate params -> call -> encode result
  (let ((request "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[2,3],\"id\":1}")
        (*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (with-event-loop (:run nil)
      (let ((ops (bench ("rpc dispatch add-numbers" 5000)
                   (promise-wait (handle-rpc-string request (make-rpc-context)) :timeout 5))))
        (floor-ok ops 500 "rpc dispatch")))))

(deftest perf-rpc-batch-dispatch (:suite "perf")
  (let ((request (format nil "[~{~A~^,~}]"
                         (loop for i below 20
                               collect (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"greet\",\"params\":[\"user~D\"],\"id\":~D}" i i))))
        (*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (with-event-loop (:run nil)
      (let ((ops (bench ("rpc dispatch 20-call batch" 1000)
                   (promise-wait (handle-rpc-string request (make-rpc-context)) :timeout 5))))
        (floor-ok ops 50 "rpc batch dispatch")))))

(deftest perf-rpc-rejection-is-cheap (:suite "perf")
  ;; garbage must be rejected at least as fast as valid requests are served
  (let ((request "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[\"x\",3],\"id\":1}")
        (*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (with-event-loop (:run nil)
      (let ((ops (bench ("rpc invalid-params rejection" 5000)
                   (promise-wait (handle-rpc-string request (make-rpc-context)) :timeout 5))))
        (floor-ok ops 500 "rpc rejection")))))

(deftest perf-schema-validation (:suite "perf")
  (let* ((schema '(object (name (string 64)) (age (integer 0 150)) (tags (array (string 16) 8)) (kind (member "a" "b"))))
         (value (json-parse "{\"name\":\"Ada Lovelace\",\"age\":36,\"tags\":[\"math\",\"engine\",\"poetry\"],\"kind\":\"a\"}"))
         (ops (bench ("validate-value nested object" 50000) (validate-value schema value))))
    (floor-ok ops 20000 "validate-value")))

(deftest perf-http-head-parse (:suite "perf")
  (let* ((head (sb-ext:string-to-octets
                (format nil "POST /rpc HTTP/1.1~C~CHost: localhost:8080~C~CUser-Agent: perf/1.0~C~CAccept: application/json~C~CContent-Type: application/json~C~CContent-Length: 64~C~CConnection: keep-alive~C~C~C~C"
                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                :external-format :latin-1))
         (ops (bench ("parse-request-head 7 headers" 50000) (parse-request-head head (length head)))))
    (floor-ok ops 20000 "parse-request-head")))

(deftest perf-rate-limiter (:suite "perf")
  (let* ((limiter (make-rate-limiter :capacity 1000000 :refill-per-second 1000000))
         (ops (bench ("rate-limit-allow-p 100 keys" 100000)
                (rate-limit-allow-p limiter (svref #("k0" "k1" "k2" "k3" "k4" "k5" "k6" "k7" "k8" "k9") (random 10))))))
    (floor-ok ops 100000 "rate limiter")))

(deftest perf-promise-chain (:suite "perf")
  (with-event-loop (:run nil)
    (let ((ops (bench ("promise then-chain depth 10" 2000)
                 (promise-wait
                  (let ((p (resolved 0)))
                    (dotimes (i 10 p) (setf p (then p (lambda (v) (1+ v))))))
                  :timeout 5))))
      (floor-ok ops 500 "promise chain"))))

(deftest perf-async-await-loop (:suite "perf")
  (with-event-loop (:run nil)
    (let ((ops (bench ("async/await 100 sequential awaits" 200)
                 (promise-wait
                  (async (let ((total 0))
                           (dotimes (i 100) (incf total (await (resolved i))))
                           total))
                  :timeout 5))))
      (floor-ok ops 20 "async/await loop"))))

(deftest perf-transpile-module (:suite "perf")
  (let* ((source "(defstruct todo (id 0 :type fixnum) (title \"\" :type string) (done nil :type boolean))
                  (defun visible (todos filter)
                    (loop for td across todos
                          when (case filter (:open (not (todo-done td))) (:done (todo-done td)) (otherwise t))
                          collect td))
                  (export (async (defun refresh (set-todos)
                    (handler-case (funcall set-todos (await (json-fetch \"/api\")))
                      (error (e) (console-error e))))))
                  (defun summary (todos) (concat (length todos) \" items, \" (count-if #'todo-done todos) \" done\"))")
         (ops (bench ("transpile 4-form module" 300)
                (monolisp.transpiler:transpile-string source :name "perf"))))
    (floor-ok ops 20 "transpile-string")))

(deftest perf-end-to-end-http (:suite "perf")
  "Sequential keep-alive requests through the real server over loopback."
  (let ((*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (with-event-loop (:run nil)
      (let* ((server (start-server :port 0 :rate-limiter nil))
             (port (server-port server))
             (body "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[2,3],\"id\":1}")
             (ok 0) (n 300))
        (unwind-protect
             (let ((secs
                     (let ((start (get-internal-real-time)))
                       (promise-wait
                        (async
                          (dotimes (i n)
                            (let ((r (await (fetch (format nil "http://127.0.0.1:~D/rpc" port)
                                                   :method "POST" :body body
                                                   :headers '(("Content-Type" . "application/json"))))))
                              (when (= 200 (http-response-status r)) (incf ok)))))
                        :timeout 60)
                       (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
               (is-equal n ok)
               (let ((ops (/ n (max secs 1d-9))))
                 (push (list "http e2e sequential POST /rpc" ops) *bench-results*)
                 (format t "~&    BENCH http e2e sequential POST /rpc: ~,0F req/sec~%" ops)
                 (floor-ok ops 50 "http end-to-end")))
          (stop-server server))))))
