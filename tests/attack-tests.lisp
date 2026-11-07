;;;; tests/attack-tests.lisp -- hostile input at every boundary
;;;;
;;;; Every layer that touches attacker-controlled bytes is exercised with the classic attack
;;;; classes for that layer, plus randomized fuzzing that asserts the only conditions ever
;;;; escaping a parser are the designated ones (json-parse-error / http-parse-error / an RPC error
;;;; response).  Nothing here opens sockets; the socket-level attacks (slowloris, connection
;;;; floods, smuggling over the wire) live in integration-tests.lisp.

(defpackage :monolisp.test.attack
  (:use :cl :monolisp.test :monolisp.json :monolisp.rpc :monolisp.http))

(in-package :monolisp.test.attack)

;;; ------------------------------------------------------------------ helpers

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun string-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun concat-octets (&rest vectors)
  (let ((out (make-array (reduce #'+ vectors :key #'length) :element-type '(unsigned-byte 8)))
        (pos 0))
    (dolist (v vectors out)
      (replace out v :start1 pos)
      (incf pos (length v)))))

(defun repeat-string (string n)
  (with-output-to-string (s) (dotimes (i n) (write-string string s))))

(defmacro parse-fails (input &optional message-fragment)
  "INPUT must be rejected with JSON-PARSE-ERROR (and nothing else)."
  (let ((e (gensym)))
    `(let ((,e (handler-case (progn (json-parse ,input) nil)
                 (json-parse-error (c) c)
                 (error (c) (fail "~S raised ~A instead of json-parse-error: ~A" ,input (type-of c) c) :other))))
       (is (and ,e (not (eq ,e :other))) "~S should have been rejected" ,input)
       ,@(when message-fragment
           `((when (typep ,e 'json-parse-error)
               (is (search ,message-fragment (json-parse-error-message ,e))
                   "expected ~S in ~S" ,message-fragment (json-parse-error-message ,e))))))))

(defun rpc (text &key context limits)
  (let ((*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil)))
    (let ((out (if limits
                   (handle-rpc-sync text :context (or context (make-rpc-context)) :limits limits)
                   (handle-rpc-sync text :context (or context (make-rpc-context))))))
      (and out (json-parse out)))))

(defun err-code (response) (and response (json-ref response "error" "code")))
(defun err-data (response) (and response (json-ref response "error" "data")))

(defun http-status (octets &rest args)
  "Status code the parser would answer with, or NIL when the head parses."
  (handler-case (progn (apply #'parse-request-head octets (length octets) args) nil)
    (http-parse-error (e) (http-parse-error-status e))))

(defun crlf (&rest lines)
  (sb-ext:string-to-octets
   (with-output-to-string (s)
     (dolist (l lines) (write-string l s) (write-char #\Return s) (write-char #\Linefeed s))
     (write-char #\Return s) (write-char #\Linefeed s))
   :external-format :latin-1))

;;; ================================================================== JSON parser

(deftest json-deep-nesting-is-bounded (:suite "attack")
  ;; a billion-laughs style stack bomb
  (parse-fails (concatenate 'string (repeat-string "[" 100000) (repeat-string "]" 100000)) "deep")
  (parse-fails (concatenate 'string (repeat-string "{\"a\":" 5000) "1" (repeat-string "}" 5000)) "deep")
  ;; default limit is 64; 60 nested arrays is fine
  (finishes (json-parse (concatenate 'string (repeat-string "[" 60) (repeat-string "]" 60))))
  ;; a tighter custom limit is honoured
  (signals json-parse-error (json-parse "[[[[1]]]]" :limits (make-json-limits :max-depth 3))))

(deftest json-number-attacks (:suite "attack")
  ;; exponent bombs must not allocate enormous bignums or spin
  (parse-fails "1e999999999" "exponent")
  (parse-fails "1e-999999999" "exponent")
  (parse-fails (concatenate 'string "1" (repeat-string "0" 1000)) "digits")
  (parse-fails (concatenate 'string "0." (repeat-string "1" 1000)))
  ;; grammar edge cases
  (parse-fails "01") (parse-fails "-") (parse-fails "1.") (parse-fails ".5") (parse-fails "+1")
  (parse-fails "0x10") (parse-fails "1e") (parse-fails "1e+") (parse-fails "Infinity") (parse-fails "NaN")
  (parse-fails "-Infinity")
  ;; large but legal values stay exact / finite
  (is-equal 9007199254740993 (json-parse "9007199254740993"))
  (is (floatp (json-parse "1e308")))
  (is (floatp (json-parse "1.7976931348623157e308")))
  (is (zerop (json-parse "1e-400"))))

(deftest json-string-attacks (:suite "attack")
  ;; raw control characters
  (parse-fails (format nil "\"a~Cb\"" (code-char 0)) "control")
  (parse-fails (format nil "\"a~Cb\"" (code-char 31)) "control")
  (parse-fails (format nil "\"a~Cb\"" #\Newline) "control")
  ;; broken escapes
  (parse-fails "\"\\x41\"" "escape")
  (parse-fails "\"\\u12\"")
  (parse-fails "\"\\uZZZZ\"")
  (parse-fails "\"\\\"")
  (parse-fails "\"abc")
  ;; surrogate games via \u escapes
  (parse-fails "\"\\uD800\"" "surrogate")
  (parse-fails "\"\\uDC00\"" "surrogate")
  (parse-fails "\"\\uD800\\u0041\"" "surrogate")
  (parse-fails "\"\\uD800x\"" "surrogate")
  (is-equal (string (code-char #x1F600)) (json-parse "\"\\uD83D\\uDE00\""))
  ;; NUL via escape is legal JSON; make sure it survives rather than truncating
  (is-equal 3 (length (json-parse "\"a\\u0000b\""))))

(deftest json-utf8-attacks (:suite "attack")
  (flet ((bad (&rest bytes) (parse-fails (concat-octets (octets 34) (apply #'octets bytes) (octets 34)))))
    (bad #xC0 #xAF)                         ; overlong '/'
    (bad #xC1 #xBF)                         ; overlong
    (bad #xE0 #x80 #xAF)                    ; overlong 3-byte
    (bad #xF0 #x80 #x80 #xAF)               ; overlong 4-byte
    (bad #xED #xA0 #x80)                    ; encoded surrogate U+D800
    (bad #xED #xBF #xBF)                    ; encoded surrogate U+DFFF
    (bad #xF4 #x90 #x80 #x80)               ; above U+10FFFF
    (bad #xF5 #x80 #x80 #x80)               ; invalid lead
    (bad #xFF) (bad #xFE)                   ; never valid
    (bad #x80)                              ; stray continuation
    (bad #xE2 #x82)                         ; truncated sequence
    (bad #xE2 #x41 #x82)                    ; bad continuation
    (bad #xC3))                             ; truncated at end of string
  ;; valid multi-byte content is preserved
  (is-equal "héllo €" (json-parse (concat-octets (octets 34) (string-octets "héllo €") (octets 34))))
  ;; BOM is not whitespace
  (parse-fails (concat-octets (octets #xEF #xBB #xBF) (string-octets "1")))
  ;; a UTF-8 BOM / NBSP / U+2028 between tokens are not JSON whitespace either
  (parse-fails (concat-octets (string-octets "[1,") (octets #xC2 #xA0) (string-octets "2]")))
  (parse-fails (concat-octets (string-octets "[1,") (octets #xE2 #x80 #xA8) (string-octets "2]"))))

(deftest json-structure-attacks (:suite "attack")
  (parse-fails "{\"a\":1,\"a\":2}" "duplicate")
  (parse-fails "{\"__proto__\":1,\"__proto__\":2}" "duplicate")
  ;; prototype-pollution names are just keys here, but they must round-trip verbatim
  (is-equal 1 (json-get (json-parse "{\"__proto__\":1}") "__proto__"))
  (is-equal 2 (json-get (json-parse "{\"constructor\":2}") "constructor"))
  ;; trailing garbage, comments, trailing commas, single quotes
  (parse-fails "1 2") (parse-fails "{} {}") (parse-fails "[1,]") (parse-fails "{\"a\":1,}")
  (parse-fails "[1 2]") (parse-fails "{'a':1}") (parse-fails "{a:1}") (parse-fails "// c\n1")
  (parse-fails "/* c */ 1") (parse-fails "[1]garbage") (parse-fails "") (parse-fails "   ")
  (parse-fails "{\"a\"}") (parse-fails "{\"a\":}") (parse-fails "{:1}") (parse-fails "[,1]")
  (parse-fails "tru") (parse-fails "truex") (parse-fails "True") (parse-fails "nul") (parse-fails "undefined")
  (parse-fails (format nil "~C1" (code-char 0)))
  (parse-fails (format nil "1~C" (code-char 0)))
  ;; keys must be strings
  (parse-fails "{1:2}") (parse-fails "{null:2}") (parse-fails "{[1]:2}"))

(deftest json-resource-limits (:suite "attack")
  (let ((limits (make-json-limits :max-bytes 64 :max-string-bytes 16 :max-array-items 4
                                  :max-object-keys 3 :max-total-values 20 :max-depth 8)))
    (signals json-parse-error (json-parse (repeat-string " " 100) :limits limits))
    (signals json-parse-error (json-parse (format nil "\"~A\"" (repeat-string "a" 17)) :limits limits))
    (signals json-parse-error (json-parse "[1,2,3,4,5]" :limits limits))
    (signals json-parse-error (json-parse "{\"a\":1,\"b\":2,\"c\":3,\"d\":4}" :limits limits))
    (signals json-parse-error (json-parse "[[1,2],[3,4],[5,6],[7,8]]" :limits (make-json-limits :max-total-values 5)))
    (finishes (json-parse "[1,2,3,4]" :limits limits))
    (finishes (json-parse "{\"a\":1,\"b\":2,\"c\":3}" :limits limits)))
  ;; default limits: a 2 MB payload is refused up front without being scanned
  (let ((start (get-internal-real-time)))
    (signals json-parse-error (json-parse (make-array (* 2 1024 1024) :element-type '(unsigned-byte 8) :initial-element 32)))
    (is (< (- (get-internal-real-time) start) (/ internal-time-units-per-second 2)) "oversize rejection must be O(1)")))

(deftest json-encoder-defenses (:suite "attack")
  ;; the encoder must never emit something a browser could misinterpret
  (let ((out (json-encode "</script><script>alert(1)</script>")))
    (is-false (search "</script>" out) "closing script tag must be escaped: ~A" out)
    (is-false (search "<script>" out)))
  (let ((out (json-encode (format nil "a~Cb~Cc" (code-char #x2028) (code-char #x2029)))))
    (is (search "\\u2028" out))
    (is (search "\\u2029" out)))
  (let ((out (json-encode (format nil "~C~C~C" (code-char 0) (code-char 1) (code-char 127)))))
    (is (search "\\u0000" out))
    (is (search "\\u0001" out)))
  (signals json-encode-error (json-encode (string (code-char #xD800))))
  (signals json-encode-error (json-encode sb-ext:double-float-positive-infinity))
  (signals json-encode-error (json-encode (- sb-ext:double-float-positive-infinity)))
  ;; cyclic structures must not hang the encoder
  (let ((cyc (make-json-object)))
    (json-set cyc "self" cyc)
    (signals error (json-encode cyc)))
  (let ((v (vector nil)))
    (setf (aref v 0) v)
    (signals error (json-encode v)))
  ;; the encoder output is always re-parseable
  (dolist (sample (list "\\\"/\b\f\n\r\t" (string (code-char #x1F600)) (repeat-string "é" 100)
                        1/3 -0.0d0 1d300 123456789012345678901234567890))
    (finishes (json-parse (json-encode sample)))))

(deftest json-fuzz-never-crashes (:suite "attack")
  "Random mutations of valid documents may only ever produce json-parse-error."
  (let* ((*random-state* (sb-ext:seed-random-state 1234))
         (seeds (list "{\"a\":[1,2.5,-3e2,true,false,null,\"s\\n\\u00e9\"],\"b\":{\"c\":{}}}"
                      "[[[[]]],{\"k\":\"\\ud83d\\ude00\"},9007199254740993,1e-7]"
                      "\"plain string with \\\"quotes\\\" and \\\\ backslashes\""
                      "{\"__proto__\":{\"polluted\":true},\"x\":[null]}"))
         (alphabet (string-octets "{}[]\":,\\0123456789.eE+-ntrufals \\u\"/"))
         (bad 0))
    (dotimes (i 3000)
      (let* ((seed (string-octets (nth (random (length seeds)) seeds)))
             (buf (copy-seq seed)))
        ;; apply 1-4 mutations: byte flip, insert, delete, truncate, splice
        (dotimes (m (1+ (random 4)))
          (when (plusp (length buf))
            (let ((pos (random (length buf))))
              (ecase (random 5)
                (0 (setf (aref buf pos) (aref alphabet (random (length alphabet)))))
                (1 (setf (aref buf pos) (random 256)))
                (2 (setf buf (concat-octets (subseq buf 0 pos) (octets (random 256)) (subseq buf pos))))
                (3 (setf buf (concat-octets (subseq buf 0 pos) (subseq buf (min (length buf) (1+ pos))))))
                (4 (setf buf (subseq buf 0 pos)))))))
        (handler-case (progn (json-parse buf) nil)
          (json-parse-error () nil)
          (error (e) (incf bad) (when (< bad 5) (format t "~&    fuzz case ~D: ~A -> ~A ~A~%" i buf (type-of e) e))))))
    (is (zerop bad) "~D fuzz inputs escaped with a non-designated condition" bad)))

(deftest json-roundtrip-fuzz (:suite "attack")
  "Random generated values must encode and parse back to an equal value."
  (let ((*random-state* (sb-ext:seed-random-state 99)) (bad 0))
    (labels ((gen (depth)
               (case (random (if (> depth 4) 5 8))
                 (0 :null) (1 :true) (2 :false)
                 (3 (- (random 2000000) 1000000))
                 (4 (let ((s (make-string (random 12))))
                      (dotimes (i (length s) s)
                        (setf (char s i) (code-char (case (random 4)
                                                      (0 (random 128))
                                                      (1 (+ 128 (random 2000)))
                                                      (2 (+ #x10000 (random 1000)))
                                                      (3 (char-code #\a))))))))
                 (5 (* (random 1000) 0.125d0))
                 (6 (let ((v (make-array (random 4)))) (dotimes (i (length v) v) (setf (aref v i) (gen (1+ depth))))))
                 (7 (let ((o (make-json-object)))
                      (dotimes (i (random 4) o)
                        (json-set o (format nil "k~D" i) (gen (1+ depth))))))))
             (same (a b)
               (cond ((and (json-object-p a) (json-object-p b))
                      (and (= (json-object-count a) (json-object-count b))
                           (every (lambda (k) (same (json-get a k) (json-get b k))) (json-keys a))))
                     ((and (vectorp a) (not (stringp a)) (vectorp b) (not (stringp b)))
                      (and (= (length a) (length b)) (every #'same a b)))
                     ((and (numberp a) (numberp b)) (= a b)) ; integral doubles encode as integers
                     (t (equal a b)))))
      (dotimes (i 500)
        (let ((v (gen 0)))
          (handler-case
              (unless (same v (json-parse (json-encode v)))
                (incf bad) (when (< bad 5) (format t "~&    roundtrip mismatch: ~S~%" v)))
            (error (e) (incf bad) (when (< bad 5) (format t "~&    roundtrip error for ~S: ~A~%" v e))))))
      (is (zerop bad) "~D roundtrip failures" bad))))

;;; ================================================================== JSON-RPC dispatcher

(deftest rpc-envelope-attacks (:suite "attack")
  (flet ((invalid (text &optional data-fragment)
           (let ((r (rpc text)))
             (is (eql +invalid-request+ (err-code r)) "~A should be Invalid Request, got ~S" text (and r (json-encode r)))
             (when (and data-fragment r (stringp (err-data r)))
               (is (search data-fragment (err-data r)) "expected ~S in ~S" data-fragment (err-data r))))))
    ;; wrong/missing version
    (invalid "{\"method\":\"whoami\",\"id\":1}" "jsonrpc")
    (invalid "{\"jsonrpc\":\"1.0\",\"method\":\"whoami\",\"id\":1}" "jsonrpc")
    (invalid "{\"jsonrpc\":2.0,\"method\":\"whoami\",\"id\":1}" "jsonrpc")
    (invalid "{\"jsonrpc\":\"2.0 \",\"method\":\"whoami\",\"id\":1}")
    ;; unexpected members (smuggling data past the schema)
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1,\"__proto__\":{}}" "unexpected")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1,\"extra\":1}" "unexpected")
    ;; non-object envelopes
    (invalid "42") (invalid "\"whoami\"") (invalid "null") (invalid "true")
    ;; method name abuse
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":42,\"id\":1}" "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"\",\"id\":1}" "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"who ami\",\"id\":1}" "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\\n\",\"id\":1}" "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoamí\",\"id\":1}" "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"../../etc/passwd\",\"id\":1}" "method")
    (invalid (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"~A\",\"id\":1}" (repeat-string "a" 300)) "method")
    (invalid "{\"jsonrpc\":\"2.0\",\"id\":1}" "method")
    ;; id abuse
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":{}}" "id")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":[1]}" "id")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":true}" "id")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1.5}" "id")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":99999999999999999999}" "id")
    (invalid (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":\"~A\"}" (repeat-string "x" 300)) "id")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":\"a\\u0000b\"}" "id")
    ;; params type confusion
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"params\":1,\"id\":1}" "params")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"params\":\"x\",\"id\":1}" "params")
    (invalid "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"params\":null,\"id\":1}" "params")))

(deftest rpc-reserved-and-internal-names (:suite "attack")
  ;; rpc.* is reserved by the spec; internal lisp functions must not be reachable
  (dolist (name '("rpc.discover" "rpc.internal" "is-admin-p" "IS-ADMIN-P" "cl:eval" "eval"
                  "monolisp.rpc::handle-rpc-sync" "add-numbers.evil" "ADD-NUMBERS" "Add-Numbers" "add_numbers"))
    (let ((r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":~A,\"id\":1}" (json-encode name)))))
      (is (member (err-code r) (list +method-not-found+ +invalid-request+))
          "~A must not be callable (got ~S)" name (and r (json-encode r))))))

(deftest rpc-param-boundary-attacks (:suite "attack")
  (flet ((bad-params (params)
           (let ((r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":~A,\"id\":1}" params))))
             (is (eql +invalid-params+ (err-code r)) "params ~A should be rejected, got ~S" params (and r (json-encode r))))))
    (bad-params "[\"1\",2]")                      ; string where number expected
    (bad-params "[1.5,2]")                        ; float where fixnum expected
    (bad-params "[1e3,2]")                        ; float syntax for integer
    (bad-params "[true,2]")
    (bad-params "[null,2]")
    (bad-params "[[1],2]")
    (bad-params "[1]")                            ; too few
    (bad-params "[1,2,3,4]")                      ; too many
    (bad-params "[99999999999999999999,1]")       ; beyond fixnum
    (bad-params "{\"a\":1}")                      ; missing required named param
    (bad-params "{\"a\":1,\"b\":2,\"zzz\":3}")    ; unknown named param
    (bad-params "{\"a\":1,\"b\":2,\"scale\":\"x\"}")
    (bad-params "{\"A\":1,\"B\":2}"))             ; case must match exactly
  ;; strings have a declared max length
  (let ((r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"greet\",\"params\":[\"~A\"],\"id\":1}" (repeat-string "a" 65)))))
    (is (eql +invalid-params+ (err-code r))))
  (finishes (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"greet\",\"params\":[\"~A\"],\"id\":1}" (repeat-string "a" 64))))
  ;; arrays of ints: every element is validated, not just the first
  (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"async-sum\",\"params\":[[1,2,\"3\"]],\"id\":1}")))
    (is (eql +invalid-params+ (err-code r))))
  ;; error payloads never echo the offending value back (no reflected content)
  (let* ((marker "REFLECTED-MARKER-9f8e7d")
         (r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[\"~A\",2],\"id\":1}" marker))))
    (is-false (search marker (json-encode r)) "error response must not reflect input: ~A" (json-encode r))))

(deftest rpc-internal-errors-never-leak (:suite "attack")
  (let ((*debug-mode* nil))
    (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"explode\",\"id\":1}")))
      (is (eql +internal-error+ (err-code r)))
      (let ((text (json-encode r)))
        (is-false (search "hunter2" text))
        (is-false (search "database" text))
        (is-false (search "secret" text))))
    ;; the same for wrong return values (programmer error) -- no stack traces or type names
    (let ((text (json-encode (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"bad-return\",\"id\":1}"))))
      (is (search "-32603" text))
      (is-false (search "SIMPLE-" text))
      (is-false (search "not a number" text)))))

(deftest rpc-batch-attacks (:suite "attack")
  (let ((r (rpc "[]")))
    (is (eql +invalid-request+ (err-code r))))
  ;; batch over the cap is rejected as a whole before any element executes
  (let* ((limits (make-rpc-limits :max-batch 3))
         (r (rpc (format nil "[~{~A~^,~}]"
                         (loop for i below 4 collect (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"notify-sink\",\"params\":[~D],\"id\":~D}" (+ 1000 i) i)))
                 :limits limits)))
    (is (eql +invalid-request+ (err-code r)))
    (is (search "batch" (or (err-data r) ""))))
  ;; invalid elements inside a batch are reported individually; valid ones still run
  (let ((r (rpc "[1,{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[1,2],\"id\":7},\"x\"]")))
    (is (vectorp r))
    (is-equal 3 (length r))
    (is (find 3 r :key (lambda (x) (json-get x "result"))))
    (is-equal 2 (count +invalid-request+ r :key #'err-code)))
  ;; a batch of nested batches is not a thing
  (let ((r (rpc "[[{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1}]]")))
    (is (vectorp r))
    (is (eql +invalid-request+ (err-code (aref r 0))))))

(deftest rpc-payload-limits (:suite "attack")
  (let* ((limits (make-rpc-limits :max-payload-bytes 200))
         (r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"greet\",\"params\":[\"~A\"],\"id\":1}" (repeat-string "a" 300))
                 :limits limits)))
    (is (eql +invalid-request+ (err-code r)))
    (is (search "large" (or (err-data r) ""))))
  ;; a parse error carries no id and no fragment of the payload
  (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1,SECRETJUNK")))
    (is (eql +parse-error+ (err-code r)))
    (is (json-null-p (json-get r "id")))
    (is-false (search "SECRETJUNK" (json-encode r))))
  ;; deeply nested params are stopped by the rpc json limits (depth 32) not the default 64
  (let ((r (rpc (format nil "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"params\":~A~A,\"id\":1}" (repeat-string "[" 40) (repeat-string "]" 40)))))
    (is (eql +parse-error+ (err-code r)))))

(deftest rpc-auth-cannot-be-bypassed (:suite "attack")
  (let ((*authenticator* nil))
    ;; anonymous context -> unauthorized, and the id is echoed so the client can correlate
    (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":\"abc\"}")))
      (is (eql +unauthorized+ (err-code r)))
      (is-equal "abc" (json-get r "id")))
    ;; principal smuggled in params or headers is ignored
    (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"params\":{\"principal\":\"admin\"},\"id\":1}")))
      (is (member (err-code r) (list +unauthorized+ +invalid-params+))))
    (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"admin-only\",\"id\":1}"
                  :context (make-rpc-context :headers '(("x-principal" . "admin") ("authorization" . "Bearer admin"))))))
      (is (eql +unauthorized+ (err-code r))))
    ;; a non-admin principal is still refused by the custom predicate
    (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"admin-only\",\"id\":1}" :context (make-rpc-context :principal "bob"))))
      (is (eql +unauthorized+ (err-code r))))
    ;; an authenticator that errors fails closed
    (let ((*authenticator* (lambda (ctx) (declare (ignore ctx)) (error "idp down"))))
      (let ((r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":1}")))
        (is (member (err-code r) (list +unauthorized+ +internal-error+)))
        (is-false (json-has-key-p r "result"))))))

(deftest rpc-timeouts-bound-slow-handlers (:suite "attack")
  (let ((start (get-internal-real-time))
        (r (rpc "{\"jsonrpc\":\"2.0\",\"method\":\"slow-echo\",\"params\":[\"x\",5000],\"id\":1}")))
    (is (eql +timeout+ (err-code r)))
    (is (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))
        "a 5s handler with a 200ms timeout must answer well within 2s")))

(deftest rpc-fuzz-never-crashes (:suite "attack")
  "Mutated envelopes may only yield a JSON-RPC error response (or nothing for notifications)."
  (let* ((*random-state* (sb-ext:seed-random-state 4321))
         (seeds '("{\"jsonrpc\":\"2.0\",\"method\":\"add-numbers\",\"params\":[1,2],\"id\":1}"
                  "{\"jsonrpc\":\"2.0\",\"method\":\"greet\",\"params\":{\"name\":\"x\"},\"id\":\"s\"}"
                  "[{\"jsonrpc\":\"2.0\",\"method\":\"whoami\",\"id\":null},{\"jsonrpc\":\"2.0\",\"method\":\"notify-sink\",\"params\":[1]}]"
                  "{\"jsonrpc\":\"2.0\",\"method\":\"async-sum\",\"params\":[[1,2,3]],\"id\":2}"))
         (tokens '("\"jsonrpc\"" "\"2.0\"" "\"method\"" "\"params\"" "\"id\"" "null" "true" "[" "]" "{" "}" "," ":"
                   "\"add-numbers\"" "\"explode\"" "\"rpc.x\"" "1e400" "-1" "\"\\u0000\"" "99999999999999999999"))
         (bad 0))
    (dotimes (i 400)
      (let ((text (nth (random (length seeds)) seeds)))
        (dotimes (m (1+ (random 3)))
          (let ((pos (random (1+ (length text)))))
            (ecase (random 3)
              (0 (setf text (concatenate 'string (subseq text 0 pos) (nth (random (length tokens)) tokens) (subseq text pos))))
              (1 (setf text (concatenate 'string (subseq text 0 pos) (subseq text (min (length text) (1+ pos))))))
              (2 (setf text (concatenate 'string (subseq text 0 pos) (string (code-char (random 128))) (subseq text pos)))))))
        (handler-case
            (let ((r (rpc text)))
              ;; whatever comes back must itself be valid JSON-RPC 2.0
              (when r
                (flet ((check (one)
                         (unless (and (json-object-p one) (equal (json-get one "jsonrpc") "2.0")
                                      (or (json-has-key-p one "result") (json-has-key-p one "error")))
                           (incf bad) (when (< bad 5) (format t "~&    malformed response for ~S: ~A~%" text (json-encode one))))))
                  (if (vectorp r) (map nil #'check r) (check r)))))
          (error (e) (incf bad) (when (< bad 5) (format t "~&    rpc fuzz ~D: ~S -> ~A ~A~%" i text (type-of e) e))))))
    (is (zerop bad) "~D fuzz cases produced crashes or malformed responses" bad)))

;;; ================================================================== HTTP parser

(deftest http-request-smuggling-vectors (:suite "attack")
  ;; CL + TE together: must never be accepted (501 because TE is unsupported entirely)
  (is-equal 501 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5" "Transfer-Encoding: chunked")))
  (is-equal 501 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Transfer-Encoding: chunked")))
  (is (member (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Transfer-Encoding : chunked")) '(400 501)))
  (is-equal 501 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "transfer-encoding: identity")))
  (is-equal 501 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Transfer-Encoding: xchunked")))
  ;; duplicate / conflicting / malformed Content-Length
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5" "Content-Length: 6")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5, 6")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5 6")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: +5")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: -1")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 0x10")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5abc")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: ")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 99999999999999999999999")))
  ;; two identical Content-Length headers are tolerated by RFC 9110 but we refuse ambiguity
  (is (member (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 5" "Content-Length: 5")) '(400 nil)))
  ;; obsolete line folding (header continuation) is an attack surface -> 400
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "X-A: 1" " continued")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "X-A: 1" "	tabbed"))))

(deftest http-header-injection-and-syntax (:suite "attack")
  ;; whitespace before the colon enables desync between proxies
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host : h")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "X-Bad Name: 1")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" ": novalue")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "NoColon")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" (format nil "X-A: a~Cb" (code-char 0)))))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" (format nil "X-A: a~Cb" (code-char 127)))))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" (format nil "X-~C: 1" (code-char 1)))))
  ;; non-ASCII header names / values
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" (format nil "X-~C: 1" (code-char 200)))))
  ;; bare CR / LF inside the head
  (is-equal 400 (http-status (string-octets (format nil "POST /rpc HTTP/1.1~CHost: h~C~C~C~C" #\Newline #\Return #\Newline #\Return #\Newline))))
  (is-equal 400 (http-status (string-octets (format nil "POST /rpc HTTP/1.1~CHost: h~C~C~C~C" #\Return #\Return #\Newline #\Return #\Newline))))
  ;; Host header rules: required exactly once
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: a" "Host: b")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: a b")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1" "Host: a/b"))))

(deftest http-request-line-attacks (:suite "attack")
  (is-equal 400 (http-status (crlf "POST  /rpc HTTP/1.1" "Host: h")))            ; double space
  (is-equal 400 (http-status (crlf "POST /rpc  HTTP/1.1" "Host: h")))
  (is-equal 400 (http-status (crlf " POST /rpc HTTP/1.1" "Host: h")))
  (is-equal 400 (http-status (crlf "POST /rpc HTTP/1.1 " "Host: h")))
  (is-equal 400 (http-status (crlf "POST	/rpc	HTTP/1.1" "Host: h")))             ; tabs as separators
  (is-equal 400 (http-status (crlf "POST /rpc" "Host: h")))                         ; HTTP/0.9 style
  (is-equal 400 (http-status (crlf "POST /rpc http/1.1" "Host: h")))
  (is (member (http-status (crlf "POST /rpc HTTP/1.1.1" "Host: h")) '(400 505)))
  (is-equal 505 (http-status (crlf "POST /rpc HTTP/2.0" "Host: h")))
  (is-equal 505 (http-status (crlf "POST /rpc HTTP/9.9" "Host: h")))
  ;; unknown / lowercase methods are syntactically valid tokens; the router answers 405 (integration tests)
  (let ((head (crlf "P0ST /rpc HTTP/1.1" "Host: h")))
    (is-equal "P0ST" (request-method (parse-request-head head (length head)))))
  (let ((head (crlf "post /rpc HTTP/1.1" "Host: h")))
    (is-equal "post" (request-method (parse-request-head head (length head)))))
  (is-equal 400 (http-status (crlf "POST rpc HTTP/1.1" "Host: h")))                 ; no leading slash
  (is-equal 400 (http-status (crlf "POST http://evil/rpc HTTP/1.1" "Host: h")))     ; absolute-form
  (is-equal 400 (http-status (crlf "POST /rpc?x=\"y HTTP/1.1" "Host: h")))         ; illegal target char
  (is-equal 400 (http-status (crlf "POST /r pc HTTP/1.1" "Host: h")))
  (is-equal 400 (http-status (crlf "POST /rpc# HTTP/1.1" "Host: h")))
  (is-equal 400 (http-status (crlf "POST /rpc\\x HTTP/1.1" "Host: h")))
  (is-equal 400 (http-status (crlf (format nil "POST /rpc~C HTTP/1.1" (code-char 0)) "Host: h")))
  (is-equal 400 (http-status (crlf (format nil "POST /rp~Cc HTTP/1.1" (code-char 233)) "Host: h")))
  ;; method length bomb
  (is (member (http-status (crlf (format nil "~A /rpc HTTP/1.1" (repeat-string "A" 100)) "Host: h")) '(400 414 501)))
  ;; percent-encoding is accepted syntactically; the router decides later
  (is-false (http-status (crlf "POST /rpc%2F.. HTTP/1.1" "Host: h"))))

(deftest http-size-floods (:suite "attack")
  ;; header count flood
  (is-equal 431 (http-status (apply #'crlf "POST /rpc HTTP/1.1" "Host: h"
                                    (loop for i below 200 collect (format nil "X-H~D: v" i)))))
  ;; single enormous header
  (is-equal 431 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" (format nil "X-A: ~A" (repeat-string "v" 9000)))))
  ;; total header bytes
  (is-equal 431 (http-status (apply #'crlf "POST /rpc HTTP/1.1" "Host: h"
                                    (loop for i below 60 collect (format nil "X-H~D: ~A" i (repeat-string "v" 400))))))
  ;; huge URL
  (is-equal 414 (http-status (crlf (format nil "POST /~A HTTP/1.1" (repeat-string "a" 3000)) "Host: h")))
  ;; a head that never ends: incomplete up to the cap, then 431
  (let ((partial (string-octets (format nil "POST /rpc HTTP/1.1~C~CHost: h~C~CX: ~A" #\Return #\Newline #\Return #\Newline (repeat-string "y" 100)))))
    (is (eq :incomplete (parse-request-head partial (length partial)))))
  (let ((partial (string-octets (format nil "POST /rpc HTTP/1.1~C~CHost: h~C~CX: ~A" #\Return #\Newline #\Return #\Newline (repeat-string "y" 20000)))))
    (is-equal 431 (http-status partial)))
  ;; body larger than the max is refused at header time, before any body byte is read
  (is-equal 413 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 1048577")))
  (is-equal 413 (http-status (crlf "POST /rpc HTTP/1.1" "Host: h" "Content-Length: 10")
                             :limits (make-http-limits :max-body 9))))

(deftest http-fuzz-never-crashes (:suite "attack")
  (let* ((*random-state* (sb-ext:seed-random-state 777))
         (seed (format nil "POST /rpc HTTP/1.1~C~CHost: localhost:8080~C~CContent-Type: application/json~C~CContent-Length: 17~C~CConnection: keep-alive~C~C~C~C"
                       #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (bad 0))
    (dotimes (i 3000)
      (let ((buf (string-octets seed)))
        (dotimes (m (1+ (random 4)))
          (when (plusp (length buf))
            (let ((pos (random (length buf))))
              (ecase (random 5)
                (0 (setf (aref buf pos) (random 256)))
                (1 (setf (aref buf pos) (aref (octets 13 10 32 58 9 0 127) (random 7))))
                (2 (setf buf (concat-octets (subseq buf 0 pos) (octets (random 256)) (subseq buf pos))))
                (3 (setf buf (concat-octets (subseq buf 0 pos) (subseq buf (min (length buf) (1+ pos))))))
                (4 (setf buf (subseq buf 0 pos)))))))
        (handler-case (parse-request-head buf (length buf))
          (http-parse-error () nil)
          (error (e) (incf bad) (when (< bad 5) (format t "~&    http fuzz ~D: ~A -> ~A ~A~%" i buf (type-of e) e))))))
    (is (zerop bad) "~D fuzz inputs escaped the http parser with a non-designated condition" bad)))

;;; ================================================================== server-side type schema

(deftest schema-validation-resists-confusion (:suite "attack")
  ;; strings vs numbers vs booleans never coerce
  (signals schema-error (validate-value 'fixnum "1"))
  (signals schema-error (validate-value 'fixnum 1.0d0))
  (signals schema-error (validate-value 'fixnum :true))
  (signals schema-error (validate-value 'string 1))
  (signals schema-error (validate-value 'boolean 1))
  (signals schema-error (validate-value 'boolean "true"))
  (signals schema-error (validate-value 'boolean :null))
  ;; null is not an empty object or array
  (signals schema-error (validate-value '(array fixnum) :null))
  (signals schema-error (validate-value '(object (a fixnum)) :null))
  (signals schema-error (validate-value '(object (a fixnum)) (vector)))
  (signals schema-error (validate-value '(array fixnum) (make-json-object)))
  ;; unknown object keys are refused (prevents mass-assignment)
  (signals schema-error (validate-value '(object (a fixnum)) (make-json-object "a" 1 "b" 2)))
  (signals schema-error (validate-value '(object (a fixnum)) (make-json-object "a" 1 "__proto__" 2)))
  ;; bounds are enforced on every element, deep inside
  (signals schema-error (validate-value '(array (string 2)) (vector "ab" "abc")))
  (signals schema-error (validate-value '(array (array fixnum)) (vector (vector 1) (vector "x"))))
  (signals schema-error (validate-value '(integer 0 10) 11))
  (signals schema-error (validate-value '(integer 0 10) -1))
  (signals schema-error (validate-value '(member "a" "b") "c"))
  (signals schema-error (validate-value '(member "a" "b") "A"))
  ;; huge inputs where a scalar is expected are rejected without being walked
  (let ((big (make-array 100000 :initial-element 1)))
    (signals schema-error (validate-value 'fixnum big))))

;;; ================================================================== transpiler reader

(deftest transpiler-reader-attacks (:suite "attack")
  (flet ((rejected (src)
           (is (handler-case (progn (monolisp.transpiler:transpile-string src :name "t") nil)
                 (monolisp.transpiler:transpile-error () t)
                 (error (e) (fail "~S: ~A ~A escaped the transpiler" src (type-of e) e) nil))
               "~S must be rejected cleanly" src)))
    ;; code execution at read time
    (rejected "(defvar x #.(sb-ext:run-program \"calc\" nil))")
    (rejected "(defvar x #.(+ 1 2))")
    ;; the standard dispatch macros that reach into the host image
    (rejected "(defvar x #'cl:eval)")
    (rejected "(eval '(+ 1 2))")
    (rejected "(defvar x (cl:load \"foo\"))")
    (rejected "(defvar x #p\"/etc/passwd\")")
    (rejected "(defvar x #+sbcl (sb-posix:getenv \"HOME\") #-sbcl 1)")
    ;; package traversal must not reach the host
    (rejected "(cl-user::run-shell-command \"rm -rf /\")")
    (rejected "(sb-ext:quit)")
    (rejected "(asdf:load-system :monolisp)")
    ;; reader bombs / malformed input
    (rejected (repeat-string "(" 20000))
    (rejected (repeat-string ")" 100))
    (rejected "(defvar x \"unterminated")
    (rejected "(defvar x #1=(a . #1#))")
    (rejected "(defvar x '(a . b))")
    (rejected "(defvar x #c(1 2))")
    ;; a user macro cannot escape into the host image either
    (rejected "(defmacro evil () (sb-ext:run-program \"calc\" nil) 1) (evil)")
    (rejected "(defmacro evil () (eval '(+ 1 2))) (evil)")
    (rejected "(defmacro evil () `(progn (setf a 1) ,(eval '(+ 1 2)))) (evil)")
    (rejected "(defmacro evil () `(progn ,@(list `(a ,(delete-file \"x\"))))) (evil)")
    (rejected "(defmacro evil () `(progn ,(sb-ext:posix-getenv \"PATH\"))) (evil)")
    (rejected "(defmacro evil () (funcall (intern \"QUIT\" \"SB-EXT\"))) (evil)")
    (rejected "(defmacro evil () (delete-file \"important.txt\")) (evil)")
    (rejected "(defmacro evil () (with-open-file (s \"x\" :direction :output) 1)) (evil)")
    (rejected "(defmacro evil () (setf (symbol-function 'car) #'cdr) 1) (evil)")
    (rejected "(defmacro evil () (defun car (x) x) 1) (evil)")
    (rejected "(defmacro evil () (intern \"QUIT\" \"SB-EXT\")) (evil)")
    ;; the expansion of a macro is checked too, not just its definition
    (rejected "(defmacro sneaky () (list (intern \"QUIT\" (find-package \"SB-EXT\")))) (sneaky)"))
  ;; well-behaved macros keep working
  (finishes (monolisp.transpiler:transpile-string "(defmacro twice (f) `(progn ,f ,f)) (twice (console-log 1))" :name "t"))
  (finishes (monolisp.transpiler:transpile-string
             "(defmacro with-log ((name) &body body) (let ((g (gensym))) `(let ((,g ,name)) (console-log ,g) ,@body))) (with-log (\"x\") 1)" :name "t")))

(deftest transpiler-output-is-safe-text (:suite "attack")
  ;; string literals cannot break out of the generated JS/TS
  (let ((out (monolisp.transpiler:transpile-string
              (format nil "(defvar s ~S)" (format nil "</script>~C~C${x}`'\"\\" (code-char #x2028) (code-char #x2029)))
              :name "t")))
    (is (search "\\u2028" out))
    (is (search "\\u2029" out))
    (is-false (search (string (code-char #x2028)) out))
    (is (search "\\\"" out))
    (is (search "\\\\" out)))
  ;; template literals escape ${ and backticks coming from user strings
  (let ((out (monolisp.transpiler:transpile-string "(defvar s (concat \"a`b${c}\" x))" :name "t")))
    (is-false (search "`a`b" out))
    (is (search "\\${c}" out) "interpolation must be escaped: ~A" out)
    (is-false (search "}${c}" out)))
  ;; identifiers are always valid JS identifiers (no injection through symbol names)
  (let ((out (monolisp.transpiler:transpile-string "(defvar |a;alert(1);//| 1)" :name "t")))
    (is-false (search "alert(1)" out) "symbol name must be mangled: ~A" out))
  ;; JSX text content is escaped
  (let ((out (monolisp.transpiler:transpile-string "(defun f () (as-html-string (:div \"<script>x</script>\")))" :name "t" :tsx-p t)))
    (is-false (search "<script>x</script></div>" out))))
