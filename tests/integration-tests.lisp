;;;; tests/integration-tests.lisp -- the whole stack over a real TCP socket
;;;;
;;;; Uses the API defined in rpc-tests.lisp (add-numbers, greet, whoami, explode, ...).

(defpackage :monolisp.test.integration
  (:use :cl :monolisp.test :monolisp.json :monolisp.rpc :monolisp.async :monolisp.http))

(in-package :monolisp.test.integration)

(defvar *port* nil)

(defmacro with-test-server ((&rest server-args) &body body)
  "Run BODY (which may AWAIT) against a freshly started server on an ephemeral port."
  `(with-event-loop (:run nil)
     (let ((*error-logger* (lambda (c ctx) (declare (ignore c ctx)) nil))
           (server (start-server ,@server-args :port 0 :rate-limiter nil)))
       (let ((*port* (server-port server)))
         (unwind-protect
              (promise-wait (async ,@body) :timeout 20)
           (stop-server server))))))

(defun url (&optional (path "/rpc"))
  (format nil "http://127.0.0.1:~D~A" *port* path))

(define-async post-json (text &key (path "/rpc") headers)
  (await (fetch (url path) :method "POST" :body text
                           :headers (append headers '(("Content-Type" . "application/json"))))))

(defun rpc-body (method params &optional (id 1))
  (json-encode (make-json-object "jsonrpc" "2.0" "method" method "params" params "id" id)))

(defun header (response name)
  (cdr (assoc name (http-response-headers response) :test #'string=)))

(define-async raw-exchange (octets &key (read-until-close t))
  "Write OCTETS to the server and return everything it sends back as a string."
  (let ((conn (await (tcp-connect "127.0.0.1" *port*)))
        (acc (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (unwind-protect
         (progn
           (conn-set-timeout conn 3000)
           (await (conn-write conn (if (stringp octets) (string-to-octets octets) octets)))
           (loop
             (let ((chunk (handler-case (await (conn-read conn)) (timeout-error () nil))))
               (when (null chunk) (return))
               (loop for b across chunk do (vector-push-extend b acc))
               (unless read-until-close (return))))
           (sb-ext:octets-to-string (coerce acc '(simple-array (unsigned-byte 8) (*))) :external-format :latin-1))
      (conn-close conn))))

(defun crlf (&rest lines)
  (with-output-to-string (s)
    (dolist (l lines) (write-string l s) (write-char #\Return s) (write-char #\Linefeed s))
    (write-char #\Return s) (write-char #\Linefeed s)))

(defun response-status (text)
  (and (>= (length text) 12) (string= "HTTP/1.1 " text :end2 9) (parse-integer text :start 9 :end 12)))

;;; ------------------------------------------------------------------ happy paths

(deftest e2e-call-and-result (:suite "integration")
  (with-test-server ()
    (let ((r (await (post-json (rpc-body "add-numbers" #(2 3))))))
      (is-equal 200 (http-response-status r))
      (is-equal "application/json; charset=utf-8" (header r "content-type"))
      (is-equal "nosniff" (header r "x-content-type-options"))
      (is-equal "no-store" (header r "cache-control"))
      (is-false (header r "server"))
      (let ((j (json-parse (http-response-text r))))
        (is-equal 5 (json-get j "result"))
        (is-equal 1 (json-get j "id"))))))

(deftest e2e-struct-result (:suite "integration")
  (with-test-server ()
    (let* ((r (await (post-json (rpc-body "greet" (make-json-object "name" "Ada")))))
           (j (json-parse (http-response-text r))))
      (is-equal "Hello, Ada!" (json-ref j "result" "text"))
      (is-equal 3 (json-ref j "result" "length")))))

(deftest e2e-notification-is-204 (:suite "integration")
  (with-test-server ()
    (let ((r (await (post-json (json-encode (make-json-object "jsonrpc" "2.0" "method" "notify-sink"
                                                              "params" #(42)))))))
      (is-equal 204 (http-response-status r))
      (is-equal 0 (length (http-response-body r))))))

(deftest e2e-batch (:suite "integration")
  (with-test-server ()
    (let* ((batch (json-encode (vector (make-json-object "jsonrpc" "2.0" "method" "add-numbers" "params" #(1 1) "id" "a")
                                       (make-json-object "jsonrpc" "2.0" "method" "nope" "id" "b"))))
           (r (await (post-json batch)))
           (j (json-parse (http-response-text r))))
      (is-equal 200 (http-response-status r))
      (is-equal 2 (length j))
      (is-equal 2 (json-get (aref j 0) "result"))
      (is-equal -32601 (json-ref (aref j 1) "error" "code")))))

(deftest e2e-health (:suite "integration")
  (with-test-server ()
    (let ((r (await (fetch (url "/health")))))
      (is-equal 200 (http-response-status r))
      (is-equal "ok" (json-get (json-parse (http-response-text r)) "status")))))

(deftest e2e-internal-error-is-opaque (:suite "integration")
  (with-test-server ()
    (let* ((r (await (post-json (rpc-body "explode" #()))))
           (text (http-response-text r)))
      (is-equal 200 (http-response-status r))
      (is-equal -32603 (json-ref (json-parse text) "error" "code"))
      (is-false (search "hunter2" text))
      (is-false (search "database" text)))))

(deftest e2e-concurrent-requests (:suite "integration")
  (with-test-server ()
    (let* ((promises (loop for i from 0 below 20
                           collect (post-json (rpc-body "add-numbers" (vector i i) i))))
           (responses (await (all promises))))
      (loop for r in responses
            for i from 0
            do (let ((j (json-parse (http-response-text r))))
                 (is-equal (* 2 i) (json-get j "result"))
                 (is-equal i (json-get j "id")))))))

(deftest e2e-keep-alive-pipelining (:suite "integration")
  (with-test-server ()
    (let* ((b1 (rpc-body "add-numbers" #(1 2) 1))
           (b2 (rpc-body "add-numbers" #(3 4) 2))
           (text (await (raw-exchange
                         (concatenate 'string
                                      (crlf "POST /rpc HTTP/1.1" "Host: t" "Content-Type: application/json"
                                            (format nil "Content-Length: ~D" (length b1)))
                                      b1
                                      (crlf "POST /rpc HTTP/1.1" "Host: t" "Content-Type: application/json"
                                            "Connection: close"
                                            (format nil "Content-Length: ~D" (length b2)))
                                      b2)))))
      (is-equal 2 (loop with start = 0 for pos = (search "HTTP/1.1 200" text :start2 start)
                        while pos count t do (setf start (1+ pos))))
      (is (search "\"result\":3" text))
      (is (search "\"result\":7" text))
      (is (search "Connection: keep-alive" text))
      (is (search "Connection: close" text)))))

;;; ------------------------------------------------------------------ rejections

(deftest e2e-wrong-content-type-415 (:suite "integration")
  (with-test-server ()
    (let ((r (await (fetch (url) :method "POST" :body (rpc-body "add-numbers" #(1 2))
                                 :headers '(("Content-Type" . "text/plain"))))))
      (is-equal 415 (http-response-status r)))
    ;; form-encoded (what a CSRF-ing <form> can send) is rejected too
    (let ((r (await (fetch (url) :method "POST" :body (rpc-body "add-numbers" #(1 2))
                                 :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))))
      (is-equal 415 (http-response-status r)))
    (let ((r (await (fetch (url) :method "POST" :body (rpc-body "add-numbers" #(1 2))
                                 :headers '(("Content-Type" . "text/plain; application/json"))))))
      (is-equal 415 (http-response-status r)))))

(deftest e2e-get-on-rpc-405 (:suite "integration")
  (with-test-server ()
    (let ((r (await (fetch (url)))))
      (is-equal 405 (http-response-status r))
      (is-equal "POST, OPTIONS" (header r "allow")))))

(deftest e2e-unknown-path-404 (:suite "integration")
  (with-test-server ()
    (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :path "/admin"))))
      (is-equal 404 (http-response-status r)))
    (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :path "/rpc/"))))
      (is-equal 404 (http-response-status r)))
    (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :path "/rpc?x=1"))))
      (is-equal 404 (http-response-status r)))))

(deftest e2e-missing-content-length-411 (:suite "integration")
  (with-test-server ()
    (let ((text (await (raw-exchange (crlf "POST /rpc HTTP/1.1" "Host: t" "Content-Type: application/json")))))
      (is-equal 411 (response-status text)))))

(deftest e2e-oversized-body-413 (:suite "integration")
  (with-test-server (:rpc-limits (make-rpc-limits :max-payload-bytes 100))
    (let ((r (await (post-json (rpc-body "greet" (make-json-object "name" (make-string 200 :initial-element #\a)))))))
      (is-equal 413 (http-response-status r)))))

(deftest e2e-malformed-request-line-400 (:suite "integration")
  (with-test-server ()
    (let ((text (await (raw-exchange (format nil "GARBAGE~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed)))))
      (is-equal 400 (response-status text)))))

(deftest e2e-chunked-smuggling-501 (:suite "integration")
  (with-test-server ()
    (let ((text (await (raw-exchange (crlf "POST /rpc HTTP/1.1" "Host: t" "Content-Type: application/json"
                                           "Content-Length: 4" "Transfer-Encoding: chunked")))))
      (is-equal 501 (response-status text)))))

(deftest e2e-invalid-json-is-parse-error (:suite "integration")
  (with-test-server ()
    (let* ((r (await (post-json "{\"jsonrpc\": \"2.0\", \"method\": ")))
           (j (json-parse (http-response-text r))))
      (is-equal 200 (http-response-status r))
      (is-equal -32700 (json-ref j "error" "code")))))

(deftest e2e-header-timeout-408 (:suite "integration")
  (with-test-server (:header-timeout-ms 150)
    (let ((text (await (raw-exchange (format nil "POST /rpc HTTP/1.1~C~CHost: t~C~C" #\Return #\Linefeed #\Return #\Linefeed)))))
      (is-equal 408 (response-status text)))))

(deftest e2e-idle-connection-is-dropped (:suite "integration")
  (with-test-server (:header-timeout-ms 150)
    ;; nothing sent at all -> server closes silently
    (let ((text (await (raw-exchange ""))))
      (is-equal "" text))))

(deftest e2e-rate-limit-429 (:suite "integration")
  (with-test-server (:rate-limiter (make-rate-limiter :capacity 3 :refill-per-second 0))
    (let ((statuses (loop repeat 5
                          collect (http-response-status (await (post-json (rpc-body "add-numbers" #(1 2))))))))
      (is-equal '(200 200 200 429 429) statuses))))

(deftest e2e-connection-cap-refuses (:suite "integration")
  (with-test-server (:max-connections 2 :header-timeout-ms 2000)
    (let* ((c1 (await (tcp-connect "127.0.0.1" *port*)))
           (c2 (await (tcp-connect "127.0.0.1" *port*)))
           (c3 (await (tcp-connect "127.0.0.1" *port*))))
      (conn-set-timeout c3 1500)
      ;; the third one gets closed immediately by the server
      (is-equal nil (handler-case (await (conn-read c3)) (timeout-error () :timeout)))
      (conn-close c1) (conn-close c2) (conn-close c3))))

(deftest e2e-request-cap-per-connection (:suite "integration")
  (with-test-server (:max-requests-per-connection 1)
    (let* ((b (rpc-body "add-numbers" #(1 2)))
           (req (concatenate 'string (crlf "POST /rpc HTTP/1.1" "Host: t" "Content-Type: application/json"
                                           (format nil "Content-Length: ~D" (length b))) b))
           (text (await (raw-exchange (concatenate 'string req req)))))
      ;; only one response, with Connection: close
      (is-equal 1 (loop with start = 0 for pos = (search "HTTP/1.1 " text :start2 start)
                        while pos count t do (setf start (1+ pos))))
      (is (search "Connection: close" text)))))

;;; ------------------------------------------------------------------ CORS

(deftest e2e-cors-disabled-rejects-origin (:suite "integration")
  (with-test-server ()
    (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :headers '(("Origin" . "https://evil.example"))))))
      (is-equal 403 (http-response-status r)))
    (let ((r (await (fetch (url) :method "OPTIONS" :headers '(("Origin" . "https://evil.example"))))))
      (is-equal 403 (http-response-status r)))))

(deftest e2e-cors-allow-list (:suite "integration")
  (with-test-server (:cors-origins '("https://app.example"))
    (let ((r (await (fetch (url) :method "OPTIONS"
                                 :headers '(("Origin" . "https://app.example")
                                            ("Access-Control-Request-Method" . "POST"))))))
      (is-equal 204 (http-response-status r))
      (is-equal "https://app.example" (header r "access-control-allow-origin"))
      (is-equal "Origin" (header r "vary")))
    (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :headers '(("Origin" . "https://app.example"))))))
      (is-equal 200 (http-response-status r))
      (is-equal "https://app.example" (header r "access-control-allow-origin")))
    ;; near-misses
    (dolist (bad '("https://app.example.evil" "http://app.example" "https://APP.example" "https://app.example/"))
      (let ((r (await (post-json (rpc-body "add-numbers" #(1 2)) :headers (list (cons "Origin" bad))))))
        (is-true (= 403 (http-response-status r)) "origin ~A should be refused" bad)))))

(deftest e2e-cors-origin-cannot-inject (:suite "integration")
  (with-test-server (:cors-origins :any)
    (let ((text (await (raw-exchange (crlf "OPTIONS /rpc HTTP/1.1" "Host: t" "Origin: https://a.example\"<script>")))))
      (is-equal 403 (response-status text)))))

;;; ------------------------------------------------------------------ auth plumbing

(deftest e2e-authenticator-sees-headers (:suite "integration")
  ;; Bound outside the async body: special bindings do not survive across AWAIT.
  (let ((*authenticator* (lambda (ctx)
                           (let ((auth (context-header ctx "authorization")))
                             (and auth (string= auth "Bearer sekrit") "admin")))))
    (with-test-server ()
      (let* ((r (await (post-json (rpc-body "whoami" #()) :headers '(("Authorization" . "Bearer sekrit")))))
             (j (json-parse (http-response-text r))))
        (is-equal "admin" (json-get j "result")))
      (let* ((r (await (post-json (rpc-body "whoami" #()))))
             (j (json-parse (http-response-text r))))
        (is-equal -32001 (json-ref j "error" "code"))))))

(deftest e2e-server-stats (:suite "integration")
  (with-test-server ()
    (await (post-json (rpc-body "add-numbers" #(1 2))))
    (await (fetch (url) :method "POST" :body "x" :headers '(("Content-Type" . "text/plain"))))
    (let ((stats (server-stats)))
      (is-equal 2 (getf stats :requests))
      (is-equal 1 (getf stats :rejected)))))
