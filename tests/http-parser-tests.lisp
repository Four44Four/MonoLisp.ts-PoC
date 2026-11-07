;;;; tests/http-parser-tests.lisp -- strict HTTP request-head parser

(defpackage :monolisp.test.http
  (:use :cl :monolisp.test :monolisp.http))

(in-package :monolisp.test.http)

(defun crlf (&rest lines)
  "Join LINES with CRLF and terminate the head with a blank line."
  (let ((text (with-output-to-string (s)
                (dolist (l lines) (write-string l s) (write-char #\Return s) (write-char #\Linefeed s))
                (write-char #\Return s) (write-char #\Linefeed s))))
    (sb-ext:string-to-octets text :external-format :latin-1)))

(defun parse (octets &rest args)
  (apply #'parse-request-head octets (length octets) args))

(defun status-of (octets &rest args)
  (handler-case (progn (apply #'parse octets args) nil)
    (http-parse-error (e) (http-parse-error-status e))))

(deftest parses-minimal-post (:suite "http-parser")
  (multiple-value-bind (req len)
      (parse (crlf "POST /rpc HTTP/1.1" "Host: example.com" "Content-Type: application/json" "Content-Length: 12"))
    (is-equal "POST" (request-method req))
    (is-equal "/rpc" (request-target req))
    (is-equal "HTTP/1.1" (request-version req))
    (is-equal 12 (request-content-length req))
    (is-equal "application/json" (request-header req "Content-Type"))
    (is-equal "example.com" (request-header req "host"))
    (is (request-keep-alive-p req))
    (is-equal 93 len)))

(deftest incomplete-head (:suite "http-parser")
  (let ((bytes (sb-ext:string-to-octets (format nil "POST /rpc HTTP/1.1~C~CHost: x~C~C" #\Return #\Linefeed #\Return #\Linefeed))))
    (is-equal :incomplete (parse bytes))))

(deftest head-length-excludes-body (:suite "http-parser")
  (let* ((head (crlf "POST /rpc HTTP/1.1" "Host: x" "Content-Length: 2"))
         (all (concatenate '(vector (unsigned-byte 8)) head (sb-ext:string-to-octets "{}"))))
    (multiple-value-bind (req len) (parse-request-head all (length all))
      (is-equal 2 (request-content-length req))
      (is-equal (length head) len))))

(deftest header-value-trimming-and-case (:suite "http-parser")
  (let ((req (parse (crlf "GET /health HTTP/1.1" "HOST:   spaced.example   " "X-Thing:	tabbed"))))
    (is-equal "spaced.example" (request-header req "host"))
    (is-equal "tabbed" (request-header req "x-thing"))))

(deftest http10-defaults-to-close (:suite "http-parser")
  (let ((req (parse (crlf "GET /health HTTP/1.0"))))
    (is-false (request-keep-alive-p req)))
  (let ((req (parse (crlf "GET /health HTTP/1.0" "Connection: keep-alive"))))
    (is (request-keep-alive-p req)))
  (let ((req (parse (crlf "GET /health HTTP/1.1" "Host: a" "Connection: close"))))
    (is-false (request-keep-alive-p req))))

(deftest rejects-bare-lf (:suite "http-parser")
  (is-equal 400 (status-of (sb-ext:string-to-octets (format nil "GET / HTTP/1.1~CHost: a~C~C" #\Linefeed #\Linefeed #\Linefeed))))
  ;; even before the head completes
  (is-equal 400 (status-of (sb-ext:string-to-octets (format nil "GET / HTTP/1.1~CHost" #\Linefeed)))))

(deftest rejects-bare-cr (:suite "http-parser")
  (is-equal 400 (status-of (sb-ext:string-to-octets (format nil "GET / HTTP/1.1~CHost: a~C~C~C~C" #\Return #\Return #\Linefeed #\Return #\Linefeed)))))

(deftest rejects-malformed-request-lines (:suite "http-parser")
  (is-equal 400 (status-of (crlf "GET" "Host: a")))
  (is-equal 400 (status-of (crlf "GET /" "Host: a")))
  (is-equal 400 (status-of (crlf "GET  / HTTP/1.1" "Host: a")))
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1 extra" "Host: a")))
  (is-equal 400 (status-of (crlf "GET http://evil/ HTTP/1.1" "Host: a")))
  (is-equal 400 (status-of (crlf "GET * HTTP/1.1" "Host: a")))
  (is-equal 400 (status-of (crlf "GET /a#frag HTTP/1.1" "Host: a")))
  (is-equal 400 (status-of (crlf "G<T / HTTP/1.1" "Host: a")))
  (is-equal 400 (status-of (crlf "GET / HTTQ/1.1" "Host: a")))
  (is-equal 505 (status-of (crlf "GET / HTTP/2.0" "Host: a")))
  (is-equal 505 (status-of (crlf "GET / HTTP/0.9" "Host: a")))
  (is-equal 501 (status-of (crlf "AVERYVERYLONGMETHODNAME / HTTP/1.1" "Host: a"))))

(deftest rejects-request-target-with-controls-or-unicode (:suite "http-parser")
  (is-equal 400 (status-of (crlf (format nil "GET /a~Cb HTTP/1.1" (code-char 1)) "Host: a")))
  (is-equal 400 (status-of (crlf (format nil "GET /~A HTTP/1.1" (code-char 233)) "Host: a"))))

(deftest host-header-rules (:suite "http-parser")
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1")))
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" "Host: b")))
  (is-equal nil (status-of (crlf "GET / HTTP/1.0"))))

(deftest header-syntax-rules (:suite "http-parser")
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" "Bad Header: x")))        ; space in name
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" "Bad : x")))               ; space before colon
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" ": x")))                   ; empty name
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" "nocolon")))
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" "X: first" " folded")))     ; obs-fold
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" (format nil "X: ~Cctl" (code-char 7)))))
  (is-equal 400 (status-of (crlf "GET / HTTP/1.1" "Host: a" (format nil "X: ~A" (code-char 200))))))

(deftest content-length-rules (:suite "http-parser")
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 1a")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: +1")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: -1")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 1 2")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length:")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 5" "Content-Length: 6")))
  (is-equal nil (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 5" "Content-Length: 5")))
  (is-equal 400 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 99999999999999999999")))
  (is-equal 413 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 999999999"))))

(deftest transfer-encoding-rejected (:suite "http-parser")
  (is-equal 501 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Transfer-Encoding: chunked")))
  ;; classic smuggling combo
  (is-equal 501 (status-of (crlf "POST / HTTP/1.1" "Host: a" "Content-Length: 4" "Transfer-Encoding: chunked")))
  (is-equal 501 (status-of (crlf "POST / HTTP/1.1" "Host: a" "transfer-encoding: identity"))))

(deftest size-limits (:suite "http-parser")
  (let ((limits (make-http-limits :max-request-line 64 :max-header-count 2 :max-header-line 32 :max-headers-bytes 200)))
    (is-equal 414 (status-of (crlf (format nil "GET /~A HTTP/1.1" (make-string 100 :initial-element #\a)) "Host: a") :limits limits))
    (is-equal 431 (status-of (crlf "GET / HTTP/1.1" "Host: a" "A: 1" "B: 2") :limits limits))
    (is-equal 431 (status-of (crlf "GET / HTTP/1.1" (format nil "Host: ~A" (make-string 40 :initial-element #\h))) :limits limits))
    ;; incomplete but already too large
    (is-equal 431 (status-of (sb-ext:string-to-octets (format nil "GET / HTTP/1.1~C~C~A" #\Return #\Linefeed (make-string 300 :initial-element #\x))) :limits limits))
    ;; incomplete request line exceeding its limit
    (is-equal 414 (status-of (sb-ext:string-to-octets (format nil "GET /~A" (make-string 100 :initial-element #\a))) :limits limits))))

(deftest status-texts (:suite "http-parser")
  (is-equal "OK" (status-text 200))
  (is-equal "Too Many Requests" (status-text 429))
  (is-equal "Unknown" (status-text 299)))
