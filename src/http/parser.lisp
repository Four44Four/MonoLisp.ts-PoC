;;;; src/http/parser.lisp -- strict incremental HTTP/1.1 request-head parser
;;;;
;;;; Only what a JSON-RPC endpoint needs is accepted; everything else is a 4xx/5xx:
;;;;   * CRLF line endings only (bare LF or bare CR -> 400), no obsolete line folding
;;;;   * request-line: TOKEN SP origin-form-target SP HTTP/1.[01], single spaces, bounded lengths
;;;;   * header names are tokens with no whitespace before the colon; values are visible ASCII + HTAB
;;;;   * Content-Length: digits only, at most one distinct value; Transfer-Encoding -> 501
;;;;     (chunked framing is the main request-smuggling vector, and JSON-RPC never needs it)
;;;;   * exactly one Host header for HTTP/1.1
;;;;   * hard caps on request-line, header line, header count, total head bytes and body size

(in-package :monolisp.http)

(defstruct (http-limits (:copier copy-http-limits))
  (max-request-line 2048 :type fixnum)
  (max-header-count 64 :type fixnum)
  (max-header-line 8192 :type fixnum)
  (max-headers-bytes 16384 :type fixnum)
  (max-body (* 1024 1024) :type fixnum))

(defvar *http-limits* (make-http-limits))

(define-condition http-parse-error (error)
  ((status :initarg :status :reader http-parse-error-status)
   (message :initarg :message :reader http-parse-error-message))
  (:report (lambda (c s) (format s "HTTP ~D: ~A" (http-parse-error-status c) (http-parse-error-message c)))))

(defstruct (http-request (:conc-name request-))
  (method "" :type string)
  (target "" :type string)
  (version "" :type string)
  (headers '() :type list)          ; alist (lowercase-name . value) in order
  (content-length nil))

(defun request-header (request name)
  (cdr (assoc (string-downcase name) (request-headers request) :test #'string=)))

(defun request-keep-alive-p (request)
  (let ((conn (request-header request "connection")))
    (if (string= (request-version request) "HTTP/1.0")
        (and conn (string-equal (string-trim " " conn) "keep-alive"))
        (not (and conn (search "close" (string-downcase conn)))))))

(defparameter *status-text*
  '((200 . "OK") (204 . "No Content") (400 . "Bad Request") (403 . "Forbidden") (404 . "Not Found")
    (405 . "Method Not Allowed") (408 . "Request Timeout") (411 . "Length Required") (413 . "Payload Too Large")
    (414 . "URI Too Long") (415 . "Unsupported Media Type") (429 . "Too Many Requests")
    (431 . "Request Header Fields Too Large") (500 . "Internal Server Error") (501 . "Not Implemented")
    (503 . "Service Unavailable") (505 . "HTTP Version Not Supported")))

(defun status-text (code)
  (or (cdr (assoc code *status-text*)) "Unknown"))

(defun http-fail (status message)
  (error 'http-parse-error :status status :message message))

;;; ------------------------------------------------------------------ character classes

(declaim (inline tchar-p vchar-p))
(defun tchar-p (b)
  "RFC 7230 token characters."
  (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122)
      (member b '(33 35 36 37 38 39 42 43 45 46 94 95 96 124 126))))

(defun vchar-p (b) (<= 33 b 126))

(defun target-char-p (b)
  "Characters allowed in an origin-form request target (RFC 3986 pchar / query):
unreserved, sub-delims, pct-encoded (%), and : @ / ?.  Everything else -- quotes, backslash,
angle brackets, braces, pipe, caret, backtick, '#', controls, non-ASCII -- must be rejected."
  (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122)
      (member b '(45 46 95 126                      ; - . _ ~
                  33 36 38 39 40 41 42 43 44 59 61  ; ! $ & ' ( ) * + , ; =
                  37 58 64 47 63))))                ; % : @ / ?

(defun valid-host-p (value)
  "Host = uri-host [ \":\" port ] with uri-host a reg-name, IPv4 or bracketed IPv6 literal."
  (let* ((len (length value))
         (colon (if (and (plusp len) (char= (char value 0) #\[))
                    (let ((close (position #\] value)))
                      (unless close (return-from valid-host-p nil))
                      (loop for i from 1 below close
                            unless (or (digit-char-p (char value i) 16) (char= (char value i) #\:) (char= (char value i) #\.))
                              do (return-from valid-host-p nil))
                      (cond ((= (1+ close) len) nil)
                            ((char= (char value (1+ close)) #\:) (1+ close))
                            (t (return-from valid-host-p nil))))
                    (position #\: value)))
         (host-end (or colon len)))
    (and (<= 0 len 255)
         (or (and (plusp len) (char= (char value 0) #\[))
             (loop for i from 0 below host-end
                   always (let ((c (char value i)))
                            (or (alphanumericp c) (find c "-._~%!$&'()*+,;=")))))
         (or (null colon)
             (and (< (1+ colon) len) (<= (- len colon 1) 5)
                  (loop for i from (1+ colon) below len always (digit-char-p (char value i)))))
         ;; non-ASCII never passes alphanumericp here because header values are already ASCII-only
         t)))

(defun find-head-end (buffer end)
  "Index of the CRLFCRLF terminating the head, or NIL."
  (loop for i from 0 to (- end 4)
        when (and (= (aref buffer i) 13) (= (aref buffer (+ i 1)) 10)
                  (= (aref buffer (+ i 2)) 13) (= (aref buffer (+ i 3)) 10))
          return i))

(defun ascii-string (buffer start end)
  (let ((s (make-string (- end start))))
    (loop for i from start below end
          for j from 0
          do (setf (schar s j) (code-char (aref buffer i))))
    s))

;;; ------------------------------------------------------------------ parsing

(defun parse-request-head (buffer end &key (limits *http-limits*))
  "Parse the request head in BUFFER[0,end).  Returns (values HTTP-REQUEST head-length) when complete,
:INCOMPLETE when more bytes are needed, or signals HTTP-PARSE-ERROR."
  (let ((head-end (find-head-end buffer end)))
    (cond
      ((null head-end)
       (when (> end (http-limits-max-headers-bytes limits))
         (http-fail 431 "request head too large"))
       ;; An early bare LF (no CR) is never acceptable; detect it before the head completes.
       (loop for i from 0 below end
             when (and (= (aref buffer i) 10) (or (zerop i) (/= (aref buffer (1- i)) 13)))
               do (http-fail 400 "bare LF in request head"))
       (when (> end (http-limits-max-request-line limits))
         (unless (loop for i from 0 below (1- end) thereis (and (= (aref buffer i) 13) (= (aref buffer (1+ i)) 10)))
           (http-fail 414 "request line too long")))
       :incomplete)
      ((> head-end (http-limits-max-headers-bytes limits))
       (http-fail 431 "request head too large"))
      (t
       (let ((request (parse-head-bytes buffer head-end limits)))
         (values request (+ head-end 4)))))))

(defun split-crlf-lines (buffer end)
  "Split BUFFER[0,end) on CRLF; signals on bare CR or LF."
  (let ((lines '()) (start 0) (i 0))
    (loop while (< i end)
          do (let ((b (aref buffer i)))
               (cond ((= b 13)
                      (unless (and (< (1+ i) end) (= (aref buffer (1+ i)) 10))
                        (http-fail 400 "bare CR in request head"))
                      (push (cons start i) lines)
                      (setf start (+ i 2) i (+ i 2)))
                     ((= b 10) (http-fail 400 "bare LF in request head"))
                     (t (incf i)))))
    (when (< start end) (push (cons start end) lines))
    (nreverse lines)))

(defun parse-head-bytes (buffer head-end limits)
  (let* ((lines (split-crlf-lines buffer head-end))
         (request-line (first lines)))
    (unless request-line (http-fail 400 "empty request"))
    (let ((request (parse-request-line buffer (car request-line) (cdr request-line) limits)))
      (when (> (length (rest lines)) (http-limits-max-header-count limits))
        (http-fail 431 "too many header fields"))
      (let ((headers '()) (content-length nil) (host-count 0))
        (dolist (range (rest lines))
          (destructuring-bind (start . end) range
            (when (> (- end start) (http-limits-max-header-line limits))
              (http-fail 431 "header line too long"))
            (when (= start end) (http-fail 400 "empty header line"))
            (let ((first (aref buffer start)))
              (when (or (= first 32) (= first 9))
                (http-fail 400 "obsolete line folding is not allowed")))
            (let ((colon (loop for i from start below end when (= (aref buffer i) 58) return i)))
              (unless colon (http-fail 400 "header line without colon"))
              (when (= colon start) (http-fail 400 "empty header name"))
              (loop for i from start below colon
                    unless (tchar-p (aref buffer i))
                      do (http-fail 400 "invalid header name"))
              (let ((vstart (1+ colon)) (vend end))
                ;; trim OWS
                (loop while (and (< vstart vend) (member (aref buffer vstart) '(32 9))) do (incf vstart))
                (loop while (and (< vstart vend) (member (aref buffer (1- vend)) '(32 9))) do (decf vend))
                (loop for i from vstart below vend
                      for b = (aref buffer i)
                      unless (or (vchar-p b) (= b 32) (= b 9))
                        do (http-fail 400 "invalid header value"))
                (let ((name (string-downcase (ascii-string buffer start colon)))
                      (value (ascii-string buffer vstart vend)))
                  (cond
                    ((string= name "content-length")
                     (unless (and (plusp (length value)) (<= (length value) 18) (every #'digit-char-p value))
                       (http-fail 400 "invalid Content-Length"))
                     (let ((n (parse-integer value)))
                       (when (and content-length (/= content-length n))
                         (http-fail 400 "conflicting Content-Length values"))
                       (setf content-length n)))
                    ((string= name "transfer-encoding")
                     (http-fail 501 "Transfer-Encoding is not supported"))
                    ((string= name "host")
                     (unless (valid-host-p value) (http-fail 400 "invalid Host header"))
                     (incf host-count)))
                  (push (cons name value) headers))))))
        (when (and (string= (request-version request) "HTTP/1.1") (/= host-count 1))
          (http-fail 400 (if (zerop host-count) "missing Host header" "multiple Host headers")))
        (when (and content-length (> content-length (http-limits-max-body limits)))
          (http-fail 413 "body too large"))
        (setf (request-headers request) (nreverse headers)
              (request-content-length request) content-length)
        request))))

(defun parse-request-line (buffer start end limits)
  (when (> (- end start) (http-limits-max-request-line limits))
    (http-fail 414 "request line too long"))
  (let ((sp1 (loop for i from start below end when (= (aref buffer i) 32) return i)))
    (unless sp1 (http-fail 400 "malformed request line"))
    (let ((sp2 (loop for i from (1+ sp1) below end when (= (aref buffer i) 32) return i)))
      (unless sp2 (http-fail 400 "malformed request line"))
      (when (or (= sp1 start) (= sp2 (1+ sp1)) (= sp2 (1- end)))
        (http-fail 400 "malformed request line"))
      (loop for i from (1+ sp2) below end when (= (aref buffer i) 32) do (http-fail 400 "malformed request line"))
      (when (> (- sp1 start) 16) (http-fail 501 "method too long"))
      (loop for i from start below sp1 unless (tchar-p (aref buffer i)) do (http-fail 400 "invalid method"))
      (unless (= (aref buffer (1+ sp1)) 47) (http-fail 400 "request target must be origin-form"))
      (loop for i from (1+ sp1) below sp2
            for b = (aref buffer i)
            unless (target-char-p b)                    ; RFC 3986 path/query chars only: no fragments,
              do (http-fail 400 "invalid character in request target")) ; quotes, backslashes, controls...
      (let ((method (ascii-string buffer start sp1))
            (target (ascii-string buffer (1+ sp1) sp2))
            (version (ascii-string buffer (1+ sp2) end)))
        (unless (or (string= version "HTTP/1.1") (string= version "HTTP/1.0"))
          (if (and (>= (length version) 5) (string= "HTTP/" version :end2 5))
              (http-fail 505 "unsupported HTTP version")
              (http-fail 400 "malformed HTTP version")))
        (make-http-request :method method :target target :version version)))))
