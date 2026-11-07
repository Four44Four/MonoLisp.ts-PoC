;;;; src/async/http-client.lisp -- minimal asynchronous HTTP/1.1 client (`fetch`)
;;;;
;;;; Lets server functions call slow upstream APIs without blocking the loop or a connection.
;;;; Plain HTTP only (TLS termination is expected at the edge / a sidecar); IPv4 literals or
;;;; names resolved through the thread pool.  Written with the toolkit's own ASYNC/AWAIT.

(in-package :monolisp.async)

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message)
   (status :initarg :status :initform nil :reader http-error-status))
  (:report (lambda (c s) (format s "HTTP error: ~A~@[ (status ~D)~]" (http-error-message c) (http-error-status c)))))

(defstruct (http-response (:conc-name http-response-))
  (status 0 :type fixnum)
  (headers '())        ; alist of (lowercase-name . value)
  (body (make-octet-buffer 0) :type octets))

(defun http-response-text (response)
  (octets-to-string (http-response-body response)))

(defun %parse-url (url)
  "Returns (values host port path)."
  (let ((scheme-end (search "://" url)))
    (unless (and scheme-end (string-equal (subseq url 0 scheme-end) "http"))
      (error 'http-error :message (format nil "only http:// URLs are supported: ~A" url)))
    (let* ((rest (subseq url (+ scheme-end 3)))
           (slash (position #\/ rest))
           (authority (subseq rest 0 slash))
           (path (if slash (subseq rest slash) "/"))
           (colon (position #\: authority))
           (host (if colon (subseq authority 0 colon) authority))
           (port (if colon (parse-integer authority :start (1+ colon)) 80)))
      (when (or (zerop (length host)) (not (<= 1 port 65535)))
        (error 'http-error :message (format nil "bad URL ~A" url)))
      (values host port path))))

(defun %find-crlfcrlf (buf end)
  (loop for i from 0 to (- end 4)
        when (and (= (aref buf i) 13) (= (aref buf (+ i 1)) 10)
                  (= (aref buf (+ i 2)) 13) (= (aref buf (+ i 3)) 10))
          return i))

(defun %split-lines (text)
  (loop with start = 0
        for pos = (search (string #\Return) text :start2 start)
        collect (subseq text start pos)
        while pos
        do (setf start (min (length text) (+ pos 2)))))

(defun %parse-response-head (octets end)
  "Returns (values status headers-alist)."
  (let* ((text (handler-case (octets-to-string octets :end end)
                 (error () (error 'http-error :message "non-UTF-8 response head"))))
         (lines (remove "" (%split-lines text) :test #'string=))
         (status-line (first lines)))
    (unless (and status-line (>= (length status-line) 12) (string= "HTTP/1." status-line :end2 7))
      (error 'http-error :message "malformed status line"))
    (let ((status (or (parse-integer status-line :start 9 :end 12 :junk-allowed t)
                      (error 'http-error :message "malformed status code")))
          (headers (loop for line in (rest lines)
                         for colon = (position #\: line)
                         when colon
                           collect (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                                         (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))
      (values status headers))))

(defun %decode-chunked (body)
  "Decode a Transfer-Encoding: chunked body."
  (let ((out (make-array (length body) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (pos 0) (len (length body)))
    (loop
      (let ((line-end (loop for i from pos below (1- len)
                            when (and (= (aref body i) 13) (= (aref body (1+ i)) 10)) return i)))
        (unless line-end (error 'http-error :message "truncated chunked body"))
        (let* ((size-text (octets-to-string body :start pos :end line-end))
               (semi (position #\; size-text))
               (size (or (parse-integer size-text :end semi :radix 16 :junk-allowed t)
                         (error 'http-error :message "bad chunk size"))))
          (setf pos (+ line-end 2))
          (when (zerop size) (return))
          (when (> (+ pos size) len) (error 'http-error :message "truncated chunk"))
          (loop for i from pos below (+ pos size) do (vector-push-extend (aref body i) out))
          (setf pos (+ pos size 2)))))
    (coerce out 'octets)))

(define-async fetch (url &key (method "GET") headers body (timeout 30000) (max-body (* 8 1024 1024)))
  "Fetch URL asynchronously.  Resolves to an HTTP-RESPONSE.  BODY may be a string or octets."
  (multiple-value-bind (host port path) (%parse-url url)
    (let* ((body-octets (etypecase body
                          (null nil)
                          (string (string-to-octets body))
                          ((vector (unsigned-byte 8)) (coerce body 'octets))))
           (head (with-output-to-string (s)
                   (format s "~A ~A HTTP/1.1~C~C" method path #\Return #\Linefeed)
                   (format s "Host: ~A~C~C" (if (= port 80) host (format nil "~A:~D" host port)) #\Return #\Linefeed)
                   (format s "Connection: close~C~C" #\Return #\Linefeed)
                   (format s "User-Agent: monolisp-async/0.1~C~C" #\Return #\Linefeed)
                   (when body-octets
                     (format s "Content-Length: ~D~C~C" (length body-octets) #\Return #\Linefeed))
                   (loop for (name . value) in headers
                         do (when (or (find #\Return (string value)) (find #\Linefeed (string value))
                                      (find #\Return (string name)) (find #\Linefeed (string name)))
                              (error 'http-error :message "CR/LF in request header"))
                            (format s "~A: ~A~C~C" name value #\Return #\Linefeed))
                   (format s "~C~C" #\Return #\Linefeed)))
           (conn (await (with-timeout (timeout) (tcp-connect host port :timeout timeout)))))
      (unwind-protect
           (progn
             (conn-set-timeout conn timeout)
             (await (conn-write conn (if body-octets
                                         (octets-concat (string-to-octets head) body-octets)
                                         (string-to-octets head))))
             ;; Read everything until EOF (we asked for Connection: close), bounded by MAX-BODY.
             (let ((acc (make-array 4096 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
                   (head-end nil))
               (loop
                 (let ((chunk (await (conn-read conn))))
                   (when (null chunk) (return))
                   (loop for b across chunk do (vector-push-extend b acc))
                   (when (> (fill-pointer acc) (+ max-body 65536))
                     (error 'http-error :message "response exceeds max-body"))
                   (unless head-end
                     (setf head-end (%find-crlfcrlf acc (fill-pointer acc))))
                   ;; Stop early when Content-Length is satisfied.
                   (when head-end
                     (multiple-value-bind (status headers) (%parse-response-head (coerce acc 'octets) head-end)
                       (declare (ignore status))
                       (let ((cl (cdr (assoc "content-length" headers :test #'string=))))
                         (when (and cl (every #'digit-char-p cl)
                                    (>= (- (fill-pointer acc) head-end 4) (parse-integer cl)))
                           (return)))))))
               (unless head-end (error 'http-error :message "no response head received"))
               (let ((all (coerce acc 'octets)))
                 (multiple-value-bind (status headers) (%parse-response-head all head-end)
                   (let* ((raw (subseq-octets all (+ head-end 4)))
                          (te (cdr (assoc "transfer-encoding" headers :test #'string=)))
                          (body (if (and te (search "chunked" (string-downcase te)))
                                    (%decode-chunked raw)
                                    raw)))
                     (when (> (length body) max-body)
                       (error 'http-error :message "response exceeds max-body"))
                     (make-http-response :status status :headers headers :body body))))))
        (conn-close conn)))))
