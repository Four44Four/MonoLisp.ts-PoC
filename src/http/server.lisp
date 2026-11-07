;;;; src/http/server.lisp -- the JSON-RPC-only HTTP/1.1 server on the async reactor
;;;;
;;;; Routes (everything else is 404):
;;;;   POST   <rpc-path>      application/json body -> JSON-RPC 2.0 (200, or 204 for notifications)
;;;;   OPTIONS <rpc-path>     CORS preflight when :cors-origins is configured
;;;;   GET    <health-path>   {"status":"ok"}
;;;;
;;;; Defenses: strict parser (parser.lisp), per-IP token-bucket rate limiting, header/body read
;;;; timeouts (slowloris), connection cap, request-per-connection cap, exact Content-Type check
;;;; (forces CORS preflight for cross-site browsers), allow-listed CORS origins, hardened response
;;;; headers, and no server fingerprint.

(in-package :monolisp.http)

(defstruct (http-server (:conc-name server-))
  tcp
  (port 0)
  (rpc-path "/rpc")
  (health-path "/health")
  (limits *http-limits*)
  (rpc-limits *rpc-limits*)
  (rate-limiter nil)
  (cors-origins nil)                 ; nil | :any | list of origin strings
  (header-timeout-ms 10000)
  (body-timeout-ms 30000)
  (keep-alive-timeout-ms 15000)
  (max-requests-per-connection 100)
  (remote-address-fn nil)            ; (conn request) -> string, e.g. to honour a trusted proxy header
  (requests 0 :type sb-ext:word)
  (rejected 0 :type sb-ext:word)
  (running nil))

(defvar *server* nil)

(defun server-stats (&optional (server *server*))
  (list :requests (server-requests server)
        :rejected (server-rejected server)
        :connections (hash-table-count (tcp-server-connections (server-tcp server)))
        :accepted (tcp-server-accepted (server-tcp server))
        :refused (tcp-server-refused (server-tcp server))))

;;; ------------------------------------------------------------------ responses

(defparameter *security-headers*
  '(("X-Content-Type-Options" . "nosniff")
    ("Cache-Control" . "no-store")
    ("Content-Security-Policy" . "default-src 'none'; frame-ancestors 'none'")
    ("X-Frame-Options" . "DENY")
    ("Referrer-Policy" . "no-referrer")))

(defun build-response (status &key body (content-type "application/json; charset=utf-8") headers (keep-alive nil))
  "Serialize an HTTP/1.1 response.  BODY is octets or NIL."
  (let ((head (with-output-to-string (s)
                (format s "HTTP/1.1 ~D ~A~C~C" status (status-text status) #\Return #\Linefeed)
                (dolist (h *security-headers*)
                  (format s "~A: ~A~C~C" (car h) (cdr h) #\Return #\Linefeed))
                (dolist (h headers)
                  (format s "~A: ~A~C~C" (car h) (cdr h) #\Return #\Linefeed))
                (when body
                  (format s "Content-Type: ~A~C~C" content-type #\Return #\Linefeed))
                (format s "Content-Length: ~D~C~C" (if body (length body) 0) #\Return #\Linefeed)
                (format s "Connection: ~A~C~C" (if keep-alive "keep-alive" "close") #\Return #\Linefeed)
                (format s "~C~C" #\Return #\Linefeed))))
    (if body
        (concatenate '(simple-array (unsigned-byte 8) (*)) (string-to-octets head) body)
        (string-to-octets head))))

(defun error-body (status message)
  (json-encode-to-octets (make-json-object "error" (make-json-object "status" status "message" message))))

;;; ------------------------------------------------------------------ CORS

(defun cors-headers (server origin)
  "Headers for an allowed ORIGIN, or :forbidden when the origin is not allow-listed."
  (let ((allowed (server-cors-origins server)))
    (cond ((null origin) '())
          ((null allowed) :forbidden)
          ((or (eq allowed :any) (member origin allowed :test #'string=))
           (list (cons "Access-Control-Allow-Origin" (if (eq allowed :any) "*" origin))
                 (cons "Vary" "Origin")
                 (cons "Access-Control-Allow-Methods" "POST, OPTIONS")
                 (cons "Access-Control-Allow-Headers" "Content-Type, Authorization")
                 (cons "Access-Control-Max-Age" "600")))
          (t :forbidden))))

(defun valid-origin-header-p (origin)
  "Origins are echoed into a response header; make sure they cannot inject anything."
  (and (<= 1 (length origin) 256)
       (every (lambda (c) (and (< 32 (char-code c) 127) (not (find c "\"<>\\")))) origin)))

;;; ------------------------------------------------------------------ request handling

(defun json-content-type-p (value)
  (and value
       (let* ((v (string-downcase (string-trim " " value)))
              (semi (position #\; v))
              (media (string-trim " " (subseq v 0 semi)))
              (params (if semi (string-trim " " (subseq v (1+ semi))) "")))
         (and (string= media "application/json")
              (or (string= params "")
                  (string= params "charset=utf-8")
                  (string= params "charset=\"utf-8\""))))))

(defun remote-address (server conn request)
  (or (and (server-remote-address-fn server)
           (ignore-errors (funcall (server-remote-address-fn server) conn request)))
      (let ((peer (conn-peer conn)))
        (if (and peer (vectorp (first peer)))
            (format nil "~{~D~^.~}" (coerce (first peer) 'list))
            "unknown"))))

(define-async handle-connection (server conn)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (served 0))
    (flet ((append-chunk (chunk)
             (loop for b across chunk do (vector-push-extend b buffer)))
           (consume (n)
             (let ((remaining (- (fill-pointer buffer) n)))
               (replace buffer buffer :start1 0 :start2 n)
               (setf (fill-pointer buffer) remaining)))
           (send (octets) (conn-write conn octets)))
      (unwind-protect
           (block connection
             (loop
               ;; ---- read the head
               (conn-set-timeout conn (server-header-timeout-ms server) :close nil)
               (let ((request nil) (head-length 0))
                 (loop
                   (let ((parsed (handler-case (parse-request-head buffer (fill-pointer buffer) :limits (server-limits server))
                                   (http-parse-error (e)
                                     (sb-ext:atomic-incf (server-rejected server))
                                     (await (send (build-response (http-parse-error-status e)
                                                                  :body (error-body (http-parse-error-status e)
                                                                                    (http-parse-error-message e)))))
                                     (return-from connection)))))
                     (cond ((eq parsed :incomplete)
                            (let ((chunk (handler-case (await (conn-read conn))
                                           (timeout-error ()
                                             (when (plusp (fill-pointer buffer))
                                               (await (send (build-response 408 :body (error-body 408 "request timeout")))))
                                             (return-from connection)))))
                              (when (null chunk) (return-from connection))
                              (append-chunk chunk)))
                           (t (setf request parsed)
                              (return)))))
                 ;; (await ...) collapses multiple values, so re-derive the head length here
                 (setf head-length (+ 4 (find-head-end buffer (fill-pointer buffer))))
                 (incf served)
                 (sb-ext:atomic-incf (server-requests server))
                 (let* ((keep-alive (and (request-keep-alive-p request)
                                         (< served (server-max-requests-per-connection server))))
                        (response (await (dispatch-request server conn request buffer head-length)))
                        (keep-alive (and keep-alive (not (getf response :close)))))
                   (await (send (build-response (getf response :status)
                                                :body (getf response :body)
                                                :headers (getf response :headers)
                                                :keep-alive keep-alive)))
                   (consume (getf response :consumed head-length))
                   (unless keep-alive (return-from connection))
                   (conn-set-timeout conn (server-keep-alive-timeout-ms server))))))
        (conn-close conn)))))

(define-async dispatch-request (server conn request buffer head-length)
  "Returns a plist (:status :body :headers :consumed :close)."
  (let ((method (request-method request))
        (target (request-target request)))
    (cond
      ;; health
      ((string= target (server-health-path server))
       (if (or (string= method "GET") (string= method "HEAD"))
           (list :status 200 :body (json-encode-to-octets (make-json-object "status" "ok")) :consumed head-length)
           (list :status 405 :headers '(("Allow" . "GET, HEAD")) :body (error-body 405 "method not allowed") :consumed head-length)))
      ((not (string= target (server-rpc-path server)))
       (list :status 404 :body (error-body 404 "not found") :consumed head-length :close t))
      ;; CORS preflight
      ((string= method "OPTIONS")
       (let* ((origin (request-header request "origin"))
              (cors (if (and origin (valid-origin-header-p origin)) (cors-headers server origin) :forbidden)))
         (cond ((eq cors :forbidden)
                (list :status 403 :body (error-body 403 "origin not allowed") :consumed head-length :close t))
               (t (list :status 204 :headers cors :consumed head-length)))))
      ((not (string= method "POST"))
       (list :status 405 :headers '(("Allow" . "POST, OPTIONS")) :body (error-body 405 "method not allowed")
             :consumed head-length :close t))
      (t
       (let* ((origin (request-header request "origin"))
              (cors (cond ((null origin) '())
                          ((not (valid-origin-header-p origin)) :forbidden)
                          (t (cors-headers server origin))))
              (remote (remote-address server conn request)))
         (cond
           ((eq cors :forbidden)
            (sb-ext:atomic-incf (server-rejected server))
            (list :status 403 :body (error-body 403 "origin not allowed") :consumed head-length :close t))
           ((and (server-rate-limiter server) (not (rate-limit-allow-p (server-rate-limiter server) remote)))
            (sb-ext:atomic-incf (server-rejected server))
            (list :status 429 :headers '(("Retry-After" . "1")) :body (error-body 429 "rate limited")
                  :consumed head-length :close t))
           ((not (json-content-type-p (request-header request "content-type")))
            (sb-ext:atomic-incf (server-rejected server))
            (list :status 415 :body (error-body 415 "Content-Type must be application/json") :consumed head-length :close t))
           ((null (request-content-length request))
            (sb-ext:atomic-incf (server-rejected server))
            (list :status 411 :body (error-body 411 "Content-Length required") :consumed head-length :close t))
           ((> (request-content-length request) (rpc-limits-max-payload-bytes (server-rpc-limits server)))
            (sb-ext:atomic-incf (server-rejected server))
            (list :status 413 :body (error-body 413 "payload too large") :consumed head-length :close t))
           (t
            ;; ---- read the body
            (let ((needed (+ head-length (request-content-length request))))
              (conn-set-timeout conn (server-body-timeout-ms server) :close nil)
              (loop while (< (fill-pointer buffer) needed)
                    do (let ((chunk (handler-case (await (conn-read conn))
                                      (timeout-error () nil))))
                         (when (null chunk)
                           (return-from dispatch-request
                             (list :status 408 :body (error-body 408 "body timeout") :consumed (fill-pointer buffer) :close t)))
                         (loop for b across chunk do (vector-push-extend b buffer))))
              (let* ((body (subseq buffer head-length needed))
                     (context (make-rpc-context :remote-address remote
                                                :headers (request-headers request)
                                                :request-id (random-token 8)))
                     (result (await (handle-rpc-octets body context :limits (server-rpc-limits server)))))
                (if result
                    (list :status 200 :body result :headers cors :consumed needed)
                    (list :status 204 :headers cors :consumed needed)))))))))))

;;; ------------------------------------------------------------------ lifecycle

(defun start-server (&key (host "127.0.0.1") (port 8080) (rpc-path "/rpc") (health-path "/health")
                       (limits *http-limits*) (rpc-limits *rpc-limits*)
                       (rate-limiter (make-rate-limiter :capacity 100 :refill-per-second 50))
                       cors-origins (max-connections 512)
                       (header-timeout-ms 10000) (body-timeout-ms 30000) (keep-alive-timeout-ms 15000)
                       (max-requests-per-connection 100) remote-address-fn)
  "Start listening on HOST:PORT using the current event loop.  Returns an HTTP-SERVER."
  (unless *loop* (error "start-server: no event loop is current (use WITH-EVENT-LOOP or RUN-SERVER)"))
  (let ((server (make-http-server :rpc-path rpc-path :health-path health-path :limits limits
                                  :rpc-limits rpc-limits :rate-limiter rate-limiter :cors-origins cors-origins
                                  :header-timeout-ms header-timeout-ms :body-timeout-ms body-timeout-ms
                                  :keep-alive-timeout-ms keep-alive-timeout-ms
                                  :max-requests-per-connection max-requests-per-connection
                                  :remote-address-fn remote-address-fn)))
    (setf (server-tcp server)
          (tcp-listen (lambda (conn) (handle-connection server conn))
                      :host host :port port :max-connections max-connections))
    (setf (server-port server) (tcp-server-port (server-tcp server))
          (server-running server) t
          *server* server)
    server))

(defun stop-server (&optional (server *server*))
  (when (and server (server-running server))
    (setf (server-running server) nil)
    (tcp-server-close (server-tcp server))
    (when (eq server *server*) (setf *server* nil))
    t))

(defun run-server (&rest args &key &allow-other-keys)
  "Blocking convenience: create a loop, start the server, run until STOP-SERVER."
  (with-event-loop (:run nil)
    (let ((server (apply #'start-server args)))
      (format t "~&Monolisp JSON-RPC server listening on port ~D (~D server function~:P)~%"
              (server-port server) (length (list-server-functions)))
      (run-loop)
      server)))
