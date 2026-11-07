;;;; src/rpc/jsonrpc.lisp -- the JSON-RPC 2.0 boundary
;;;;
;;;; Everything in a request is hostile until proven otherwise.  Processing order per request:
;;;;   bytes -> bounded JSON parse -> envelope validation (exact member set, exact version string,
;;;;   method charset/length, id shape) -> registry lookup (string hash, never INTERN) ->
;;;;   auth -> parameter binding + schema validation -> timed call -> result schema encoding.
;;;; Error responses never contain payload content or internal condition text (unless *DEBUG-MODE*).

(in-package :monolisp.rpc)

(defconstant +parse-error+ -32700)
(defconstant +invalid-request+ -32600)
(defconstant +method-not-found+ -32601)
(defconstant +invalid-params+ -32602)
(defconstant +internal-error+ -32603)
(defconstant +unauthorized+ -32001)
(defconstant +rate-limited+ -32002)
(defconstant +timeout+ -32003)

(define-condition rpc-error (error)
  ((code :initarg :code :reader rpc-error-code)
   (message :initarg :message :reader rpc-error-message)
   (data :initarg :data :initform nil :reader rpc-error-data))
  (:report (lambda (c s) (format s "RPC error ~D: ~A" (rpc-error-code c) (rpc-error-message c)))))

(defun rpc-fail (message &key (code -32000) data)
  "Signal an application-level JSON-RPC error that is returned to the client verbatim.
CODE must be in the server-defined range -32000..-32099 or a positive application code."
  (unless (or (<= -32099 code -32000) (plusp code))
    (error "rpc-fail: code ~D is reserved" code))
  (error 'rpc-error :code code :message message :data data))

;;; ------------------------------------------------------------------ stats

(defstruct rpc-stats
  (requests 0 :type sb-ext:word)
  (batches 0 :type sb-ext:word)
  (errors 0 :type sb-ext:word)
  (rejected 0 :type sb-ext:word)
  (timeouts 0 :type sb-ext:word))

(defvar *rpc-stats* (make-rpc-stats))

;;; ------------------------------------------------------------------ responses

(defun error-response (id code message &optional data)
  (let ((err (make-json-object "code" code "message" message)))
    (when data (json-set err "data" data))
    (make-json-object "jsonrpc" "2.0" "error" err "id" (if (eq id :absent) :null id))))

(defun result-response (id result)
  (make-json-object "jsonrpc" "2.0" "result" result "id" id))

;;; ------------------------------------------------------------------ envelope validation

(defparameter *allowed-members* '("jsonrpc" "method" "params" "id"))

(defun valid-id-p (id limits)
  "JSON-RPC ids: string, number (we insist on safe integers) or null."
  (cond ((eq id :null) t)
        ((integerp id) (<= (abs id) +js-max-safe-integer+))
        ((stringp id) (and (<= (length id) (rpc-limits-max-id-length limits))
                           (notany (lambda (c) (< (char-code c) 32)) id)))
        (t nil)))

(defun valid-method-name-p (name limits)
  (and (stringp name)
       (<= 1 (length name) (rpc-limits-max-method-length limits))
       (every (lambda (c) (and (< (char-code c) 128) (or (alphanumericp c) (find c "_.-")))) name)))

(defun validate-envelope (request limits)
  "Returns (values method params id) or signals RPC-ERROR with +invalid-request+.
ID is :ABSENT for notifications."
  (unless (json-object-p request)
    (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "request must be an object"))
  (dolist (key (json-keys request))
    (unless (member key *allowed-members* :test #'string=)
      (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "unexpected member")))
  (multiple-value-bind (version has-version) (json-get request "jsonrpc")
    (unless (and has-version (stringp version) (string= version "2.0"))
      (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "jsonrpc must be \"2.0\"")))
  (multiple-value-bind (id has-id) (json-get request "id")
    (let ((id (if has-id id :absent)))
      (when (and has-id (not (valid-id-p id limits)))
        (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "invalid id"))
      (multiple-value-bind (method has-method) (json-get request "method")
        (unless (and has-method (valid-method-name-p method limits))
          (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "invalid method name"))
        (multiple-value-bind (params has-params) (json-get request "params")
          (when (and has-params (not (or (json-object-p params) (simple-vector-p params))))
            (error 'rpc-error :code +invalid-request+ :message "Invalid Request" :data "params must be array or object"))
          (values method (if has-params params :absent) id))))))

;;; ------------------------------------------------------------------ parameter binding

(defun bind-params (sf params)
  "Match PARAMS (vector, object or :absent) to the function's parameter specs; returns the argument list."
  (let* ((specs (server-function-params sf))
         (required (count-if-not #'param-optional-p specs))
         (total (length specs)))
    (flet ((invalid (msg) (error 'rpc-error :code +invalid-params+ :message "Invalid params" :data msg)))
      (cond
        ((eq params :absent)
         (unless (zerop required) (invalid (format nil "expected ~D argument~:P" required)))
         (mapcar #'param-default specs))
        ((simple-vector-p params)
         (unless (<= required (length params) total)
           (invalid (format nil "expected ~D to ~D positional arguments, got ~D" required total (length params))))
         (loop for spec in specs
               for i from 0
               collect (if (< i (length params))
                           (validate-param spec (svref params i) (list i))
                           (param-default spec))))
        (t
         (let ((allowed (mapcar #'param-json-name specs)))
           (dolist (key (json-keys params))
             (unless (member key allowed :test #'string=)
               (invalid "unknown named parameter"))))
         (loop for spec in specs
               collect (multiple-value-bind (v present) (json-get params (param-json-name spec))
                         (cond (present (validate-param spec v (list (param-json-name spec))))
                               ((param-optional-p spec) (param-default spec))
                               (t (invalid (format nil "missing required parameter ~A" (param-json-name spec))))))))))))

(defun validate-param (spec value path)
  (handler-case (validate-value (param-schema spec) value path)
    (schema-error (e)
      (error 'rpc-error :code +invalid-params+ :message "Invalid params"
                        :data (format nil "~A: expected ~A" (format-path (schema-error-path e)) (schema-error-expected e))))))

;;; ------------------------------------------------------------------ auth

(defun check-auth (sf context)
  (let ((auth (server-function-auth sf)))
    (cond ((eq auth :public) t)
          ((eq auth :authenticated)
           (unless (context-authenticated-p context)
             (error 'rpc-error :code +unauthorized+ :message "Unauthorized")))
          (t (unless (handler-case (funcall auth context) (error () nil))
               (error 'rpc-error :code +unauthorized+ :message "Unauthorized"))))))

(defun authenticate (context)
  "Populate the principal once per request using *AUTHENTICATOR*; failures mean 'anonymous'."
  (when (and *authenticator* (null (context-principal context)))
    (setf (context-principal context)
          (handler-case (funcall *authenticator* context)
            (error () nil))))
  context)

;;; ------------------------------------------------------------------ invocation

(defun invoke-server-function (sf args context limits)
  "Call SF with ARGS.  Returns a promise of the JSON result value."
  (let* ((timeout (or (server-function-timeout-ms sf) (rpc-limits-call-timeout-ms limits)))
         (call (lambda ()
                 (let ((*request-context* context))
                   (apply (server-function-function sf) args))))
         (raw (handler-case (funcall call)
                (rpc-error (e) (rejected e))
                (error (e) (rejected e))))
         (p (if (promisep raw) raw (resolved raw)))
         (p (if (and timeout *loop*) (with-timeout (timeout) p) p)))
    (then p
          (lambda (result)
            (handler-case (encode-value (server-function-returns sf) result)
              (schema-error (e)
                (funcall *error-logger*
                         (make-condition 'simple-error
                                         :format-control "result of ~A does not match :returns schema: ~A"
                                         :format-arguments (list (server-function-rpc-name sf) e))
                         context)
                (error 'rpc-error :code +internal-error+ :message "Internal error")))))))

(defun process-request (request context limits)
  "Promise of a response JSON-OBJECT, or NIL for notifications."
  (sb-ext:atomic-incf (rpc-stats-requests *rpc-stats*))
  (let ((id :absent))
    (flet ((respond-error (e)
             (sb-ext:atomic-incf (rpc-stats-errors *rpc-stats*))
             (if (eq id :absent)
                 nil                                    ; notifications never get a response
                 (error-response id (rpc-error-code e) (rpc-error-message e) (rpc-error-data e)))))
      (handler-case
          (multiple-value-bind (method params req-id) (validate-envelope request limits)
            (setf id req-id)
            (let ((sf (find-server-function method)))
              (unless sf (error 'rpc-error :code +method-not-found+ :message "Method not found"))
              (authenticate context)
              (check-auth sf context)
              (let ((args (bind-params sf params)))
                (then (invoke-server-function sf args context limits)
                      (lambda (result)
                        (if (eq id :absent) nil (result-response id result)))
                      (lambda (reason)
                        (cond ((typep reason 'rpc-error) (respond-error reason))
                              ((typep reason 'timeout-error)
                               (sb-ext:atomic-incf (rpc-stats-timeouts *rpc-stats*))
                               (respond-error (make-condition 'rpc-error :code +timeout+ :message "Request timed out")))
                              (t
                               (funcall *error-logger* reason context)
                               (respond-error
                                (make-condition 'rpc-error :code +internal-error+ :message "Internal error"
                                                           :data (when *debug-mode* (princ-to-string reason)))))))))))
        (rpc-error (e)
          (resolved (if (and (eq id :absent) (= (rpc-error-code e) +invalid-request+))
                        ;; an invalid request with no usable id still gets an error with id null
                        (error-response :null (rpc-error-code e) (rpc-error-message e) (rpc-error-data e))
                        (respond-error e))))))))

;;; ------------------------------------------------------------------ entry points

(defun handle-rpc-octets (octets context &key (limits *rpc-limits*))
  "Process one JSON-RPC payload.  Returns a promise of response octets, or of NIL when no response
is due (notification / batch of notifications).  Never signals: every failure becomes a JSON-RPC error."
  (progn
    (when (> (length octets) (rpc-limits-max-payload-bytes limits))
      (sb-ext:atomic-incf (rpc-stats-rejected *rpc-stats*))
      (return-from handle-rpc-octets
        (resolved (json-encode-to-octets (error-response :null +invalid-request+ "Invalid Request" "payload too large")))))
    (let ((parsed (handler-case (json-parse-octets octets :limits (rpc-limits-json-limits limits))
                    (json-parse-error (e)
                      (sb-ext:atomic-incf (rpc-stats-rejected *rpc-stats*))
                      (return-from handle-rpc-octets
                        (resolved (json-encode-to-octets
                                   (error-response :null +parse-error+ "Parse error"
                                                   (json-parse-error-message e))))))
                    (error ()
                      (sb-ext:atomic-incf (rpc-stats-rejected *rpc-stats*))
                      (return-from handle-rpc-octets
                        (resolved (json-encode-to-octets (error-response :null +parse-error+ "Parse error"))))))))
      (cond
        ((simple-vector-p parsed)
         (sb-ext:atomic-incf (rpc-stats-batches *rpc-stats*))
         (cond ((zerop (length parsed))
                (resolved (json-encode-to-octets (error-response :null +invalid-request+ "Invalid Request" "empty batch"))))
               ((> (length parsed) (rpc-limits-max-batch limits))
                (sb-ext:atomic-incf (rpc-stats-rejected *rpc-stats*))
                (resolved (json-encode-to-octets (error-response :null +invalid-request+ "Invalid Request" "batch too large"))))
               (t
                (then (all (map 'list (lambda (req) (safe-process req context limits)) parsed))
                      (lambda (responses)
                        (let ((real (remove nil responses)))
                          (if real
                              (json-encode-to-octets (coerce real 'simple-vector))
                              nil)))))))
        (t
         (then (safe-process parsed context limits)
               (lambda (response)
                 (and response (json-encode-to-octets response)))))))))

(defun safe-process (request context limits)
  "PROCESS-REQUEST that converts any unexpected failure into an internal-error response."
  (handler-case (process-request request context limits)
    (error (e)
      (funcall *error-logger* e context)
      (resolved (error-response :null +internal-error+ "Internal error")))))

(defun handle-rpc-string (string context &key (limits *rpc-limits*))
  "Convenience: string in, promise of string (or NIL) out."
  (then (handle-rpc-octets (sb-ext:string-to-octets string :external-format :utf-8) context :limits limits)
        (lambda (octets) (and octets (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun handle-rpc-sync (string &key (context (make-rpc-context)) (limits *rpc-limits*) (timeout 10))
  "Blocking helper for tests and scripts: returns the response string or NIL."
  (if (and *loop* (loop-thread-p *loop*))
      (promise-wait (handle-rpc-string string context :limits limits) :timeout timeout)
      (with-event-loop (:run nil)
        (promise-wait (handle-rpc-string string context :limits limits) :timeout timeout))))
