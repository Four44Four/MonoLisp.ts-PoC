;;;; src/transpiler/align.lisp -- backend <-> frontend alignment
;;;;
;;;; The backend's server-function registry produces a manifest (SERVER-MANIFEST).  From it we
;;;;   1. generate `server-functions.ts`: one typed async stub per server function plus one
;;;;      interface per RPC struct, so `tsc` verifies every frontend call against the backend;
;;;;   2. check calls at transpile time (arity, literal/declared argument types) so mistakes are
;;;;      reported with the Lisp source form, before TypeScript ever runs;
;;;;   3. stamp the stubs with a digest of the manifest so a stale frontend build is detected.

(in-package :monolisp.transpiler)

(defparameter +runtime-file-name+ "monolisp-runtime.ts")
(defparameter +stubs-file-name+ "server-functions.ts")

;;; ------------------------------------------------------------------ static type inference (best effort)

(defun infer-type (form env)
  "A Lisp type specifier for FORM when it can be determined statically, else NIL."
  (cond
    ((null form) 'null)
    ((eq form t) 'boolean)
    ((member form '(:true :false)) 'boolean)
    ((eq form :undefined) :undefined)
    ((keywordp form) 'string)
    ((stringp form) (list 'string (length form)))
    ((integerp form) 'fixnum)
    ((floatp form) 'float)
    ((characterp form) 'string)
    ((symbolp form) (lookup-type form env))
    ((and (vectorp form) (not (stringp form))) '(array))
    ((consp form)
     (let ((op (sym-name (car form))))
       (cond
         ((null op) nil)
         ((string= op "THE") (second form))
         ((string= op "QUOTE")
          (let ((x (second form)))
            (cond ((and (consp x) (sym-is (car x) "%OBJECT")) 'object)
                  ((or (consp x) (vectorp x)) '(array))
                  ((symbolp x) 'string)
                  (t (infer-type x env)))))
         ((string= op "%OBJECT") 'object)
         ((member op '("VECTOR" "MAKE-ARRAY" "APPEND" "REVERSE" "MAPCAR" "REMOVE-IF" "REMOVE-IF-NOT" "SORT" "COPY-SEQ") :test #'string=) '(array))
         ((member op '("CONCAT" "FORMAT" "PRINC-TO-STRING" "STRING-UPCASE" "STRING-DOWNCASE" "SUBSEQ" "STRING-TRIM" "STRING") :test #'string=)
          (if (and (string= op "FORMAT") (eq (second form) t)) nil 'string))
         ((member op '("+" "-" "*" "/" "MOD" "REM" "EXPT" "1+" "1-" "LENGTH" "FLOOR" "CEILING" "ROUND" "TRUNCATE" "ABS" "MIN" "MAX" "PARSE-INTEGER" "HASH-TABLE-COUNT") :test #'string=)
          (if (and (member op '("+" "-" "*" "1+" "1-" "ABS" "MIN" "MAX") :test #'string=)
                   (every (lambda (a) (member (infer-type a env) '(fixnum integer))) (cdr form)))
              'fixnum
              'number))
         ((member op '("NOT" "NULL" "=" "/=" "<" ">" "<=" ">=" "EQ" "EQL" "EQUAL" "STRING=" "ZEROP" "PLUSP" "MINUSP" "EVENP" "ODDP" "AND" "OR" "INTEGERP" "STRINGP" "NUMBERP") :test #'string=)
          (if (member op '("AND" "OR") :test #'string=) nil 'boolean))
         ((string= op "MAKE-HASH-TABLE") 'hash-table)
         ((string= op "AWAIT")
          (let ((inner (infer-type (second form) env)))
            (if (and (consp inner) (sym-is (car inner) "PROMISE")) (second inner) nil)))
         ((and (not (lookup-var (car form) env)) (manifest-function (car form)))
          (list (lang-sym "PROMISE") (list :schema (getf (manifest-function (car form)) :returns))))
         ((and *module* (gethash (car form) (module-functions *module*)))
          (let ((f (gethash (car form) (module-functions *module*))))
            (and (fn-info-return-type f)
                 (if (fn-info-async-p f) (list (lang-sym "PROMISE") (fn-info-return-type f)) (fn-info-return-type f)))))
         ((and (struct-accessor (car form)) (eq (cdr (struct-accessor (car form))) :make))
          (struct-info-name (car (struct-accessor (car form)))))
         (t nil))))
    (t nil)))

;;; ------------------------------------------------------------------ schema vs type compatibility

(defun type-name-in (type names)
  (and (symbolp type) (member (symbol-name type) names :test #'string=)))

(defun schema-accepts-type-p (schema type &optional literal literal-p)
  "Can a frontend value of static TYPE (and literal value LITERAL when LITERAL-P) satisfy SCHEMA?
Unknown information is accepted (NIL type) -- we only reject provable mismatches."
  (let ((s (normalize-schema schema)))
    (cond
      ((null type) t)
      ((eq s 'any) t)
      ((and (consp type) (eq (car type) :schema)) (equal (normalize-schema (second type)) s))
      ((eq type :undefined) nil)
      ;; (member "a" "b") / (the (member ...) x): every literal of the type must be acceptable
      ((and (consp type) (sym-is (car type) "MEMBER"))
       (every (lambda (lit)
                (cond ((stringp lit) (schema-accepts-type-p s 'string lit t))
                      ((integerp lit) (schema-accepts-type-p s 'fixnum lit t))
                      (t t)))
              (cdr type)))
      ((member s '(string))
       (or (type-name-in type '("STRING" "SYMBOL" "KEYWORD" "CHARACTER"))
           (and (consp type) (sym-is (car type) "STRING"))))
      ((and (consp s) (eq (car s) 'string))
       (and (or (type-name-in type '("STRING" "SYMBOL" "KEYWORD" "CHARACTER")) (and (consp type) (sym-is (car type) "STRING")))
            (or (not literal-p) (not (stringp literal)) (null (second s)) (<= (length literal) (second s)))))
      ((member s '(integer fixnum))
       (and (type-name-in type '("FIXNUM" "INTEGER" "NUMBER" "BIGNUM" "REAL"))
            (or (not literal-p) (not (numberp literal)) (integerp literal))))
      ((and (consp s) (eq (car s) 'integer))
       (and (type-name-in type '("FIXNUM" "INTEGER" "NUMBER" "BIGNUM" "REAL"))
            (or (not literal-p) (not (integerp literal))
                (destructuring-bind (&optional (lo '*) (hi '*)) (cdr s)
                  (and (or (eq lo '*) (>= literal lo)) (or (eq hi '*) (<= literal hi)))))))
      ((member s '(number float))
       (type-name-in type '("FIXNUM" "INTEGER" "NUMBER" "BIGNUM" "REAL" "FLOAT" "DOUBLE-FLOAT" "SINGLE-FLOAT")))
      ((eq s 'boolean) (type-name-in type '("BOOLEAN")))
      ((eq s 'null) (type-name-in type '("NULL")))
      ((and (consp s) (eq (car s) 'or))
       (some (lambda (alt) (schema-accepts-type-p alt type literal literal-p)) (cdr s)))
      ((and (consp s) (eq (car s) 'member))
       (cond ((and literal-p (stringp literal))
              (and (find literal (cdr s) :test (lambda (l x) (or (and (stringp x) (string= l x))
                                                                  (and (keywordp x) (string= l (string-downcase (symbol-name x))))))) t))
             ((and literal-p (integerp literal)) (and (member literal (cdr s)) t))
             (t (type-name-in type '("STRING" "FIXNUM" "INTEGER" "NUMBER" "KEYWORD" "SYMBOL")))))
      ((and (consp s) (member (car s) '(array tuple)))
       (or (type-name-in type '("ARRAY" "VECTOR" "SIMPLE-VECTOR" "SEQUENCE"))
           (and (consp type) (member (sym-name (car type)) '("ARRAY" "VECTOR" "SIMPLE-ARRAY" "SIMPLE-VECTOR") :test #'string=))))
      ((and (consp s) (member (car s) '(map object)))
       (or (type-name-in type '("OBJECT" "HASH-TABLE"))
           (and (consp type) (sym-is (car type) "OBJECT"))))
      ((symbolp s)                                   ; struct
       (or (type-name-in type '("OBJECT"))
           (and (symbolp type) (string= (symbol-name type) (symbol-name s)))
           (and (consp type) (sym-is (car type) "OBJECT"))))
      (t t))))

(defun check-server-call (form fn env)
  "Validate a frontend call FORM against manifest entry FN.  Signals TRANSPILE-ERROR in strict mode."
  (let* ((params (getf fn :params))
         (args (cdr form))
         (required (count-if-not (lambda (p) (getf p :optional)) params))
         (report (if (config-strict-server-calls *config*) #'tfail #'twarn)))
    (when (some (lambda (a) (and (consp a) (sym-is (car a) "SPREAD"))) args)
      (return-from check-server-call nil))
    (when (< (length args) required)
      (funcall report form "server function ~A expects at least ~D argument~:P, got ~D"
               (getf fn :rpc-name) required (length args)))
    (when (> (length args) (length params))
      (funcall report form "server function ~A takes at most ~D argument~:P, got ~D"
               (getf fn :rpc-name) (length params) (length args)))
    (loop for arg in args
          for p in params
          do (let* ((literal-p (or (stringp arg) (numberp arg) (keywordp arg) (null arg) (eq arg t)))
                    (literal (cond ((keywordp arg) (format nil ":~A" (property-name arg))) (t arg)))
                    (type (infer-type arg env)))
               (unless (schema-accepts-type-p (getf p :schema) type literal literal-p)
                 (funcall report form "argument ~A of server function ~A must be ~A, but ~A was given"
                          (property-name (getf p :name)) (getf fn :rpc-name) (schema->ts (getf p :schema))
                          (if literal-p (abbreviate-form arg) (format nil "a value of type ~A" (type->ts type)))))))))

;;; ------------------------------------------------------------------ manifest digest

(defun manifest-digest (manifest)
  "Stable FNV-1a hash (hex) of the manifest's TypeScript-relevant content."
  (let ((text (with-standard-io-syntax
                (let ((*print-case* :downcase) (*print-pretty* nil))
                  (prin1-to-string
                   (list (mapcar (lambda (f) (list (getf f :rpc-name) (getf f :ts-name)
                                                   (mapcar (lambda (p) (list (getf p :json-name) (schema->ts (getf p :schema)) (getf p :optional))) (getf f :params))
                                                   (schema->ts (getf f :returns))))
                                 (getf manifest :functions))
                         (mapcar (lambda (s) (list (getf s :ts-name)
                                                   (mapcar (lambda (f) (list (getf f :json-name) (schema->ts (getf f :schema)) (getf f :optional))) (getf s :fields))))
                                 (getf manifest :structs)))))))
        (hash #xcbf29ce484222325))
    (loop for ch across text
          do (setf hash (logand #xFFFFFFFFFFFFFFFF (* (logxor hash (char-code ch)) #x100000001b3))))
    (format nil "~(~16,'0X~)" hash)))

;;; ------------------------------------------------------------------ TypeScript stubs

(defun generate-server-stubs (manifest &key (runtime-module "./monolisp-runtime"))
  "TypeScript source of server-functions.ts for MANIFEST."
  (with-output-to-string (s)
    (format s "// Generated by Monolisp.ts from the backend server-function manifest. DO NOT EDIT.~%")
    (format s "// manifest-digest: ~A~%" (manifest-digest manifest))
    (format s "import { rpcCall, trimArgs } from ~A;~%~%" (js-string runtime-module))
    (dolist (st (getf manifest :structs))
      (format s "export interface ~A {~%" (getf st :ts-name))
      (dolist (f (getf st :fields))
        (format s "  ~A~:[~;?~]: ~A;~%" (getf f :json-name) (getf f :optional) (schema->ts (getf f :schema))))
      (format s "}~%~%"))
    (dolist (f (getf manifest :functions))
      (let ((params (getf f :params)))
        (when (getf f :doc) (format s "/** ~A */~%" (substitute #\Space #\Newline (getf f :doc))))
        (format s "export function ~A(~{~A~^, ~}): Promise<~A> {~%"
                (getf f :ts-name)
                (mapcar (lambda (p) (format nil "~A~:[~;?~]: ~A" (getf p :json-name) (getf p :optional) (schema->ts (getf p :schema)))) params)
                (schema->ts (getf f :returns)))
        (format s "  return rpcCall<~A>(~A, ~:[[~{~A~^, ~}]~;trimArgs([~{~A~^, ~}])~]);~%}~%~%"
                (schema->ts (getf f :returns))
                (js-string (getf f :rpc-name))
                (some (lambda (p) (getf p :optional)) params)
                (mapcar (lambda (p) (getf p :json-name)) params))))
    (format s "export const serverFunctionNames = [~{~A~^, ~}] as const;~%"
            (mapcar (lambda (f) (js-string (getf f :rpc-name))) (getf manifest :functions)))))

(defun generate-runtime ()
  "TypeScript source of the browser-side JSON-RPC client."
  "// Monolisp.ts browser runtime: a strict JSON-RPC 2.0 client for the server-function endpoint.
// Generated by Monolisp.ts. DO NOT EDIT.

export class RpcError extends Error {
  readonly code: number;
  readonly data: unknown;
  constructor(code: number, message: string, data?: unknown) {
    super(message);
    this.name = \"RpcError\";
    this.code = code;
    this.data = data;
  }
}

export interface RpcClientOptions {
  endpoint?: string;
  headers?: Record<string, string> | (() => Record<string, string>);
  timeoutMs?: number;
  fetch?: typeof fetch;
  credentials?: RequestCredentials;
}

const options: Required<Omit<RpcClientOptions, \"fetch\">> & { fetch: typeof fetch | null } = {
  endpoint: \"/rpc\",
  headers: {},
  timeoutMs: 30000,
  fetch: null,
  credentials: \"same-origin\",
};

export function configureRpc(opts: RpcClientOptions): void {
  if (opts.endpoint !== undefined) options.endpoint = opts.endpoint;
  if (opts.headers !== undefined) options.headers = opts.headers;
  if (opts.timeoutMs !== undefined) options.timeoutMs = opts.timeoutMs;
  if (opts.fetch !== undefined) options.fetch = opts.fetch;
  if (opts.credentials !== undefined) options.credentials = opts.credentials;
}

let nextId = 1;

/** Drop trailing `undefined` optional arguments so the server sees the shortest valid params array. */
export function trimArgs(args: unknown[]): unknown[] {
  let end = args.length;
  while (end > 0 && args[end - 1] === undefined) end--;
  return args.slice(0, end);
}

function isRecord(x: unknown): x is Record<string, unknown> {
  return typeof x === \"object\" && x !== null && !Array.isArray(x);
}

export async function rpcCall<T>(method: string, params: unknown[]): Promise<T> {
  const id = nextId++;
  const body = JSON.stringify({ jsonrpc: \"2.0\", method, params, id });
  const extra = typeof options.headers === \"function\" ? options.headers() : options.headers;
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), options.timeoutMs);
  const fetchImpl = options.fetch ?? fetch;
  let response: Response;
  try {
    response = await fetchImpl(options.endpoint, {
      method: \"POST\",
      headers: { ...extra, \"Content-Type\": \"application/json\" },
      body,
      signal: controller.signal,
      credentials: options.credentials,
    });
  } catch (e) {
    throw new RpcError(-32000, controller.signal.aborted ? \"request timed out\" : \"network error\", e);
  } finally {
    clearTimeout(timer);
  }
  if (!response.ok) {
    throw new RpcError(-32000, `HTTP ${response.status}`, { status: response.status });
  }
  const text = await response.text();
  let json: unknown;
  try {
    json = JSON.parse(text);
  } catch {
    throw new RpcError(-32700, \"invalid JSON in server response\");
  }
  if (!isRecord(json) || json.jsonrpc !== \"2.0\" || json.id !== id) {
    throw new RpcError(-32600, \"malformed JSON-RPC response\");
  }
  if (\"error\" in json) {
    const err = json.error;
    if (isRecord(err) && typeof err.code === \"number\" && typeof err.message === \"string\") {
      throw new RpcError(err.code, err.message, err.data);
    }
    throw new RpcError(-32603, \"malformed error object\");
  }
  if (!(\"result\" in json)) {
    throw new RpcError(-32603, \"response has neither result nor error\");
  }
  return json.result as T;
}

/** Fire-and-forget notification (no id, no response expected). */
export async function rpcNotify(method: string, params: unknown[]): Promise<void> {
  const extra = typeof options.headers === \"function\" ? options.headers() : options.headers;
  const fetchImpl = options.fetch ?? fetch;
  await fetchImpl(options.endpoint, {
    method: \"POST\",
    headers: { ...extra, \"Content-Type\": \"application/json\" },
    body: JSON.stringify({ jsonrpc: \"2.0\", method, params }),
    credentials: options.credentials,
  });
}
")

(defun write-text-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string text out))
  path)

(defun write-alignment-files (out-dir &key (manifest (server-manifest)))
  "Write monolisp-runtime.ts, server-functions.ts and manifest.json into OUT-DIR."
  (let ((dir (uiop:ensure-directory-pathname out-dir)))
    (write-text-file (merge-pathnames +runtime-file-name+ dir) (generate-runtime))
    (write-text-file (merge-pathnames +stubs-file-name+ dir) (generate-server-stubs manifest))
    (write-text-file (merge-pathnames "manifest.json" dir)
                     (let ((*print-pretty* nil)) (json-encode (server-manifest-json))))
    (list (merge-pathnames +runtime-file-name+ dir) (merge-pathnames +stubs-file-name+ dir)
          (merge-pathnames "manifest.json" dir))))

(defun alignment-stale-p (stubs-path &key (manifest (server-manifest)))
  "True when STUBS-PATH is missing or was generated from a different manifest."
  (if (not (probe-file stubs-path))
      t
      (let ((digest (manifest-digest manifest)))
        (with-open-file (in stubs-path :external-format :utf-8)
          (loop for line = (read-line in nil nil)
                for i from 0 below 5
                while line
                do (let ((pos (search "manifest-digest: " line)))
                     (when pos
                       (return (not (string= digest (string-trim " " (subseq line (+ pos 17))))))))
                finally (return t))))))
