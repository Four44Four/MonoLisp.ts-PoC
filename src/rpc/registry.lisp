;;;; src/rpc/registry.lisp -- DEFINE-SERVER-FUNCTION and the server-function registry
;;;;
;;;; A server function is the *only* thing the outside world can call.  The macro name and shape
;;;; are deliberately loud:
;;;;
;;;;   (define-server-function add-numbers ((a fixnum) (b fixnum) &optional ((scale fixnum) 1))
;;;;       (:returns fixnum :auth :public :doc "Adds two numbers.")
;;;;     (* scale (+ a b)))
;;;;
;;;; Every parameter and the return value carry a schema (see types.lisp).  The same declarations
;;;; are exported through SERVER-MANIFEST so the frontend transpiler can type-check calls and emit
;;;; typed TypeScript stubs -- the backend declaration is the single source of truth.

(in-package :monolisp.rpc)

(defstruct (param-spec (:conc-name param-))
  name json-name schema optional-p default)

(defstruct server-function
  name                      ; Lisp symbol
  rpc-name                  ; public method name (string)
  params                    ; list of PARAM-SPEC
  returns                   ; schema
  (auth :public)            ; :public | :authenticated | (function context) -> boolean
  doc
  (timeout-ms nil)
  function
  source)

(defvar *server-functions* (make-hash-table :test 'equal)
  "rpc-name -> SERVER-FUNCTION")

(defun find-server-function (rpc-name)
  (gethash rpc-name *server-functions*))

(defun list-server-functions ()
  (sort (loop for f being the hash-values of *server-functions* collect f)
        #'string< :key #'server-function-rpc-name))

(defun clear-server-functions () (clrhash *server-functions*))

(defun valid-rpc-name-p (name)
  (and (stringp name)
       (<= 1 (length name) 128)
       (every (lambda (c) (or (alphanumericp c) (find c "_.-"))) name)
       (alpha-char-p (char name 0))
       (not (and (>= (length name) 4) (string-equal "rpc." name :end2 4)))))

(defun parse-server-params (lambda-list)
  "Returns (values param-specs defun-lambda-list)."
  (let ((specs '()) (ll '()) (optional nil))
    (dolist (item lambda-list)
      (cond ((eq item '&optional) (setf optional t) (push '&optional ll))
            ((member item '(&rest &key &aux &body))
             (error "define-server-function: ~A is not allowed; use &optional with typed defaults" item))
            (optional
             (destructuring-bind ((name schema) &optional default) (if (consp (first item)) item (list item))
               (push (make-param-spec :name name :json-name (lisp-name->camel name) :schema schema
                                      :optional-p t :default default)
                     specs)
               (push (list name default) ll)))
            (t
             (destructuring-bind (name schema) item
               (push (make-param-spec :name name :json-name (lisp-name->camel name) :schema schema) specs)
               (push name ll)))))
    (values (nreverse specs) (nreverse ll))))

(defmacro define-server-function (name lambda-list (&key returns (auth :public) rpc-name doc timeout-ms) &body body)
  "Define NAME as an ordinary function *and* expose it as a JSON-RPC method.

LAMBDA-LIST: ((param schema)... [&optional ((param schema) default)...])
RETURNS: schema of the result (required).  AUTH: :public, :authenticated or a predicate symbol."
  (unless returns (error "define-server-function ~A: :returns schema is required" name))
  (multiple-value-bind (specs ll) (parse-server-params lambda-list)
    (let ((rpc-name (or rpc-name (string-downcase (symbol-name name)))))
      (unless (valid-rpc-name-p rpc-name)
        (error "define-server-function ~A: invalid rpc name ~S" name rpc-name))
      `(progn
         (defun ,name ,ll
           ,@(when doc (list doc))
           ,@body)
         (register-server-function
          (make-server-function
           :name ',name
           :rpc-name ,rpc-name
           :params (list ,@(mapcar (lambda (p)
                                     `(make-param-spec :name ',(param-name p) :json-name ,(param-json-name p)
                                                       :schema ',(param-schema p) :optional-p ,(param-optional-p p)
                                                       :default ',(param-default p)))
                                   specs))
           :returns ',returns
           :auth ,(if (keywordp auth) auth `',auth)
           :doc ,doc
           :timeout-ms ,timeout-ms
           :function #',name
           :source ,(or *compile-file-truename* *load-truename*)))
         ',name))))

(defun register-server-function (sf)
  (dolist (p (server-function-params sf))
    (unless (schema-valid-p (param-schema p))
      (error "server function ~A: parameter ~A has invalid schema ~S"
             (server-function-rpc-name sf) (param-name p) (param-schema p))))
  (unless (schema-valid-p (server-function-returns sf))
    (error "server function ~A: invalid :returns schema ~S" (server-function-rpc-name sf) (server-function-returns sf)))
  (unless (member (server-function-auth sf) '(:public :authenticated))
    ;; a predicate may be defined later in the file; symbols are resolved at call time
    (unless (or (functionp (server-function-auth sf)) (and (symbolp (server-function-auth sf)) (not (keywordp (server-function-auth sf)))))
      (error "server function ~A: :auth must be :public, :authenticated or a predicate" (server-function-rpc-name sf))))
  (setf (gethash (server-function-rpc-name sf) *server-functions*) sf)
  sf)

;;; ------------------------------------------------------------------ manifest

(defun server-function-ts-signature (sf)
  "TypeScript signature text, e.g. addNumbers(a: number, b: number, scale?: number): Promise<number>"
  (format nil "~A(~{~A~^, ~}): Promise<~A>"
          (lisp-name->camel (server-function-name sf))
          (mapcar (lambda (p) (format nil "~A~:[~;?~]: ~A" (param-json-name p) (param-optional-p p) (schema->ts (param-schema p))))
                  (server-function-params sf))
          (schema->ts (server-function-returns sf))))

(defun server-manifest ()
  "Plist describing every exposed function and struct; the contract shared with the frontend build."
  (list :version 1
        :functions
        (mapcar (lambda (sf)
                  (list :name (server-function-name sf)
                        :rpc-name (server-function-rpc-name sf)
                        :ts-name (lisp-name->camel (server-function-name sf))
                        :params (mapcar (lambda (p)
                                          (list :name (param-name p) :json-name (param-json-name p)
                                                :schema (normalize-schema (param-schema p))
                                                :optional (param-optional-p p)))
                                        (server-function-params sf))
                        :returns (normalize-schema (server-function-returns sf))
                        :auth (let ((a (server-function-auth sf))) (if (keywordp a) a :custom))
                        :doc (server-function-doc sf)))
                (list-server-functions))
        :structs
        (mapcar (lambda (s)
                  (list :name (rpc-struct-name s)
                        :ts-name (rpc-struct-ts-name s)
                        :fields (mapcar (lambda (f)
                                          (list :name (field-name f) :json-name (field-json-name f)
                                                :schema (normalize-schema (field-schema f))
                                                :optional (field-optional-p f)))
                                        (rpc-struct-fields s))))
                (sort (list-rpc-structs) #'string< :key #'rpc-struct-ts-name))))

(defun server-manifest-json ()
  "The manifest as a JSON-OBJECT (for manifest.json)."
  (let ((m (server-manifest)))
    (make-json-object
     "version" (getf m :version)
     "functions"
     (coerce (mapcar (lambda (f)
                       (make-json-object
                        "rpcName" (getf f :rpc-name)
                        "tsName" (getf f :ts-name)
                        "params" (coerce (mapcar (lambda (p)
                                                   (make-json-object "name" (getf p :json-name)
                                                                     "schema" (schema->json (getf p :schema))
                                                                     "ts" (schema->ts (getf p :schema))
                                                                     "optional" (json-bool (getf p :optional))))
                                                 (getf f :params))
                                         'simple-vector)
                        "returns" (schema->json (getf f :returns))
                        "returnsTs" (schema->ts (getf f :returns))
                        "auth" (string-downcase (symbol-name (getf f :auth)))
                        "doc" (or (getf f :doc) :null)))
                     (getf m :functions))
             'simple-vector)
     "structs"
     (coerce (mapcar (lambda (s)
                       (make-json-object
                        "tsName" (getf s :ts-name)
                        "fields" (coerce (mapcar (lambda (f)
                                                   (make-json-object "name" (getf f :json-name)
                                                                     "schema" (schema->json (getf f :schema))
                                                                     "ts" (schema->ts (getf f :schema))
                                                                     "optional" (json-bool (getf f :optional))))
                                                 (getf s :fields))
                                         'simple-vector)))
                     (getf m :structs))
             'simple-vector))))
