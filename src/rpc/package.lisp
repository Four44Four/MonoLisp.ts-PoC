;;;; src/rpc/package.lisp

(defpackage :monolisp.rpc
  (:use :cl :monolisp.json :monolisp.async)
  (:nicknames :mlrpc)
  (:export
   ;; names
   #:lisp-name->camel #:lisp-name->pascal #:camel->lisp-name
   ;; schemas
   #:schema-error #:schema-error-path #:schema-error-expected
   #:validate-value #:encode-value #:schema->ts #:schema->json #:normalize-schema #:schema-valid-p
   #:define-rpc-struct #:find-rpc-struct #:rpc-struct #:rpc-struct-name #:rpc-struct-fields #:rpc-struct-ts-name
   #:rpc-struct-field #:field-name #:field-schema #:field-optional-p #:field-json-name
   #:list-rpc-structs #:clear-rpc-structs
   ;; security
   #:rpc-limits #:make-rpc-limits #:*rpc-limits* #:copy-rpc-limits
   #:rpc-limits-max-batch #:rpc-limits-max-method-length #:rpc-limits-max-id-length
   #:rpc-limits-json-limits #:rpc-limits-call-timeout-ms #:rpc-limits-max-payload-bytes
   #:rpc-context #:make-rpc-context #:context-remote-address #:context-headers #:context-principal
   #:context-header #:context-request-id #:context-started-at #:context-authenticated-p
   #:*authenticator* #:*error-logger* #:*debug-mode* #:*request-context*
   #:rate-limiter #:make-rate-limiter #:rate-limit-allow-p #:rate-limiter-prune #:rate-limiter-count
   #:constant-time-equal #:random-token
   ;; registry
   #:define-server-function #:server-function #:server-function-name #:server-function-rpc-name
   #:server-function-params #:server-function-returns #:server-function-auth #:server-function-doc
   #:server-function-timeout-ms #:server-function-function #:server-function-p
   #:param-spec #:param-name #:param-schema #:param-optional-p #:param-default #:param-json-name
   #:find-server-function #:list-server-functions #:clear-server-functions #:server-manifest
   #:server-manifest-json #:server-function-ts-signature
   ;; dispatcher
   #:handle-rpc-octets #:handle-rpc-string #:handle-rpc-sync #:rpc-error #:rpc-error-code #:rpc-error-message
   #:rpc-error-data #:rpc-fail
   #:+parse-error+ #:+invalid-request+ #:+method-not-found+ #:+invalid-params+ #:+internal-error+
   #:+unauthorized+ #:+rate-limited+ #:+timeout+
   #:rpc-stats #:*rpc-stats*))
