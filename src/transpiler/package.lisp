;;;; src/transpiler/package.lisp

(defpackage :monolisp.transpiler
  (:use :cl :monolisp.rpc :monolisp.json)
  (:nicknames :mlts)
  (:export
   ;; conditions
   #:transpile-error #:transpile-error-form #:transpile-error-message #:transpile-error-file
   #:transpile-warning
   ;; reader
   #:read-client-source #:make-client-package #:*client-lang-package*
   ;; names / types
   #:mangle-name #:type->ts
   ;; compiler entry points
   #:transpile-string #:transpile-forms #:transpile-file #:transpile-project #:compile-module
   #:make-transpile-config #:transpile-config #:config-react-p #:config-manifest #:config-strict-server-calls
   #:module #:module-name #:module-tsx-p #:module-output #:module-warnings #:module-exports #:module-imports
   #:module-server-calls
   ;; alignment
   #:generate-server-stubs #:generate-runtime #:write-alignment-files #:alignment-stale-p
   #:manifest-digest #:check-server-call #:+runtime-file-name+ #:+stubs-file-name+
   ;; loop
   #:expand-loop
   ;; prelude
   #:*prelude-source* #:prelude-inlines))

(defpackage :monolisp.client-lang
  (:use)
  (:documentation "Names recognised by the transpiler that are not part of Common Lisp.")
  ;; IMPORT and EXPORT are inherited from CL (the transpiler dispatches on symbol names).
  (:export #:async #:await #:for-each #:spread #:nil-or #:.. #:export-default
           #:new #:%object #:as-html-string #:concat #:instanceof #:typeof #:void #:delete-property
           #:%while #:%for-of #:%for #:%break #:%continue #:%label #:%result #:this #:template
           #:server-call #:defcomponent #:fragment #:%jsx #:%raw-js #:undefined-p #:defined-p
           #:array-push #:array-pop #:await-all #:obj #:->))
