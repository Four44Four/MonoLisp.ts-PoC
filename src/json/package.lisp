;;;; src/json/package.lisp

(defpackage :monolisp.json
  (:use :cl)
  (:nicknames :mlj)
  (:export
   ;; values
   #:json-object #:json-object-p #:make-json-object #:json-get #:json-set #:json-keys #:json-object-count
   #:json-has-key-p #:json-ref #:json-object-alist #:json-null-p #:json-true-p #:json-false-p #:json-bool
   ;; parsing
   #:json-parse #:json-parse-octets #:json-parse-error #:json-parse-error-position #:json-parse-error-message
   #:json-limits #:make-json-limits #:*default-limits* #:copy-json-limits
   #:json-limits-max-depth #:json-limits-max-bytes #:json-limits-max-string-bytes
   #:json-limits-max-number-chars #:json-limits-max-integer-digits #:json-limits-max-exponent
   #:json-limits-max-array-items #:json-limits-max-object-keys #:json-limits-max-total-values
   #:json-limits-duplicate-keys
   ;; encoding
   #:json-encode #:json-encode-to-octets #:json-encode-error #:encode-json-value #:*escape-html*
   #:json-encode-object-slots))
