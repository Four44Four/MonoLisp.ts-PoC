;;;; examples/dashboard/intl-aliases.client.lisp -- number and time formatting with Intl
;;;;
;;;; Formatters are expensive to construct, so they are real exported constants and the aliases
;;;; refer to them.  An importer of fmt-percent automatically gets
;;;; `import { PERCENT } from "./intl-aliases"`: this module *is* loaded at runtime, once.

(export (defconstant +percent+ (new Intl.NumberFormat :undefined '{}(:style "percent" :maximumFractionDigits 1))))
(export (defconstant +millis+ (new Intl.NumberFormat :undefined '{}(:style "unit" :unit "millisecond"
                                                                    :maximumFractionDigits 0))))
(export (defconstant +clock+ (new Intl.DateTimeFormat :undefined '{}(:timeStyle "medium"))))

(declaim (inline fmt-percent fmt-ms fmt-clock))

(export (defun fmt-percent (ratio) (.. +percent+ (format ratio))))
(export (defun fmt-ms (ms) (.. +millis+ (format ms))))
(export (defun fmt-clock (epoch-ms) (.. +clock+ (format epoch-ms))))
