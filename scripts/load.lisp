;;;; scripts/load.lisp -- register the Monolisp systems with ASDF (no Quicklisp required).
;;;;
;;;; Usage:  sbcl --non-interactive --load scripts/load.lisp --eval '(asdf:load-system "monolisp")'

(require :asdf)

(defvar cl-user::*monolisp-root*
  (let ((this (or *load-truename* *compile-file-truename*)))
    (merge-pathnames "../" (make-pathname :name nil :type nil :defaults this))))

(let ((root (truename cl-user::*monolisp-root*)))
  (pushnew root asdf:*central-registry* :test #'equal)
  (pushnew (merge-pathnames "src/async/" root) asdf:*central-registry* :test #'equal))

;; Security posture for everything loaded afterwards: never evaluate reader-time code.
(setf *read-eval* nil)
