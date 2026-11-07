;;;; src/transpiler/conditions.lisp

(in-package :monolisp.transpiler)

(defvar *current-file* nil "File being transpiled, for error messages.")
(defvar *current-toplevel* nil "Top-level form being transpiled, for error messages.")

(define-condition transpile-error (error)
  ((message :initarg :message :reader transpile-error-message)
   (form :initarg :form :initform nil :reader transpile-error-form)
   (file :initarg :file :initform *current-file* :reader transpile-error-file))
  (:report (lambda (c s)
             (format s "~@[~A: ~]~A~@[~%  in form: ~S~]"
                     (transpile-error-file c)
                     (transpile-error-message c)
                     (transpile-error-form c)))))

(define-condition transpile-warning (warning)
  ((message :initarg :message :reader transpile-warning-message)
   (form :initarg :form :initform nil :reader transpile-warning-form))
  (:report (lambda (c s) (format s "~A~@[ in ~S~]" (transpile-warning-message c) (transpile-warning-form c)))))

(defun tfail (form fmt &rest args)
  (error 'transpile-error :message (apply #'format nil fmt args) :form (abbreviate-form form)
                          :file *current-file*))

(defun twarn (form fmt &rest args)
  (warn 'transpile-warning :message (apply #'format nil fmt args) :form (abbreviate-form form)))

(defun abbreviate-form (form)
  "Keep error messages short: never embed huge forms."
  (let ((*print-length* 8) (*print-level* 4) (*print-case* :downcase))
    (handler-case (let ((s (prin1-to-string form)))
                    (if (> (length s) 200) (concatenate 'string (subseq s 0 197) "...") s))
      (error () "<unprintable form>"))))
