;;;; examples/dashboard/react-aliases.client.lisp -- Lisp-style names for the React hooks this app uses
;;;;
;;;; An alias file for an npm library (docs/inline-aliases.md).  Importers get the aliases at build
;;;; time *and* the `import { useState, ... } from "react"` lines they need, so a component only
;;;; imports from here.

(import :from "react" :targets (useState useEffect useMemo useRef))

(declaim (inline use-state use-effect use-memo use-ref ref-current))

(export (defun use-state (initial) (useState initial)))

;; &rest + spread turns the remaining arguments into the dependency array:
;;   (use-effect f)        ==>  useEffect(f, [])
;;   (use-effect f a b)    ==>  useEffect(f, [a, b])
(export (defun use-effect (fn &rest deps) (useEffect fn (vector (spread deps)))))
(export (defun use-memo (fn &rest deps) (useMemo fn (vector (spread deps)))))

(export (defun use-ref (initial) (useRef initial)))
(export (defun ref-current (ref) (.. ref current)))
