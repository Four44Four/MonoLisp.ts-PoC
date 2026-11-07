;;;; examples/guestbook/browser-aliases.client.lisp -- inline shorthands for the browser APIs this app uses
;;;;
;;;; An alias file (docs/inline-aliases.md): every exported inline defun is a compile-time rewrite.
;;;; (input-value "message") is pasted into the caller as
;;;; (document.getElementById("message") as HTMLInputElement).value, so this module adds nothing to the
;;;; JavaScript bundle and importers never load it at runtime.

(declaim (inline element input-el input-value set-input-value show-el hide-el on-submit on-click
                 storage-get storage-set storage-remove format-time))

;; `the` is a TypeScript `as` cast.  Aliases are pasted into the caller, so the cast is exactly
;; what tsc sees at every call site.
(export (defun element (id) (the |HTMLElement| (document.getElementById id))))
(export (defun input-el (id) (the |HTMLInputElement| (document.getElementById id))))

;; aliases may use other aliases; importers receive them transitively
(export (defun input-value (id) (.. (input-el id) value)))
(export (defun set-input-value (id value) (setf (.. (input-el id) value) value)))

;; `nil` is null in the frontend dialect; `:false` is the literal false
(export (defun show-el (el) (setf (.. el hidden) :false)))
(export (defun hide-el (el) (setf (.. el hidden) t)))

(export (defun on-submit (el handler) (.. el (addEventListener "submit" handler))))
(export (defun on-click (el handler) (.. el (addEventListener "click" handler))))

(export (defun storage-get (key) (localStorage.getItem key)))
(export (defun storage-set (key value) (localStorage.setItem key value)))
(export (defun storage-remove (key) (localStorage.removeItem key)))

;; :undefined in the middle of an argument list is passed through (the browser's default locale)
(export (defun format-time (ms)
  (.. (new Intl.DateTimeFormat :undefined '{}(:dateStyle "medium" :timeStyle "short")) (format ms))))
