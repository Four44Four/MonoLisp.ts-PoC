;;;; examples/guestbook/session.client.lisp -- the bearer token: kept in localStorage, sent on every call

(import :from "./monolisp-runtime" :targets (configureRpc))
(import :from "browser-aliases" :targets (storage-get storage-set storage-remove))

(defconstant +token-key+ "guestbook.token")

(export (defun current-token ()
  (declare (returns (or string null)))
  (storage-get +token-key+)))

(export (defun install-auth-header ()
  "The runtime calls the headers function before each request, so signing in or out needs no
re-configuration."
  (configureRpc '{}(:headers (lambda ()
                               ;; a declared return type becomes the arrow function's annotation
                               (declare (returns (record string string)))
                               (let ((:const token (current-token)))
                                 (if token '{}(:Authorization (concat "Bearer " token)) '{})))))))

(export (async (defun sign-in-as (name)
  "Exchange a display name for a session token.  Returns the name the server accepted."
  (declare (string name) (returns string))
  (let ((:const session (await (sign-in name))))   ; sign-in is a server function
    (storage-set +token-key+ (.. session token))
    (.. session name)))))

(export (async (defun sign-out-now ()
  (handler-case (await (sign-out))
    (error (e) (console-warn "sign-out failed; forgetting the token anyway" e)))
  (storage-remove +token-key+))))
