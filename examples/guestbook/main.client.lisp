;;;; examples/guestbook/main.client.lisp -- entry point: wires index.html to the backend

(import :from "./monolisp-runtime" :targets (RpcError))
(import :from "browser-aliases" :targets (element input-value set-input-value show-el hide-el on-submit on-click))
(import :from "session" :targets (current-token install-auth-header sign-in-as sign-out-now))
(import :from "view" :targets (render-entry))

(defconstant +page-size+ 5)

(defstruct page-state
  (me nil :type (or string null))           ; signed-in name
  (cursor nil :type (or fixnum null)))      ; nextCursor of the last page loaded

(defvar *state* (make-page-state))

;;; ------------------------------------------------------------------ errors

(defun show-error (message)
  (declare (string message))
  (let ((:const box (element "error")))
    (set-inner-text box message)
    (show-el box)))

(defun explain (e)
  "A sentence for a failed server call.  RpcError.code is the JSON-RPC error code: the standard
ones below, or the application codes passed to rpc-fail in api.server.lisp."
  (declare (|RpcError| e) (returns string))
  (case (.. e code)
    (-32001 (setf (page-state-me *state*) nil)
            (render-session)
            "Please sign in first.")
    (-32002 "Slow down a little.")
    (-32602 "The server rejected that input.")
    (otherwise (.. e message))))            ; rpc-fail messages are written for humans

;; A macro is expanded at build time, inside each function that uses it.
(defmacro reporting-errors (&body body)
  `(handler-case (progn (hide-el (element "error")) ,@body)
     (RpcError (e) (show-error (explain e)))
     (error (e) (show-error (concat "Unexpected error: " e)))))

;;; ------------------------------------------------------------------ rendering

(defun render-session ()
  (let ((:const me (page-state-me *state*)))
    (set-inner-text (element "whoami") (if me (concat "Signed in as " me) "Not signed in"))
    (cond (me (hide-el (element "sign-in-form"))
              (show-el (element "post-form"))
              (show-el (element "sign-out")))
          (t (show-el (element "sign-in-form"))
             (hide-el (element "post-form"))
             (hide-el (element "sign-out"))))))

(async (defun load-entries (more)
  "Reload the first page, or append the next one when MORE is true."
  (declare (boolean more))
  (reporting-errors
    (let ((:const page (await (list-entries +page-size+ (if more (page-state-cursor *state*) nil))))
          (:const ul (element "entries")))
      (unless more (set-inner-html ul ""))
      (for-each (entry (.. page entries))
        (append-child ul (render-entry entry (page-state-me *state*) delete-and-reload)))
      (setf (page-state-cursor *state*) (.. page next-cursor))
      (if (.. page next-cursor)
          (show-el (element "more"))
          (hide-el (element "more")))))))

(async (defun delete-and-reload (id)
  (declare (fixnum id))
  (reporting-errors
    (when (await (delete-entry id))
      (await (load-entries :false))))))

;;; ------------------------------------------------------------------ wiring

(async (defun main ()
  (install-auth-header)
  (when (current-token)
    ;; the server is the source of truth: an expired token just yields null
    (setf (page-state-me *state*) (await (whoami))))
  (render-session)

  (on-submit (element "sign-in-form")
    (async (lambda (e)
      (prevent-default e)
      (reporting-errors
        (setf (page-state-me *state*) (await (sign-in-as (input-value "name"))))
        (render-session)
        (await (load-entries :false))))))

  (on-submit (element "post-form")
    (async (lambda (e)
      (prevent-default e)
      (reporting-errors
        ;; the cast narrows the <select> value to the server's enum type
        (await (post-entry (input-value "message") (the (member "happy" "neutral" "sad") (input-value "mood"))))
        (set-input-value "message" "")
        (await (load-entries :false))))))

  (on-click (element "sign-out")
    (async (lambda ()
      (await (sign-out-now))
      (setf (page-state-me *state*) nil)
      (render-session)
      (await (load-entries :false)))))

  (on-click (element "more") (lambda () (load-entries t)))
  (await (load-entries :false))))

(main)
