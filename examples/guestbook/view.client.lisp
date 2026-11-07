;;;; examples/guestbook/view.client.lisp -- DOM rendering with the prelude's built-in aliases

(import :from :server :targets ((:type guest-entry)))   ; the interface generated from define-rpc-struct
(import :from "browser-aliases" :targets (format-time))

(export (defun mood-icon (mood)
  (declare (string mood) (returns string))
  (case mood
    ("happy" "😊")
    ("sad" "😢")
    (otherwise "😐"))))

(export (defun render-entry (entry me on-delete)
  "One <li> per entry.  ME is the signed-in name (or null); only your own entries get a button."
  (declare (guest-entry entry) ((or string null) me) (function on-delete) (returns |HTMLLIElement|))
  (let ((:const li (doc-create-element "li"))
        (:const header (doc-create-element "header"))
        (:const body (doc-create-element "p")))
    (add-class li "entry" (concat "mood-" (.. entry mood)))
    (set-inner-text header (concat (mood-icon (.. entry mood)) " " (.. entry author)
                                   " · " (format-time (.. entry created-at))))
    (set-inner-text body (.. entry message))
    (append-child li header)
    (append-child li body)
    (when (equal me (.. entry author))
      (let ((:const button (doc-create-element "button")))
        (set-inner-text button "delete")
        (add-event-listener button "click" (lambda () (funcall on-delete (.. entry id))))
        (append-child li button)))
    li)))
