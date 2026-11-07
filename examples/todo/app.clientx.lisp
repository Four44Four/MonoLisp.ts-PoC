;;;; examples/todo/app.clientx.lisp -- React frontend calling the backend as plain functions
;;;;
;;;; Backend functions (list-todos, add-todo, ...) are called like local Lisp functions.  The
;;;; transpiler checks the calls against the backend manifest and imports the typed stubs; tsc
;;;; then verifies everything end to end.

(import :from "react" :targets (useState useEffect useCallback))
(import :from "utils" :targets (format-count matches-filter percent make-filter-state (:type filter-state)))
(import :from :server :targets (list-todos add-todo))   ; explicit import is optional: others are auto-imported

(declaim (type (array string) +suggestions+))
(defconstant +suggestions+ (vector "Write docs" "Ship it" "Celebrate"))

(defstruct view-state
  (todos (vector) :type (array any))
  (loading nil :type boolean)
  (error nil :type (or string null)))

(export (async (defun refresh (set-state)
  (declare (function set-state))
  (handler-case
      (let ((todos (await (list-todos))))
        (funcall set-state (make-view-state :todos todos :loading nil :error nil)))
    (error (e)
      (funcall set-state (make-view-state :todos (vector) :loading nil :error (concat "failed: " e))))))))

(export (async (defun create (title set-state)
  (declare (string title) (function set-state))
  (when (> (length (trim title)) 0)
    ;; typed call: title must be a string of at most 200 chars, tags an optional string array
    (await (add-todo title (vector "ui")))
    (await (refresh set-state))))))

(defun todo-row (todo on-toggle on-remove)
  (as-html-string
    (:li :key (.. todo id) :class (if (.. todo done) "done" "open")
      (:input :type "checkbox" :checked (.. todo done) :onChange (lambda (e) (funcall on-toggle (.. todo id))))
      (:span :class "title" (.. todo title))
      (map (tag (nil-or (.. todo tags) (vector)))
        (:em :key tag :class "tag" tag))
      (:button :onClick (lambda () (funcall on-remove (.. todo id))) "x"))))

(export-default (defun App ()
  ;; React hooks return tuples: a `:const :multi` let binding is `const [state, setState] = useState(...)`
  (let* ((:const :multi (state set-state) (useState (make-view-state :loading t)))
         (:const :multi (filter set-filter) (useState (make-filter-state)))
         (:const :multi (draft set-draft) (useState ""))
         (on-toggle (useCallback (async (lambda (id)
                                          (declare (fixnum id))
                                          (await (toggle-todo id))
                                          (await (refresh set-state))))
                                 (vector)))
         (on-remove (useCallback (async (lambda (id)
                                          (declare (fixnum id))
                                          (when (await (remove-todo id))
                                            (await (refresh set-state)))))
                                 (vector)))
         (visible (remove-if-not (lambda (td) (matches-filter (.. td title) (.. td done) filter))
                                 (view-state-todos state))))
    (useEffect (lambda () (refresh set-state) :undefined) (vector))
    (as-html-string
      (:div :class "app"
        (:h1 "Monolisp todos")
        (when (view-state-error state)
          (:p :class "error" (view-state-error state)))
        (:form :onSubmit (lambda (e)
                           (prevent-default e)
                           (create draft set-state)
                           (funcall set-draft ""))
          (:input :value draft :placeholder "What needs doing?"
                  :onChange (lambda (e) (funcall set-draft (.. e target value))))
          (:button :type "submit" :disabled (view-state-loading state) "Add"))
        (:input :value (filter-state-query filter) :placeholder "filter"
                :onChange (lambda (e)
                            (funcall set-filter (make-filter-state :query (.. e target value)
                                                                   :show-done (filter-state-show-done filter)))))
        (:ul (map (todo visible)
               (todo-row todo on-toggle on-remove)))
        (:footer
          (format-count (length visible))
          (:ul :class "suggestions"
            (map (s +suggestions+ i)
              (:li :key i (:button :onClick (lambda () (create s set-state)) s)))))
        (:StatsPanel))))))

(defun StatsPanel ()
  (let ((:const :multi (text set-text) (useState "...")))
    (useEffect (lambda ()
                 (let ((cancelled nil))
                   (.. (stats) (then (lambda (s) (unless cancelled (funcall set-text (percent (.. s ratio)))))))
                   ;; React cleanup functions must return void: end with :undefined
                   (lambda () (setf cancelled t) :undefined)))
               (vector))
    (as-html-string (:p :class "stats" "Done: " text))))
