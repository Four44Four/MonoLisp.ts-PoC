;;;; examples/todo/api.server.lisp -- backend of the example application
;;;;
;;;; Every DEFINE-SERVER-FUNCTION becomes a JSON-RPC 2.0 method *and* a typed TypeScript stub
;;;; (build/server-functions.ts) the frontend calls like an ordinary function.

(in-package :monolisp-user)

(define-rpc-struct todo
  (id fixnum)
  (title (string 200))
  (done boolean)
  (tags (array (string 32) 16) :optional t))

(defvar *todos* (make-array 0 :adjustable t :fill-pointer 0))
(defvar *next-id* 1)

(define-server-function list-todos ()
    (:returns (array todo) :doc "All todos, oldest first.")
  (coerce *todos* 'list))

(define-server-function add-todo ((title (string 200)) &optional ((tags (array (string 32) 16)) nil))
    (:returns todo :doc "Create a todo and return it.")
  (let ((todo (make-todo :id *next-id* :title title :done nil :tags (and tags (coerce tags 'list)))))
    (incf *next-id*)
    (vector-push-extend todo *todos*)
    todo))

(define-server-function toggle-todo ((id fixnum))
    (:returns (nullable todo))
  (let ((todo (find id *todos* :key #'todo-id)))
    (when todo (setf (todo-done todo) (not (todo-done todo))))
    todo))

(define-server-function remove-todo ((id fixnum))
    (:returns boolean)
  (let ((before (length *todos*)))
    (setf *todos* (delete id *todos* :key #'todo-id))
    (/= before (length *todos*))))

(define-server-function stats ()
    (:returns (object (total fixnum) (done fixnum) (ratio float)))
  (let ((total (length *todos*))
        (done (count-if #'todo-done *todos*)))
    (list :total total :done done :ratio (if (zerop total) 0d0 (/ done (float total 1d0))))))

(define-server-function slow-search ((query (string 64)) &optional ((limit (integer 1 50)) 10))
    (:returns (array todo) :timeout-ms 2000 :doc "Simulates slow async work on the event loop.")
  ;; Server functions may return promises; the dispatcher awaits them without blocking the loop.
  (then (delay 50)
        (lambda (ignored)
          (declare (ignore ignored))
          (let ((hits (remove-if-not (lambda (td) (search query (todo-title td) :test #'char-equal)) *todos*)))
            (coerce (subseq hits 0 (min limit (length hits))) 'list)))))

(define-server-function admin-reset ()
    (:returns boolean :auth :authenticated)
  (setf (fill-pointer *todos*) 0 *next-id* 1)
  t)
