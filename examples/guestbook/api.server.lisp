;;;; examples/guestbook/api.server.lisp -- backend: sessions, auth guards, typed errors, pagination
;;;;
;;;; Shows the parts of the server-function boundary the todo example does not: an *AUTHENTICATOR*
;;;; that turns a bearer token into a principal, :auth :authenticated and :auth <predicate> guards,
;;;; application errors with stable codes (RPC-FAIL), enum and nullable schemas, cursor pagination.

(in-package :monolisp-user)

(define-rpc-struct guest-entry
  (id fixnum)
  (author (string 40))
  (message (string 280))
  (mood (member "happy" "neutral" "sad"))   ; TypeScript: "happy" | "neutral" | "sad"
  (created-at float))                       ; epoch milliseconds

(define-rpc-struct guest-session
  (token (string 64))
  (name (string 40)))

(defvar *guest-entries* '() "Newest first.")
(defvar *guest-sessions* (make-hash-table :test 'equal) "Bearer token -> display name.")
(defvar *next-guest-entry-id* 1)

;;; Application error codes.  JSON-RPC 2.0 reserves -32000..-32099 for the server; the frontend
;;; switches on RpcError.code, so these are part of the API just like the schemas.
(defconstant +bad-name+ -32010)
(defconstant +empty-message+ -32011)
(defconstant +duplicate-message+ -32012)
(defconstant +not-your-entry+ -32013)

;;; ------------------------------------------------------------------ authentication

(defun bearer-token (context)
  "The token from an `Authorization: Bearer <token>` header, or NIL."
  (let ((header (context-header context "authorization")))
    (and header (> (length header) 7) (string= "Bearer " header :end2 7)
         (subseq header 7))))

;; Runs once per request, before any guarded function.  Whatever it returns becomes
;; (context-principal *request-context*); NIL means anonymous.  It must not signal.
(setf *authenticator*
      (lambda (context)
        (let ((token (bearer-token context)))
          (and token (= (length token) 64) (gethash token *guest-sessions*)))))

(defvar *moderator-key* (sb-ext:posix-getenv "GUESTBOOK_MODERATOR_KEY")
  "Shared secret for moderator-only functions.  When unset nobody is a moderator (fail closed).")

(defun moderator-p (context)
  "A custom :auth predicate: the request must carry the moderator key in X-Moderator-Key."
  (let ((key (context-header context "x-moderator-key")))
    (and *moderator-key* key (constant-time-equal key *moderator-key*))))

(defun valid-display-name-p (name)
  (let ((trimmed (string-trim " " name)))
    (and (plusp (length trimmed))
         (every (lambda (c) (or (alphanumericp c) (find c " _-"))) trimmed))))

;;; ------------------------------------------------------------------ server functions

(define-server-function sign-in ((name (string 40)))
    (:returns guest-session :doc "Start a session; send the token back as `Authorization: Bearer <token>`.")
  (unless (valid-display-name-p name)
    (rpc-fail "Names may only contain letters, digits, spaces, '_' and '-'." :code +bad-name+))
  (let ((token (random-token 32))
        (display (string-trim " " name)))
    (setf (gethash token *guest-sessions*) display)
    (make-guest-session :token token :name display)))

(define-server-function sign-out ()
    (:returns boolean :auth :authenticated)
  (remhash (bearer-token *request-context*) *guest-sessions*))

(define-server-function whoami ()
    (:returns (nullable (string 40)) :doc "The signed-in name, or null for anonymous callers.")
  (context-principal *request-context*))

(define-server-function post-entry ((message (string 280))
                                    &optional ((mood (member "happy" "neutral" "sad")) "neutral"))
    (:returns guest-entry :auth :authenticated)
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) message))
        (author (context-principal *request-context*)))
    (when (zerop (length text))
      (rpc-fail "Say something first." :code +empty-message+))
    (let ((duplicate (find-if (lambda (e) (and (string= (guest-entry-author e) author)
                                               (string= (guest-entry-message e) text)))
                              *guest-entries*)))
      (when duplicate
        ;; :data travels to the client as RpcError.data
        (rpc-fail "You already posted that." :code +duplicate-message+ :data (guest-entry-id duplicate))))
    (let ((entry (make-guest-entry :id *next-guest-entry-id* :author author :message text
                                   :mood mood :created-at (now-ms))))
      (incf *next-guest-entry-id*)
      (push entry *guest-entries*)
      entry)))

(define-server-function list-entries (&optional ((limit (integer 1 50)) 10)
                                                ((before-id (nullable fixnum)) nil))
    (:returns (object (entries (array guest-entry)) (next-cursor (nullable fixnum)))
     :doc "Newest first.  Pass the previous page's nextCursor as beforeId for older entries.")
  (let* ((older (if before-id
                    (remove-if-not (lambda (e) (< (guest-entry-id e) before-id)) *guest-entries*)
                    *guest-entries*))
         (page (subseq older 0 (min limit (length older)))))
    (list :entries page
          :next-cursor (and (> (length older) limit) (guest-entry-id (car (last page)))))))

(define-server-function delete-entry ((id fixnum))
    (:returns boolean :auth :authenticated :doc "Delete one of your own entries.")
  (let ((entry (find id *guest-entries* :key #'guest-entry-id)))
    (cond ((null entry) nil)
          ((string/= (guest-entry-author entry) (context-principal *request-context*))
           (rpc-fail "You can only delete your own entries." :code +not-your-entry+))
          (t (setf *guest-entries* (remove entry *guest-entries*))
             t))))

(define-server-function clear-guestbook ()
    (:returns fixnum :auth moderator-p :doc "Moderators only: delete every entry, return how many.")
  (prog1 (length *guest-entries*)
    (setf *guest-entries* '())))
