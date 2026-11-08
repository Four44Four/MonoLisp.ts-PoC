# TODO list
# Simpler things
 - Add an explicit simple server-function and frontend usage pair example in the README's Quick Start section that exemplifies the type safe drift prevention mechanism in this framework

 - Change the frontend Lisp raw JS object indicator from `'{}` to just `{}` (update in documentation, tests, source code, skills, and examples)

 - `defvar` with `:multi` and `:const` support for frontend Lisp
    - Replace any current `defvar`, `defparameter`, or `defconstant` implementation in frontend Lisp right now (check documentation, tests, source code, skills, and examples) 
```lisp
(defvar my-var "hi") ;; let myVar = "hi"
(defvar :const my-var "hi") ;; const myVar = "hi"
(defvar :multi (my-var-1 my-var-2) (my-multi-returning-function)) ;; let [myVar1, myVar2] = myMultiReturningFunction()
(defvar :multi :const (my-var-1 my-var-2) (my-multi-returning-function)) ;; const [myVar1, myVar2] = myMultiReturningFunction()
```

 - Macro shadowing should be banned
    - If a function or another macro tries to be defined with the same name as an existing macro:
       - Transpiler will error

 - `use-effect` built-in macro to account for developer inexperience with Lisp implicit returns
    - Eliminate any existing inline-wrappers that try to cover `useEffect` (from examples and tests and source code)
    - Omitting `:depends` will default to a `[]` on the emitted Typescript
    - Omitting `:clean-up` will default to nothing being returned in the `useEffect` callback in the emitted Typescript
```lisp
;; useEffect(() => {
;;    console.log("hiiii")
;;    console.log("hellohello")
;;    return () => {
;;       console.log("good-bye")
;;       console.log("byebye")
;;    }
;; }, [<var-dependency>, <var-2-dependency>, ...])
(use-effect 
   :callback (progn
               (console-log "hiiii")
               (console-log "hellohello"))
   :clean-up (progn
               (console-log "good-bye")
               (console-log "byebye"))
   :depends (<var-dependency> <var-2-dependency> ...))
```

 - Rename `define-rpc-struct` to `define-shared-struct`
    - To accomodate the use of them in both the JSON-RPC2 endpoints and the realtime data endpoints

# More complex things
 - Support splitting up a monorepo backend into different deployable segments
    - Developer can specify that a specific .server.lisp file will be part of a specific server segment
       - Developer also has to specify in a configuration file what deployment targets/hosts the explicit segments map to (i.e. deploy one segment to a specific AWS lambda, Cloudflare worker, or a raw VPS Linux box with a custom deployment mechanism)
       - Developer can separate testing auto deployments from production auto deployments
          - Must be clear to developer what they are deploying to, lest they accidentally deploy an internal tool to prod
       - By default, the .server.lisp files that are not specified to be part of any explicit server segment will be deployed on the "main node" as normal, which is the one that the developer is on right now
          - Developer can explicitly configure their default deployment serving target to be a specific target/host through the same configuration process as configging an explicit segment to be deployed to a specific target/host
    - Include guide on how to go through the process of building these backends for development/testing and production for dummies
    - Try to make the developer-facing process as similar to deploying a monorepo as a single backend deployment
       - Contrast the differences in the guide

 - Serverside realtime type-safe data streaming support
    - Can be configured to be one-way (simplex server-to-client) or two way (full duplex)
        - One-way will use SSE
        - Two-way will use Websocket
    - Must be hardered as it present a new potential attack vector with unique complications compared to the JSON-RPC2 endpoints
    - Realtime endpoints are defined in .server.lisp files using `define-realtime`
    - The foundational web-server supports running websocket/SSE connections alongside the JSON-RPC2 endpoint connections
      - There are 3 different modes for the web-server:
         1. Dedicated JSON-RPC2 HTTP endpoint mode
         2. Dedicated websocket/SSE high concurrent long connections mode
         3. Hybrid of the previous 2 modes (supports both types of connections on the same web-server)
      - If there are `define-realtime` and `define-server-function` JSON-RPC2 endpoints on this specific server deployment segment:
         - Start-up the web-server in hybrid websocket/SSE + JSON-RPC2 endpoints mode
      - If there are only `define-server-function` JSON-RPC2 endpoints on this specific server deployment segment:
         - Start-up the web-server in dedicated JSON-RPC2 endpoints mode
      - If there are only `define-realtime` endpoints on this specific server deployment segment:
         - Start-up the web-server in websocket/SSE only mode
    - EXAMPLES:
```lisp
;; server
...
;; `:two-way` -> WebSocket
(define-realtime data-streaming-thingymajigy-1
  :type fixnum
  :direction :two-way
  ;; `accept-fn` will accept custom data being passed to the next function (can be `nil`)
  ;; `reject-fn` will accept a HTTP code + error JSON body
  :on-handshake (lambda (conn-in reject-fn accept-fn) ...)
  ;; `data-in` will come from the `accept-fn` or be `nil` by default
  :on-connect (lambda (conn-in data-in) ...)
  :on-disconnect #'my-disconnect-callback
  :on-error (lambda (conn-in error-in) ...)
  ;; `data-in` is either SSE or WebSocket messages that are server-bound
  :on-receive (lambda (conn-in data-in) ...))
...
(dotimes (i 10)
   (send-realtime data-streaming-thingymajigy-1
      (write-to-string i)))
...
;; `*-realtime` receiving functions will always try to accumulate connection data fragments until the message is complete and they can be reassembled into the specified `:type`
;;    otherwise, an error should be triggered if there would be a situation where a partial message comes through and it can't be assembled (no silent errors when message cannot be fully reassembled)
;;    pay special attention to mitigating danger from partial message attacks as realtime endpoints with a specified `:type` can be vulernable to reading malformed data fragments under some conditions
;;       the data/type parsing here should be taken as seriously, if not more seriously than the JSON-RPC2 client-server boundary
(define-shared-struct my-cool-struct
   (id fixnum)
   (message string)
   (flags (array boolean 3)))
;; `:one-way` -> SSE (server-to-client)
(define-realtime data-streaming-thingymajigy-2
  :type my-cool-struct
  :direction :one-way
  :on-handshake (lambda (conn-in reject-fn accept-fn) ...)
  :on-connect (lambda (conn-in data-in) ...)
  :on-disconnect #'my-disconnect-callback-2
  :on-error (lambda (conn-in error-in) ...)
  ;; `:one-way` realtime endpoints cannot receive, they can only be used to send in the server
  ;; :on-receive 
  )
...

;; client
...
(defun my-component ()
   (let* ((:const :multi (my-int set-my-int) (use-state 0))
          (:const :multi (realtime-conn set-realtime-conn (use-state nil)))
          (:const send-message (lambda ()
                                 (when realtime-conn
                                    (send-realtime realtime-conn
                                                   "{\"add\": 15}")))))
      (use-effect
         :callback (connect-realtime data-streaming-thingymajigy-1
                     :on-connect (lambda (conn-in)
                                    (funcall set-realtime-conn
                                             conn-in))
                     :on-receive (lambda (conn-in data-in)
                                    (funcall set-my-int
                                             (lambda (prev-my-int)
                                                (+ prev-my-int (parse-int data-in)))))
         :clean-up (when realtime-conn 
                     (close-conn-realtime realtime-conn))))
      (as-html-string
         (:div (:p "hello world !!")
               (:p my-int)
               (:button :onClick send-message)))))
```

 - Add option to integrate Quicklisp and other ASDF systems into the backend as library packages/dependencies
    - Must respect existing framework philosophies about tight type control and integration into the existing server functions system
    - If this specification is simple:
       - Integrate it with the other source code in this framework
    - If this specification is complex:
       - Make a properly named folder to put all the code for implementing such integration
       - Make a special markdown file as a guide for Common Lisp developers working on the backend and coming from a Common Lisp development ecosystem (using Quicklisp and whatnot)

 - Add hot reloading so that editing and saving shared struct definitions, endpoints, and other things will reload across the entire fullstack app
    - (Re)transpile as little code as possible without compromising functionality to ensure that in a giant app, changing 1 small thing won't force the entire app to re-transpile
    - Should be configurable to be on or off by default by the developer
    - If necessary, include a markdown guide on how to handle bugs possibly resulting from hot reloading mistakes (how to clean restart the app to ensure everything gets re-transpiled)

# Probably far off in the future things

 - Integration of a Common Lisp SQL webserver (for PostgreSQL or MySQL) for direct client to database communication with type safety
    - Client calls a normal looking function
       - That function is a backend defined server function running right at the DB
       - Can execute DB level operations without needing to pass through edge server nodes (lowers latency for data intensive webapps)
       - Can subscribe to receive realtime data updates from the DB
    - Could be built on a MonoLisp server (?)
       - Would need to support 1 MonoLisp frontend being able to transpile against multiple MonoLisp backends
    - See [PostgREST](https://github.com/postgrest/postgrest)