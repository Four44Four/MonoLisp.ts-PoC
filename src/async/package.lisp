;;;; src/async/package.lisp

(defpackage :monolisp.async
  (:use :cl :sb-alien)
  (:nicknames :mla)
  (:export
   ;; promises
   #:promise #:promisep #:make-promise #:resolve #:reject #:resolved #:rejected
   #:promise-state #:promise-value #:promise-pending-p #:promise-settled-p
   #:then #:catch* #:finally* #:all #:race #:any #:all-settled #:promisify
   #:promise-wait #:promise-error #:promise-error-reason #:aggregate-error #:aggregate-error-reasons
   #:*unhandled-rejection-hook*
   ;; event loop
   #:event-loop #:make-event-loop #:*loop* #:with-event-loop #:run-loop #:run-once #:stop-loop
   #:loop-post #:post #:next-tick #:set-timeout #:set-interval #:clear-timer #:timer-p
   #:delay #:loop-thread-p #:loop-running-p #:loop-stats #:loop-ref #:loop-unref
   #:loop-error #:*loop-error-hook*
   ;; reactor
   #:watch-fd #:unwatch-fd #:fd-interest #:would-block-p
   ;; thread pool
   #:thread-pool #:make-thread-pool #:offload #:shutdown-pool #:*default-pool* #:pool-stats
   #:pool-saturated #:with-pool
   ;; channels
   #:channel #:make-channel #:channel-send #:channel-receive #:channel-close #:channel-closed-p
   #:channel-closed #:semaphore #:make-async-semaphore #:semaphore-acquire #:semaphore-release
   #:with-semaphore
   ;; macros
   #:async #:await #:define-async #:async-lambda #:alet #:alet* #:chain #:with-timeout #:timeout-error
   #:async-handler-case #:wait-all #:await-all #:async-dolist #:async-dotimes #:async-loop
   #:cancellation-token #:make-cancellation-token #:cancel #:cancelled-p #:cancelled-error #:check-cancelled
   #:cancellation-token-promise
   ;; tcp
   #:tcp-listen #:tcp-close #:tcp-server #:tcp-server-port #:tcp-server-connections
   #:tcp-server-accepted #:tcp-server-refused #:connection-closed
   #:tcp-connection #:conn-read #:conn-write #:conn-close #:conn-peer #:conn-closed-p
   #:conn-set-timeout #:tcp-connect #:tcp-server-close
   ;; http client
   #:fetch #:http-response #:http-response-status #:http-response-headers #:http-response-body
   #:http-response-text #:http-error
   ;; utilities
   #:octets #:string-to-octets #:octets-to-string #:now-ms #:monotonic-ms))
