;;;; tests/async-tests.lisp -- unit tests for the asynchronous toolkit

(defpackage :monolisp.test.async
  (:use :cl :monolisp.test :monolisp.async))

(in-package :monolisp.test.async)

(defmacro with-loop-test ((&key (timeout 10)) &body body)
  "Run BODY inside a fresh event loop with a hang guard; BODY may return a promise which is awaited."
  `(with-timeout-guard (,timeout)
     (with-event-loop (:run nil)
       (let ((result (progn ,@body)))
         (if (promisep result) (promise-wait result :timeout ,timeout) result)))))

;;; ------------------------------------------------------------------ promises

(deftest promise-basic-resolve (:suite "async")
  (with-loop-test ()
    (let ((p (make-promise)))
      (is (promise-pending-p p))
      (resolve p 42)
      (is-equal :fulfilled (promise-state p))
      (is-equal 42 (promise-wait p)))))

(deftest promise-reject-signals-condition (:suite "async")
  (with-loop-test ()
    (let ((p (rejected (make-condition 'simple-error :format-control "boom"))))
      (signals simple-error (promise-wait p)))))

(deftest promise-reject-non-condition (:suite "async")
  (with-loop-test ()
    (signals promise-error (promise-wait (rejected "just a string")))))

(deftest promise-then-chaining (:suite "async")
  (with-loop-test ()
    (is-equal 8 (promise-wait (then (then (resolved 2) (lambda (v) (* v 2))) (lambda (v) (* v 2)))))))

(deftest promise-then-adopts-promise (:suite "async")
  (with-loop-test ()
    (is-equal :inner (promise-wait (then (resolved 1) (lambda (v) (declare (ignore v)) (delay 5 :inner)))))))

(deftest promise-catch-recovers (:suite "async")
  (with-loop-test ()
    (is-equal :recovered
              (promise-wait (catch* (then (resolved 1) (lambda (v) (declare (ignore v)) (error "nope")))
                                    (lambda (e) (declare (ignore e)) :recovered))))))

(deftest promise-finally-passthrough (:suite "async")
  (with-loop-test ()
    (let ((ran nil))
      (is-equal 7 (promise-wait (finally* (resolved 7) (lambda () (setf ran t)))))
      (is ran))))

(deftest promise-settles-only-once (:suite "async")
  (with-loop-test ()
    (let ((p (make-promise)))
      (resolve p 1)
      (is-false (resolve p 2))
      (is-false (reject p "x"))
      (is-equal 1 (promise-wait p)))))

(deftest promise-self-resolution-rejects (:suite "async")
  (with-loop-test ()
    (let ((p (make-promise)))
      (resolve p p)
      (signals error (promise-wait p)))))

(deftest promise-all (:suite "async")
  (with-loop-test ()
    (is-equal '(1 2 3) (promise-wait (all (list (delay 10 1) (resolved 2) (delay 1 3)))))
    (is-equal '() (promise-wait (all '())))))

(deftest promise-all-rejects-fast (:suite "async")
  (with-loop-test ()
    (signals simple-error (promise-wait (all (list (delay 50 1) (rejected (make-condition 'simple-error))))))))

(deftest promise-race (:suite "async")
  (with-loop-test ()
    (is-equal :fast (promise-wait (race (list (delay 50 :slow) (delay 1 :fast)))))))

(deftest promise-any (:suite "async")
  (with-loop-test ()
    (is-equal :ok (promise-wait (any (list (rejected "a") (delay 5 :ok)))))
    (signals aggregate-error (promise-wait (any (list (rejected "a") (rejected "b")))))))

(deftest promise-all-settled (:suite "async")
  (with-loop-test ()
    (let ((r (promise-wait (all-settled (list (resolved 1) (rejected "x"))))))
      (is-equal '(:fulfilled 1) (first r))
      (is-equal :rejected (first (second r))))))

(deftest promise-cross-thread-resolve (:suite "async")
  (with-loop-test ()
    (let ((p (make-promise)))
      (sb-thread:make-thread (lambda () (sleep 0.05) (resolve p :from-thread)))
      (is-equal :from-thread (promise-wait p :timeout 5)))))

(deftest promise-wait-timeout (:suite "async")
  (with-loop-test ()
    (signals timeout-error (promise-wait (make-promise) :timeout 0.05))))

(deftest unhandled-rejection-hook-fires (:suite "async")
  (with-loop-test ()
    (let* ((seen nil)
           (*unhandled-rejection-hook* (lambda (p r) (declare (ignore p)) (setf seen r))))
      (rejected :nobody-listens)
      (run-loop)
      (is-equal :nobody-listens seen))))

(deftest handled-rejection-does-not-fire-hook (:suite "async")
  (with-loop-test ()
    (let* ((seen nil)
           (*unhandled-rejection-hook* (lambda (p r) (declare (ignore p)) (setf seen r))))
      (catch* (rejected :handled) (lambda (r) (declare (ignore r)) nil))
      (run-loop)
      (is-false seen))))

;;; ------------------------------------------------------------------ event loop

(deftest timers-fire-in-order (:suite "async")
  (with-loop-test ()
    (let ((order '()))
      (set-timeout (lambda () (push 3 order)) 30)
      (set-timeout (lambda () (push 1 order)) 1)
      (set-timeout (lambda () (push 2 order)) 15)
      (run-loop)
      (is-equal '(1 2 3) (reverse order)))))

(deftest clear-timer-prevents-firing (:suite "async")
  (with-loop-test ()
    (let ((fired nil))
      (clear-timer (set-timeout (lambda () (setf fired t)) 1))
      (run-loop)
      (is-false fired))))

(deftest interval-repeats (:suite "async")
  (with-loop-test ()
    (let ((n 0) (timer nil))
      (setf timer (set-interval (lambda () (when (= (incf n) 3) (clear-timer timer))) 2))
      (run-loop)
      (is-equal 3 n))))

(deftest posted-tasks-run-fifo (:suite "async")
  (with-loop-test ()
    (let ((order '()))
      (post (lambda () (push :a order)))
      (post (lambda () (push :b order)))
      (run-loop)
      (is-equal '(:a :b) (reverse order)))))

(deftest loop-survives-callback-errors (:suite "async")
  (with-loop-test ()
    (let* ((errors 0) (after nil)
           (*loop-error-hook* (lambda (c ctx) (declare (ignore c ctx)) (incf errors))))
      (post (lambda () (error "bad callback")))
      (post (lambda () (setf after t)))
      (run-loop)
      (is-equal 1 errors)
      (is after))))

(deftest cross-thread-post-wakes-loop (:suite "async")
  (with-loop-test ()
    (let ((got nil) (loop *loop*))
      (set-timeout (lambda () nil) 2000)   ; keeps loop alive and asleep in poll
      (sb-thread:make-thread (lambda () (sleep 0.05) (loop-post loop (lambda () (setf got t) (stop-loop loop)))))
      (let ((start (monotonic-ms)))
        (run-loop)
        (is got)
        (is (< (- (monotonic-ms) start) 1500) "post did not wake the poller promptly")))))

(deftest loop-exits-when-idle (:suite "async")
  (with-loop-test ()
    (is-equal :idle (run-loop))))

(deftest loop-stats-accumulate (:suite "async")
  (with-loop-test ()
    (post (lambda () nil))
    (set-timeout (lambda () nil) 1)
    (run-loop)
    (let ((stats (loop-stats)))
      (is (>= (getf stats :tasks-run) 2))
      (is (= (getf stats :timers-fired) 1)))))

;;; ------------------------------------------------------------------ thread pool

(deftest offload-returns-result (:suite "async")
  (with-loop-test ()
    (with-pool (pool :size 2)
      (is-equal 10 (promise-wait (offload (lambda () (+ 4 6)) :pool pool))))))

(deftest offload-propagates-errors (:suite "async")
  (with-loop-test ()
    (with-pool (pool :size 1)
      (signals division-by-zero (promise-wait (offload (lambda () (/ 1 0)) :pool pool))))))

(deftest offload-runs-in-parallel (:suite "async")
  (with-loop-test ()
    (with-pool (pool :size 4)
      (let ((start (monotonic-ms)))
        (promise-wait (all (loop repeat 4 collect (offload (lambda () (sleep 0.1) t) :pool pool))))
        (is (< (- (monotonic-ms) start) 350) "4 x 100ms jobs on 4 workers should overlap")))))

(deftest offload-saturation-rejects (:suite "async")
  (with-loop-test ()
    (with-pool (pool :size 1 :max-queue 2)
      (let ((blocker (offload (lambda () (sleep 0.3)) :pool pool)))
        ;; wait until the single worker has picked up the blocker
        (loop repeat 100 until (plusp (getf (pool-stats pool) :active)) do (sleep 0.005))
        (offload (lambda () 1) :pool pool)
        (offload (lambda () 2) :pool pool)
        ;; queue now has 2 entries (worker is busy with blocker)
        (let ((rejected-p nil))
          (catch* (offload (lambda () 3) :pool pool)
                  (lambda (e) (setf rejected-p (typep e 'pool-saturated))))
          (promise-wait blocker)
          (run-loop)
          (is rejected-p))))))

;;; ------------------------------------------------------------------ channels

(deftest channel-send-receive (:suite "async")
  (with-loop-test ()
    (let ((ch (make-channel)))
      (channel-send ch 1)
      (channel-send ch 2)
      (is-equal 1 (promise-wait (channel-receive ch)))
      (is-equal 2 (promise-wait (channel-receive ch))))))

(deftest channel-receiver-waits (:suite "async")
  (with-loop-test ()
    (let* ((ch (make-channel))
           (p (channel-receive ch)))
      (set-timeout (lambda () (channel-send ch :late)) 5)
      (is-equal :late (promise-wait p)))))

(deftest channel-bounded-backpressure (:suite "async")
  (with-loop-test ()
    (let* ((ch (make-channel :capacity 1))
           (first (channel-send ch :a))
           (second (channel-send ch :b)))
      (is (promise-settled-p first))
      (is (promise-pending-p second))
      (is-equal :a (promise-wait (channel-receive ch)))
      (is-equal t (promise-wait second))
      (is-equal :b (promise-wait (channel-receive ch))))))

(deftest channel-close-rejects-waiters (:suite "async")
  (with-loop-test ()
    (let* ((ch (make-channel))
           (p (channel-receive ch)))
      (channel-close ch)
      (signals channel-closed (promise-wait p))
      (signals channel-closed (promise-wait (channel-send ch 1))))))

(deftest semaphore-limits-concurrency (:suite "async")
  (with-loop-test ()
    (let ((sem (make-async-semaphore 2)) (active 0) (max-active 0))
      (promise-wait
       (all (loop repeat 6
                  collect (with-semaphore (sem)
                            (incf active)
                            (setf max-active (max max-active active))
                            (then (delay 5) (lambda (v) (declare (ignore v)) (decf active)))))))
      (is-equal 2 max-active))))

;;; ------------------------------------------------------------------ async / await

(deftest async-returns-promise-of-last-form (:suite "async")
  (with-loop-test ()
    (is-equal 3 (promise-wait (async 1 2 3)))))

(deftest await-suspends-and-resumes (:suite "async")
  (with-loop-test ()
    (is-equal 11 (promise-wait (async (let ((a (await (delay 5 5)))
                                            (b (await (resolved 6))))
                                        (+ a b)))))))

(deftest await-non-promise-value (:suite "async")
  (with-loop-test ()
    (is-equal 5 (promise-wait (async (await 5))))))

(deftest await-in-function-arguments-preserves-order (:suite "async")
  (with-loop-test ()
    (let ((trace '()))
      (flet ((note (x) (push x trace) x))
        (is-equal '(1 2 3)
                  (promise-wait (async (list (note 1) (await (delay 2 (note 2))) (note 3)))))
        (is-equal '(1 2 3) (reverse trace))))))

(deftest await-rejection-signals-at-await-point (:suite "async")
  (with-loop-test ()
    (is-equal :caught
              (promise-wait (async (handler-case (progn (await (rejected (make-condition 'simple-error))) :not-here)
                                     (simple-error () :caught)))))))

(deftest async-handler-case-with-await-in-handler (:suite "async")
  (with-loop-test ()
    (is-equal :handled-async
              (promise-wait (async (handler-case (await (rejected (make-condition 'simple-error)))
                                     (error () (await (delay 2 :handled-async)))))))))

(deftest async-handler-case-does-not-catch-later-errors (:suite "async")
  ;; Classic CPS pitfall: code *after* a handler-case must not be covered by its handlers.
  (with-loop-test ()
    (let ((result (promise-wait
                   (catch* (async (handler-case (await (delay 1 :ok))
                                    (error () :wrongly-caught))
                                  (error "after the handler-case"))
                           (lambda (e) (list :outer (princ-to-string e)))))))
      (is-equal :outer (first result)))))

(deftest async-unwind-protect-runs-cleanup-on-both-paths (:suite "async")
  (with-loop-test ()
    (let ((cleanups 0))
      (promise-wait (async (unwind-protect (await (delay 1 :v)) (incf cleanups))))
      (promise-wait (catch* (async (unwind-protect (await (rejected "x")) (incf cleanups)))
                            (lambda (e) (declare (ignore e)) nil)))
      (is-equal 2 cleanups))))

(deftest async-if-cond-case-and-or (:suite "async")
  (with-loop-test ()
    (is-equal '(:yes :c2 :k2 7 :first)
              (promise-wait
               (async (list (if (await (resolved t)) :yes :no)
                            (cond ((await (resolved nil)) :c1)
                                  ((await (resolved t)) :c2)
                                  (t :c3))
                            (case (await (resolved 2)) (1 :k1) (2 :k2) (otherwise :k3))
                            (and (await (resolved 1)) (await (resolved 7)))
                            (or (await (resolved nil)) :first (await (resolved :never)))))))))

(deftest async-setq-setf-the-mvb (:suite "async")
  (with-loop-test ()
    (is-equal '(10 20 30)
              (promise-wait
               (async (let ((x 0) (v (vector 0)))
                        (setq x (await (resolved 10)))
                        (setf (aref v 0) (await (resolved 20)))
                        (multiple-value-bind (a) (the fixnum (await (resolved 30)))
                          (list x (aref v 0) a))))))))

(deftest async-dolist-dotimes (:suite "async")
  (with-loop-test ()
    (is-equal '(6 3)
              (promise-wait
               (async (let ((sum 0) (count 0))
                        (dolist (x '(1 2 3)) (incf sum (await (resolved x))))
                        (dotimes (i 3) (await (delay 1)) (incf count))
                        (list sum count)))))))

(deftest async-loop-subset (:suite "async")
  (with-loop-test ()
    (is-equal '((2 4 6) 6 3 :done)
              (promise-wait
               (async (list (loop for x in '(1 2 3) collect (await (resolved (* 2 x))))
                            (loop for i from 1 to 3 sum (await (resolved i)))
                            (let ((n 0)) (loop while (< n 3) do (await (delay 1)) (incf n)) n)
                            (loop repeat 2 do (await (resolved nil)) finally (return :done))))))))

(deftest async-return-from-block (:suite "async")
  (with-loop-test ()
    (is-equal :early
              (promise-wait (async (block b
                                     (await (delay 1))
                                     (return-from b :early)
                                     :late))))))

(deftest async-dolist-return-breaks (:suite "async")
  (with-loop-test ()
    (is-equal 2
              (promise-wait (async (dolist (x '(1 2 3) :none)
                                     (await (resolved x))
                                     (when (= x 2) (return x))))))))

(define-async fetch-twice (n)
  (when (< n 0) (return-from fetch-twice :negative))
  (* 2 (await (delay 1 n))))

(deftest define-async-and-early-return (:suite "async")
  (with-loop-test ()
    (is-equal 10 (promise-wait (fetch-twice 5)))
    (is-equal :negative (promise-wait (fetch-twice -1)))))

(deftest async-synchronous-error-rejects (:suite "async")
  (with-loop-test ()
    (signals division-by-zero (promise-wait (async (await (delay 1)) (/ 1 0))))))

(deftest async-deep-iteration-does-not-blow-stack (:suite "async")
  (with-loop-test (:timeout 30)
    (is-equal 20000
              (promise-wait (async (let ((n 0))
                                     (dotimes (i 20000) (incf n))   ; no await: stays synchronous
                                     (dotimes (i 5000) (await (resolved i)) (incf n 0))
                                     n))))))

(defmacro await-twice (p) `(progn (await ,p) (await ,p)))

(deftest async-macro-expanding-to-await (:suite "async")
  (with-loop-test ()
    (is-equal :v (promise-wait (async (await-twice (resolved :v)))))))

(deftest await-outside-async-errors (:suite "async")
  (signals error (await (resolved 1))))

;;; ------------------------------------------------------------------ chaining macros

(deftest alet-parallel (:suite "async")
  (with-loop-test ()
    (is-equal 3 (promise-wait (alet ((a (delay 5 1)) (b (delay 1 2))) (+ a b))))))

(deftest alet*-sequential (:suite "async")
  (with-loop-test ()
    (is-equal 6 (promise-wait (alet* ((a (delay 1 2)) (b (resolved (* a 3)))) b)))))

(deftest chain-threads-values (:suite "async")
  (with-loop-test ()
    (flet ((add (v n) (+ v n)))
      (is-equal 7 (promise-wait (chain (resolved 1) (add 2) (lambda (v) (* v 2)) (add 1)))))))

(deftest chain-catch-and-finally (:suite "async")
  (with-loop-test ()
    (let ((finally-ran nil))
      (is-equal :rescued
                (promise-wait (chain (rejected (make-condition 'simple-error))
                                     (:catch (e) (declare (ignore e)) :rescued)
                                     (:finally (setf finally-ran t)))))
      (is finally-ran))))

(deftest with-timeout-rejects-slow (:suite "async")
  (with-loop-test ()
    (signals timeout-error (promise-wait (with-timeout (10) (delay 500 :slow))))
    (is-equal :fast (promise-wait (with-timeout (500) (delay 1 :fast))))))

(deftest cancellation-token (:suite "async")
  (with-loop-test ()
    (let ((token (make-cancellation-token)))
      (is-false (cancelled-p token))
      (cancel token)
      (is (cancelled-p token))
      (signals cancelled-error (check-cancelled token))
      (signals cancelled-error (promise-wait (race (list (delay 1000) (cancellation-token-promise token))))))))

;;; ------------------------------------------------------------------ TCP

(deftest tcp-echo-roundtrip (:suite "async")
  (with-loop-test (:timeout 15)
    (let* ((server (tcp-listen (lambda (conn)
                                 (async (loop
                                          (let ((chunk (await (conn-read conn))))
                                            (when (null chunk) (return))
                                            (await (conn-write conn chunk))))
                                        (conn-close conn)))))
           (port (tcp-server-port server)))
      (unwind-protect
           (is-equal "hello"
                     (promise-wait
                      (async (let ((c (await (tcp-connect "127.0.0.1" port))))
                               (await (conn-write c "hello"))
                               (let ((reply (octets-to-string (await (conn-read c)))))
                                 (conn-close c)
                                 reply)))))
        (tcp-server-close server)))))

(deftest tcp-server-refuses-over-capacity (:suite "async")
  (with-loop-test (:timeout 15)
    (let* ((server (tcp-listen (lambda (conn) (declare (ignore conn)) nil) :max-connections 1))
           (port (tcp-server-port server)))
      (unwind-protect
           (promise-wait
            (async (let ((c1 (await (tcp-connect "127.0.0.1" port))))
                     (await (delay 50))
                     (let ((c2 (await (tcp-connect "127.0.0.1" port))))
                       (await (delay 50))
                       ;; c2 is accepted by the kernel then immediately closed by the server
                       (is-equal nil (await (conn-read c2)))
                       (is (>= (tcp-server-refused server) 1))
                       (conn-close c1) (conn-close c2)))))
        (tcp-server-close server)))))

(deftest tcp-idle-timeout-fires (:suite "async")
  (with-loop-test (:timeout 15)
    (let* ((timed-out nil)
           (server (tcp-listen (lambda (conn)
                                 (conn-set-timeout conn 50)
                                 (catch* (conn-read conn)
                                         (lambda (e) (setf timed-out (typep e 'timeout-error)))))))
           (port (tcp-server-port server)))
      (unwind-protect
           (promise-wait
            (async (let ((c (await (tcp-connect "127.0.0.1" port))))
                     (await (delay 300))
                     (conn-close c))))
        (tcp-server-close server))
      (is timed-out))))

(deftest tcp-connect-refused (:suite "async")
  (with-loop-test (:timeout 15)
    ;; find a closed port by binding then closing a listener
    (let* ((probe (tcp-listen (lambda (c) (declare (ignore c)))))
           (port (tcp-server-port probe)))
      (tcp-server-close probe)
      (signals error (promise-wait (tcp-connect "127.0.0.1" port :timeout 2000))))))

;;; ------------------------------------------------------------------ HTTP client

(defun start-fake-http-server (status body &key chunked)
  "Tiny HTTP responder used to test FETCH.  Returns the tcp-server."
  (tcp-listen
   (lambda (conn)
     (async (let ((acc (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
              (loop
                (let ((chunk (await (conn-read conn))))
                  (when (null chunk) (return))
                  (loop for b across chunk do (vector-push-extend b acc))
                  (when (search #(13 10 13 10) acc) (return))))
              (let ((response
                      (if chunked
                          (format nil "HTTP/1.1 ~D OK~C~CTransfer-Encoding: chunked~C~CContent-Type: text/plain~C~C~C~C~X~C~C~A~C~C0~C~C~C~C"
                                  status #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
                                  (length body) #\Return #\Linefeed body #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)
                          (format nil "HTTP/1.1 ~D OK~C~CContent-Length: ~D~C~CContent-Type: text/plain~C~C~C~C~A"
                                  status #\Return #\Linefeed (length body) #\Return #\Linefeed #\Return #\Linefeed
                                  #\Return #\Linefeed body))))
                (await (conn-write conn response))
                (conn-close conn)))))))

(deftest fetch-content-length (:suite "async")
  (with-loop-test (:timeout 15)
    (let ((server (start-fake-http-server 200 "hello fetch")))
      (unwind-protect
           (let ((r (promise-wait (fetch (format nil "http://127.0.0.1:~D/x" (tcp-server-port server))))))
             (is-equal 200 (http-response-status r))
             (is-equal "hello fetch" (http-response-text r))
             (is-equal "text/plain" (cdr (assoc "content-type" (http-response-headers r) :test #'string=))))
        (tcp-server-close server)))))

(deftest fetch-chunked (:suite "async")
  (with-loop-test (:timeout 15)
    (let ((server (start-fake-http-server 201 "chunky" :chunked t)))
      (unwind-protect
           (let ((r (promise-wait (fetch (format nil "http://127.0.0.1:~D/" (tcp-server-port server))))))
             (is-equal 201 (http-response-status r))
             (is-equal "chunky" (http-response-text r)))
        (tcp-server-close server)))))

(deftest fetch-rejects-header-injection (:suite "async")
  (with-loop-test (:timeout 15)
    (signals http-error
             (promise-wait (fetch "http://127.0.0.1:1/"
                                  :headers (list (cons "X-Evil" (format nil "a~C~CInjected: yes" #\Return #\Linefeed))))))))

(deftest tcp-connect-refused-is-fast (:suite "async")
  ;; A refused connection must be reported promptly, not via the connect timeout.
  (with-loop-test (:timeout 15)
    (let* ((probe (tcp-listen (lambda (c) (declare (ignore c)))))
           (port (tcp-server-port probe)))
      (tcp-server-close probe)
      (let ((start (monotonic-ms)))
        (signals error (promise-wait (tcp-connect "127.0.0.1" port :timeout 8000)))
        (is (< (- (monotonic-ms) start) 3000) "refused connect took too long")))))

(deftest fetch-rejects-non-http-scheme (:suite "async")
  (with-loop-test ()
    (signals http-error (promise-wait (fetch "file:///etc/passwd")))
    (signals http-error (promise-wait (fetch "https://example.com/")))))

(deftest fetch-does-not-block-loop (:suite "async")
  ;; A slow upstream must not stop timers from firing.
  (with-loop-test (:timeout 15)
    (let* ((server (tcp-listen (lambda (conn)
                                 (async (await (delay 300))
                                        (await (conn-write conn (format nil "HTTP/1.1 200 OK~C~CContent-Length: 2~C~C~C~Cok"
                                                                        #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed)))
                                        (conn-close conn)))))
           (ticks 0)
           (interval (set-interval (lambda () (incf ticks)) 10)))
      (unwind-protect
           (let ((r (promise-wait (fetch (format nil "http://127.0.0.1:~D/" (tcp-server-port server))))))
             (is-equal "ok" (http-response-text r))
             (is (>= ticks 10) "timers starved during fetch: only ~D ticks" ticks))
        (clear-timer interval)
        (tcp-server-close server)))))
