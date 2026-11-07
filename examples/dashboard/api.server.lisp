;;;; examples/dashboard/api.server.lisp -- backend: async fan-out, thread-pool offload, rich schemas
;;;;
;;;; Server functions run on the event loop.  Anything slow must not block it: wait on promises
;;;; (DELAY, FETCH, ALL) or push CPU-bound work to the thread pool with OFFLOAD.  The dispatcher
;;;; awaits whatever promise a server function returns.

(in-package :monolisp-user)

(define-rpc-struct metric-sample
  (at float)                                 ; epoch milliseconds
  (value float))

(define-rpc-struct metric-series
  (name (member "cpu" "memory" "latency"))
  (unit (member "ratio" "ms"))
  (samples (array metric-sample 120)))

(define-rpc-struct service-health
  (service (string 32))
  (status (member "up" "degraded" "down"))
  (latency-ms float))

(defparameter *sample-interval-ms* 5000d0)

;;; ------------------------------------------------------------------ synthetic metrics

(defun synthetic-value (name tick)
  "A deterministic wave plus noise, so successive calls look like a live feed."
  (let ((noise (/ (random 1000) 1000d0)))
    (cond ((string= name "cpu") (min 1d0 (max 0d0 (+ 0.45d0 (* 0.3d0 (sin (/ tick 6d0))) (* 0.1d0 noise)))))
          ((string= name "memory") (min 1d0 (+ 0.6d0 (* 0.05d0 (sin (/ tick 20d0))) (* 0.02d0 noise))))
          (t (+ 40d0 (* 25d0 (abs (sin (/ tick 4d0)))) (* 30d0 noise noise))))))

(define-server-function get-series ((name (member "cpu" "memory" "latency"))
                                    &optional ((points (integer 10 120)) 60))
    (:returns metric-series :doc "The last POINTS samples of one metric, oldest first.")
  (let* ((now (now-ms))
         (tick-now (floor now *sample-interval-ms*)))
    (make-metric-series
     :name name
     :unit (if (string= name "latency") "ms" "ratio")
     :samples (loop for i from (1- points) downto 0
                    for tick = (- tick-now i)
                    collect (make-metric-sample :at (* tick *sample-interval-ms*)
                                                :value (synthetic-value name tick))))))

;;; ------------------------------------------------------------------ async fan-out

(define-async probe (service base-latency-ms)
  "Pretend to health-check SERVICE over the network: resolves after a jittered delay."
  (let ((latency (+ base-latency-ms (random 40))))
    (await (delay latency))
    (make-service-health :service service
                         :status (cond ((> latency 180) "down")
                                       ((> latency 120) "degraded")
                                       (t "up"))
                         :latency-ms (float latency 1d0))))

(define-server-function check-services ()
    (:returns (array service-health) :timeout-ms 1000
     :doc "Probe every upstream in parallel: total time is the slowest probe, not the sum.")
  ;; ALL is Promise.all: it never blocks the loop, so other requests are served meanwhile
  (all (list (probe "auth" 30) (probe "billing" 110) (probe "search" 60) (probe "email" 160))))

;;; ------------------------------------------------------------------ CPU-bound work off the loop

(defun percentile (sorted p)
  (let ((n (length sorted)))
    (if (zerop n)
        0d0
        (float (aref sorted (min (1- n) (floor (* p n)))) 1d0))))

(define-server-function latency-percentiles ((samples (array float 10000)))
    (:returns (object (p50 float) (p90 float) (p99 float) (count fixnum))
     :doc "Percentiles of SAMPLES.  The sort runs on a worker thread via OFFLOAD.")
  (offload (lambda ()
             (let ((sorted (sort (copy-seq samples) #'<)))
               (list :p50 (percentile sorted 0.5d0)
                     :p90 (percentile sorted 0.9d0)
                     :p99 (percentile sorted 0.99d0)
                     :count (length sorted))))))

;;; ------------------------------------------------------------------ map schema

(define-server-function latest-values ()
    (:returns (map float) :doc "Metric name -> latest value, as a plain object (Record<string, number>).")
  (let ((table (make-hash-table :test 'equal))
        (tick (floor (now-ms) *sample-interval-ms*)))
    (dolist (name '("cpu" "memory" "latency") table)
      (setf (gethash name table) (synthetic-value name tick)))))
