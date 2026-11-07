;;;; examples/dashboard/app.clientx.lisp -- React dashboard polling the backend

(import :from "react-aliases" :targets (use-state use-effect use-ref ref-current))
(import :from "intl-aliases" :targets (fmt-percent fmt-ms fmt-clock))
(import :from "charts" :targets (draw-sparkline))
(import :from :server :targets ((:type metric-series service-health)))

(deftype percentile-summary () '(object (p50 float) (p90 float) (p99 float) (count fixnum)))

;; Build-time macro: an effect that runs BODY now and then every MS milliseconds, and clears the
;; timer when the component unmounts.
(defmacro every-ms (ms &body body)
  `(use-effect (lambda ()
                 (let* ((:const tick (lambda () ,@body))
                        (:const id (set-interval tick ,ms)))
                   (tick)
                   (lambda () (clear-interval id) :undefined)))))

(defun series-color (name)
  (declare (string name) (returns string))
  (case name
    ("cpu" "#e4572e")
    ("memory" "#4c78a8")
    (otherwise "#59a14f")))

(defun format-value (series value)
  (declare (metric-series series) (float value) (returns string))
  (if (equal (.. series unit) "ms") (fmt-ms value) (fmt-percent value)))

(async (defun refresh (set-series set-health set-percentiles set-updated set-problem)
  (declare (function set-series set-health set-percentiles set-updated set-problem))
  (handler-case
      ;; three calls in flight at once; Promise.all of a literal array keeps the tuple types
      (let* ((:const :multi (cpu memory latency)
               (await (promise-all (vector (get-series "cpu") (get-series "memory") (get-series "latency" 120)))))
             (:const services (await (check-services)))
             (:const summary (await (latency-percentiles (map (s (.. latency samples)) (.. s value))))))
        (funcall set-series (vector cpu memory latency))
        (funcall set-health services)
        (funcall set-percentiles summary)
        (funcall set-updated (date-now))
        (funcall set-problem nil))
    (error (e) (funcall set-problem (concat "Refresh failed: " e))))))

(defun SeriesCard (props)
  (declare ((object (series metric-series)) props))
  (let* ((:const series (.. props series))
         (:const color (series-color (.. series name)))
         (:const samples (.. series samples))
         (:const latest (nil-or (.. samples (aref (- (length samples) 1)) (:? value)) 0))
         ;; useRef(null as HTMLCanvasElement | null): `the` stands in for a generic argument
         (:const canvas-ref (use-ref (the (or |HTMLCanvasElement| null) nil))))
    (use-effect (lambda ()
                  (let ((:const canvas (ref-current canvas-ref)))
                    (when canvas (draw-sparkline canvas samples color)))
                  :undefined)
                series)
    (as-html-string
      (:article :class "card"
        (:h3 (.. series name))
        (:p :class "value" :style '{}(:color color) (format-value series latest))
        (:canvas :ref canvas-ref :width 240 :height 60)))))

(export-default (defun Dashboard ()
  (let* ((:const :multi (series set-series) (use-state (the (array metric-series) (vector))))
         (:const :multi (health set-health) (use-state (the (array service-health) (vector))))
         (:const :multi (percentiles set-percentiles) (use-state (the (or percentile-summary null) nil)))
         (:const :multi (updated set-updated) (use-state 0))
         (:const :multi (problem set-problem) (use-state (the (or string null) nil))))
    (every-ms 5000
      (refresh set-series set-health set-percentiles set-updated set-problem))
    (as-html-string
      (:main :class "dashboard"
        (:header
          (:h1 "Service dashboard")
          (:p :class "updated" (if (> updated 0) (concat "Updated " (fmt-clock updated)) "Loading...")))
        (when problem
          (:p :class "error" problem))
        (:section :class "series"
          (map (s series)
            (:SeriesCard :key (.. s name) :series s)))
        (when percentiles
          (:p :class "percentiles"
            "Latency p50 " (fmt-ms (.. percentiles p50))
            " / p90 " (fmt-ms (.. percentiles p90))
            " / p99 " (fmt-ms (.. percentiles p99))
            " over " (.. percentiles count) " samples"))
        (:section :class "services"
          (:h2 "Upstream services")
          (:ul
            (map (h health)
              (:li :key (.. h service) :class (concat "status-" (.. h status))
                (.. h service) ": " (.. h status) " (" (fmt-ms (.. h latency-ms)) ")")))))))))
