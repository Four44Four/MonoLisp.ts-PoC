;;;; examples/dashboard/charts.client.lisp -- a sparkline renderer built on the canvas aliases

(import :from "canvas-aliases" :targets (canvas-2d clear-canvas begin-path move-to line-to stroke-path))
(import :from :server :targets ((:type metric-sample)))

(export (defun draw-sparkline (canvas samples color)
  (declare (|HTMLCanvasElement| canvas) ((array metric-sample) samples) (string color))
  (let* ((:const ctx (canvas-2d canvas))
         (:const w (.. canvas width))
         (:const h (.. canvas height))
         (:const values (map (s samples) (.. s value)))
         (:const lo (math-min (spread values)))
         (:const hi (math-max (spread values)))
         (:const span (if (> hi lo) (- hi lo) 1))
         (:const last-index (max 1 (- (length values) 1))))
    (clear-canvas ctx)
    (begin-path ctx)
    (loop for v across values
          for i from 0
          do (let ((:const x (* w (/ i last-index)))
                   (:const y (- h 2 (* (- h 4) (/ (- v lo) span)))))
               (if (= i 0) (move-to ctx x y) (line-to ctx x y))))
    (stroke-path ctx color 2))))
