;;;; examples/dashboard/canvas-aliases.client.lisp -- shorthands for the Canvas 2D API

(declaim (inline canvas-2d clear-canvas begin-path move-to line-to stroke-path))

(export (defun canvas-2d (canvas) (the |CanvasRenderingContext2D| (.. canvas (getContext "2d")))))

;; CTX appears three times: a non-trivial argument is bound to a temporary first, so it is
;; still evaluated once
(export (defun clear-canvas (ctx) (.. ctx (clearRect 0 0 (.. ctx canvas width) (.. ctx canvas height)))))

(export (defun begin-path (ctx) (.. ctx (beginPath))))
(export (defun move-to (ctx x y) (.. ctx (moveTo x y))))
(export (defun line-to (ctx x y) (.. ctx (lineTo x y))))

;; several statements are fine; use the alias in statement position (in an expression it would
;; become an immediately invoked function)
(export (defun stroke-path (ctx color width)
  (setf (.. ctx strokeStyle) color)
  (setf (.. ctx lineWidth) width)
  (.. ctx (stroke))))
