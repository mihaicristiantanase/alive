(in-package #:alive)

(defun scale (a in-min in-max dst-min dst-max)
  (+ (* (/ (- a in-min) (- in-max in-min))
        (- dst-max dst-min))
     dst-min))

(defmacro draw-scene-pixels (&rest body)
  `(progn
     (cairo:set-source-rgb 0.2 0.2 0.2)
     (cairo:paint)
     (let* ((w (cairo:width cairo:*context*))
            (h (cairo:height cairo:*context*))
            (a (/ w h)))
       (loop for px from 0 upto w by *2d-plot-point-size* do
         (loop for py from 0 upto h by *2d-plot-point-size* do
           (multiple-value-bind (r g b) ,@body
             (cairo:set-source-rgb r g b)
             (cairo:rectangle px py *2d-plot-point-size* *2d-plot-point-size*)
             (cairo:fill-path)))))))
