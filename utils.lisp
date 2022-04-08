(in-package #:alive)

(defun scale (a in-min in-max dst-min dst-max)
  (+ (* (/ (- a in-min) (- in-max in-min))
        (- dst-max dst-min))
     dst-min))
