(in-package #:alive)

(defparameter *mandelbrot-factor* 0)
(defparameter *mandelbrot-max* 1e6)
(defparameter *max* (* *mandelbrot-max* (sqrt 2)))
(defparameter *2max* (* 2 *max*))

(defun mandelbrot-try1 (x y &key (constant *mandelbrot-factor*) (iterations 10))
  (let ((c (complex x y)))
    (loop for i from 0 to iterations
          while (and (< (abs (realpart c)) *max*) (< (abs (imagpart c)) *max*))
          do (setf c (+ (* c c) constant)))
    (let ((r (realpart c))
          (i (imagpart c)))
      (values (/ (+ r *max*) *2max*)
              (/ (+ r *max*) *2max*)
              (/ (+ r *max*) *2max*)))))
