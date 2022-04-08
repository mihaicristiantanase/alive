(in-package #:alive)

(defparameter *mandelbrot-factor* 0)
(defparameter *mandelbrot-max* 1e6)
(defparameter *max* (* *mandelbrot-max* (sqrt 2)))
(defparameter *2max* (* 2 *max*))

(defun mandelbrot-try-base (x y &key constant iterations)
  (let ((c (complex x y))
        (last-i 0))
    (loop for i from 0 to iterations
          while (and (< (abs (realpart c)) *max*) (< (abs (imagpart c)) *max*))
          do (progn (setf c (+ (* c c) constant)
                          last-i i)))
    (values (realpart c)
            (imagpart c)
            (/ last-i iterations))))

(defun mandelbrot-try1 (x y &key (constant *mandelbrot-factor*) (iterations 10))
  (multiple-value-bind (r i s)
      (mandelbrot-try-base x y :constant constant :iterations iterations)
    (values (/ (+ r *max*) *2max*)
            (/ (+ r *max*) *2max*)
            (/ (+ r *max*) *2max*))))

(defun mandelbrot-try2 (x y &key (constant *mandelbrot-factor*) (iterations 10))
  (multiple-value-bind (r i s)
      (mandelbrot-try-base x y :constant constant :iterations iterations)
    (values (/ (+ r *max*) *2max*)
            (/ (+ i *max*) *2max*)
            0.4)))

(defun mandelbrot-try3 (x y &key (constant *mandelbrot-factor*) (iterations 10))
  (multiple-value-bind (r i s)
      (mandelbrot-try-base x y :constant constant :iterations iterations)
    (values (/ (+ r *max*) *2max*)
            (/ (+ i *max*) *2max*)
            (abs (cos (* 0.3 (/ (+ r *max*) *2max*)))))))

(defun mandelbrot-try4 (x y &key (constant *mandelbrot-factor*) (iterations 10))
  (multiple-value-bind (r i s)
      (mandelbrot-try-base x y :constant constant :iterations iterations)
    (values (* s (/ (+ r *max*) *2max*))
            (* s (/ (+ i *max*) *2max*))
            0.0)))

(defun mandelbrot (px py w h a &key (max-iteration 100))
  (let ((x0 (* a (scale px 0 w -2.0 0.47)))
        (y0 (scale py 0 h -1.12 1.12) )
        (x 0)
        (y 0)
        (tmp))
    (let ((solution-iteration
            (loop for iteration from 0 upto max-iteration
                  while (<= (+ (* x x) (* y y)) (* 2 2)) do
                    (setf tmp (+ (* x x) (- (* y y)) x0))
                    (setf y (+ (* 2 x y) y0))
                    (setf x tmp)
                  finally (return (1- iteration)))))
      (values
       (/ solution-iteration max-iteration)
       (/ solution-iteration max-iteration)
       (/ solution-iteration max-iteration)))))
