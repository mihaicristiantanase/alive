(in-package #:alive)

(defparameter *mandelbrot-factor* 0)
(defparameter *mandelbrot-max* 1e6)
(defparameter *max* (* *mandelbrot-max* (sqrt 2)))
(defparameter *2max* (* 2 *max*))
(defparameter *mandelbrot-iterations* 1)
(defparameter *mandelbrot-zoom* (make-instance 'a-speed :v 1 :vmin 0.2))
(defparameter *mandelbrot-xoffset* (make-instance 'reach-target))
(defparameter *mandelbrot-yoffset* (make-instance 'reach-target))
(defparameter *mandelbrot-x* (make-instance 'a-speed :vmin -2.0 :vmax 2.0))
(defparameter *mandelbrot-y* (make-instance 'a-speed :vmin -2.0 :vmax 2.0))

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

(defun mandelbrot (x y w h a &key (max-iteration 100) (offset (make-instance 'pos)) (zoom 1.0))
  (let* ((x0 (* (/ 1 zoom) a (+ (scale x 0 w -2.0 0.47) (x offset))))
         (y0 (* (/ 1 zoom) (+ (scale y 0 h -1.12 1.12) (y offset))))
         (c (complex x0 y0))
         (z (complex 0)))
    (let ((solution-iteration
            (loop for iteration from 0 upto max-iteration
                  while (<= (abs z) 2) do
                    (setf z (+ (* z z) c))
                  finally (return (1- iteration)))))
      (let ((r (/ solution-iteration max-iteration)))
        (values r r r)))))

(defun mandelbrot-iterated (x y w h a)
  (mandelbrot x y w h a :max-iteration *mandelbrot-iterations* :zoom 0.7))

(defun mandelbrot-zoomed (x y w h a)
  (let ((zoom (slot-value *mandelbrot-zoom* 'v)))
    (mandelbrot x y w h a
                :offset (make-instance 'pos :x (* zoom (speed-v *mandelbrot-x*))
                                            :y (* zoom (speed-v *mandelbrot-y*)))
                :zoom zoom)))
