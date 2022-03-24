(in-package #:alive)

(defun canvas-get (var canvas)
  (cdr (assoc var canvas)))

(defmacro with-canvas ((x y canvas &key (xscale 6) (yscale 40)) &rest body)
  `(let* ((w (canvas-get 'w ,canvas))
          (h (canvas-get 'h ,canvas))
          (x-unit (/ ,x w))
          (y-unit (/ ,y h))
          (xs (* ,xscale (1- (* 2 x-unit))))
          (ys (* ,yscale (- (1- (* 2 y-unit))))))
     ,@body))

(defun 2dp-dist (f x y &optional (delta 8.0))
  (- (abs (- (funcall f x) y)) delta))

(defparameter *2dp-gradient*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values x-unit
                y-unit
                (+ x y (+ w h))))))

(defparameter *2dp-right-corner*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values (expt x-unit 16)
                (expt y-unit 16)
                0.2))))

(defparameter *osciallation-factor* 4)

(defparameter *2dp-osciallations*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
          (values (sin (* (- (* x-unit 2) 1) *osciallation-factor*))
                  (cos (* (- (* y-unit 2) 1) *osciallation-factor*))
                  1))))

(defparameter *2dp-smooth*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values x-unit
                y-unit
                (+ (* x-unit x-unit) (* y-unit y-unit))))))

(defparameter *2dp-electric-arcs*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)
                (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)
                (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)))))

(defparameter *2dp-electric-arcs-colored*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values (/ (1+ (sin (* x-unit y-unit 50))) 2)
                (/ (1+ (cos (* x-unit y-unit 50))) 2)
                (/ (1+ (tan (* x-unit y-unit 50))) 2)))))

(defparameter *2dp-spotlight*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (values (tan (* (/ 1 (+ x-unit 0.00001)) y-unit))
                (tan (* (/ 1 (+ x-unit 0.00001)) y-unit))
                (* x-unit y-unit)))))

(defparameter *2dp-beam*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let ((_x (* 2 x-unit)))
          (values (if (< _x 1.0) _x (- 2 _x))
                  (if (< _x 1.0) _x (- 2 _x))
                  (if (< _x 1.0) _x (- 2 _x)))))))

(defparameter *beam-split* 2.0)

(defparameter *2dp-double-beams*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let ((_x (* 2 x-unit))
              (_y (* 2 y-unit)))
          (values (if (< _x 1.0) _x (- 2 _x))
                  (if (< _y 1.0) _y (- 2 _y))
                  0.3)))))

(defparameter *2dp-television*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let* ((_x (* *beam-split* x-unit))
               (_y (* *beam-split* y-unit))
               (_xf (floor _x))
               (_yf (floor _y)))
          (values (if (= (mod _xf 2) 0.0) (- _x _xf) (1- (- _x _xf)))
                  (if (= (mod _yf 2) 0.0) (- _y _yf) (1- (- _y _yf)))
                  0.3)))))

(defparameter *holes-factor* 0.0)

(defparameter *2dp-holes*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let* ((xs (sin (* 12.6 x-unit)))
               (ys (sin (* 12.6 y-unit))))
          (values (- 1 (/ (abs (* xs ys)) *holes-factor*))
                  (- 1 (/ (abs (* xs ys)) *holes-factor*))
                  (- 1 (/ (abs (* xs ys)) *holes-factor*)))))))

(defparameter *sin-wave* 0.0)

(defparameter *2dp-sine*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let* ((xs (+ *sin-wave* (* 20 x-unit)))
               (ys (- 3 (* 6 y-unit))))
          (values (abs (- (abs (- (sin xs) ys)) 1.4))
                  x-unit
                  y-unit)))))

(defparameter *2dp-xcube*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let* ((xs (* 5 (1- (* 2 x-unit))))
               (ys (* 40 (- (1- (* 2 y-unit))))))
          (values (- (abs (- (expt xs 3) ys)) 20.0)
                  y-unit
                  x-unit)))))

(defparameter *2dp-xsquare*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas)
        (let* ((xs (* 6 (1- (* 2 x-unit))))
               (ys (* 40 (- (1- (* 2 y-unit))))))
          (values (- (abs (- (expt xs 2) ys)) 8.0)
                  y-unit
                  x-unit)))))

(defparameter *2dp-tans*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 10 :yscale 30)
                   (values (2dp-dist #'(lambda (x) (tan x)) xs ys)
                           (2dp-dist #'(lambda (x) (tan x)) xs ys)
                           (2dp-dist #'(lambda (x) (tan x)) xs ys)))))

(defparameter *repell-factor* 0.0)

(defparameter *2dp-repel*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 0.5 :yscale (* 30 (sin *repell-factor*)))
                   (values (2dp-dist #'(lambda (x) (/ (+ x 0.001))) xs ys)
                           (- 1 x-unit)
                           (- 1 y-unit)))))

(defparameter *2dp-circle*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 30 :yscale 20)
                   (values (if (<= (abs
                                    (- (+ (expt xs 2) (expt ys 2))
                                       (expt *ripple-factor* 2)))
                                   8.0)
                               1.0 0.0)
                           (- 0.5 x-unit)
                           (- 1.2 y-unit)))))

(defparameter *2dp-simple-circle*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 20 :yscale 10)
                   (values (2dp-dist #'(lambda (x)
                                         (- (+ (expt x 2) (expt ys 2))
                                            (expt 8.0 2)))
                                     xs ys 2.0)
                           (- 1.0 x-unit)
                           (- 1.0 y-unit)))))

(defparameter *2dp-smooth-circle*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 10 :yscale 5.5)
                   (values (* 0.4 (abs (- (+ (expt xs 2) (expt ys 2)) (expt 4.0 2))))
                           (- 0.5 x-unit)
                           (- 1.2 y-unit)))))

(defparameter *ripple-factor* 2.0)

(defparameter *2dp-ripples*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 30 :yscale 20)
                   (values (sin (- (+ (expt xs 2) (expt ys 2)) (expt *ripple-factor* 2)))
                           (- 0.5 x-unit)
                           (- 1.2 y-unit)))))

(defparameter *2dp-perspective-ripple*
  #'(lambda (x y canvas)
      (with-canvas (x y canvas :xscale 4 :yscale 10)
                   (values (sin (- (+ (expt xs 2) (expt ys 2)) (expt *ripple-factor* 2)))
                           (- 0.5 x-unit)
                           (- 1.2 y-unit)))))
