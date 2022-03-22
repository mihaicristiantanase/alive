(in-package #:alive)

(defmacro with-canvas (x y canvas &body body)
  `(let* ((w (canvas-get 'w canvas))
          (h (canvas-get 'h canvas))
          (x-unit (/ x w))
          (y-unit (/ y h)))
     ,@body))

(defparameter *2dp-gradient*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values x-unit
                y-unit
                (+ x y (+ w h))))))

(defparameter *2dp-right-corner*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values (expt x-unit 16)
                (expt y-unit 16)
                0.2))))

(defparameter *osciallation-factor* 4)

(defparameter *2dp-osciallations*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
          (values (sin (* (- (* x-unit 2) 1) *osciallation-factor*))
                  (cos (* (- (* y-unit 2) 1) *osciallation-factor*))
                  1))))

(defparameter *2dp-smooth*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values x-unit
                y-unit
                (+ (* x-unit x-unit) (* y-unit y-unit))))))

(defparameter *2dp-electric-arcs*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)
                (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)
                (/ (1+ (sin (* (* x-unit y-unit) *osciallation-factor*))) 2)))))

(defparameter *2dp-electric-arcs-colored*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values (/ (1+ (sin (* x-unit y-unit 50))) 2)
                (/ (1+ (cos (* x-unit y-unit 50))) 2)
                (/ (1+ (tan (* x-unit y-unit 50))) 2)))))

(defparameter *2dp-spotlight*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (values (tan (* (/ 1 (+ x-unit 0.00001)) y-unit))
                (tan (* (/ 1 (+ x-unit 0.00001)) y-unit))
                (* x-unit y-unit)))))

(defparameter *2dp-beam*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (let ((_x (* 2 x-unit)))
          (values (if (< _x 1.0) _x (- 2 _x))
                  (if (< _x 1.0) _x (- 2 _x))
                  (if (< _x 1.0) _x (- 2 _x)))))))

(defparameter *beam-split* 2.0)

(defparameter *2dp-double-beams*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (let ((_x (* 2 x-unit))
              (_y (* 2 y-unit)))
          (values (if (< _x 1.0) _x (- 2 _x))
                  (if (< _y 1.0) _y (- 2 _y))
                  0.3)))))

(defparameter *2dp-television*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
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
      (with-canvas x y canvas
        (let* ((xs (sin (* 12.6 x-unit)))
               (ys (sin (* 12.6 y-unit))))
          (values (- 1 (/ (abs (* xs ys)) *holes-factor*))
                  (- 1 (/ (abs (* xs ys)) *holes-factor*))
                  (- 1 (/ (abs (* xs ys)) *holes-factor*)))))))

(defparameter *sin-wave* 0.0)

(defparameter *2dp-sine*
  #'(lambda (x y canvas)
      (with-canvas x y canvas
        (let* ((xs (+ *sin-wave* (* 20 x-unit)))
               (ys (- 3 (* 6 y-unit))))
          (values (abs (- (abs (- (sin xs) ys)) 1.4))
                  x-unit
                  y-unit)))))
