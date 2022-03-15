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

;(dotimes (i 420)
;  (setf *osciallation-factor* (/ i 40))
;  (sleep 0.2))
