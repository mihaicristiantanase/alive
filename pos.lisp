(in-package #:alive)

(defparameter *adjust-type* 'diagonal)
(defparameter *adjust-factor* 3.0)

(defclass pos ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(defgeneric adjust-pos (pos type)
  (:documentation "Adjust position"))

(defmethod adjust-pos ((pos pos) type)
  (with-slots (x y) pos
    (case type
      (diagonal
       (setf x (+ x 0.2))
       (setf y (+ y 0.2)))
      (randomly
       (setf x (+ x (* *adjust-factor* (1- (random 2.0)))))
       (setf y (+ y (* *adjust-factor* (1- (random 2.0)))))))))
