(in-package #:alive)

(defclass object ()
  ((pos :accessor o-pos :initarg :pos :initform (make-instance 'pos))
   (w :accessor o-w :initarg :w :initform 0)
   (h :accessor o-h :initarg :h :initform 0)
   (r :accessor o-r :initarg :r :initform 0)
   (g :accessor o-g :initarg :g :initform 0)
   (b :accessor o-b :initarg :b :initform 0)))

(defclass connectr (object)
  ((lr :accessor c-lr :initarg :lr :initform 0)
   (lg :accessor c-lg :initarg :lg :initform 0)
   (lb :accessor c-lb :initarg :lb :initform 0)))

(defgeneric top-left (object))
(defgeneric top-right (object))
(defgeneric bottom-right (object))
(defgeneric bottom-left (object))
(defgeneric draw-object-rect (object))
(defgeneric draw-object (object))

(defmethod top-left ((o object))
  (o-pos o))

(defmethod top-right ((o object))
  (make-instance 'pos
                 :x (+ (x (o-pos o)) (o-w o))
                 :y (y (o-pos o))))

(defmethod bottom-right ((o object))
  (make-instance 'pos
                 :x (+ (x (o-pos o)) (o-w o))
                 :y (+ (y (o-pos o)) (o-h o))))

(defmethod bottom-left ((o object))
  (make-instance 'pos
                 :x (x (o-pos o))
                 :y (+ (y (o-pos o)) (o-h o))))

(defmethod draw-object-rect ((o object))
  (draw-rect (o-pos o) (o-w o) (o-h o) (o-r o) (o-g o) (o-b o)))

(defmethod draw-object ((o connectr))
  (let ((w_ (cairo:width cairo:*context*))
        (h_ (cairo:height cairo:*context*)))
    (draw-object-rect o)
    (flet ((connect-to (x y r g b)
             (draw-line (top-left o) x y r g b)
             (draw-line (top-right o) x y r g b)
             (draw-line (bottom-right o) x y r g b)
             (draw-line (bottom-left o) x y r g b)))
      (connect-to 0 0 (c-lr o) (c-lg o) (c-lb o))
      (connect-to w_ 0 (* (c-lr o) 0.1) (c-lg o) (c-lb o))
      (connect-to w_ h_ (c-lr o) (* (c-lg o) 0.2) (c-lb o))
      (connect-to 0 h_ (c-lr o) (* (c-lg o) 0.5) (c-lb o)))))
