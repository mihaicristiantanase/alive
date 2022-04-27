(in-package #:alive)

(defclass a-speed ()
  ((v :accessor speed-v :initarg :v :initform 0)
   (a :accessor a :initarg :a :initform 0)
   (vmin :accessor vmin :initarg :vmin :initform nil)
   (vmax :accessor vmax :initarg :vmax :initform nil)))

(defmethod speed-update ((s a-speed))
  (with-slots (v a vmin vmax) s
    (incf v a)
    (when (and vmin (< v vmin)) (setf v vmin))
    (when (and vmax (> v vmax)) (setf v vmax))))

(defmethod speed-inc-a ((s a-speed) a)
  (incf (slot-value s 'a) a))
