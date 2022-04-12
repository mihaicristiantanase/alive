(in-package #:alive)

(defclass target ()
  ((start :accessor start)
   (stop :accessor stop :initarg :stop)
   (steps :accessor steps :initarg :steps :initform 20)
   (current-step :accessor current-step :initform 0)
   (interpolator :accessor interpolator :initform #'(lambda (x) x))))

(defclass reach-target ()
  ((val :accessor val :initarg :val)
   (tgt :accessor tgt :initarg :tgt)))

(defmethod new-target? ((tgt target))
  (zerop (current-step tgt)))

(defmethod target-goto-next-step ((tgt target))
  (setf (current-step tgt) (min (1+ (current-step tgt))
                                (steps tgt))))

(defmethod target-current ((tgt target))
  (let* ((f (/ (current-step tgt) (steps tgt)))
         (f-interpolated (funcall (interpolator tgt) f)))
    (+ (* f-interpolated (- (stop tgt) (start tgt)))
       (start tgt))))

(defmethod update-target ((rt reach-target))
  (let ((tgt (tgt rt)))
    (when (new-target? tgt)
      (setf (start tgt) (val rt)))
    (target-goto-next-step tgt)
    (setf (val rt) (target-current tgt))))
