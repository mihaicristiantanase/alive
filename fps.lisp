(in-package #:alive)

(defparameter *sleep* 0.1)
(defparameter *fps-last-update* nil)
(defparameter *fps-min-diff* 0.0001)
(defparameter *fps-max* (/ 1 *sleep*))
(defparameter *fps-acc* '())
(defparameter *fps-acc-length* 3)

(defun print-fps ()
  (let* ((now (local-time:now))
         (fps (/ 1
                 (+ (local-time:timestamp-difference now (or *fps-last-update* now))
                    *fps-min-diff*))))
    (push fps *fps-acc*)
    (when (= (length *fps-acc*) *fps-acc-length*)
      (format t "~&~a FPS: ~4,2f (max ~4,2f)~%"
              now
              (/ (reduce '+ *fps-acc*) *fps-acc-length*)
              *fps-max*)
      (setf *fps-acc* nil))
    (setf *fps-last-update* now)))
