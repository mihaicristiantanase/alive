(in-package #:alive)

(progn
  (defparameter *sleep* 0.02)
  (defparameter *fps-max* (/ 1 *sleep*)))
(defparameter *fps-latest-value* 0)
(defparameter *fps-last-update* nil)
(defparameter *fps-min-diff* 0.0001)
(defparameter *fps-acc* '())
(defparameter *fps-acc-length* 3)

(defun update-fps ()
  (let* ((now (local-time:now))
         (fps (/ 1
                 (+ (local-time:timestamp-difference now (or *fps-last-update* now))
                    *fps-min-diff*))))
    (push fps *fps-acc*)
    (when (= (length *fps-acc*) *fps-acc-length*)
      (setf *fps-latest-value*
            (format nil "FPS: ~4,2f (max ~4,2f)"
                    (/ (reduce '+ *fps-acc*) *fps-acc-length*)
                    *fps-max*))
      (setf *fps-acc* nil))
    (setf *fps-last-update* now)))

(defun print-fps ()
  (format t "~&~a~%" *fps-latest-value*))
