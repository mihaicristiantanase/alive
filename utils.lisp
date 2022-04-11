(in-package #:alive)

;; Colors

(defun rand-interval (left right)
  (+ (random (1+ (- right left))) left))

(defun rand-col ()
  (random 1.0))

(defun generate-color-palette ()
  (loop for i below 16 collect (list (rand-col) (rand-col) (rand-col))))

;;; Pixels Pixels Pixels

(defun scale (a in-min in-max dst-min dst-max)
  (+ (* (/ (- a in-min) (- in-max in-min))
        (- dst-max dst-min))
     dst-min))

;; split by 6
(defmacro split-by-6 (w h)
  `(list
    (cons (cons 0 (floor (* ,w 0.33))) (cons 0 (floor (* ,h 0.5))))
    (cons (cons (floor (* ,w 0.33)) (floor (* ,w 0.67))) (cons 0 (floor (* ,h 0.5))))
    (cons (cons (floor (* ,w 0.67)) ,w) (cons 0 (floor (* ,h 0.5))))
    (cons (cons 0 (floor (* ,w 0.33))) (cons (floor (* ,h 0.5)) ,h))
    (cons (cons (floor (* ,w 0.33)) (floor (* ,w 0.67))) (cons (floor (* ,h 0.5)) ,h))
    (cons (cons (floor (* ,w 0.67)) ,w) (cons (floor (* ,h 0.5)) ,h))))

;; split by 4
(defmacro split-by-4 (w h)
  `(list (cons (cons 0 (floor (* ,w 0.5))) (cons 0 (floor (* ,h 0.5))))
         (cons (cons (floor (* ,w 0.5)) ,w) (cons 0 (floor (* ,h 0.5))))
         (cons (cons 0 (floor (* ,w 0.5))) (cons (floor (* ,h 0.5)) ,h))
         (cons (cons (floor (* ,w 0.5)) ,w) (cons (floor (* ,h 0.5)) ,h))))

;; split by 2
(defmacro split-by-2 (w h)
  `(list (cons (cons 0 (floor (* ,w 0.5))) (cons 0 ,h))
         (cons (cons (floor (* ,w 0.5)) ,w) (cons 0 ,h))))

;; split by n
(defmacro split-by-n (w h n)
  ;; TODO
  (cond
    ((<= n 1) (values 1 1))
    ((<= n 2) (values (/ 1 2) 1))
    ((<= n 3) (values (/ 1 3) 1)))
  )

(defmacro draw-scene-pixels (&rest body)
  `(progn
     (cairo:set-source-rgb 0.2 0.2 0.2)
     (cairo:paint)
     (let* ((w (cairo:width cairo:*context*))
            (h (cairo:height cairo:*context*))
            (a (/ w h))
            (canvas (make-array (list w h) :initial-element (list 0 0 0))))
       ;; assume number of threads is 4
       ;; TODO: handle proper split when *2d-plot-point-size* is not 1
       (let ((t1 (bordeaux-threads:make-thread
                  #'(lambda ()
                      (loop for x from 0 below (/ w 2) by *2d-plot-point-size* do
                        (loop for y from 0 below (/ h 2) by *2d-plot-point-size* do
                          (multiple-value-bind (r g b) ,@body
                            (setf (aref canvas x y) (list r g b))))))
                  :name "draw-scene-pixels-1"))
             (t2 (bordeaux-threads:make-thread
                  #'(lambda ()
                      (loop for x from (/ w 2) below w by *2d-plot-point-size* do
                        (loop for y from 0 below (/ h 2) by *2d-plot-point-size* do
                          (multiple-value-bind (r g b) ,@body
                            (setf (aref canvas x y) (list r g b))))))
                  :name "draw-scene-pixels-2"))
             (t3 (bordeaux-threads:make-thread
                  #'(lambda ()
                      (loop for x from 0 below (/ w 2) by *2d-plot-point-size* do
                        (loop for y from (/ h 2) below h by *2d-plot-point-size* do
                          (multiple-value-bind (r g b) ,@body
                            (setf (aref canvas x y) (list r g b))))))
                  :name "draw-scene-pixels-3"))
             (t4 (bordeaux-threads:make-thread
                  #'(lambda ()
                      (loop for x from (/ w 2) below w by *2d-plot-point-size* do
                        (loop for y from (/ h 2) below h by *2d-plot-point-size* do
                          (multiple-value-bind (r g b) ,@body
                            (setf (aref canvas x y) (list r g b))))))
                  :name "draw-scene-pixels-4")))
         (bordeaux-threads:join-thread t1)
         (bordeaux-threads:join-thread t2)
         (bordeaux-threads:join-thread t3)
         (bordeaux-threads:join-thread t4))
       ;; actual drawing needs to be executed on the same thread
       (loop for x from 0 below w by *2d-plot-point-size* do
         (loop for y from 0 below h by *2d-plot-point-size* do
           (let ((color (aref canvas x y)))
             (cairo:set-source-rgb (nth 0 color) (nth 1 color) (nth 2 color))
             (cairo:rectangle x y *2d-plot-point-size* *2d-plot-point-size*)
             (cairo:fill-path)))))))
