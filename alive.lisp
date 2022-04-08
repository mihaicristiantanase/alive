(in-package #:alive)

; examples:
; ~/quicklisp/dists/quicklisp/software/cl-cairo2-20160531-git/tutorial/cairo-samples.lisp

(defparameter *scenes* '(disco 2d-plot fractals algo-fractals))
(defparameter *scene* (nth 3 *scenes*))
(defparameter *animate* t)
(defparameter *line-alpha* 1.0)
(defparameter *objects* nil)
(defparameter *2d-plot-point-size* 1)
(defparameter *2d-plot-f* *2dp-hot-sun*)
(defparameter *fractal-f* #'mandelbrot-try4)
(defparameter *fractal-algo-f* #'mandelbrot-iterated)
(defparameter *save-frames* nil)
(defparameter *img-idx* 0)

(defun rand-interval (left right)
  (+ (random (1+ (- right left))) left))
  
(defun rand-col ()
  (random 1.0))

(defun draw-checkered-board (w h)
  (cairo:set-line-width 10.0)
  (let ((r (rand-col))
        (g (rand-col))
        (b (rand-col))
        (step 50))
    (loop for x from 0 to w by step do
      (loop for y from 0 to h by step do
        (cairo:set-source-rgb r g b)
        (cairo:rectangle x y (- step 2) (- step 2))
        (cairo:fill-path)
        (setf r (+ (* r 0.9) (* (rand-col) 0.1)))
        (setf g (+ (* g 0.9) (* (rand-col) 0.1)))
        (setf b (+ (* b 0.9) (* (rand-col) 0.1))))))
  (cairo:set-line-width 2.0))

(defun draw-gradient (ctx)
  (let* ((w (cairo:width ctx))
         (h (cairo:height ctx))
         (cx (/ w 2))
         (cy (/ h 2)))
    (unwind-protect
         (cairo:with-context (ctx)
           (cairo:set-source-rgb 0 1 0)
           (cairo:paint)
           (cairo:with-patterns ((lpat (cairo:create-linear-pattern 0 0 w h))
                                 (rpat (cairo:create-radial-pattern cx cy 20 cx cy 100)))
             (cairo:pattern-add-color-stop-rgba lpat 0 0 0 0 1)
             (cairo:pattern-add-color-stop-rgba lpat 1 1 0 1 1)
             (cairo:rectangle 30 30 (- w 60) (- h 60))
             (cairo:set-source lpat)
             (cairo:set-line-width 10.0)
             (cairo:fill-path)
             (cairo:pattern-add-color-stop-rgba rpat 0 0 1 0 0)
             (cairo:pattern-add-color-stop-rgba rpat 1 1 0 1 1)
             (cairo:set-source rpat)
             (cairo:arc cx cy 100 0 (* 2 pi)) (cairo:fill-path))))))

(defun draw-hipnotic-gradients (ctx)
  (let* ((w (cairo:width ctx))
         (h (cairo:height ctx))
         (step 10))
    (unwind-protect
         (cairo:with-context (ctx)
           (loop for offset from 0 upto (/ (min w h) 2) by step do
                                        ;(break)
             (cairo:with-patterns
                 ((lpat (cairo:create-linear-pattern offset offset (- w offset) (- h offset))))
               (cairo:pattern-add-color-stop-rgba lpat 0 0 0 0 1)
               (cairo:pattern-add-color-stop-rgba lpat 1 1 0 1 1)
               (cairo:rectangle offset offset (- w (* offset 2)) (- h (* offset 2)))
               (cairo:set-source lpat)
               (cairo:fill-path)))))))

(defun draw-arc1 (ctx)
  (let* ((w (cairo:width ctx))
         (h (cairo:height ctx))
         (size (min w h))
         (xc (/ w 2.0))
         (yc (/ h 2.0))
         (radius (- (/ size 2.0) (/ size 10.0)))
         (angle1 (* 145.0 (/ pi 180.0)))
         (angle2 (* 280.0 (/ pi 180.0))))
    (unwind-protect
         (cairo:with-context (ctx)
           (cairo:set-source-rgb 0 0 0)
           (cairo:paint)

           (draw-checkered-board w h)

           (cairo:set-source-rgb 0 0 0)
           (cairo:set-line-width 20.0)
           (cairo:arc xc yc radius angle1 angle2)
           (cairo:fill-path)

           (cairo:set-source-rgba 1 0.2 0.2 0.6)
           (cairo:set-line-width 6.0)
           (cairo:arc xc yc 10.0 0 (* 2.0 pi))
           (cairo:fill-path)

           (cairo:arc xc yc radius angle1 angle1)
           (cairo:line-to xc yc)
           (cairo:arc xc yc radius angle2 angle2)
           (cairo:line-to xc yc)
     
      (cairo:stroke)))))

(defun draw-rect (pos w h r g b)
  (cairo:set-source-rgba r g b 0.7)
  (cairo:rectangle (x pos) (y pos) w h)
  (cairo:fill-path))

(defun draw-line (p0 x1 y1 r g b)
  (cairo:set-source-rgba r g b *line-alpha*)
  (cairo:move-to (x p0) (y p0))
  (cairo:line-to x1 y1)
  (cairo:stroke))

(defun continue? ()
  *animate*)

(defun draw-scene ()
  ; TODO(mihai): compose function name and eval it
  (ecase *scene*
    (disco (draw-scene-disco))
    (2d-plot (draw-scene-2d-plot))
    (fractals (draw-scene-fractals))
    (algo-fractals (draw-scene-algo-fractals))))

(defun draw-bg-disco ()
  (cairo:set-source-rgb 0 0 0)
  (cairo:paint)
  (let ((w_ (cairo:width cairo:*context*))
        (h_ (cairo:height cairo:*context*)))
    (draw-checkered-board w_ h_))
  (cairo:set-source-rgba 0 0 0 0.5)
  (cairo:paint))

(defun draw-scene-disco ()
  (draw-bg-disco)
  (dolist (o *objects*)
    (draw-object o)))

(defun draw-scene-2d-plot ()
  (draw-scene-pixels
   (funcall *2d-plot-f*
            x
            y
            (list (cons 'w w) (cons 'h h)))))

(defun draw-scene-fractals ()
  (draw-scene-pixels
   (funcall *fractal-f*
            (/ (- x (/ w 2)) 30)
            (/ (- y (/ h 2)) 30))))

(defun draw-scene-algo-fractals ()
  (draw-scene-pixels
   (funcall *fractal-algo-f* x y w h a)))

(defun update ()
  (print-fps)

  (incf *osciallation-factor* 0.1)
  (incf *beam-split* 0.01)
  (incf *holes-factor* 0.02)
  (incf *sin-wave* 0.1)
  (incf *repell-factor* 0.01)
  (incf *ripple-factor* 0.05)
  (incf *sun-factor* 0.005)
  (incf *mandelbrot-factor* 0.3)
  (incf *mandelbrot-iterations*)

  (dolist (o *objects*)
    (adjust-pos (o-pos o) 'randomly)))

(defun draw-img-from-surface (surface)
  (cairo:set-source-surface surface 0 0)
  (cairo:paint))

(defun save-img-from-surface (surface)
  (cairo:with-png-file ((format nil "/tmp/gif/frame-~a.png" *img-idx*)
                        :RGB24
                        (cairo:width surface)
                        (cairo:height surface))
    (cairo:set-source-surface surface 0 0)
    (cairo:paint))
  (incf *img-idx*))

(defun setup ()
  (setf cairo:*context*
        (cairo:create-xlib-image-context 700 360
                                         :window-name "Live Drawing"
                                         :background-color cl-colors:+black+)))

(defun init ()
  (setf *img-idx* 0)
  (setf *osciallation-factor* 0)
  (setf *beam-split* 2.0)
  (setf *holes-factor* 0.001)
  (setf *sin-wave* 0.0)
  (setf *repell-factor* 0.0)
  (setf *ripple-factor* 2.0)
  (setf *sun-factor* 0.0)
  (setf *mandelbrot-factor* -80)
  (setf *mandelbrot-iterations* 1))

(defun draw-loop ()
  (init)

  (let ((img-surface (cairo:create-image-surface :rgb24 700 360)))
    (loop do
      (let ((cairo:*context* (cairo:create-context img-surface)))
        ; TODO(mihai): handle tearing effect due to drawing in a separate thread
        (draw-scene)
        (update))
      (draw-img-from-surface img-surface)
      (when *save-frames*
        (save-img-from-surface img-surface))
      (unless (continue?)
        (break))
      (sleep *sleep*))))

(defun draw-loop-async ()
  (bordeaux-threads:make-thread #'draw-loop :name "alive"))

; call once (setup)
;
; then, as many times as you like: (draw-loop) or (draw-loop-async)
; a separate thread is needed to allow "live" editing

(dotimes (i 100)
  (push (make-instance 'connectr
                       :pos (make-instance 'pos :x (rand-interval 40 600) :y (rand-interval 40 400))
                       :w (rand-interval 20 90)
                       :h (rand-interval 20 90)
                       :r (rand-col)
                       :g (rand-col)
                       :b (rand-col)
                       :lr (rand-col)
                       :lg (rand-col)
                       :lb (rand-col))
        *objects*))

;(dotimes (i 200)
;  (format t "~a~%" (/ (1+ (sin (/ i 10.0))) 2))
;  (setf *line-alpha* (/ (1+ (sin (/ i 10.0))) 2))
;  (sleep 0.1))
