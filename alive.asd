;;;; alive.asd

(asdf:defsystem #:alive
  :description "Describe alive here"
  :author "Mihai Cristian Tănase"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-cairo2 #:cl-cairo2-xlib #:bordeaux-threads #:local-time)
  :components ((:file "package")
               (:file "a-speed")
               (:file "targets")
               (:file "utils")
               (:file "fps")
               (:file "pos")
               (:file "object")
               (:file "2d-plots")
               (:file "fractals")
               (:file "alive")))
