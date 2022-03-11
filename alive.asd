;;;; alive.asd

(asdf:defsystem #:alive
  :description "Describe alive here"
  :author "Mihai Cristian Tănase"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-cairo2 #:cl-cairo2-xlib)
  :components ((:file "package")
               (:file "pos")
               (:file "object")
               (:file "alive")))
