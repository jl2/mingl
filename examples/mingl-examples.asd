;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :mingl.examples-asd
  (:use :cl :asdf))
(in-package :mingl.examples-asd)

(asdf:defsystem #:mingl.examples
  :description "Mingl examples"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t

  :depends-on (#:mingl
               #:alexandria
               #:spacenav
               #:alexandria
               #:cl-glfw3
               #:cl-opengl
               #:3d-vectors
               #:3d-matrices
               #:ieee-floats
               #:local-time
               #:osicat
               #:zpng
               #:simple-png
               #:bordeaux-threads
               #:trivial-main-thread
               #:lparallel)

  :components ((:module "mandelbrot"
                :components
                ((:file "mandelbrot")))))
