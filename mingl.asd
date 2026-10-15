;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:mingl
  :description "A minimal OpenGL library."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t

  :depends-on (#:alexandria
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
               #:lparallel
               #:gtools)

  :components ((:file "package")
               (:file "common")
               (:file "buffers")
               (:file "gl-shader")
               (:file "shader-program")
               (:file "texture")
               (:file "png-texture")
               (:file "gl-window")
               (:file "3d-window")
               (:file "mingl"))

  :in-order-to ((test-op (test-op
                          mingl.test))))
