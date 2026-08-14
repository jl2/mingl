;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>
(in-package :cl-user)
(defpackage :mingl.test
  (:use :cl
        :fiveam
        :alexandria
        :mingl))

(in-package :mingl.test)

(def-suite :mingl)
(in-suite :mingl)

(test display
  (5am:finishes  (mingl:display
                  (make-instance 'mingl:gl-window))))
