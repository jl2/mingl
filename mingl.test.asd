;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :mingl.test-asd
  (:use :cl :asdf))
(in-package :mingl.test-asd)

(asdf:defsystem #:mingl.test
  :description "Tests for mingl package."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t

  :depends-on (:mingl
               :fiveam)
  
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op
            :after (op c)
            (eval
             (read-from-string
              "(every #'fiveam::test-passed-p
                     (5am:run :mingl))"))))
