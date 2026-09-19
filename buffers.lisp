;; buffers.lisp
;;
;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:mingl)

;; (defun new-gl-array (type data)
;;   (etypecase data
;;     (list
;;      (etypecase (car data)
;;        (real
;;         ))
;;      (length data))))

(defun gl-iset (array idx value)
  "Set array position idx to value. value must be a fixnum"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type fixnum value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-fset (array idx value)
  "Set array position idx to value. value is coerced to a single-float."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type single-float value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-dset (array idx value)
  "Set array position idx to value. value is coerced to a double-float."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type double-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-get (array idx)
  "Return the array value at idx."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type gl:gl-array array))
  (gl:glaref array idx))



(defun to-gl-array (gl-type size data)
  "Create an OpenGL array of the specified type and size, initialized with the contents of arr.  If data is"
  (let* ((gl-array (gl:alloc-gl-array gl-type size)))
    (fill-pointer-offset data gl-array 0)
    gl-array))


(defun fill-buffer (buffer data)
  "Fill buffer with data."
  (with-slots (pointer) buffer
    (fill-pointer-offset data pointer 0)))


(defmethod fill-pointer-offset ((data vec2) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (+ 2 offset))


(defmethod fill-pointer-offset ((data vec3) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (+ 3 offset))

(defmethod fill-pointer-offset ((data vec4) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (gl-fset ptr (+ 3 offset) (vw data))
  (+ 4 offset))

(defmethod fill-pointer-offset ((data mat3) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for off fixnum :from 0
    :for d :of-type real :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))

(defmethod fill-pointer-offset ((data mat4) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for off fixnum :from 0
    :for d :of-type real :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))


(defmethod fill-pointer-offset ((data integer) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-iset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data real) ptr offset)
  (gl-fset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data vector) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for obj :across data
    :for off fixnum = offset :then next-off

    :for next-off fixnum = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defmethod fill-pointer-offset ((data list) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for obj :in data
    :for off fixnum = offset :then next-off
    :for next-off fixnum = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defun show-gl-array (array &key (stream t) count stride)
  "Print the contents of array to standard out."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
 (loop
    :for i fixnum :below (if count
                             (min count
                                  (gl::gl-array-size array))
                             (gl::gl-array-size array))
    :do
       (when (and stride (zerop (mod i stride)))
         (terpri stream))
       (format stream "~a " (gl:glaref array i)))
  (terpri stream))

(defun from-gl-array (array &optional count)
  "Return a Lisp array with the contents of array to standard out."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :with real-count fixnum = (if count
                                  (min count
                                       (gl::gl-array-size array))
                                  (gl::gl-array-size array))
    :with rval = (make-array real-count)
    :for i fixnum :below real-count
    :do
       (setf (aref rval i) (gl:glaref array i))
    :finally (return rval)))
