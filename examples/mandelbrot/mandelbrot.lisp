(in-package :cl-user)

(defpackage :mingl.mandelbrot
  (:use :cl :mingl #:3d-vectors #:3d-matrices)
  (:export #:mandelbrot-window
           #:start))

(in-package :mingl.mandelbrot)

(defclass mandelbrot-window (mingl:gl-window)
  ((buffers)
   (center :initarg :center :initform #C(-0.4f0 0.0f0))
   (radius :initarg :radius :initform #C(2.0f0 2.0f0))
   (max-iterations :initarg :max-iterations :initform 500)
   (vao :initform 0)
   (program :initform nil)
   (vertices)
   (indices)))

(defun compute-min-max (window)
  "Returns (values real-min real-max imag-min imag-max)"
  (with-slots (center radius) window
    (let ((cr (realpart center))
          (ci (imagpart center))
          (rr (realpart radius))
          (ri (imagpart radius)))

      (list -1.0f0  1.0f0 0.0f0
            (- cr rr) (+ ci ri)

            -1.0f0 -1.0f0 0.0f0
            (- cr rr) (- ci ri)

            1.0f0  1.0f0 0.0f0
            (+ cr rr) (+ ci ri)

            1.0f0 -1.0f0 0.0f0
            (+ cr rr) (- ci ri)))))

(defmethod initialize ((window mandelbrot-window))
  (with-slots (vao buffers vertices indices program max-iterations) window
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (setf buffers (gl:gen-buffers 2))
    (gl:bind-buffer :array-buffer (car buffers))
    

    (setf vertices (let ((window (compute-min-max window)))
                     (mingl:to-gl-array :float
                                        (length window)
                                        window)))
    
    (gl:buffer-data :array-buffer :static-draw vertices)
    (gl:bind-buffer :element-array-buffer (cadr buffers))

    (setf indices (mingl:to-gl-array :unsigned-int 6 #(0 1 2 1 3 2)))
    (gl:buffer-data :element-array-buffer :static-draw indices)

    (setf program (mingl:make-shader-program "mandelbrot.frag" "mandelbrot.vert"))
    (mingl:build-shader-program program)
    (mingl:use-shader-program program)
    (with-slots (program) program
      (let ((location (gl:get-uniform-location program  "maxIterations")))
        (when (> location 0)
          (gl:program-uniformi program location
                               max-iterations))))
    
    (with-slots (mingl::program) program

      (let ((pos-location (gl:get-attrib-location mingl::program "in_position"))
            (pos-uv (gl:get-attrib-location mingl::program "in_uv")))
        (format t "in_position: ~a~%in_uv ~a~%" pos-location pos-uv)
        (when (>= pos-location 0)
          (gl:enable-vertex-attrib-array pos-location)
          (gl:vertex-attrib-pointer pos-location
                                    3
                                    :float
                                    nil
                                    (* 5 4)
                                    0)
          )
        (when (>= pos-uv 0 )
          (gl:enable-vertex-attrib-array pos-uv)
          (gl:vertex-attrib-pointer pos-uv
                                    2
                                    :float
                                    nil
                                    (* 5 4)
                                    (* 3 4))))))
  (call-next-method))

(defmethod render ((window mandelbrot-window))
  (call-next-method)
  (with-slots (vao buffers program) window
    (gl:bind-vertex-array vao)
    (mingl:use-shader-program program)
    
    (gl:bind-buffer :array-buffer (car buffers))
    (gl:bind-buffer :element-array-buffer (cadr buffers))
    (gl:draw-elements :triangles
                      (gl:make-null-gl-array :unsigned-int)
                      :count 6)))

(defmethod cleanup ((window mandelbrot-window))
  (when (slot-boundp window 'vertices)
    (with-slots (vao buffers vertices indices) window
      (gl:bind-vertex-array vao)

      (gl:delete-buffers buffers)      
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:free-gl-array vertices)
      (gl:free-gl-array indices)
      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))))
  
  (call-next-method))

(defun move-relative (gl-window amount)
  (with-slots (center radius) gl-window
    (incf center (complex (* (realpart amount) (realpart radius))
                          (* (imagpart amount) (imagpart radius))))))

(defun zoom-relative (gl-window amount)
  (with-slots (radius) gl-window
    (setf radius (* radius amount))))

(defmethod handle-key ((gl-window mandelbrot-window) key scancode action mod-keys)
  (when (null (call-next-method))
    (let* (
           (shift-down (find :shift mod-keys :test #'eql))
           (alt-down (find :alt mod-keys :test #'eql))
           (ctrl-down (find :control mod-keys :test #'eql))
           (super-down (find :super mod-keys :test #'eql))
           (move-amount (if alt-down
                            0.005
                            (if shift-down
                                0.1
                                0.02)))
           (zoom-amount (if alt-down
                            (/ 0.125 4)
                            (if shift-down
                                0.75
                                0.5)))
           
           )

      (declare (ignorable shift-down alt-down ctrl-down super-down))

      ;;(format t "~a ~a ~a ~a ~a ~a ~a~%" mod-keys shift-down alt-down ctrl-down super-down move-amount zoom-amount)
      
      (with-slots (vao max-iterations vertices buffers program) gl-window

        (gl:bind-vertex-array vao)
        (cond

          ((and (or (eql action :press)
                    (eql action :repeat))
                (and ctrl-down
                     (or (eql key :left)
                         (eql key :right))))
           (let* ((amount (if alt-down
                             2
                             (if shift-down
                                 20
                                 10)))
                  (multiplier (if (eql key :left)
                                  -1
                                  1)))
             
             (setf max-iterations (min 20000
                                       (max 10
                                            (+ max-iterations (* multiplier amount))))))
           (with-slots (mingl:program) program
             (let ((location (gl:get-uniform-location mingl:program "maxIterations")))
               (when (>= location 0)
                 (gl:program-uniformi mingl:program location
                                      max-iterations)))))
          
          ;; zoom by zoom-amount when up/down arrow and ctrl key, or page up/page down are pressed
          ((and (or (eql action :press)
                    (eql action :repeat))
                (or (and ctrl-down
                         (or (eql key :up)
                             (eql key :down)))
                    (or (eql key :page-up)
                        (eql key :page-down))))
               

           (zoom-relative gl-window
                          (case key
                            (:page-up (- 1 zoom-amount))
                            (:page-down (+ 1 zoom-amount))
                            (:up (- 1 zoom-amount))
                            (:down (+ 1 zoom-amount))))
           (fill-pointer-offset (compute-min-max gl-window) vertices 0)
           (gl:buffer-sub-data :array-buffer vertices))

          ;; move around by move-amount when arrow keys are pressed
          ((and (or (eql action :press)
                    (eql action :repeat))
                (and (not ctrl-down))
                (or (eql key :up)
                    (eql key :down)
                    (eql key :left)
                    (eql key :right)))
           (move-relative gl-window
                          (case key
                            (:up (complex 0 move-amount))
                            (:down (complex 0 (- move-amount)))
                            (:right (complex move-amount))
                            (:left (complex (- move-amount) 0))))
           (fill-pointer-offset (compute-min-max gl-window) vertices 0)
           (gl:buffer-sub-data :array-buffer vertices))

              
              
          ;; Wasn't handled
          (t nil))))))

(defun start ()

  (mingl:add-shader-search-directory (asdf:system-relative-pathname :mingl.examples "mandelbrot/"))
  (let ((window (make-instance 'mandelbrot-window
                               :use-main-thread nil
                               :wait-for-exit t)))
    (mingl:display window)))

;; (start)

