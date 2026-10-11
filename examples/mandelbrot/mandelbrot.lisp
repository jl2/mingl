(in-package :cl-user)

(defpackage :mingl.mandelbrot
  (:use :cl :mingl #:3d-vectors #:3d-matrices)
  (:export #:mandelbrot-window
           #:start))

(in-package :mingl.mandelbrot)

(defclass mandelbrot-window (mingl:gl-window)
  ((center :initarg :center :initform #C(-0.4f0 0.0f0))
   (radius :initarg :radius :initform #C(2.0f0 2.0f0))
   (max-iterations :initarg :max-iterations :initform 1200)
   (click-start :initform nil)

   (vao :initform 0)
   (program :initform nil)
   (joystick :initform 0 :initarg :joystick)
   (buffers)
   (vertices)
   (indices)))

(defun reload-buffers (gl-window)
  (with-slots (vertices) gl-window
    (fill-pointer-offset (compute-min-max gl-window) vertices 0)
    (gl:buffer-sub-data :array-buffer vertices)))

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

(defmethod initialize ((gl-window mandelbrot-window))
  (with-slots (vao buffers vertices indices program max-iterations window center radius) gl-window
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (setf buffers (gl:gen-buffers 2))
    (gl:bind-buffer :array-buffer (car buffers))

    (let* ((win-size (glfw:get-window-size window))
             (width (car win-size))
             (height (cadr win-size)))
        (setf radius
              (if (< width height)
                  (complex (* 1.5(/ width height)) 2.0)
                  (complex 2.0 (* 1.5 (/ width height))))))
    (setf vertices (let ((window-coordinates (compute-min-max gl-window)))
                     (mingl:to-gl-array :float
                                        (length window-coordinates)
                                        window-coordinates)))

    (gl:buffer-data :array-buffer :static-draw vertices)
    (gl:bind-buffer :element-array-buffer (cadr buffers))

    (setf indices (mingl:to-gl-array :unsigned-int 6 #(0 1 2 1 3 2)))
    (gl:buffer-data :element-array-buffer :static-draw indices)

    (setf program (mingl:make-shader-program "mandelbrot.frag" "mandelbrot.vert"))
    (mingl:build-shader-program program)
    (mingl:use-shader-program program)

    (set-uniform program "maxIterations" :int max-iterations)

    (with-slots (mingl::program) program
      (let ((pos-location (gl:get-attrib-location mingl::program "in_position"))
            (pos-uv (gl:get-attrib-location mingl::program "in_uv")))
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
                                    #.(* 3 4))))))
  (call-next-method))

(defmethod render ((window mandelbrot-window))
  (call-next-method)
  (with-slots (vao buffers program) window
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

(defun screen-to-mandelbrot (x y
                             width height
                             gl-window)
  (with-slots (center radius) gl-window
    (let ((real-min (- (realpart center) (realpart radius)))
          (imag-min (- (imagpart center) (imagpart radius)))

          (real-max (+ (realpart center) (realpart radius)))
          (imag-max (+ (imagpart center) (imagpart radius))))


      (complex
       (coerce (+ (* (/ x width) (- real-max real-min))
                  real-min)
               'single-float)
       (coerce (+ (* (/ y height) (- imag-min imag-max))
                  imag-min) 'single-float)))))

(defun move-absolute (gl-window amount)
  (with-slots (center radius) gl-window
    (incf center amount)))

(defun move-relative (gl-window amount)
  (with-slots (center radius) gl-window
    (incf center (complex (* (realpart amount) (realpart radius))
                          (* (imagpart amount) (imagpart radius))))))

(defun zoom-relative (gl-window amount)
  (with-slots (radius) gl-window
    (setf radius (* radius amount))))

(defmethod mingl:handle-cursor-position ((gl-window mandelbrot-window) x y)
  (with-slots (window program
               vao max-iterations
               vertices buffers
               center radius
               click-start) gl-window

    (when (null click-start)
      (return-from mingl:handle-cursor-position t))

    (let* ((win-size (glfw:get-window-size window))
           (width (car win-size))
           (height (cadr win-size))
           (start-x (car click-start))
           (start-y (cadr click-start))
           (click-original (screen-to-mandelbrot start-x
                                                 start-y
                                                 width
                                                 height
                                                 gl-window))
           (click-now (screen-to-mandelbrot x
                                            y
                                            width
                                            height
                                            gl-window))
           )


      (move-absolute gl-window
                     (- click-original click-now))
      (glfw:set-cursor-position start-x start-y window)
      (reload-buffers gl-window))))

(defmethod mingl:handle-click ((gl-window mandelbrot-window) mouse-pos mod-keys action button)
  (with-slots (window program
               vao max-iterations
               vertices buffers
               center radius
               click-start) gl-window

    (cond
      ((and (eql action :press))
       (setf click-start mouse-pos))

      ((eql action :release)
       (setf click-start nil)))
    t))

(defmethod mingl:handle-scroll ((gl-window mandelbrot-window) mouse-pos x-scroll y-scroll)

  (with-slots (vao max-iterations vertices buffers program mingl::window center) gl-window
    (let*
        ((win-size
           (glfw:get-window-size window))

         (width
           (car win-size))
         (height
           (cadr win-size))

         (alt-down
           (or (eql :press (glfw:get-key :right-alt mingl::window))
               (eql :press (glfw:get-key :left-alt mingl::window))))
         (shift-down
           (or (eql :press (glfw:get-key :right-shift mingl::window))
               (eql :press (glfw:get-key :left-shift mingl::window))))

         (zoom-amount
           (if alt-down
               (/ 0.125 4)
               (if shift-down
                   0.5
                   0.25)))
         (real-amount
           (coerce (if (< 0 y-scroll)
                       (- 1 zoom-amount)
                       (+ 1 zoom-amount))
                   'single-float))

         (scroll-center
           (screen-to-mandelbrot (car mouse-pos)
                                 (cadr mouse-pos)
                                 width
                                 height
                                 gl-window))
         (move-amount (- center scroll-center)))

      (declare (ignorable shift-down alt-down))

      (move-absolute gl-window (/ move-amount width))
      (zoom-relative gl-window real-amount)

      (reload-buffers gl-window))))

(defmethod handle-3d-mouse-event ((gl-window mandelbrot-window) (event sn:motion-event))
  (with-slots (radius) gl-window
    (with-slots (sn:x sn:z sn:y) event
      (zoom-relative gl-window
                     (+ 1.0f0 (* 0.05 (/ sn:y 500.0))))
      (move-absolute gl-window
                     (complex (* 0.1 (realpart radius) (/ sn:x 500.0))
                              (* 0.1 (imagpart radius)  (/ sn:z 500.0))))
      (reload-buffers gl-window)))
  t)

(defmethod mingl:handle-joystick ((gl-window gl-window))
  (with-slots (radius joystick) gl-window
    (when (and joystick (glfw:joystick-present-p joystick))
      (let* ((axes (glfw:get-joystick-axes 0))
             (buttons (glfw:get-joystick-buttons 0))
             (xd (car axes))
             (yd (cadr axes))
             (zd (caddr axes))
             (real-comp (* 0.02 (realpart radius) xd))
             (imag-comp (* -0.02 (imagpart radius)  yd))
             (zoom-comp (* 0.01 zd)))


        (declare (ignorable buttons axes))
        (when (< 0.1 (abs zd))
          (zoom-relative gl-window
                         (+ 1.0f0 zoom-comp)))
        (move-absolute gl-window
                        (complex
                         (if (> 0.01 (abs xd))
                             0.0
                             real-comp)
                         (if (> 0.01 (abs yd))
                             0.0
                             imag-comp)))
        (reload-buffers gl-window)
        t)))
  nil)


(defmethod handle-3d-mouse-event ((gl-window mandelbrot-window) (event sn:button-event))
  (call-next-method))

(defmethod mingl:handle-key ((gl-window mandelbrot-window) key scancode action mod-keys)
  (with-slots (vao max-iterations center radius vertices buffers program) gl-window
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
                                  0.5))))

        (declare (ignorable shift-down alt-down ctrl-down super-down))

        (cond
          ((and (or (eql action :press))
                (eql key :s))
           (format t "(make-instance 'mandelbrot-window :center ~a :radius ~a :max-iterations ~a)~%"
                   center radius max-iterations))

          ((and (or (eql action :press)
                    (eql action :repeat))
                (eql key :f5))
           (setf center #C(-0.4f0 0.0f0))
           (setf radius #C(2.0f0 2.0f0))
           (reload-buffers gl-window))

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
           (reload-buffers gl-window))

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
           (reload-buffers gl-window))

          ;; Wasn't handled
          (t nil))))))

(defun start ()

  (mingl:add-shader-search-directory (asdf:system-relative-pathname :mingl.examples "mandelbrot/"))
  (let ((window (make-instance 'mandelbrot-window
                               :use-main-thread t
                               :wait-for-exit nil)))
    (mingl:display window)))

;; (start)
