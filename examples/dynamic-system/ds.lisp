(in-package :cl-user)

(defpackage :mingl.ds
  (:use :cl :mingl #:3d-vectors #:3d-matrices)
  (:export #:ds-window
           #:start))

(in-package :mingl.ds)

(defclass ds-window (mingl:3d-window)
  (

   (dh :initform 0.125)

   (primitive-type :initform :points)
   (width :initform 128 :initarg :width)
   (height :initform 128 :initarg :height)
   (depth :initform 2 :initarg :depth)
   (min-corner :initform (vec3 -10.0f0 -10.0f0 -4.0f0) :initarg :min-corner)
   (max-corner :initform (vec3 10.0f0 10.0f0 4.0f0) :initarg :max-corner)
   (click-start :initform nil)

   (vao :initform 0)
   (program :initform nil)
;;   (joystick :initform 0 :initarg :joystick)
   (buffers)
   (vertices)
   (indices)))


(defmethod initialize ((gl-window ds-window))
  (with-slots (vao buffers vertices indices program
               dh width height depth max-corner min-corner) gl-window
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (setf buffers (gl:gen-buffers 2))
    (gl:bind-buffer :array-buffer (car buffers))

    (let* ((step (v/ (v- max-corner
                         min-corner)
                     (vec3 (if (= width 1)
                               1
                               (1- width))
                           (if (= height 1)
                               1
                               (1- height))
                           (if (= depth 1)
                               1
                               (1- depth)))))
           (data (loop
                   :for i :below width
                   :collecting
                   (loop
                     :for j :below height
                     :collecting
                     (loop
                       :for k :below depth
                       :for pt = (v+
                                  min-corner
                                  (v* (vec3 i j k) step))
                       :collecting pt
                       :collecting (let* ((r (random 1.0))
                                          (g (+ 0.5 (random 0.5)))
                                          (b (random 1.0))
                                          (alpha 0.5))
                                     (vec4 r g b alpha)))))))
      (setf vertices (mingl:to-gl-array :float
                                  (* 7 width height depth)
                                  data))
      (gl:buffer-data :array-buffer :static-draw vertices)
      (gl:bind-buffer :element-array-buffer (cadr buffers))

      (setf indices (mingl:to-gl-array :unsigned-int
                                       (* width height depth)
                                       (loop :for i :below (* width height depth) :collecting i)))
      (gl:buffer-data :element-array-buffer :static-draw indices))

    (setf program (mingl:make-shader-program "ds.geom" "ds.frag" "ds.vert"))
    (mingl:build-shader-program program)
    (mingl:use-shader-program program)

    (set-uniform program "dh" :float dh)

    (with-slots (mingl::program) program
      (let ((pos-location (gl:get-attrib-location mingl::program "in_position"))
            (pos-color (gl:get-attrib-location mingl::program "in_color")))
        (when (>= pos-location 0)
          (gl:enable-vertex-attrib-array pos-location)
          (gl:vertex-attrib-pointer pos-location
                                    3
                                    :float
                                    nil
                                    (* 7 4)
                                    0))
        (when (>= pos-color 0 )
          (gl:enable-vertex-attrib-array pos-color)
          (gl:vertex-attrib-pointer pos-color
                                    4
                                    :float
                                    nil
                                    (* 5 4)
                                    #.(* 3 4))))))
  (call-next-method))

(defmethod render ((window ds-window))
  (call-next-method)
  (with-slots (vao buffers program primitive-type depth width height) window
    (mingl:use-shader-program program)

    (gl:bind-buffer :array-buffer (car buffers))
    (gl:bind-buffer :element-array-buffer (cadr buffers))
    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count (* depth width height))))

(defmethod cleanup ((window ds-window))
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

;; (defmethod mingl:handle-cursor-position ((gl-window ds-window) x y)
;;   (with-slots (window program
;;                vao max-iterations
;;                vertices buffers
;;                center radius
;;                click-start) gl-window

;;     (when (null click-start)
;;       (return-from mingl:handle-cursor-position t))

;;     (let* ((win-size (glfw:get-window-size window))
;;            (width (car win-size))
;;            (height (cadr win-size))
;;            (start-x (car click-start))
;;            (start-y (cadr click-start))
;;            )

;;       (glfw:set-cursor-position start-x start-y window)
;;       (reload-buffers gl-window))))

;; (defmethod mingl:handle-click ((gl-window ds-window) mouse-pos mod-keys action button)
;;   (with-slots (window program
;;                vao max-iterations
;;                vertices buffers
;;                center radius
;;                click-start) gl-window

;;     (cond
;;       ((and (eql action :press))
;;        (setf click-start mouse-pos))

;;       ((eql action :release)
;;        (setf click-start nil)))
;;     t))

;; (defmethod mingl:handle-scroll ((gl-window ds-window) mouse-pos x-scroll y-scroll)

;;   (with-slots (vao max-iterations vertices buffers program mingl::window center) gl-window
;;     (let*
;;         ((win-size
;;            (glfw:get-window-size window))

;;          (width
;;            (car win-size))
;;          (height
;;            (cadr win-size))

;;          (alt-down
;;            (or (eql :press (glfw:get-key :right-alt mingl::window))
;;                (eql :press (glfw:get-key :left-alt mingl::window))))
;;          (shift-down
;;            (or (eql :press (glfw:get-key :right-shift mingl::window))
;;                (eql :press (glfw:get-key :left-shift mingl::window))))

;;          (zoom-amount
;;            (if alt-down
;;                (/ 0.125 4)
;;                (if shift-down
;;                    0.5
;;                    0.25)))
;;          (real-amount
;;            (coerce (if (< 0 y-scroll)
;;                        (- 1 zoom-amount)
;;                        (+ 1 zoom-amount))
;;                    'single-float))

;;          (scroll-center
;;            (screen-to-mandelbrot (car mouse-pos)
;;                                  (cadr mouse-pos)
;;                                  width
;;                                  height
;;                                  gl-window))
;;          (move-amount (- center scroll-center)))

;;       (declare (ignorable shift-down alt-down))

;;       (move-absolute gl-window (/ move-amount width))
;;       (zoom-relative gl-window real-amount)

;;       (reload-buffers gl-window))))

;; (defmethod handle-3d-mouse-event ((gl-window ds-window) (event sn:motion-event))
;;   (with-slots (radius) gl-window
;;     (with-slots (sn:x sn:z sn:y) event
;;       (zoom-relative gl-window
;;                      (+ 1.0f0 (* 0.05 (/ sn:y 500.0))))
;;       (move-absolute gl-window
;;                      (complex (* 0.1 (realpart radius) (/ sn:x 500.0))
;;                               (* 0.1 (imagpart radius)  (/ sn:z 500.0))))
;;       (reload-buffers gl-window)))
;;   t)

  ;; (defmethod mingl:handle-joystick ((gl-window gl-window))
;;   (with-slots (radius joystick) gl-window
;;     (when (and joystick (glfw:joystick-present-p joystick))
;;       (let* ((axes (glfw:get-joystick-axes 0))
;;              (buttons (glfw:get-joystick-buttons 0))
;;              (xd (car axes))
;;              (yd (cadr axes))
;;              (zd (caddr axes))
;;              (real-comp (* 0.02 (realpart radius) xd))
;;              (imag-comp (* -0.02 (imagpart radius)  yd))
;;              (zoom-comp (* 0.01 zd)))


;;         (declare (ignorable buttons axes))
;;         (when (< 0.1 (abs zd))
;;           (zoom-relative gl-window
;;                          (+ 1.0f0 zoom-comp)))
;;         (move-absolute gl-window
;;                         (complex
;;                          (if (> 0.01 (abs xd))
;;                              0.0
;;                              real-comp)
;;                          (if (> 0.01 (abs yd))
;;                              0.0
;;                              imag-comp)))
;;         (reload-buffers gl-window)
;;         t)))
;;   nil)


;; (defmethod handle-3d-mouse-event ((gl-window ds-window) (event sn:button-event))
;;   (call-next-method))

;; (defmethod mingl:handle-key ((gl-window ds-window) key scancode action mod-keys)
;;   (with-slots (vao max-iterations center radius vertices buffers program) gl-window
;;     (when (null (call-next-method))
;;       (let* (
;;              (shift-down (find :shift mod-keys :test #'eql))
;;              (alt-down (find :alt mod-keys :test #'eql))
;;              (ctrl-down (find :control mod-keys :test #'eql))
;;              (super-down (find :super mod-keys :test #'eql))
;;              (move-amount (if alt-down
;;                               0.005
;;                               (if shift-down
;;                                   0.1
;;                                   0.02)))
;;              (zoom-amount (if alt-down
;;                               (/ 0.125 4)
;;                               (if shift-down
;;                                   0.75
;;                                   0.5))))

;;         (declare (ignorable shift-down alt-down ctrl-down super-down))

;;         (cond
;;           ((and (or (eql action :press))
;;                 (eql key :s))
;;            (format t "(make-instance 'ds-window :center ~a :radius ~a :max-iterations ~a)~%"
;;                    center radius max-iterations))

;;           ((and (or (eql action :press)
;;                     (eql action :repeat))
;;                 (eql key :f5))
;;            (setf center #C(-0.4f0 0.0f0))
;;            (setf radius #C(2.0f0 2.0f0))
;;            (reload-buffers gl-window))

;;           ((and (or (eql action :press)
;;                     (eql action :repeat))
;;                 (and ctrl-down
;;                      (or (eql key :left)
;;                          (eql key :right))))
;;            (let* ((amount (if alt-down
;;                               2
;;                               (if shift-down
;;                                   20
;;                                   10)))
;;                   (multiplier (if (eql key :left)
;;                                   -1
;;                                   1)))

;;              (setf max-iterations (min 20000
;;                                        (max 10
;;                                             (+ max-iterations (* multiplier amount))))))
;;            (with-slots (mingl:program) program
;;              (let ((location (gl:get-uniform-location mingl:program "maxIterations")))
;;                (when (>= location 0)
;;                  (gl:program-uniformi mingl:program location
;;                                       max-iterations)))))

;;           ;; zoom by zoom-amount when up/down arrow and ctrl key, or page up/page down are pressed
;;           ((and (or (eql action :press)
;;                     (eql action :repeat))
;;                 (or (and ctrl-down
;;                          (or (eql key :up)
;;                              (eql key :down)))
;;                     (or (eql key :page-up)
;;                         (eql key :page-down))))


;;            (zoom-relative gl-window
;;                           (case key
;;                             (:page-up (- 1 zoom-amount))
;;                             (:page-down (+ 1 zoom-amount))
;;                             (:up (- 1 zoom-amount))
;;                             (:down (+ 1 zoom-amount))))
;;            (reload-buffers gl-window))

;;           ;; move around by move-amount when arrow keys are pressed
;;           ((and (or (eql action :press)
;;                     (eql action :repeat))
;;                 (and (not ctrl-down))
;;                 (or (eql key :up)
;;                     (eql key :down)
;;                     (eql key :left)
;;                     (eql key :right)))
;;            (move-relative gl-window
;;                           (case key
;;                             (:up (complex 0 move-amount))
;;                             (:down (complex 0 (- move-amount)))
;;                             (:right (complex move-amount))
;;                             (:left (complex (- move-amount) 0))))
;;            (reload-buffers gl-window))

;;           ;; Wasn't handled
;;           (t nil))))))

(defun start ()

  (mingl:add-shader-search-directory (asdf:system-relative-pathname :mingl.examples "dynamic-system/"))
  (let ((window (make-instance 'ds-window
                               :use-main-thread t
                               :wait-for-exit nil)))
    (mingl:display window)))

;; (start)
