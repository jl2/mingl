;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:mingl)

(defclass 3d-window (gl-window)

  ((pos
    :initform (vec3 0 0 -5)
    :initarg :pos
    :type vec3
    :documentation "Camera position.")
   (quat
    :initform (vec4 0 0 0 1.0)
    :initarg :pos
    :type vec4
    :documentation "Orientation quaternion (x,y,z,w) w:real xyz: imaginary")
   (rotation-enabled
    :initform t
    :type (or t nil)
    :initarg :rotation-enabled
    :documentation "nil if view rotation is disabled.  non-nil if view rotation is enabled.")
   (zoom-enabled
    :initform t
    :type (or t nil)
    :initarg :zoom-enabled
    :documentation "nil if zooming is disabled.  non-nil if zooming is enabled.")
   (field-of-view
    :initform 80.0
    :initarg :field-of-view
    :type real
    :documentation "The view field of view for perspective viewing.")
   (camera-type
    :initform :perspective
    :initarg :camera-typpe
    :documentation "The camera type: :orthographic or :perspective")
   (near
    :initform 0.1
    :initarg :near
    :type real
    :documentation "The view's near distance.")
   (far
    :initform 100.0
    :initarg :far
    :type real
    :documentation "The view's far distance.")
   )

  (:documentation "A viewer with keyboard and 3d mouse camera rotation around a target point."))

(defgeneric view-matrix (window))
(defmethod view-matrix ((viewer 3d-window))
  (with-slots (program field-of-view aspect-ratio near far quat pos camera-type) viewer
    (let ((xform (m*
                  (ecase camera-type
                    (:orthographic (mortho -1.0 1.0 -1.0 1.0 near far))
                    (:perspective (mperspective field-of-view aspect-ratio near far)))
                  (mtranslation pos)
                  (gtools:make-mat4 quat))))
      (set-uniform program "view_transform" :mat4
                   xform)
      xform)))

(defgeneric reset-view-safe (window))
(defmethod reset-view-safe ((window 3d-window))
  (with-slots (pos quat) window
    (setf pos (vec3 0 0 -5))
    (setf quat (vec4 0 0 0.0 1))))

(defmethod render :before ((gl-window gl-window))
  (view-matrix gl-window))

(defmethod handle-key ((viewer 3d-window) key scancode action mod-keys)
  (declare (ignorable scancode mod-keys)
           (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type list mod-keys))
  (with-gl-window-lock (viewer)
    (with-slots (aspect-ratio rotation-enabled zoom-enabled pos quat) viewer
      (let* (
             (view-changed nil)
             (x 0)
             (y 0)
             (z 0)
             (rx 0)
             (ry 0)
             (rz 0))
        (cond

          ((and (eq key :r) (eq action :press))
           (reset-view-safe viewer)
           (setf view-changed t)
           t)


          ((and (eq key :x)
                (find action '(:repeat :press)))
           (incf rx)
           (setf view-changed t)
           t)

          ((and (eq key :x)
                (find :shift mod-keys)
                (find action '(:repeat :press)))
           (decf rx)
           (setf view-changed t)
           t)


          ((and (eq key :y) (find action '(:repeat :press)))
           (incf ry)
           (setf view-changed t)
           t)

          ((and (eq key :y)
                (find :shift mod-keys)
                (find action '(:repeat :press)))
           (decf ry)
           (setf view-changed t)
           t)

          ((and (eq key :z) (find action '(:repeat :press)))
           (incf rz)
           (setf view-changed t)
           t)

          ((and (eq key :z) (find action '(:repeat :press)))
           (incf rz)
           (setf view-changed t)
           t)

          ((and (eq key :x)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf x)
           (setf view-changed t)
           t)
          ((and (eq key :x)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf x)
           (setf view-changed t)
           t)
          ((and (eq key :y)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf y)
           (setf view-changed t)
           t)
          ((and (eq key :y)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf y)
           (setf view-changed t)
           t)
          ((and (eq key :z)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf z)
           (setf view-changed t)
           t)
          ((and (eq key :z)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf z)
           (setf view-changed t)
           t)


          (t nil))
        (when view-changed
          (let ((len (sqrt
                      (+ (* rx rx)
                         (* ry ry)
                         (* rz rz)))))
            (when (and rotation-enabled
                       (not (zerop len)))
              (setf quat (gtools:qrotate quat
                                         (* len 0.01)
                                         (vec3 (/ rx len -1)
                                               (/ ry len -1)
                                               (/ rz len 1)))))
            (when zoom-enabled
              (setf pos (v+ pos
                            (vec3-quaternion-rotate quat
                                                    (vec3 (- x)
                                                          (- y)
                                                          z))))))
          t))))

  ;; TODO: Figure out a way to avoid locking the mutex a second time here,
  ;;       preferably without using a recursive lock.
  (call-next-method))

(defmethod handle-3d-mouse-event ((viewer 3d-window) (event sn:motion-event))
  (with-gl-window-lock (viewer)
    (with-slots (pos
                 quat
                 zoom-enabled
                 rotation-enabled
                 view-changed) viewer
      (setf view-changed t)
      (with-slots (sn:x sn:y sn:z
                   sn:rx sn:ry sn:rz) event


        (let* ((nx (/ sn:x 500.0))
               (ny (/ sn:y 500.0))
               (nz (/ sn:z 500.0))
               (nrx (/ sn:rx 2500.0))
               (nry (/ sn:ry 2500.0))
               (nrz (/ sn:rz 2500.0))
               (len (sqrt (+ (* nrx nrx)
                             (* nry nry)
                             (* nrz nrz)))))
          (when (and rotation-enabled
                     (not (zerop len)))
            (setf quat (gtools:qrotate quat
                                       len
                                       (vec3 (/ nrx len
                                                -1.0)
                                             (/ nry  len
                                                -1.0)
                                             (/ nrz  len
                                                1.0)))))
          (when zoom-enabled
            (setf pos (v+ pos (vec3-quaternion-rotate
                               quat
                               (vec3 (* nx -1.8)
                                     (* ny -1.8)
                                     (* nz 1.8)))))))))))

(defmethod handle-joystick ((viewer 3d-window))
  (with-slots (pos
               quat
               zoom-enabled
               joystick
               rotation-enabled
               view-changed) viewer
    (when (null joystick)
      (return-from handle-joystick nil))
    (let* ((buttons (make-array (length #1=(the list (glfw:get-joystick-buttons joystick))) :element-type 'fixnum :initial-contents #1#))
           (axes (make-array (length #2= (the list (glfw:get-joystick-axes joystick))) :element-type 'single-float :initial-contents #2#)))
      (declare (ignorable buttons))
      (setf view-changed t)
      (let ((len (sqrt (+ (* (aref axes 0) (aref axes 0))
                          (* (aref axes 1) (aref axes 1))
                          (* (aref axes 2) (aref axes 2))))))
        (declare (ignorable len))
        (when zoom-enabled
          (setf pos
                (v+ pos
                    (vec3-quaternion-rotate
                     quat
                     (vec3
                      (* (if (> (abs (aref axes 0)) 0.02)
                             (aref axes 0)
                             0.0)
                         0.2)
                      (* (if (> (abs (aref axes 2)) 0.02)
                             (aref axes 2)
                             0.0)
                         0.1)
                      (*  (if (> (abs (aref axes 1)) 0.02)
                              (aref axes 1)
                              0.0)
                          -0.5))))))))))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-window) (event sn:button-event))
  (with-slots (rotation-enabled window) viewer
    (cond
      ((sn:button-press-p event :joystick)
       (format t ":joystick pressed!~%")
       (with-gl-window-lock (viewer)
         (setf rotation-enabled (not rotation-enabled)))
       t)
      ((sn:button-press-p event :fit)
       (format t ":fit pressed!~%")
       (reset-view-safe viewer)
       t)
      (t
       (call-next-method)))))
