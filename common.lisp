;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:mingl)

(deftype point () 'vec3)

(deftype normal () 'vec3)

(deftype color () 'vec4)

(declaim (ftype (function (#.3d-vectors::*float-type*) #.3d-vectors::*float-type*) degree-to-radian radian-to-degree)
         (inline degree-to-radian radian-to-degree))
(defun degree-to-radian (deg)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
           (type #.3d-vectors::*float-type* deg))
  (the #.3d-vectors::*float-type* (* deg
                                     (/ (coerce  pi (quote #.3d-vectors::*float-type*)) 180))))

(defun radian-to-degree (rad)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
           (type #.3d-vectors::*float-type* rad))
  (the #.3d-vectors::*float-type* (/ rad
                                     (/ (coerce pi (quote #.3d-vectors::*float-type*)) 180))))

(defun set-uniform (name type value program)
  "Bind the uniform's value in the program."

  (let ((location (gl:get-uniform-location program name)))

    ;; Only assign values to uniforms that are used by the program
    (when (>= location 0)
      (ecase type
        (:mat4 (gl:program-uniform-matrix program location
                                          4
                                          (vector (marr4 value))
                                          t))

        (:mat3 (gl:program-uniform-matrix program location
                                          3
                                          (vector (marr3 value))
                                          t))
        (:dmat4 (gl:program-uniform-matrix program location
                                           4
                                           (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr4 value)))
                                           t))

        (:dmat3 (gl:program-uniform-matrix program location
                                           3
                                           (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr3 value)))
                                           t))

        (:int (gl:program-uniformi program location
                                   value))

        (:float (gl:program-uniformf program location
                                     value))

        (:vec2 (gl:program-uniformf program location
                                    (vx value)
                                    (vy value)))

        (:vec3 (gl:program-uniformf program location
                                    (vx value)
                                    (vy value)
                                    (vz value)))

        (:vec4 (gl:program-uniformf program location
                                    (vx value)
                                    (vy value)
                                    (vz value)
                                    (vw value)))))))

;; Input handlers
(defgeneric handle-key (gl-window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (gl-window mouse-pos mod-keys action button)
  (:documentation "Handle a mouse click."))

(defgeneric handle-joystick (gl-window)
  (:documentation "Handle joystick movement."))

(defgeneric handle-scroll (gl-window mouse-pos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-resize (gl-window width height)
  (:documentation "Handle window resize."))

(defgeneric handle-3d-mouse-event (gl-window event)
  (:documentation "Handle 3D mouse events."))

(defgeneric fill-texture (obj))

(defgeneric initialize (object)
  (:documentation "Initialize an OpenGL object.  Default implementation calls the initialize-* methods."))

(defgeneric render (object)
  (:documentation "Render an OpenGL object."))

(defgeneric bind (object)
  (:documentation "Bind buffers, textures, etc. for an object."))

(defgeneric reload (object)
  (:documentation "Copy new data for object to OpenGL."))

(defgeneric update (object elapsed-seconds)
  (:documentation "Called on an object before rendering, to update for the next animation frame.  Returns nil or an update-result"))

(defgeneric cleanup (object)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(defgeneric initialized-p (object)
  (:documentation "Returns nil if object is not initialized, non-nil otherwise."))


(defun show-slots (white-space object slots)
  "Print the specified slots of object, one per line, indenting with white-space on each line.~
   Use 'nil' slot name to print an empty line to separate values."
  (dolist (slot slots)
    (cond ((null slot)
           (format t "~%"))
          (t
           (format t "~a~a: ~a~%" white-space slot (slot-value object slot))))))

(defgeneric show-info (object &key indent)
  (:documentation "Show OpenGL information for an object."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))
