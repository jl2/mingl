;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:mingl)

(defclass shader-program ()
  ((program :initform 0
            :accessor program
            :type fixnum
            :documentation "OpenGL program handle.")
   (shaders :initform nil
            :type (or null list)
            :accessor shaders
            :initarg :shaders
            :documentation "List of shaders for this shader-program."))
  (:documentation "One or more shaders that work together."))

(defmethod needs-rebuild ((shader-program shader-program))
  (with-slots (shaders) shader-program
    (loop
      :for shader :in shaders
      :when (needs-rebuild shader)
        :collect shader)))

;; (defmethod show-info ((shader-program shader-program))
;;   (let ((this-ws (indent-whitespace indent))
;;         (plus-ws (indent-whitespace (+ 1 indent)))
;;         (plus-plus-ws (indent-whitespace (+ 2 indent))))
;;     (format t "~aShader-Program:~%" this-ws)
;;     (show-slots plus-ws shader-program '(shaders program ))

;;     (with-slots (program) shader-program
;;       (format t "~aProgram ~a:~%" plus-ws program)
;;       (dolist (attrib '(:delete-status
;;                         :link-status
;;                         :validate-status
;;                         :info-log-length
;;                         :attached-shaders
;;                         :active-atomic-counter-buffers
;;                         :active-attributes
;;                         :active-attribute-max-length
;;                         :active-uniforms
;;                         :active-uniform-blocks
;;                         :active-uniform-block-max-name-length
;;                         :active-uniform-max-length
;;                         ;; :compute-work-group-size
;;                         :program-binary-length
;;                         :transform-feedback-buffer-mode
;;                         :transform-feedback-varyings
;;                         :transform-feedback-varying-max-length
;;                         ;; :geometry-vertices-out
;;                         ;; :geometry-input-type
;;                         ;; :geometry-output-type
;;                         ))
;;         (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-program program attrib)))
;;       (dotimes (idx (gl:get-program program :active-attributes))
;;         (multiple-value-bind (size type name) (gl:get-active-attrib program idx)
;;           (format t "~aAttrib ~a ~a size ~a~%" plus-plus-ws name type size)))

;;       (let ((gl-shaders (gl:get-attached-shaders program)))
;;         (format t "~aOpenGL Shaders: ~a~%" plus-ws gl-shaders)
;;         (dolist (shader gl-shaders)
;;           (format t "~aShader ~a~%" plus-ws shader)
;;           (dolist (attrib '(:shader-type
;;                             :delete-status :compile-status :info-log-length :shader-source-length))
;;             (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-shader shader attrib))))))))

(defmethod cleanup ((shader-program shader-program))
  (with-slots (shaders program) shader-program
    (when shaders
      (dolist (shade shaders)
        (with-slots (shader) shade
          (when (> shader 0)
            (gl:detach-shader program shader)
            (setf shader 0)))))
    (when (not (zerop program))
      (gl:delete-program program)
      (setf program 0))))

(define-condition shader-link-error (shader-error) ())
(define-condition shader-validate-error (shader-error) ())

(defun build-shader-program (shader-program)
  "Compile and link a shader program, including validation."
  (declare (type shader-program shader-program))
  (with-slots (program shaders) shader-program
    ;; Compile all shaders
    (when (zerop program)
      (setf program (gl:create-program)))

    (dolist (gl-shader shaders)
      (when (not (zerop (shader gl-shader)))
        (gl:detach-shader program (shader gl-shader)))
      (initialize gl-shader)
      (gl:attach-shader program (shader gl-shader))
      (gl:delete-shader (shader gl-shader)))

    ;; Link
    (gl:link-program program)

    ;; Check for errors and validate program
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq t status))
        (format t "~a ~a ~a~%" status program (gl:get-program-info-log program))
        (error 'shader-link-error
               :status status
               :object program
               :info-log (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (restart-case
            (error 'shader-link-error
                   :status status
                   :object program
                   :info-log (gl:get-program-info-log program))
          (ignore-validation-error () t))))
    program))

(defun use-shader-program (shader-program)
  (declare (type shader-program shader-program))
  (with-slots (program) shader-program
    (gl:use-program program))
  t)


(defun make-shader-program (&rest shaders)
  (when (null shaders)
    (error "No shaders for shader-program."))
  (make-instance 'shader-program :shaders (map 'list #'read-shader shaders)))

(defun make-shader-program-from-directory (directory-name &optional (pattern "*"))
  (apply #'make-shader-program (uiop:directory-files directory-name pattern)))
