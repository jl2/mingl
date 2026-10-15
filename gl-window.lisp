;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:mingl)

(defparameter *want-forward-context*
  #+(or windows linux freebsd) t
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defparameter *info-stream* t
  "Stream to write error information (usually GLFW errors).")

(defvar *gl-windows* (make-hash-table :test 'equal)
  "All gl-windows that have been created.")

(defparameter *thread-count* 23)
(defparameter *default-radius* 1.0)
(defparameter *default-φ* 0.0)
(defparameter *default-θ* 0.0)

(defclass gl-window ()
  ((window :initform nil
           :documentation "The GLFW window for this gl-window.")

   (gl-window-mutex :initform (bt:make-lock "gl-window-lock")
                 :documentation "Mutex to synchronize multi-threaded access to this gl-window.")

   (objects :initform nil
            :initarg :objects
            :type (or null cons)
            :accessor objects
            :documentation "The list of objects shown in this gl-window.")

   (view-changed :initform t
                 :documentation "t when the view has changed recently.")

   (aspect-ratio :initform 1.0
                 :initarg :aspect-ratio
                 :type real
                 :accessor aspect-ratio
                 :documentation "Aspect ratio of the view window.")

   (focused :initform t
            :type t
            :documentation "Whether the window has focus.")

   (show-fps :initform nil
             :initarg :show-fps
             :type t
             :accessor show-fps
             :documentation "t to show FPS to *info-stream*.  nil to disable.")

   (desired-fps :initform 60
                :initarg :desired-fps
                :type fixnum
                :accessor desired-fps
                :documentation "Target FPS. The gl-window should achieve this frame rate as long as render and update calls are fast enough.")

   (seconds-between-updates :initform (/ 1 30)
                            :initarg :seconds-between-updates
                            :documentation "Ideal number of seconds between each update call.  Should be ~ (1 / desired-fps)")

   (enable-update :initform t
                  :initarg :enable-update
                  :accessor enable-update
                  :documentation "t to enable calls to (update), nil to disable. Used to pause or continue animations.")

   (background-color :initform (vec4 0.08f0 0.08f0 0.08f0 1.0)
                     :initarg :background
                     :accessor background-color
                     :documentation "The background color.")

   (decorated :initform t
              :initarg :decorated
              :accessor decorated
              :documentation "Whether to use window decorations.")

   (debug-context :initform nil
                  :initarg :debug-context
                  :accessor debug-context
                  :documentation "t to use a debug OpenGL context.")

   (forward-context :initform *want-forward-context*
                    :initarg :forward-context
                    :accessor forward-context
                    :documentation "t to use a forward context")

   (samples :initform 0
            :initarg :samples
            :accessor samples
            :documentation "The number of samples per pixel.")

   (resizable :initform t
              :initarg :resizable
              :accessor resizable
              :documentation "Whether the window is resizable.")

   (use-shader-callback :initform t
                        :initarg :use-shader-callback
                        :accessor use-shader-callback
                        :documentation "Whether or not to log shader debug messages.")

   (gl-major-version :initform 4
                     :type fixnum
                     :initarg :gl-major-version
                     :accessor gl-major-version
                     :documentation "OpenGL major version to use for the OpenGL context.")

   (gl-minor-version :initform 6
                     :type fixnum
                     :initarg :gl-minor-version
                     :accessor gl-minor-version
                     :documentation "OpenGL minor version to use for the OpenGL context.")

   (discarded-objects :initform nil
                      :documentation "Objects that have been removed from the gl-window, but still need to be cleaned up by OpenGL.")

   (height :initform 800
           :initarg :height
           :documentation "Requested initial window height.")

   (width :initform 800
          :initarg :width
          :documentation "Requested initial window width.")

   (previous-seconds :initform 0.0
                     :documentation "Timestamp of previous render loop.")

   (use-main-thread :initform t
                    :initarg :use-main-thread
                    :documentation "Whether or not to run on the main thread.
Some platforms require this (OSX, for example), and may be
best practices on other platforms too.")

   (wait-for-exit :initform nil
                  :initarg :wait-for-exit
                  :documentation "t to wait for the window to close, nil to run in a background thread.")

   (error-stream :initform t
                 :initarg :error-stream
                 :documentation "Stream where GLFW will write error logs.")

   (synchronous-output :initform nil
                       :initarg :synchronous-output
                       :documentation "Whether or not to force OpenGL output to be synchronous.")

   (joystick :initform nil
              :initarg :joystick
              :type (or fixnum list null)
              :documentation "")

   (lparallel-kernel :initform (lparallel:make-kernel *thread-count*)
                     :initarg :lparallel-kernel
                     :documentation "nil to use a single thread for updates.  Or the lparallel kernel to use.")
   (info-stream :initform *standard-output*
                :initarg :info-stream
                :documentation "A stream (or t or nil) to log messages to.")
   (reload-shaders :initform t
                   :initarg :reload-shaders
                   :documentation "If non-nil then recompile and relink shader programs when they've changed."))
  (:documentation "A collection of objects and a viewport."))


(declaim ;;(ftype (function (cffi:foreign-pointer) (or gl-window nil)) find-gl-window)
         (inline find-gl-window
                 add-gl-window
                 rm-gl-window
                 rm-all-gl-windows))
(defun find-gl-window (window)
  "Find a gl-window associated with the given GLFW window."
  (gethash (cffi:pointer-address window) *gl-windows*))

(defun add-gl-window (window gl-window)
  "Add a new gl-window for the GLFW window."
  (setf (gethash (cffi:pointer-address window) *gl-windows*) gl-window))

(defun rm-gl-window (window)
  "Remove the gl-window associated with the GLFW gl-window."
  (remhash (cffi:pointer-address window) *gl-windows*))

(defun rm-all-gl-windows ()
  "Remove all gl-windows."
  (loop
    :for window :being :the :hash-keys :of *gl-windows*
      :using (hash-value gl-window)
    :do
       (cleanup gl-window)
       (glfw:destroy-window window))
  (clrhash *gl-windows*))


(glfw:def-window-focus-callback focus-handler (window focusedp)
  (progn
    "GLFW keyboard handler.  Finds a gl-window for the window and calls the (handle-key) method."
    (when-let (gl-window (find-gl-window window))
      (handle-focus gl-window focusedp))))

;; Keyboard callback.
(glfw:def-key-callback keyboard-handler (window key scancode action mod-keys)
  (progn
    "GLFW keyboard handler.  Finds a gl-window for the window and calls the (handle-key) method."
    (when-let (gl-window (find-gl-window window))
      (handle-key gl-window key scancode action mod-keys))))

;; Mouse handler callback
(glfw:def-mouse-button-callback mouse-handler (window button action mod-keys)
  (progn
    "GLFW mouse handler.  Finds a gl-window for the window and calls the (handle-click) method."
    (when-let (gl-window (find-gl-window window))
      (handle-click gl-window (glfw:get-cursor-position window) mod-keys action button))))

;; Mouse move callback
(glfw:def-cursor-pos-callback cursor-position-handler (window x y)
  (progn
    "GLFW mouse handler.  Finds a gl-window for the window and calls the (handle-click) method."
    (when-let (gl-window (find-gl-window window))
      (handle-cursor-position gl-window x y))))

(glfw:def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (progn
    "GLFW scroll handler.  Finds a gl-window for the window and calls the (handle-scroll) method."
    (when-let (gl-window (find-gl-window window))
      (let ((cpos (glfw:get-cursor-position window)))
        (handle-scroll gl-window cpos x-scroll y-scroll)))))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (progn
    "GLFW resize handler.  Finds a gl-window for the window and calls the (handle-resize) method."
    (when-let (gl-window (find-gl-window window))
      (handle-resize gl-window width height))))

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (progn
    "GLFW error callback.  Writes errors to *info-stream*"
    (format *info-stream* "Error: ~a~%" message)))


(defmacro with-gl-window-lock ((gl-window) &body body)
  `(with-slots (gl-window-mutex) ,gl-window
     (bt:with-lock-held (gl-window-mutex)
       ,@body)))

(defmethod initialize ((gl-window gl-window))
  (gl:enable :line-smooth
             :depth-test
             :program-point-size)
  (gl:depth-func :lequal))


(defmethod cleanup ((gl-window gl-window))
  )


(defmethod handle-joystick ((gl-window gl-window))
  (declare (ignorable gl-window))
  nil)

(defmethod handle-focus ((gl-window gl-window) focusedp)
  (with-slots (focused) gl-window
    (setf focused focusedp)
    (format t "focused: ~a~%" focused)))

(defmethod handle-key ((gl-window gl-window) key scancode action mod-keys)
  (declare  (optimize (speed 3) (safety 0) (debug 3) (space 0)))
  (with-slots (window) gl-window
    (let ((shift-down (find :shift mod-keys))
          (alt-down (find :alt mod-keys))
          (ctrl-down (find :ctrl mod-keys))
          (super-down (find :super mod-keys)))

      (declare (ignorable shift-down alt-down ctrl-down super-down))

      (cond
        ((and (eql key :escape) (eql action :press))
         (glfw:set-window-should-close window)
         t)

        ((and (eql key :f1)
              (eql action :press)
              alt-down)
         (format t "Toggling debug message callback.~%")
         (with-gl-window-lock (gl-window)
           (with-slots (use-shader-callback) gl-window
             (setf use-shader-callback (not use-shader-callback))
             (%gl:debug-message-callback (if use-shader-callback
                                             (cffi:callback gl-debug-callback)
                                             (cffi:null-pointer))
                                         (cffi:null-pointer))
             (%gl:debug-message-control :dont-care
                                        :dont-care
                                        :dont-care 0 (cffi:null-pointer) (if use-shader-callback :true :false))))
         t)

        ((and (eql key :f7) (eql action :press))
         (show-open-gl-info)
         t)

        ((and (eql key :f9) (eql action :press))
         (with-gl-window-lock (gl-window)
           (with-slots (show-fps) gl-window
             (setf show-fps (not show-fps)))
           t))

        ((and (eql key :f10) (eql action :press))
         (with-gl-window-lock (gl-window)
           (with-slots (enable-update) gl-window
             (cond ((integerp enable-update)
                    (setf enable-update t))
                   ((null enable-update)
                    (setf enable-update t))
                   (t
                    (setf enable-update nil)))
             (format t "enable-update: ~a~%" enable-update))
           t))

        ((and (eql key :f11) (eql action :press))
         (with-gl-window-lock (gl-window)
           (with-slots (enable-update) gl-window
             (cond ((null enable-update)
                    (setf enable-update 1))
                   ((integerp enable-update)
                    (incf enable-update)))
             (format t "enable-update: ~a~%" enable-update))
           t))


        ((and (eql key :f12) (eql action :press))
         ;; TODO: Run this in a background thread?
         (screenshot gl-window)
         t)

        (t
         ;; (format t "Ignoring window: ~a key: ~a scancode ~a action ~a mod-keys ~a~%"
         ;;         window key scancode action mod-keys)
         nil
         )))))

(defmethod handle-click ((gl-window gl-window) mouse-pos mod-keys action button)
  (format t "~a ~a ~a ~a~%"
          mouse-pos mod-keys action button))

(defmethod handle-cursor-position ((gl-window gl-window) x y)
  ;; (format t "~a ~a~%"
  ;;         x y)
  )

(defmethod handle-scroll ((gl-window gl-window) mouse-pos x-scroll y-scroll)
  ;; (format t "~a ~a ~a~%"
  ;;         mouse-pos x-scroll y-scroll)
  )

(defun screenshot (gl-window)
  (declare  (optimize (speed 3) (safety 0) (debug 3) (space 0)))
  (with-slots (window) gl-window
    (let* ((win-size (glfw:get-window-size window))
           (width (car win-size))
           (height (cadr win-size))
           (data (gl:read-pixels 0 0 width height :rgba :unsigned-byte))

           (fname (format nil
                          "/home/jeremiah/screenshots/mingl-screenshot~a.png"
                          (local-time:format-rfc3339-timestring t (local-time:now)))))
      (declare (type fixnum width height)
               (type (simple-array (unsigned-byte 8) *) data))
      (lparallel.thread-util:with-thread (:name fname)
        (format t "Screenshot to: ~a~%" fname)
        (time
         (loop :for this-line fixnum :below height
               :for this-line-idx fixnum = (* this-line 4 width)
               :for that-line fixnum :from (1- height) :downto (ceiling height 2)
               :for that-line-idx fixnum = (* that-line 4 width)
               :do
                  (loop :for column fixnum :below (* 4 width)
                        :for this-idx fixnum = (+ column this-line-idx)
                        :for that-idx fixnum = (+ column that-line-idx)
                        :do
                           (rotatef (aref data this-idx) (aref data that-idx)))))
        (zpng:write-png
         (make-instance 'zpng:png :color-type :truecolor-alpha
                                  :image-data data
                                  :width width
                                  :height height)
         fname))
      t)))


(defmethod handle-resize ((gl-window gl-window) new-width new-height)
  (with-gl-window-lock (gl-window)
    (with-slots (aspect-ratio width height view-changed) gl-window
      (setf width new-width
            height new-height
            aspect-ratio (if (< width height )
                             (/ height width 1.0)
                             (/ width height 1.0))
            view-changed t)
      (gl:viewport 0 0 new-width new-height))))

(defmethod update ((gl-window gl-window) elapsed-seconds)
  (declare  (optimize (speed 3) (safety 0) (debug 3) (space 0))
            (ignorable gl-window elapsed-seconds))
  nil)

(defmethod render ((gl-window gl-window))
  (with-slots (background-color) gl-window
    (gl:clear-color (vx background-color)
                    0.4 ;;(vy background-color)
                    (vz background-color)
                    (vw background-color)))
  (gl:clear :color-buffer
            :depth-buffer))


(cffi:defcallback gl-debug-callback :void ((source :int) (type :int) (id :int)
                                           (severity :int) (length :int)
                                           (message :string) (param :pointer))
  (declare (ignorable param length))

  (format t "~a ~a ~a ~a~%    ~s~%"
          (cffi:foreign-enum-keyword '%gl:enum source)
          (cffi:foreign-enum-keyword '%gl:enum type)
          (cffi:foreign-enum-keyword '%gl:enum severity)
          id
          message))

(defun display (gl-window)
  "High level function to display a gl-window and start processing in a background thread."
  (declare  (optimize (speed 3) (safety 0) (debug 3) (space 0)))
  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  (when (slot-value gl-window 'info-stream)
    (let ((*info-stream* (slot-value gl-window 'info-stream)))
      (glfw:set-error-callback 'error-callback)))


  (flet
      ((window-main ()
         (declare  (optimize (speed 3) (safety 0) (debug 3) (space 0)))
         (glfw:initialize)
         (let ((window (glfw:create-window :title "OpenGL Gl-Window"
                                           :width (slot-value gl-window 'width)
                                           :height (slot-value gl-window 'height)
                                           :decorated (slot-value gl-window 'decorated)
                                           :opengl-profile :opengl-core-profile
                                           :context-version-major (slot-value gl-window 'gl-major-version)
                                           :context-version-minor (slot-value gl-window 'gl-minor-version)
                                           :opengl-debug-context (slot-value gl-window 'debug-context)
                                           :opengl-forward-compat (slot-value gl-window 'forward-context)
                                           :samples (slot-value gl-window 'samples)
                                           :resizable (slot-value gl-window 'resizable))))
           (when (null window)
             (format t "Could not create-window!")
             (error "Could not create-window!"))

           (with-gl-window-lock (gl-window)
             (setf (slot-value gl-window 'window) window))

           (add-gl-window window gl-window)

           (when (slot-value gl-window 'synchronous-output)
             (gl:enable :debug-output-synchronous))

           (unwind-protect
                (progn

                  ;; GLFW Initialization
                  (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

                  (glfw:set-key-callback 'keyboard-handler window)
                  (glfw:set-mouse-button-callback 'mouse-handler window)
                  (glfw:set-scroll-callback 'scroll-handler window)
                  (glfw:set-cursor-position-callback 'cursor-position-handler window)
                  (glfw:set-framebuffer-size-callback 'resize-handler window)

                  ;; The event loop
                  (with-slots (previous-seconds show-fps desired-fps background-color) gl-window
                    ;; Load objects for the first time
                    (initialize gl-window)

                    #+spacenav(sn:sn-open)
                    #+spacenav(sn:sensitivity 1.0d0)

                    (loop
                      :with start-time = (glfw:get-time)
                      :for total-frame-count :from 0
                      :for frame-count :from 0
                      :until (glfw:window-should-close-p window)

                      :for current-seconds = (glfw:get-time)
                      :for elapsed-seconds = (- current-seconds previous-seconds)
                      :for elapsed-time = (- current-seconds start-time)

                      :when (and show-fps (> elapsed-seconds 0.25))
                        :do
                           (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                           (setf previous-seconds current-seconds)
                           (setf frame-count 0)


                      :when (slot-value gl-window 'focused)
                        :do
                      #+spacenav
                       (when-let (ev (sn:poll-event))
                         (handle-3d-mouse-event gl-window ev)
                         (sn:remove-events :motion))
                       (handle-joystick gl-window)

                      :do
                         (with-gl-window-lock (gl-window)

                           ;; Update for next frame
                           (with-slots (lparallel-kernel) gl-window
                               (let ((lparallel:*kernel* lparallel-kernel))
                                 (update gl-window elapsed-time))))
                         (incf total-frame-count)
                         (render gl-window)
                         (glfw:swap-buffers window)

                      :do
                         (glfw:poll-events)
                      :do
                         (let* ((now (glfw:get-time))
                                (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                             now)))
                           ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                           (when (> rem-time 0)
                             (sleep rem-time))))))
             (progn
               ;; Cleanup before exit
               (cleanup gl-window)
               (rm-gl-window window))

             #+spacenav (sn:sn-close)

             (glfw:destroy-window window)
             (glfw:poll-events)))))
    (cond
      ((slot-value gl-window 'use-main-thread)
       (format t "Running in main thread~%")
       (tmt:call-in-main-thread #'window-main :blocking (slot-value gl-window 'wait-for-exit)))
      (t
       (format t "Running in THIS thread~%")
       (window-main)))))

(defun show-open-gl-info ()
  "Print OpenGL limits"
  (format t "gl-version: ~a~%" (gl:gl-version))
  (loop
    :for field :in '(:max-combined-texture-image-units
                     :max-cube-map-texture-size
                     :max-draw-buffers
                     :max-fragment-uniform-components
                     :max-texture-size
                     ;; :max-varying-floats
                     :max-vertex-attribs
                     :max-vertex-texture-image-units
                     :max-vertex-uniform-components
                     :max-viewport-dims
                     :texture-binding-2d
                     :max-patch-vertices
                     :stereo)
    :do
       (format t "~a : ~a~%" field (gl:get-integer field)))
  (loop
    :for field :in '(:active-texture
                     :array-buffer-binding
                     :blend
                     :current-program
                     :line-width
                     :vertex-array-binding
                     :viewport)
    :do
       (format t "~a : ~a~%" field (gl:get-integer field))))

#+spacenav
(defmethod handle-3d-mouse-event ((gl-window gl-window) (event sn:motion-event))
  (format t "Handling event: ~a~%" event)
  t)

#+spacenav
(defmethod handle-3d-mouse-event ((gl-window gl-window) (event sn:button-event))
  (with-gl-window-lock (gl-window)
    (with-slots (view-changed
                 window
                 enable-update
                 objects) gl-window
      (cond
        ((sn:button-press-p event :esc)
         (format t ":esc pressed!~%")
         (glfw:set-window-should-close window)
         (setf view-changed t)
         t)

        ((sn:button-press-p event :menu)
         (format t ":menu pressed!~%")
         (show-open-gl-info)
         nil)

        ((sn:button-press-p event :ctrl)
         (cond ((integerp enable-update)
                (setf enable-update t))
               ((null enable-update)
                (setf enable-update t))
               (t
                (setf enable-update nil)))
         (format t "enable-update: ~a~%" enable-update)
         (format t ":esc pressed!~%")
         t)
        ((sn:button-press-p event :rotate)
         (screenshot gl-window)
         t)
        (t nil)))))

(defun pause (gl-window timeout)
  (with-gl-window-lock (gl-window)
    (sleep timeout)))

(defmacro with-object-in-gl-window-lock ((variable name gl-window) &body body)
  (with-gensyms (obj
                 name-sym
                 gl-window-sym)
    `(let ((,name-sym ,name)
           (,gl-window-sym ,gl-window))
       (when-let (,obj (assoc ,name-sym (slot-value ,gl-window-sym 'objects)))
         (with-gl-window-lock (,gl-window-sym)
           (let ((,variable (cdr ,obj)))
             ,@body))))))
