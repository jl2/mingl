;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.cam>

(defpackage :mingl

  (:nicknames)

  (:use #:cl
        #:alexandria
        #:3d-vectors
        #:3d-matrices)
  (:export

   #:initialize
   #:cleanup
   #:show-info
   #:show-slots


   #:gl-window
   #:program
   #:with-gl-window-lock
   #:display
   #:handle-key
   #:screenshot
   #:handle-resize
   #:update
   #:render
   #:show-open-gl-info
   #:handle-3d-mouse-event
   #:pause

   #:add-shader-directory
   #:add-shader-search-directory
   #:rm-shader-directory

   #:gl-shader
   #:find-gl-shader
   #:read-shader
   #:lookup-shader-type
   #:get-source
   #:compile-shader

   #:shader-error
   #:shader-link-error
   #:shader-validate-error

   #:shader-program
   #:make-shader-program
   #:build-shader-program
   #:use-shader-program
   #:make-shader-program-from-directory

   #:set-uniform
   #:show-gl-array
   #:from-gl-array
   #:to-gl-array
   #:fill-pointer-offset
   #:fill-buffer
   ))
