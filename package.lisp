;; Copyright (c) 2026 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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
   ))
