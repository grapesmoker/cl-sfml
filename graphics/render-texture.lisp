(in-package :sfml)

(defclass render-texture (texture)
  ((depth-buffer :initarg :depth-buffer
		 :initform nil
		 :accessor render-texture-depth-buffer)))

(defcfun ("sfRenderTexture_create" sf-render-texture-create) :pointer
  (width :unsigned-int)
  (height :unsigned-int)
  (depth-buffer sf-bool))

(defun make-render-texture (width height depth-buffer)
  (make-instance 'render-texture
		 :width width
		 :height height
		 :depth-buffer depth-buffer
		 :pointer (sf-render-texture-create width height depth-buffer)))

