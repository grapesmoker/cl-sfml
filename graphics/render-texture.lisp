(in-package :sfml)

(defclass render-texture (texture render-window)
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

(defcfun ("sfRenderTexture_destroy" sf-render-texture-destroy) :void
  (render-texture :pointer))

(defmethod texture-destroy ((tex render-texture))
  (sf-render-texture-destroy tex))

(defcfun ("sfRenderTexture_getSize" sf-render-texture-get-size) (:struct sf-vector-2u)
  (render-texture :pointer))

(defmethod texture-size :before ((tex render-texture))
  (setf (slot-value tex 'size) (sf-render-texture-get-size (texture-pointer tex))))

(defcfun ("sfRenderTexture_isSmooth" sf-render-texture-is-smooth) sf-bool
  (texture :pointer))

(defmethod texture-smooth :before ((tex render-texture))
  (setf (slot-value tex 'smooth) (sf-render-texture-is-smooth (texture-pointer tex))))

(defcfun ("sfRenderTexture_setSmooth" sf-render-texture-set-smooth) :void
  (texture :pointer)
  (smooth sf-bool))

(defmethod (setf texture-smooth) :after (smooth (tex render-texture))
  (sf-render-texture-set-smooth (texture-pointer tex) smooth))

(defcfun ("sfRenderTexture_isRepeated" sf-render-texture-is-repeated) sf-bool
  (texture :pointer))

(defmethod texture-repeated :before ((tex render-texture))
  (setf (slot-value tex 'repeated) (sf-render-texture-is-repeated (texture-pointer tex))))

(defcfun ("sfRenderTexture_setRepeated" sf-render-texture-set-repeated) :void
  (texture :pointer)
  (repeated sf-bool))

(defmethod (setf texture-repeated) :after (repeated (tex render-texture))
  (sf-render-texture-set-repeated (texture-pointer tex) repeated))

(defcfun ("sfRenderTexture_getView" sf-render-texture-get-view) :pointer
  (render-texture :pointer))

(defmethod render-texture-view :before ((tex render-texture))
  (setf (slot-value tex 'view)
	(make-instance 'view
		       :pointer (sf-render-texture-get-view (texture-pointer tex)))))

(defcfun ("sfRenderTexture_setView" sf-render-texture-set-view) :void
  (render-texture :pointer)
  (view :pointer))

(defmethod (setf render-texture-view) :after ((v view) (tex render-texture))
  (sf-render-texture-set-view (texture-pointer tex) (view-pointer v)))

(defcfun ("sfRenderTexture_getViewport" sf-render-texture-get-viewport)
    (:struct sf-int-rect)
  (render-texture :pointer))

(defmethod render-texture-viewport :before ((tex render-texture))
  (setf (slot-value tex 'viewport) (sf-render-texture-get-viewport (texture-pointer tex))))

(defcfun ("sfRenderTexture_setActive" sf-render-texture-set-active) sf-bool
  (render-texture :pointer)
  (active sf-bool))

(defmethod (setf render-texture-active) :after (active (tex render-texture))
  (sf-render-texture-set-active (texture-pointer tex) active))

(defcfun ("sfRenderTexture_clear" sf-render-texture-clear) :void
  (render-texture :pointer)
  (color (:struct sf-color)))

(defmethod render-texture-clear ((tex render-texture) (c color))
  (sf-render-window-clear (texture-pointer tex) c))

(defcfun ("sfRenderTexture_display" sf-render-texture-display) :void
  (window :pointer))

(defmethod render-texture-display ((tex render-texture))
  (sf-render-texture-display (texture-pointer tex)))

(defcfun ("sfRenderTexture_generateMipmap" sf-render-texture-generate-mipmap) sf-bool
  (render-texture :pointer))

(defmethod render-texture-generate-mipmap ((tex texture))
  (sf-render-texture-generate-mipmap (texture-pointer tex)))

(defcfun ("sfRenderTexture_getTexture" sf-render-texture-get-texture) :pointer
  (render-texture :pointer))

(defmethod render-texture-get-texture ((tex render-texture))
  (make-instance 'texture :pointer
		 (sf-render-texture-get-texture (texture-pointer tex))))

(defcfun ("sfRenderTexture_mapPixelToCoords" sf-render-texture-map-pixel-to-coords)
    (:struct sf-vector-2f)
  (render-texture :pointer)
  (point (:struct sf-vector-2i))
  (view :pointer))

(defmethod render-texture-pixel->coords ((tex render-texture) (pt vect) (v view))
  (sf-render-texture-map-pixel-to-coords (texture-pointer tex) pt
					 (view-pointer v)))

(defcfun ("sfRenderTexture_mapCoordsToPixel" sf-render-texture-map-coords-to-pixel)
    (:struct sf-vector-2i)
  (render-texture :pointer)
  (point (:struct sf-vector-2f))
  (view :pointer))

(defmethod render-texture-coords->pixel ((tex render-texture) (pt vect) (v view))
  (sf-render-texture-map-coords-to-pixel (texture-pointer tex) pt
					 (view-pointer v)))

(defcfun ("sfRenderTexture_getDefaultView" sf-render-texture-get-default-view) :pointer
  (render-texture :pointer))

(defmethod render-texture-default-view ((tex render-texture))
  (make-instance 'view :pointer
		 (sf-render-texture-get-default-view (texture-pointer tex))))

(defcfun ("sfRenderTexture_pushGLStates" sf-render-texture-push-gl-states) :void
  (render-texture :pointer))

(defmethod render-texture-push-gl-states ((tex render-texture))
  (sf-render-texture-push-gl-states (texture-pointer tex)))

(defcfun ("sfRenderTexture_popGLStates" sf-render-texture-pop-gl-states) :void
  (render-texture :pointer))

(defmethod render-texture-pop-gl-states ((tex render-texture))
  (sf-render-texture-pop-gl-states (texture-pointer tex)))

(defcfun ("sfRenderTexture_resetGLStates" sf-render-texture-reset-gl-states) :void
  (render-texture :pointer))

(defmethod render-texture-reset-gl-states ((tex render-texture))
  (sf-render-texture-reset-gl-states (texture-pointer tex)))

(defcfun ("sfRenderTexture_drawSprite" sf-render-texture-draw-sprite) :void
  (render-texture :pointer)
  (sprite :pointer)
  (states :pointer))

(defcfun ("sfRenderTexture_drawText" sf-render-texture-draw-text) :void
  (render-texture :pointer)
  (text :pointer)
  (states :pointer))

(defcfun ("sfRenderTexture_drawCircleShape" sf-render-texture-draw-circle-shape) :void
  (render-texture :pointer)
  (circle-shape :pointer)
  (states :pointer))

(defcfun ("sfRenderTexture_drawConvexShape" sf-render-texture-draw-convex-shape) :void
  (render-texture :pointer)
  (convext-shape :pointer)
  (states :pointer))

(defcfun ("sfRenderTexture_drawRectangleShape"
	  sf-render-texture-draw-rectangle-shape) :void
  (render-texture :pointer)
  (rectangle-shape :pointer)
  (states :pointer))

(defmethod entity-draw ((spr sprite) (tex render-texture) render-states)
  (sf-render-texture-draw-sprite (texture-pointer tex)
				(sprite-pointer spr)
				render-states))

(defmethod entity-draw ((tex text) (texture render-texture) render-states)
  (sf-render-texture-draw-text (texture-pointer texture)
			      (text-pointer tex)
			      render-states))

(defmethod entity-draw ((circ circle) (tex render-texture) render-states)
  (sf-render-texture-draw-circle-shape (texture-pointer tex)
				      (shape-pointer circ)
				      render-states))

(defmethod entity-draw ((conv convex) (tex render-texture) render-states)
  (sf-render-texture-draw-convex-shape (texture-pointer tex)
				      (shape-pointer conv)
				      render-states))

(defmethod entity-draw ((rect rectangle) (tex render-texture) render-states)
  (sf-render-texture-draw-rectangle-shape (texture-pointer tex)
					 (shape-pointer rect)
					 render-states))
