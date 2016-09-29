(in-package :sfml)

(defclass convex (shape)
  ((type :initform :convex)))

(defmethod initialize-instance :after ((c convex) &key)
  (setf (shape-pointer c) (sf-convex-shape-create)))


;; all the C functions

(defcfun ("sfConvexShape_create" sf-convex-shape-create) :pointer)

(defcfun ("sfConvexShape_copy" sf-convex-shape-copy) :pointer
  (shape :pointer))

(defcfun ("sfConvexShape_destroy" sf-convex-shape-destroy) :void
  (shape :pointer))

(defcfun ("sfConvexShape_setPosition" sf-convex-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_setRotation" sf-convex-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfConvexShape_setScale" sf-convex-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_setOrigin" sf-convex-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_getPosition" sf-convex-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfConvexShape_getRotation" sf-convex-shape-get-rotation) :float
  (shape :pointer))

(defcfun ("sfConvexShape_getScale" sf-convex-shape-get-scale) :float
  (shape :pointer))

(defcfun ("sfConvexShape_getOrigin" sf-convex-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfConvexShape_move" sf-convex-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_rotate" sf-convex-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfConvexShape_scale" sf-convex-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_setTexture" sf-convex-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defcfun ("sfConvexShape_setTextureRect" sf-convex-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defcfun ("sfConvexShape_setFillColor" sf-convex-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfConvexShape_setOutlineColor" sf-convex-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfConvexShape_setOutlineThickness"
	  sf-convex-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defcfun ("sfConvexShape_getTexture" sf-convex-shape-get-texture) :pointer
  (shape :pointer))

(defcfun ("sfConvexShape_getTextureRect" sf-convex-shape-get-texture-rect) :pointer
  (shape :pointer))

(defcfun ("sfConvexShape_getFillColor"
	  sf-convex-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfConvexShape_getOutlineColor"
	  sf-convex-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfConvexShape_getOutlineThickness"
	  sf-convex-shape-get-outline-thickness) :float
  (shape :pointer))

(defcfun ("sfConvexShape_getPointCount" sf-convex-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defcfun ("sfConvexShape_getPointCount" sf-convex-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defcfun ("sfConvexShape_setSize" sf-convex-shape-set-size) :void
  (shape :pointer)
  (size (:struct sf-vector-2f)))

(defcfun ("sfConvexShape_getSize" sf-convex-shape-get-size) (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfConvexShape_getLocalBounds"
	  sf-convex-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defcfun ("sfConvexShape_getGlobalBounds"
	  sf-convex-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))


(defmethod update-convex-position ((conv convex))
  (sf-convex-shape-set-position (shape-pointer conv) (shape-position conv)))

(defmethod update-convex-rotation ((conv convex))
  (sf-convex-shape-set-rotation (shape-pointer conv) (shape-rotation conv)))

(defmethod update-convex-scale ((conv convex))
  (sf-convex-shape-set-scale (shape-pointer conv) (shape-scale conv)))

(defmethod update-convex-origin ((conv convex))
  (sf-convex-shape-set-origin (shape-pointer conv) (shape-origin conv)))

(defmethod update-convex-fill-color ((conv convex))
  (sf-convex-shape-set-fill-color (shape-pointer conv) (shape-fill-color conv)))

(defmethod update-convex-outline-color ((conv convex))
  (sf-convex-shape-set-outline-color (shape-pointer conv) (shape-outline-color conv)))

(defmethod update-convex-outline-thickness ((conv convex))
  (sf-convex-shape-set-outline-thickness (shape-pointer conv)
					    (shape-outline-thickness conv)))

(defmethod update-convex-texture ((conv convex) reset-rect)
  (sf-convex-shape-set-texture (shape-pointer conv)
				  (shape-texture conv)
				  reset-rect))

(defmethod update-convex-size ((conv convex))
  (sf-convex-shape-set-size (shape-pointer conv) (shape-size conv)))

;; update any slots that have changed via calls to C and clear the tracker

(defmethod update-convex ((conv convex))
  (loop
     for slot in (shape-changed-slots conv)
     do
       (case slot
	 (position (update-convex-position conv))
	 (rotation (update-convex-rotation conv))
	 (scale (update-convex-scale conv))
	 (size (update-convex-size conv))
	 (fill-color (update-convex-fill-color conv))
	 (outline-color (update-convex-outline-color conv))
	 (outline-thickness (update-convex-outline-thickness conv))))
  (setf (shape-changed-slots conv) '()))
	 
