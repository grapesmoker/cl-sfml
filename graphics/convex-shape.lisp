(in-package :sfml)

(defclass convex (shape)
  ((type :initform :convex)))

(defmethod initialize-instance :after ((c convex) &key)
  (setf (shape-pointer c) (sf-convex-shape-create)))


;; all the C functions

(defcfun ("sfConvexShape_create" sf-convex-shape-create) :pointer)

(defun make-convex ()
  (make-instance 'convex))

(defcfun ("sfConvexShape_copy" sf-convex-shape-copy) :pointer
  (shape :pointer))

(defmethod convex-copy ((c convex))
  (make-instance 'convex :pointer (sf-convex-shape-copy (shape-pointer c))))

(defcfun ("sfConvexShape_destroy" sf-convex-shape-destroy) :void
  (shape :pointer))

(defmethod convex-destroy ((c convex))
  (sf-convex-shape-destroy (shape-pointer c)))

(defcfun ("sfConvexShape_getPosition" sf-convex-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-position :before ((c convex))
  (setf (slot-value c 'position) (sf-convex-shape-get-position
				  (shape-pointer c))))


(defcfun ("sfConvexShape_setPosition" sf-convex-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defmethod (setf shape-position) :after ((v vect) (c convex))
  (sf-convex-shape-set-position (shape-pointer c) v))

(defcfun ("sfConvexShape_getRotation" sf-convex-shape-get-rotation) :float
  (shape :pointer))

(defmethod shape-rotation :before ((c convex))
  (setf (slot-value c 'rotation) (sf-convex-shape-get-rotation (shape-pointer c))))

(defcfun ("sfConvexShape_setRotation" sf-convex-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defmethod (setf shape-rotation) :after ((angle number) (c convex))
  (sf-convex-shape-set-rotation (shape-pointer c) (coerce angle 'float)))

(defcfun ("sfConvexShape_getScale" sf-convex-shape-get-scale) :float
  (shape :pointer))

(defmethod shape-scale :before ((c convex))
  (setf (slot-value c 'origin) (sf-convex-shape-get-scale (shape-pointer c))))

(defcfun ("sfConvexShape_setScale" sf-convex-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defmethod (setf shape-scale) :after ((factor number) (c convex))
  (sf-convex-shape-set-scale (shape-pointer c) (coerce factor 'float)))

(defcfun ("sfConvexShape_getOrigin" sf-convex-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-origin ((c convex))
  (setf (slot-value c 'origin) (sf-convex-shape-get-origin (shape-pointer c))))

(defcfun ("sfConvexShape_setOrigin" sf-convex-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defmethod (setf shape-origin) :after ((v vect) (c convex))
  (sf-convex-shape-set-origin (shape-pointer c) v))

(defcfun ("sfConvexShape_move" sf-convex-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((c convex) (offset vect))
  (sf-convex-shape-move (shape-pointer c) offset))

(defcfun ("sfConvexShape_rotate" sf-convex-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defmethod entity-rotate ((c convex) (angle number))
  (sf-convex-shape-rotate (shape-pointer c) (coerce angle 'float)))

(defcfun ("sfConvexShape_scale" sf-convex-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defmethod entity-do-scale ((c convex) (scale vect))
  (sf-convex-shape-scale (shape-pointer c) scale))

(defcfun ("sfConvexShape_getTransform" sf-convex-shape-get-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-transform :before ((c convex))
  (setf (slot-value c 'transform) (sf-convex-shape-get-transform (shape-pointer c))))

(defcfun ("sfConvexShape_getInverseTransform" sf-convex-shape-get-inverse-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-inverse-transform :before ((c convex))
  (setf (slot-value c 'inverse-transform) (sf-convex-shape-get-inverse-transform (shape-pointer c))))

(defcfun ("sfConvexShape_getTexture" sf-convex-shape-get-texture) :pointer
  (shape :pointer))

(defmethod entity-texture :before ((c convex))
  (setf (slot-value c 'texture) (sf-convex-shape-get-texture (shape-pointer c))))

(defcfun ("sfConvexShape_setTexture" sf-convex-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defmethod entity-set-texture ((c convex) (tex texture) reset-rect)
  (setf (entity-texture c) tex)
  (sf-convex-shape-set-texture (shape-pointer c) (texture-pointer tex) reset-rect))

(defcfun ("sfConvexShape_getTextureRect" sf-convex-shape-get-texture-rect) :pointer
  (shape :pointer))

(defmethod shape-texture-rect :before ((c convex))
  (setf (slot-value c 'texture-rect) (sf-convex-shape-get-texture-rect
				      (shape-pointer c))))

(defcfun ("sfConvexShape_setTextureRect" sf-convex-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defmethod (setf shape-texture-rect) :after ((r rect) (c convex))
  (sf-convex-shape-set-texture-rect (shape-pointer c) r))

(defcfun ("sfConvexShape_getFillColor"
	  sf-convex-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-fill-color :before ((c convex))
  (setf (slot-value c 'fill-color) (sf-convex-shape-get-fill-color (shape-pointer c))))

(defcfun ("sfConvexShape_setFillColor" sf-convex-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-fill-color) :after ((c color) (conv convex))
  (sf-convex-shape-set-fill-color (shape-pointer conv) c))

(defcfun ("sfConvexShape_getOutlineColor"
	  sf-convex-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-outline-color :before ((c convex))
  (setf (slot-value c 'outline-color)
	(sf-convex-shape-get-outline-color (shape-pointer c))))

(defcfun ("sfConvexShape_setOutlineColor" sf-convex-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-outline-color) :after ((c color) (conv convex))
  (sf-convex-shape-set-outline-color (shape-pointer conv) c))

(defcfun ("sfConvexShape_getOutlineThickness"
	  sf-convex-shape-get-outline-thickness) :float
  (shape :pointer))

(defmethod shape-outline-thickness :before ((c convex))
  (setf (slot-value c 'outline-thickness)
	(sf-convex-shape-get-outline-thickness (shape-pointer c))))

(defcfun ("sfConvexShape_setOutlineThickness"
	  sf-convex-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defmethod (setf shape-outline-thickness) :after ((thickness number) (conv convex))
  (sf-convex-shape-set-outline-thickness
   (shape-pointer conv) (coerce thickness 'float)))

(defcfun ("sfConvexShape_getPointCount" sf-convex-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defmethod shape-point-count :before ((c convex))
  (setf (slot-value c 'point-count)
	(sf-convex-shape-get-point-count (shape-pointer c))))

(defcfun ("sfConvexShape_setPointCount" sf-convex-shape-set-point-count) :void
  (shape :pointer)
  (count :unsigned-int))

(defmethod (setf shape-point-count) :after ((count integer) (c convex))
  (sf-convex-shape-set-point-count (shape-pointer c) count))

(defcfun ("sfConvexShape_getPoint" sf-convex-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defmethod convex-get-point ((c convex) (index integer))
  (sf-convex-shape-get-point (shape-pointer c) index))

(defcfun ("sfConvexShape_getLocalBounds"
	  sf-convex-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-local-bbox :before ((c convex))
  (setf (slot-value c 'local-bbox) (sf-convex-shape-get-local-bounds (shape-pointer c))))

(defcfun ("sfConvexShape_getGlobalBounds"
	  sf-convex-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-global-bbox :before ((c convex))
  (setf (slot-value c 'global-bbox)
	(sf-convex-shape-get-global-bounds (shape-pointer c))))
