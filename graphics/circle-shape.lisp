(in-package :sfml)

(defclass circle (shape)
  ((type :initform :circle)
   (radius :initarg :radius :initform 0 :accessor circle-radius)))

(defcfun ("sfCircleShape_setRadius" sf-circle-shape-set-radius) :void
  (shape :pointer)
  (radius :float))

(defmethod (setf circle-radius) :after ((radius number) (c circle))
  (sf-circle-shape-set-radius (shape-pointer c) (coerce radius 'float)))

;; all the C functions

(defcfun ("sfCircleShape_create" sf-circle-shape-create) :pointer)

(defun make-circle (radius)
  (let ((new-circle
	 (make-instance 'circle
			:radius (coerce radius 'float)
			:pointer (sf-circle-shape-create))))
    (setf (circle-radius new-circle) (coerce radius 'float))
    new-circle))

(defcfun ("sfCircleShape_getRadius" sf-circle-shape-get-radius) :float
  (shape :pointer))

(defmethod circle-radius :before ((c circle))
  (setf (slot-value c 'radius) (sf-circle-shape-get-radius (shape-pointer c))))

(defcfun ("sfCircleShape_copy" sf-circle-shape-copy) :pointer
  (shape :pointer))

(defmethod circle-copy ((c circle))
  (make-instance 'circle
		 :radius (circle-radius c)
		 :pointer (sf-circle-shape-copy (shape-pointer c))))

(defcfun ("sfCircleShape_destroy" sf-circle-shape-destroy) :void
  (shape :pointer))

(defmethod circle-destroy ((c circle))
  (sf-circle-shape-destroy (shape-pointer c)))

(defcfun ("sfCircleShape_getPosition" sf-circle-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-position :before ((c circle))
  (setf (slot-value c 'position) (sf-circle-shape-get-position
				  (shape-pointer c))))

(defcfun ("sfCircleShape_setPosition" sf-circle-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defmethod (setf shape-position) :after ((v vect) (c circle))
  (sf-circle-shape-set-position (shape-pointer c) v))

(defcfun ("sfCircleShape_getRotation" sf-circle-shape-get-rotation) :float
  (shape :pointer))

(defmethod shape-rotation :before ((c circle))
  (setf (slot-value c 'rotation) (sf-circle-shape-get-rotation (shape-pointer c))))

(defcfun ("sfCircleShape_setRotation" sf-circle-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defmethod (setf shape-rotation) :after ((angle number) (c circle))
  (sf-circle-shape-set-rotation (shape-pointer c) (coerce angle 'float)))

(defcfun ("sfCircleShape_getScale" sf-circle-shape-get-scale) :float
  (shape :pointer))

(defmethod shape-scale :before ((c circle))
  (setf (slot-value c 'origin) (sf-circle-shape-get-scale (shape-pointer c))))

(defcfun ("sfCircleShape_setScale" sf-circle-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defmethod (setf shape-scale) :after ((factor number) (c circle))
  (sf-circle-shape-set-scale (shape-pointer c) (coerce factor 'float)))

(defcfun ("sfCircleShape_getOrigin" sf-circle-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-origin ((c circle))
  (setf (slot-value c 'origin) (sf-circle-shape-get-origin (shape-pointer c))))

(defcfun ("sfCircleShape_setOrigin" sf-circle-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defmethod (setf shape-origin) :after ((v vect) (c circle))
  (sf-circle-shape-set-origin (shape-pointer c) v))

(defcfun ("sfCircleShape_move" sf-circle-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((c circle) (offset vect))
  (sf-circle-shape-move (shape-pointer c) offset))

(defcfun ("sfCircleShape_rotate" sf-circle-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defmethod entity-rotate ((c circle) (angle number))
  (sf-circle-shape-rotate (shape-pointer c) (coerce angle 'float)))

(defcfun ("sfCircleShape_scale" sf-circle-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defmethod entity-do-scale ((c circle) (scale vect))
  (sf-circle-shape-scale (shape-pointer c) scale))

(defcfun ("sfCircleShape_getTransform" sf-circle-shape-get-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-transform :before ((c circle))
  (setf (slot-value c 'transform) (sf-circle-shape-get-transform (shape-pointer c))))

(defcfun ("sfCircleShape_getInverseTransform" sf-circle-shape-get-inverse-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-inverse-transform :before ((c circle))
  (setf (slot-value c 'inverse-transform) (sf-circle-shape-get-inverse-transform (shape-pointer c))))

(defcfun ("sfCircleShape_getTexture" sf-circle-shape-get-texture) :pointer
  (shape :pointer))

(defmethod entity-texture :before ((c circle))
  (setf (slot-value c 'texture) (sf-circle-shape-get-texture (shape-pointer c))))

(defcfun ("sfCircleShape_setTexture" sf-circle-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defmethod entity-set-texture ((c circle) (tex texture) reset-rect)
  (setf (entity-texture c) tex)
  (sf-circle-shape-set-texture (shape-pointer c) (texture-pointer tex) reset-rect))

(defcfun ("sfCircleShape_getTextureRect" sf-circle-shape-get-texture-rect) :pointer
  (shape :pointer))

(defmethod shape-texture-rect :before ((c circle))
  (setf (slot-value c 'texture-rect) (sf-circle-shape-get-texture-rect
				      (shape-pointer c))))

(defcfun ("sfCircleShape_setTextureRect" sf-circle-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defmethod (setf shape-texture-rect) :after ((r rect) (c circle))
  (sf-circle-shape-set-texture-rect (shape-pointer c) r))

(defcfun ("sfCircleShape_getFillColor"
	  sf-circle-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-fill-color :before ((c circle))
  (setf (slot-value c 'fill-color) (sf-circle-shape-get-fill-color (shape-pointer c))))

(defcfun ("sfCircleShape_setFillColor" sf-circle-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-fill-color) :after ((c color) (circ circle))
  (sf-circle-shape-set-fill-color (shape-pointer circ) c))

(defcfun ("sfCircleShape_getOutlineColor"
	  sf-circle-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-outline-color :before ((c circle))
  (setf (slot-value c 'outline-color)
	(sf-circle-shape-get-outline-color (shape-pointer c))))

(defcfun ("sfCircleShape_setOutlineColor" sf-circle-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-outline-color) :after ((c color) (circ circle))
  (sf-circle-shape-set-outline-color (shape-pointer circ) c))

(defcfun ("sfCircleShape_getOutlineThickness"
	  sf-circle-shape-get-outline-thickness) :float
  (shape :pointer))

(defmethod shape-outline-thickness :before ((c circle))
  (setf (slot-value c 'outline-thickness)
	(sf-circle-shape-get-outline-thickness (shape-pointer c))))

(defcfun ("sfCircleShape_setOutlineThickness"
	  sf-circle-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defmethod (setf shape-outline-thickness) :after ((thickness number) (circ circle))
  (sf-circle-shape-set-outline-thickness
   (shape-pointer circ) (coerce thickness 'float)))

(defcfun ("sfCircleShape_getPointCount" sf-circle-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defmethod shape-point-count :before ((c circle))
  (setf (slot-value c 'point-count)
	(sf-circle-shape-get-point-count (shape-pointer c))))
    
(defcfun ("sfCircleShape_setPointCount" sf-circle-shape-set-point-count) :void
  (shape :pointer)
  (count :unsigned-int))

(defmethod (setf shape-point-count) :after ((count integer) (c circle))
  (sf-circle-shape-set-point-count (shape-pointer c) count))

(defcfun ("sfCircleShape_getPoint" sf-circle-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defmethod circle-get-point ((c circle) (index integer))
  (sf-circle-shape-get-point (shape-pointer c) index))

(defcfun ("sfCircleShape_getLocalBounds"
	  sf-circle-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-local-bbox :before ((c circle))
  (setf (slot-value c 'local-bbox) (sf-circle-shape-get-local-bounds (shape-pointer c))))

(defcfun ("sfCircleShape_getGlobalBounds"
	  sf-circle-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-global-bbox :before ((c circle))
  (setf (slot-value c 'global-bbox)
	(sf-circle-shape-get-global-bounds (shape-pointer c))))



