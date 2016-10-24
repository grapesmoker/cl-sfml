(in-package :sfml)

(defclass rectangle (shape)
  ((type :initform :rectangle)))

(defmethod initialize-instance :after ((r rectangle) &key)
  (setf (shape-pointer r) (sf-rectangle-shape-create)))


;; all the C functions

(defcfun ("sfRectangleShape_create" sf-rectangle-shape-create) :pointer)

(defun make-rectangle (width height)
  (make-instance 'rectangle :width width :height height))
		 
(defcfun ("sfRectangleShape_copy" sf-rectangle-shape-copy) :pointer
  (shape :pointer))

(defmethod rectangle-copy ((r rectangle))
  (make-instance 'rectangle :pointer (sf-rectangle-shape-copy (shape-pointer r))))

(defcfun ("sfRectangleShape_destroy" sf-rectangle-shape-destroy) :void
  (shape :pointer))

(defmethod rectangle-destroy ((r rectangle))
  (sf-rectangle-shape-destroy (shape-pointer r)))

(defcfun ("sfRectangleShape_getPosition" sf-rectangle-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-position :before ((r rectangle))
  (setf (slot-value r 'position) (sf-rectangle-shape-get-position
				  (shape-pointer r))))

(defcfun ("sfRectangleShape_setPosition" sf-rectangle-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defmethod (setf shape-position) :after ((v vect) (r rectangle))
  (sf-rectangle-shape-set-position (shape-pointer r) v))

(defcfun ("sfRectangleShape_getRotation" sf-rectangle-shape-get-rotation) :float
  (shape :pointer))

(defmethod shape-rotation :before ((r rectangle))
  (setf (slot-value r 'rotation) (sf-rectangle-shape-get-rotation (shape-pointer r))))

(defcfun ("sfRectangleShape_setRotation" sf-rectangle-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defmethod (setf shape-rotation) :after ((angle number) (r rectangle))
  (sf-rectangle-shape-set-rotation (shape-pointer r) (coerce angle 'float)))

(defcfun ("sfRectangleShape_getScale" sf-rectangle-shape-get-scale) :float
  (shape :pointer))

(defmethod shape-scale :before ((r rectangle))
  (setf (slot-value r 'origin) (sf-rectangle-shape-get-scale (shape-pointer r))))

(defcfun ("sfRectangleShape_setScale" sf-rectangle-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defmethod (setf shape-scale) :after ((factor number) (r rectangle))
  (sf-rectangle-shape-set-scale (shape-pointer r) (coerce factor 'float)))

(defcfun ("sfRectangleShape_getOrigin" sf-rectangle-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-origin ((r rectangle))
  (setf (slot-value r 'origin) (sf-rectangle-shape-get-origin (shape-pointer r))))

(defcfun ("sfRectangleShape_setOrigin" sf-rectangle-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defmethod (setf shape-origin) :after ((v vect) (r rectangle))
  (sf-rectangle-shape-set-origin (shape-pointer r) v))

(defcfun ("sfRectangleShape_move" sf-rectangle-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((r rectangle) (offset vect))
  (sf-rectangle-shape-move (shape-pointer r) offset))

(defcfun ("sfRectangleShape_rotate" sf-rectangle-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defmethod entity-rotate ((r rectangle) (angle number))
  (sf-rectangle-shape-rotate (shape-pointer r) (coerce angle 'float)))

(defcfun ("sfRectangleShape_scale" sf-rectangle-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defmethod entity-do-scale ((r rectangle) (scale vect))
  (sf-rectangle-shape-scale (shape-pointer r) scale))

(defcfun ("sfRectangleShape_getTransform" sf-rectangle-shape-get-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-transform :before ((r rectangle))
  (setf (slot-value r 'transform) (sf-rectangle-shape-get-transform (shape-pointer r))))

(defcfun ("sfRectangleShape_getInverseTransform" sf-rectangle-shape-get-inverse-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-inverse-transform :before ((r rectangle))
  (setf (slot-value r 'inverse-transform) (sf-rectangle-shape-get-inverse-transform (shape-pointer r))))

(defcfun ("sfRectangleShape_getTexture" sf-rectangle-shape-get-texture) :pointer
  (shape :pointer))

(defmethod entity-texture :before ((r rectangle))
  (setf (slot-value r 'texture) (sf-rectangle-shape-get-texture (shape-pointer r))))

(defcfun ("sfRectangleShape_setTexture" sf-rectangle-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defmethod entity-set-texture ((r rectangle) (tex texture) reset-rect)
  (setf (entity-texture r) tex)
  (sf-rectangle-shape-set-texture (shape-pointer r) (texture-pointer tex) reset-rect))

(defcfun ("sfRectangleShape_getTextureRect" sf-rectangle-shape-get-texture-rect) :pointer
  (shape :pointer))

(defmethod shape-texture-rect :before ((r rectangle))
  (setf (slot-value r 'texture-rect) (sf-rectangle-shape-get-texture-rect
				      (shape-pointer r))))

(defcfun ("sfRectangleShape_setTextureRect" sf-rectangle-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defmethod (setf shape-texture-rect) :after ((r rect) (rect rectangle))
  (sf-rectangle-shape-set-texture-rect (shape-pointer rect) r))

(defcfun ("sfRectangleShape_getFillColor"
	  sf-rectangle-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-fill-color :before ((r rectangle))
  (setf (slot-value r 'fill-color) (sf-rectangle-shape-get-fill-color (shape-pointer r))))

(defcfun ("sfRectangleShape_setFillColor" sf-rectangle-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-fill-color) :after ((c color) (r rectangle))
  (sf-rectangle-shape-set-fill-color (shape-pointer r) c))

(defcfun ("sfRectangleShape_getOutlineColor"
	  sf-rectangle-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defmethod shape-outline-color :before ((r rectangle))
  (setf (slot-value r 'outline-color)
	(sf-rectangle-shape-get-outline-color (shape-pointer r))))

(defcfun ("sfRectangleShape_setOutlineColor" sf-rectangle-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defmethod (setf shape-outline-color) :after ((c color) (r rectangle))
  (sf-rectangle-shape-set-outline-color (shape-pointer r) c))

(defcfun ("sfRectangleShape_getOutlineThickness"
	  sf-rectangle-shape-get-outline-thickness) :float
  (shape :pointer))

(defmethod shape-outline-thickness :before ((r rectangle))
  (setf (slot-value r 'outline-thickness)
	(sf-rectangle-shape-get-outline-thickness (shape-pointer r))))

(defcfun ("sfRectangleShape_setOutlineThickness"
	  sf-rectangle-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defmethod (setf shape-outline-thickness) :after ((thickness number) (r rectangle))
  (sf-rectangle-shape-set-outline-thickness
   (shape-pointer r) (coerce thickness 'float)))

(defcfun ("sfRectangleShape_getPointCount" sf-rectangle-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defmethod shape-point-count :before ((r rectangle))
  (setf (slot-value r 'point-count)
	(sf-rectangle-shape-get-point-count (shape-pointer r))))

(defcfun ("sfRectangleShape_getPoint" sf-rectangle-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defmethod rectangle-get-point ((r rectangle) (index integer))
  (sf-rectangle-shape-get-point (shape-pointer r) index))

(defcfun ("sfRectangleShape_getSize" sf-rectangle-shape-get-size) (:struct sf-vector-2f)
  (shape :pointer))

(defmethod shape-size :before ((r rectangle))
  (setf (slot-value r 'size) (sf-rectangle-shape-get-size (shape-pointer r))))

(defcfun ("sfRectangleShape_setSize" sf-rectangle-shape-set-size) :void
  (shape :pointer)
  (size (:struct sf-vector-2f)))

(defmethod (setf shape-size) :after ((size rect) (r rectangle))
  (sf-rectangle-shape-set-size (shape-pointer r) size))

(defcfun ("sfRectangleShape_getLocalBounds"
	  sf-rectangle-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-local-bbox :before ((r rectangle))
  (setf (slot-value r 'local-bbox) (sf-rectangle-shape-get-local-bounds (shape-pointer r))))

(defcfun ("sfRectangleShape_getGlobalBounds"
	  sf-rectangle-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defmethod shape-global-bbox :before ((r rectangle))
  (setf (slot-value r 'global-bbox)
	(sf-circle-shape-get-global-bounds (shape-pointer r))))
