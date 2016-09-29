(in-package :sfml)

(defclass circle (shape)
  ((type :initform :circle)
   (radius :initarg :radius :initform 0 :accessor circle-radius)))

(defun make-circle (radius)
  (make-instance 'circle :radius (coerce radius 'float)))

(defcfun ("sfCircleShape_setRadius" sf-circle-shape-set-radius) :void
  (shape :pointer)
  (radius :float))

(defmethod initialize-instance :after ((c circle) &key)
  (setf (shape-pointer c) (sf-circle-shape-create))
  (sf-circle-shape-set-radius (shape-pointer c) (circle-radius c)))


;; all the C functions

(defcfun ("sfCircleShape_create" sf-circle-shape-create) :pointer)

(defcfun ("sfCircleShape_copy" sf-circle-shape-copy) :pointer
  (shape :pointer))

(defcfun ("sfCircleShape_destroy" sf-circle-shape-destroy) :void
  (shape :pointer))

(defcfun ("sfCircleShape_setPosition" sf-circle-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_setRotation" sf-circle-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfCircleShape_setScale" sf-circle-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_setOrigin" sf-circle-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_getPosition" sf-circle-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfCircleShape_getRotation" sf-circle-shape-get-rotation) :float
  (shape :pointer))

(defcfun ("sfCircleShape_getScale" sf-circle-shape-get-scale) :float
  (shape :pointer))

(defcfun ("sfCircleShape_getOrigin" sf-circle-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfCircleShape_move" sf-circle-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_rotate" sf-circle-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfCircleShape_scale" sf-circle-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_setTexture" sf-circle-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defcfun ("sfCircleShape_setTextureRect" sf-circle-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defcfun ("sfCircleShape_setFillColor" sf-circle-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfCircleShape_setOutlineColor" sf-circle-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfCircleShape_setOutlineThickness"
	  sf-circle-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defcfun ("sfCircleShape_getTexture" sf-circle-shape-get-texture) :pointer
  (shape :pointer))

(defcfun ("sfCircleShape_getTextureRect" sf-circle-shape-get-texture-rect) :pointer
  (shape :pointer))

(defcfun ("sfCircleShape_getFillColor"
	  sf-circle-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfCircleShape_getOutlineColor"
	  sf-circle-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfCircleShape_getOutlineThickness"
	  sf-circle-shape-get-outline-thickness) :float
  (shape :pointer))

(defcfun ("sfCircleShape_getPointCount" sf-circle-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defcfun ("sfCircleShape_getPointCount" sf-circle-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defcfun ("sfCircleShape_setSize" sf-circle-shape-set-size) :void
  (shape :pointer)
  (size (:struct sf-vector-2f)))

(defcfun ("sfCircleShape_getSize" sf-circle-shape-get-size) (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfCircleShape_getLocalBounds"
	  sf-circle-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defcfun ("sfCircleShape_getGlobalBounds"
	  sf-circle-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))


(defmethod update-circle-position ((circ circle))
  (sf-circle-shape-set-position (shape-pointer circ) (shape-position circ)))

(defmethod update-circle-rotation ((circ circle))
  (sf-circle-shape-set-rotation (shape-pointer circ) (shape-rotation circ)))

(defmethod update-circle-scale ((circ circle))
  (sf-circle-shape-set-scale (shape-pointer circ) (shape-scale circ)))

(defmethod update-circle-origin ((circ circle))
  (sf-circle-shape-set-origin (shape-pointer circ) (shape-origin circ)))

(defmethod update-circle-fill-color ((circ circle))
  (sf-circle-shape-set-fill-color (shape-pointer circ) (shape-fill-color circ)))

(defmethod update-circle-outline-color ((circ circle))
  (sf-circle-shape-set-outline-color (shape-pointer circ) (shape-outline-color circ)))

(defmethod update-circle-outline-thickness ((circ circle))
  (sf-circle-shape-set-outline-thickness (shape-pointer circ)
					    (shape-outline-thickness circ)))

(defmethod update-circle-texture ((circ circle) reset-circ)
  (sf-circle-shape-set-texture (shape-pointer circ)
				  (shape-texture circ)
				  reset-circ))

(defmethod update-circle-size ((circ circle))
  (sf-circle-shape-set-size (shape-pointer circ) (shape-size circ)))

;; update any slots that have changed via calls to C and clear the tracker

(defmethod update-circ ((circ circle))
  (loop
     for slot in (shape-changed-slots circ)
     do
       (case slot
	 (position (update-circle-position circ))
	 (rotation (update-circle-rotation circ))
	 (scale (update-circle-scale circ))
	 (size (update-circle-size circ))
	 (fill-color (update-circle-fill-color circ))
	 (outline-color (update-circle-outline-color circ))
	 (outline-thickness (update-circle-outline-thickness circ))))
  (setf (shape-changed-slots circ) '()))
	 
