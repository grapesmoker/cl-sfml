(in-package :sfml)

(defclass rectangle (shape)
  ((type :initform :rectangle)))

(defmethod initialize-instance :after ((r rectangle) &key)
  (setf (shape-pointer r) (sf-rectangle-shape-create)))


;; all the C functions

(defcfun ("sfRectangleShape_create" sf-rectangle-shape-create) :pointer)

(defcfun ("sfRectangleShape_copy" sf-rectangle-shape-copy) :pointer
  (shape :pointer))

(defcfun ("sfRectangleShape_destroy" sf-rectangle-shape-destroy) :void
  (shape :pointer))

(defcfun ("sfRectangleShape_setPosition" sf-rectangle-shape-set-position) :void
  (shape :pointer)
  (position (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_setRotation" sf-rectangle-shape-set-rotation) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfRectangleShape_setScale" sf-rectangle-shape-set-scale) :void
  (shape :pointer)
  (scale (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_setOrigin" sf-rectangle-shape-set-origin) :void
  (shape :pointer)
  (origin (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_getPosition" sf-rectangle-shape-get-position)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfRectangleShape_getRotation" sf-rectangle-shape-get-rotation) :float
  (shape :pointer))

(defcfun ("sfRectangleShape_getScale" sf-rectangle-shape-get-scale) :float
  (shape :pointer))

(defcfun ("sfRectangleShape_getOrigin" sf-rectangle-shape-get-origin)
    (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfRectangleShape_move" sf-rectangle-shape-move) :void
  (shape :pointer)
  (offset (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_rotate" sf-rectangle-shape-rotate) :void
  (shape :pointer)
  (angle :float))

(defcfun ("sfRectangleShape_scale" sf-rectangle-shape-scale) :void
  (shape :pointer)
  (factors (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_setTexture" sf-rectangle-shape-set-texture) :void
  (shape :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defcfun ("sfRectangleShape_setTextureRect" sf-rectangle-shape-set-texture-rect) :void
  (shape :pointer)
  (rect (:struct sf-int-rect)))

(defcfun ("sfRectangleShape_setFillColor" sf-rectangle-shape-set-fill-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfRectangleShape_setOutlineColor" sf-rectangle-shape-set-outline-color) :void
  (shape :pointer)
  (color (:struct sf-color)))

(defcfun ("sfRectangleShape_setOutlineThickness"
	  sf-rectangle-shape-set-outline-thickness) :void
  (shape :pointer)
  (thickness :float))

(defcfun ("sfRectangleShape_getTexture" sf-rectangle-shape-get-texture) :pointer
  (shape :pointer))

(defcfun ("sfRectangleShape_getTextureRect" sf-rectangle-shape-get-texture-rect) :pointer
  (shape :pointer))

(defcfun ("sfRectangleShape_getFillColor"
	  sf-rectangle-shape-get-fill-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfRectangleShape_getOutlineColor"
	  sf-rectangle-shape-get-outline-color) (:struct sf-color)
  (shape :pointer))

(defcfun ("sfRectangleShape_getOutlineThickness"
	  sf-rectangle-shape-get-outline-thickness) :float
  (shape :pointer))

(defcfun ("sfRectangleShape_getPointCount" sf-rectangle-shape-get-point-count)
    :unsigned-int
  (shape :pointer))

(defcfun ("sfRectangleShape_getPointCount" sf-rectangle-shape-get-point)
    (:struct sf-vector-2f)
  (shape :pointer)
  (index :unsigned-int))

(defcfun ("sfRectangleShape_setSize" sf-rectangle-shape-set-size) :void
  (shape :pointer)
  (size (:struct sf-vector-2f)))

(defcfun ("sfRectangleShape_getSize" sf-rectangle-shape-get-size) (:struct sf-vector-2f)
  (shape :pointer))

(defcfun ("sfRectangleShape_getLocalBounds"
	  sf-rectangle-shape-get-local-bounds) (:struct sf-float-rect)
  (shape :pointer))

(defcfun ("sfRectangleShape_getGlobalBounds"
	  sf-rectangle-shape-get-global-bounds) (:struct sf-float-rect)
  (shape :pointer))


(defmethod update-rect-position ((rect rectangle))
  (sf-rectangle-shape-set-position (shape-pointer rect) (shape-position rect)))

(defmethod update-rect-rotation ((rect rectangle))
  (sf-rectangle-shape-set-rotation (shape-pointer rect) (shape-rotation rect)))

(defmethod update-rect-scale ((rect rectangle))
  (sf-rectangle-shape-set-scale (shape-pointer rect) (shape-scale rect)))

(defmethod update-rect-origin ((rect rectangle))
  (sf-rectangle-shape-set-origin (shape-pointer rect) (shape-origin rect)))

(defmethod update-rect-fill-color ((rect rectangle))
  (sf-rectangle-shape-set-fill-color (shape-pointer rect) (shape-fill-color rect)))

(defmethod update-rect-outline-color ((rect rectangle))
  (sf-rectangle-shape-set-outline-color (shape-pointer rect) (shape-outline-color rect)))

(defmethod update-rect-outline-thickness ((rect rectangle))
  (sf-rectangle-shape-set-outline-thickness (shape-pointer rect)
					    (shape-outline-thickness rect)))

(defmethod update-rect-texture ((rect rectangle) reset-rect)
  (sf-rectangle-shape-set-texture (shape-pointer rect)
				  (shape-texture rect)
				  reset-rect))

(defmethod update-rect-size ((rect rectangle))
  (sf-rectangle-shape-set-size (shape-pointer rect) (shape-size rect)))

;; update any slots that have changed via calls to C and clear the tracker

(defmethod update-rect ((rect rectangle))
  (loop
     for slot in (shape-changed-slots rect)
     do
       (case slot
	 (position (update-rect-position rect))
	 (rotation (update-rect-rotation rect))
	 (scale (update-rect-scale rect))
	 (size (update-rect-size rect))
	 (fill-color (update-rect-fill-color rect))
	 (outline-color (update-rect-outline-color rect))
	 (outline-thickness (update-rect-outline-thickness rect))))
  (setf (shape-changed-slots rect) '()))
	 
