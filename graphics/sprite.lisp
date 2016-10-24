(in-package :sfml)

(defclass sprite (entity)
  ((pointer :initarg :pointer :initform nil :accessor sprite-pointer)
   (size :initarg :size :initform 0 :accessor sprite-size)
   (texture :initarg :texture :initform nil :accessor sprite-texture)
   (texture-rect :initarg :texture-rect :initform nil :accessor sprite-texture-rect)
   (color :initarg :color :initform (make-color) :accessor sprite-color)
   ;; this slot doesn't do a whole lot other than to be used as a parameter
   ;; to sf-set-texture
   (reset-rect :initform t :accessor sprite-reset-rect)))

(defcfun ("sfSprite_create" sf-sprite-create) :pointer)

(defun make-sprite (&key (color (make-color)))
  (make-instance 'sprite
		 :pointer (sf-sprite-create)
		 :color color))
    
(defcfun ("sfSprite_copy" sf-sprite-copy) :pointer
  (sprite :pointer))

(defmethod sprite-copy ((spr sprite))
  (make-instance 'sprite :pointer (sf-sprite-copy (sprite-pointer spr))))

(defcfun ("sfSprite_destroy" sf-sprite-destroy) :void
  (sprite :pointer))

(defmethod sprite-destroy ((spr sprite))
  (sf-sprite-destroy (sprite-pointer spr)))

(defcfun ("sfSprite_getPosition" sf-sprite-get-position) (:struct sf-vector-2f)
  (sprite :pointer))

(defmethod entity-position :before ((spr sprite))
  (setf (slot-value spr 'position) (sf-sprite-get-position (sprite-pointer spr))))

(defcfun ("sfSprite_setPosition" sf-sprite-set-position) :void
  (sprite :pointer)
  (position (:struct sf-vector-2f)))

(defmethod (setf entity-position) :after ((v vect) (spr sprite))
  (sf-sprite-set-position (sprite-pointer spr) v))

(defcfun ("sfSprite_getRotation" sf-sprite-get-rotation) :float
  (sprite :pointer))

(defmethod entity-rotation :before ((spr sprite))
  (setf (slot-value spr 'rotation) (sf-sprite-get-rotation (sprite-pointer spr))))

(defcfun ("sfSprite_setRotation" sf-sprite-set-rotation) :void
  (sprite :pointer)
  (angle :float))

(defmethod (setf entity-rotation) :after ((angle number) (spr sprite))
  (sf-sprite-set-rotation (sprite-pointer spr) (coerce angle 'single-float)))

(defcfun ("sfSprite_getScale" sf-sprite-get-scale) (:struct sf-vector-2f)
  (sprite :pointer))

(defmethod entity-scale :before ((spr sprite))
  (setf (slot-value spr 'scale) (sf-sprite-get-scale (sprite-pointer spr))))

(defcfun ("sfSprite_setScale" sf-sprite-set-scale) :void
  (sprite :pointer)
  (scale (:struct sf-vector-2f)))

(defmethod (setf entity-scale) :after ((v vect) (spr sprite))
  (sf-sprite-set-scale (sprite-pointer spr) v))

(defcfun ("sfSprite_getOrigin" sf-sprite-get-origin) (:struct sf-vector-2f)
  (sprite :pointer))

(defmethod entity-origin :before ((spr sprite))
  (setf (slot-value spr 'origin) (sf-sprite-get-origin (sprite-pointer spr))))

(defcfun ("sfSprite_setOrigin" sf-sprite-set-origin) :void
  (sprite :pointer)
  (origin (:struct sf-vector-2f)))

(defmethod (setf entity-origin) :after ((v vect) (spr sprite))
  (sf-sprite-set-origin (sprite-pointer spr) v))

(defcfun ("sfSprite_move" sf-sprite-move) :void
  (sprite :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((spr sprite) (offset vect))
  (sf-sprite-move (sprite-pointer spr) offset))

(defcfun ("sfSprite_rotate" sf-sprite-rotate) :void
  (sprite :pointer)
  (angle :float))

(defmethod entity-rotate ((spr sprite) (angle number))
  (sf-sprite-rotate (sprite-pointer spr) (coerce angle 'single-float)))

(defcfun ("sfSprite_scale" sf-sprite-scale) :void
  (sprite :pointer)
  (factors (:struct sf-vector-2f)))

(defmethod entity-do-scale ((spr sprite) (scale vect))
  (sf-sprite-scale (sprite-pointer spr) scale))

(defcfun ("sfSprite_getTransform" sf-sprite-get-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-transform :before ((spr sprite))
  (setf (slot-value spr 'transform) (sf-sprite-get-transform (sprite-pointer spr))))

(defcfun ("sfSprite_getInverseTransform" sf-sprite-get-inverse-transform) (:struct sf-transform)
  (shape :pointer))

(defmethod entity-inverse-transform :before ((spr sprite))
  (setf (slot-value spr 'inverse-transform) (sf-sprite-get-inverse-transform (sprite-pointer spr))))

(defcfun ("sfSprite_getColor" sf-sprite-get-color) (:struct sf-color)
  (sprite :pointer))

(defmethod sprite-color :before ((spr sprite))
  (setf (slot-value spr 'color) (sf-sprite-get-color (sprite-pointer spr))))

(defcfun ("sfSprite_setColor" sf-sprite-set-color) :void
  (sprite :pointer)
  (color (:struct sf-color)))

(defmethod (setf sprite-color) :after ((c color) (spr sprite))
  (sf-sprite-set-color (sprite-pointer spr) c))

(defcfun ("sfSprite_getTexture" sf-sprite-get-texture) :pointer
  (sprite :pointer))

(defmethod sprite-texture :before ((spr sprite))
  (setf (slot-value spr 'texture) (sf-sprite-get-texture (sprite-pointer spr))))

(defcfun ("sfSprite_setTexture" sf-sprite-set-texture) :void
  (sprite :pointer)
  (texture :pointer)
  (reset-rect sf-bool))

(defmethod (setf sprite-texture) :after ((tex texture) (spr sprite))
  (sf-sprite-set-texture (sprite-pointer spr)
			 (texture-pointer tex)
			 (sprite-reset-rect spr)))

(defcfun ("sfSprite_getTextureRect" sf-sprite-get-texture-rect) (:struct sf-int-rect)
  (sprite :pointer))

(defmethod sprite-texture-rect :before ((spr sprite))
  (setf (slot-value spr 'texture-rect)
	(sf-sprite-get-texture-rect (sprite-pointer spr))))

(defcfun ("sfSprite_setTextureRect" sf-sprite-set-texture-rect) :void
  (sprite :pointer)
  (rectangle (:struct sf-int-rect)))

(defmethod (setf sprite-texture-rect) :after ((r rect) (spr sprite))
  (sf-sprite-set-texture-rect (sprite-pointer spr) r))

(defcfun ("sfSprite_getLocalBounds"
	  sf-sprite-get-local-bounds) (:struct sf-float-rect)
  (sprite :pointer))

(defmethod entity-local-bbox :before ((spr sprite))
  (setf (slot-value spr 'local-bbox) (sf-sprite-get-local-bounds (sprite-pointer spr))))

(defcfun ("sfSprite_getGlobalBounds"
	  sf-sprite-get-global-bounds) (:struct sf-float-rect)
  (sprite :pointer))

(defmethod entity-global-bbox :before ((spr sprite))
  (setf (slot-value spr 'global-bbox) (sf-sprite-get-global-bounds (sprite-pointer spr))))

(defcfun ("sfSprite_findCharacterPos" sf-sprite-find-character-pos) (:struct sf-vector-2f)
  (sprite :pointer)
  (index :unsigned-int))

(defmethod sprite-character-position ((spr sprite) (index integer))
  (sf-sprite-find-character-pos (sprite-pointer spr) index))
