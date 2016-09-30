(in-package :sfml)

(defcenum sf-text-style
  (:sf-text-regular 0)
  (:sf-text-bold 1)
  (:sf-text-italic 2)
  (:sf-text-underlined 4)
  (:sf-text-strike-through 8))


(defclass text (entity)
  ((pointer :initarg :pointer :initform nil :accessor text-pointer)
   (string :initarg :string :initform "" :accessor text-string)
   (font :initarg :font :initform nil :accessor text-font)
   (size :initarg :size :initform 0 :accessor text-size)
   (style :initarg :style :initform nil :accessor text-style)
   (color :initarg :color :initform (make-color) :accessor text-color)))

(defcfun ("sfText_create" sf-text-create) :pointer)

(defun make-text (&key (string "") (font nil font-p)
		    (size 0) (style nil style-p) (color (make-color)))
  (let ((new-text
	 (make-instance 'text
			:pointer (sf-text-create)
			:string string
			:font font
			:size size
			:style style
			:color color)))
    (setf (text-string new-text) string)
    (if font-p (setf (text-font new-text) font))
    (setf (text-size new-text) size)
    (if style-p (setf (text-style new-text) style))
    (setf (text-color new-text) color)
    new-text))
    
(defcfun ("sfText_copy" sf-text-copy) :pointer
  (text :pointer))

(defmethod text-copy ((tex text))
  (make-instance 'text :pointer (sf-text-copy (text-pointer tex))))

(defcfun ("sfText_destroy" sf-text-destroy) :void
  (text :pointer))

(defmethod text-destroy ((tex text))
  (sf-text-destroy (text-pointer tex)))

(defcfun ("sfText_getPosition" sf-text-get-position)
    (:struct sf-vector-2f)
  (text :pointer))

(defmethod entity-position :before ((tex text))
  (setf (slot-value tex 'position) (sf-text-get-position (text-pointer tex))))

(defcfun ("sfText_setPosition" sf-text-set-position) :void
  (text :pointer)
  (position (:struct sf-vector-2f)))

(defmethod (setf entity-position) :after ((v vect) (tex text))
  (sf-text-set-position (text-pointer tex) v))

(defcfun ("sfText_getRotation" sf-text-get-rotation) :float
  (text :pointer))

(defmethod entity-rotation :before ((tex text))
  (setf (slot-value tex 'rotation) (sf-text-get-rotation (text-pointer tex))))

(defcfun ("sfText_setRotation" sf-text-set-rotation) :void
  (text :pointer)
  (angle :float))

(defmethod (setf entity-rotation) :after ((angle number) (tex text))
  (sf-text-set-rotation (text-pointer tex) (coerce angle 'single-float)))

(defcfun ("sfText_getScale" sf-text-get-scale) (:struct sf-vector-2f)
  (text :pointer))

(defmethod entity-scale :before ((tex text))
  (setf (slot-value tex 'scale) (sf-text-get-scale (text-pointer tex))))

(defcfun ("sfText_setScale" sf-text-set-scale) :void
  (text :pointer)
  (scale (:struct sf-vector-2f)))

(defmethod (setf entity-scale) :after ((v vect) (tex text))
  (sf-text-set-scale (text-pointer tex) v))

(defcfun ("sfText_getOrigin" sf-text-get-origin) (:struct sf-vector-2f)
  (text :pointer))

(defmethod entity-origin :before ((tex text))
  (setf (slot-value tex 'origin) (sf-text-get-origin (text-pointer tex))))

(defcfun ("sfText_setOrigin" sf-text-set-origin) :void
  (text :pointer)
  (origin (:struct sf-vector-2f)))

(defmethod (setf entity-origin) :after ((v vect) (tex text))
  (sf-text-set-origin (text-pointer tex) v))

(defcfun ("sfText_move" sf-text-move) :void
  (text :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((tex text) (offset vect))
  (sf-text-move (text-pointer tex) offset))

(defcfun ("sfText_rotate" sf-text-rotate) :void
  (text :pointer)
  (angle :float))

(defmethod entity-rotate ((tex text) (angle number))
  (sf-text-rotate (text-pointer tex) (coerce angle 'single-float)))

(defcfun ("sfText_scale" sf-text-scale) :void
  (text :pointer)
  (factors (:struct sf-vector-2f)))

(defmethod entity-do-scale ((tex text) (scale vect))
  (sf-text-scale (text-pointer tex) scale))

(defcfun ("sfText_getString" sf-text-get-string) :string
  (text :pointer))

(defmethod text-string :before ((tex text))
  (setf (slot-value tex 'string) (sf-text-get-string (text-pointer tex))))

(defcfun ("sfText_setString" sf-text-set-string) :void
  (text :pointer)
  (string :string))

(defmethod (setf text-string) :after ((string string) (tex text))
  (sf-text-set-string (text-pointer tex) string))

(defcfun ("sfText_getUnicodeString" sf-text-get-unicode-string) (:pointer (sf-uint-32))
  (text :pointer))

(defcfun ("sfText_setUnicodeString" sf-text-set-unicode-string) :void
  (text :pointer)
  (string (:pointer sf-uint-32)))

(defcfun ("sfText_getFont" sf-text-get-font) :pointer
  (text :pointer))

(defmethod text-font :before ((tex text))
  (setf (slot-value tex 'font)
	(make-instance 'font :pointer (sf-text-get-font (text-pointer tex)))))

(defcfun ("sfText_setFont" sf-text-set-font) :void
  (text :pointer)
  (font :pointer))

(defmethod (setf text-font) :after ((f font) (tex text))
  (sf-text-set-font (text-pointer tex) (font-pointer f)))

(defcfun ("sfText_getCharacterSize" sf-text-get-character-size) :unsigned-int
  (text :pointer))

(defmethod text-size :before ((tex text))
  (setf (slot-value tex 'size) (sf-text-get-character-size (text-pointer tex))))

(defcfun ("sfText_setCharacterSize" sf-text-set-character-size) :void
  (text :pointer)
  (size :unsigned-int))

(defmethod (setf text-size) :after ((size integer) (tex text))
  (sf-text-set-character-size (text-pointer tex) size))

(defcfun ("sfText_getStyle" sf-text-get-style) sf-uint-32
  (text :pointer))

(defmethod text-style :before ((tex text))
  (setf (slot-value tex 'style) (sf-text-get-style (text-pointer tex))))

(defcfun ("sfText_setStyle" sf-text-set-style) :void
  (text :pointer)
  (style sf-uint-32))

(defmethod (setf text-style) :after ((style integer) (tex text))
  (sf-text-set-style (text-pointer tex) style))

(defcfun ("sfText_getColor" sf-text-get-color) (:struct sf-color)
  (text :pointer))

(defmethod text-color :before ((tex text))
  (setf (slot-value tex 'color) (sf-text-get-color (text-pointer tex))))

(defcfun ("sfText_setColor" sf-text-set-color) :void
  (text :pointer)
  (color (:struct sf-color)))

(defmethod (setf text-color) :after ((c color) (tex text))
  (sf-text-set-color (text-pointer tex) c))

(defcfun ("sfText_getLocalBounds"
	  sf-text-get-local-bounds) (:struct sf-float-rect)
  (text :pointer))

(defmethod entity-local-bbox :before ((tex text))
  (setf (slot-value tex 'local-bbox) (sf-text-get-local-bounds (text-pointer tex))))

(defcfun ("sfText_getGlobalBounds"
	  sf-text-get-global-bounds) (:struct sf-float-rect)
  (text :pointer))

(defmethod entity-global-bbox :before ((tex text))
  (setf (slot-value tex 'global-bbox) (sf-text-get-global-bounds (text-pointer tex))))

(defcfun ("sfText_findCharacterPos" sf-text-find-character-pos) (:struct sf-vector-2f)
  (text :pointer)
  (index :unsigned-int))

(defmethod text-character-position ((tex text) (index integer))
  (sf-text-find-character-pos (text-pointer tex) index))
