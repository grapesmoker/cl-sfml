(in-package :sfml)

(defclass texture ()
  ((pointer :initarg :pointer :initform nil :accessor texture-pointer)
   (width :initarg :width :initform 0 :accessor texture-width)
   (height :initarg :height :initform 0 :accessor texture-height)
   (smooth :initarg :smooth :initform nil :accessor texture-smooth)
   (repeated :initarg :repeated :initform nil :accessor texture-repeated)
   (native-handle :initform nil :accessor texture-native-handle)))

(defcfun ("sfTexture_create" sf-texture-create) :pointer
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun ("sfTexture_createFromFile" sf-texture-create-from-file) :pointer
  (filename :string)
  (area (:pointer (:struct sf-int-rect))))

(defcfun ("sfTexture_createFromImage" sf-texture-create-from-image) :pointer
  (image :pointer)
  (area (:pointer (:struct sf-int-rect))))
  

(defun make-texture (&key
		       (width 0) (height 0)
		       (image nil image-p)
		       (filename "" filename-p) (area (make-rect) area-p))
  (cond ((and image-p area-p)
	 (make-instance 'texture
			:pointer (sf-texture-create-from-image
				  (image-pointer image)
				  (convert-to-foreign area '(:struct sf-int-rect)))))
	((and filename-p area-p)
	 (with-foreign-string (fn filename)
	   (make-instance
	    'texture
	    :pointer (sf-texture-create-from-file
		      fn (convert-to-foreign area '(:struct sf-int-rect))))))
	(t
	 (make-instance 'texture :width width :height height))))

(defcfun ("sfTexture_copy" sf-texture-copy) :pointer
  (texture :pointer))

(defmethod texture-copy ((tex texture))
  (make-instance 'texture
		 :pointer (sf-texture-copy (texture-pointer tex))))

(defcfun ("sfTexture_destroy" sf-texture-destroy) :void
  (texture :pointer))

(defmethod texture-destroy ((tex texture))
  (sf-texture-destroy (texture-pointer tex)))

(defcfun ("sfTexture_getSize" sf-texture-get-size) (:struct sf-vector-2u)
  (texture :pointer))

(defmethod texture-size ((tex texture))
  (sf-texture-get-size (texture-pointer tex)))
  
(defcfun ("sfTexture_copyToImage" sf-texture-copy-to-image) :pointer
  (texture :pointer))

(defmethod texture-copy-to-image ((tex texture))
  (make-image :pointer (sf-texture-copy-to-image (texture-pointer tex))))

(defcfun ("sfTexture_updateFromPixels" sf-texture-update-from-pixels) :void
  (texture :pointer)
  (pixels (:pointer sf-uint-8))
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun ("sfTexture_updateFromImage" sf-texture-update-from-image) :void
  (texture :pointer)
  (image :pointer)
  (x :unsigned-int)
  (y :unsigned-int))

(defmethod texture-update-from-image ((tex texture) (im image) (x integer) (y integer))
  (sf-texture-update-from-image (texture-pointer tex)
				(image-pointer im)
				x y))

(defcfun ("sfTexture_updateFromWindow" sf-texture-update-from-window) :void
  (texture :pointer)
  (window :pointer)
  (x :unsigned-int)
  (y :unsigned-int))

(defmethod texture-update-from-window ((tex texture) (w window) (x integer) (y integer))
  (sf-texture-update-from-window (texture-pointer tex)
				 (window-pointer w)
				 x y))

(defcfun ("sfTexture_updateFromRenderWindow" sf-texture-update-from-render-window) :void
  (texture :pointer)
  (render-window :pointer)
  (x :unsigned-int)
  (y :unsigned-int))

;; TODO: implement method for updating from render-window
;; after implementing render-window

(defcfun ("sfTexture_isSmooth" sf-texture-is-smooth) sf-bool
  (texture :pointer))

(defcfun ("sfTexture_setSmooth" sf-texture-set-smooth) :void
  (texture :pointer)
  (smooth sf-bool))

;; interface to the set/get via specializers on the standard set/get slot functions

(defmethod texture-smooth :before ((tex texture))
  (setf (slot-value tex 'smooth) (sf-texture-is-smooth (texture-pointer tex))))

(defmethod (setf texture-smooth) :after (value (tex texture))
  (sf-texture-set-smooth (texture-pointer tex) value))

(defcfun ("sfTexture_isRepeated" sf-texture-is-repeated) sf-bool
  (texture :pointer))

(defcfun ("sfTexture_setRepeated" sf-texture-set-repeated) :void
  (texture :pointer)
  (repeated sf-bool))

(defmethod texture-repeated :before ((tex texture))
  (setf (slot-value tex 'repeated) (sf-texture-is-repeated (texture-pointer tex))))

(defmethod (setf texture-repeated) :after (value (tex texture))
  (sf-texture-set-repeated (texture-pointer tex) value))

;; this function comes with grave warnings

(defcfun ("sfTexture_getNativeHandle" sf-texture-get-native-handle) :unsigned-int
  (texture :pointer))

(defmethod texture-native-handle :before ((tex texture))
  (setf (slot-value tex 'native-handle)
	(sf-texture-get-native-handle (texture-pointer tex))))

;; only use this when mixing OpenGL

(defcfun ("sfTexture_bind" sf-texture-bind) :void
  (texture :pointer))

(defmethod texture-bind ((tex texture))
  (sf-texture-bind (texture-pointer tex)))

(defmethod texture-unbind ((tex texture))
  (sf-texture-bind (null-pointer)))
