(in-package :sfml)

(defclass texture ()
  ((pointer :initarg :pointer :initform nil :accessor texture-pointer)
   (width :initarg :width :initform 0 :accessor texture-width)
   (height :initarg :height :initform 0 :accessor texture-height)
   (smooth :initarg :smooth :initform nil :accessor texture-smooth)
   (size :initarg :size :initform nil :reader texture-size)
   (repeated :initarg :repeated :initform nil :accessor texture-repeated)
   (srgb :initform nil :accessor texture-srgb)
   (native-handle :initform nil :reader texture-native-handle)))

(defcfun ("sfTexture_create" sf-texture-create) :pointer
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun ("sfTexture_createFromFile" sf-texture-create-from-file) :pointer
  (filename :string)
  (area (:pointer (:struct sf-int-rect))))

(defcfun ("sfTexture_createFromImage" sf-texture-create-from-image) :pointer
  (image :pointer)
  (area (:pointer (:struct sf-int-rect))))
  
(defcfun ("sfTexture_createFromMemory" sf-texture-create-from-memory) :pointer
  (data :pointer)
  (size-in-bytes :unsigned-int)
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

;; I'm only implementing this function for the sake of completeness; it expects
;; a pointer to image data in order to create a texture, which means the user
;; is responsible for doing things like allocating memory for that data
;; and writing the data to memory. I don't recommend doing this, because it
;; breaks the Lispy abstraction of the CLOS API, but if you really, really want
;; to, you can.

(defun make-texture-from-memory (data-pointer size-in-bytes area)
  (sf-texture-create-from-memory data-pointer size-in-bytes
				 (convert-to-foreign area '(:struct sf-int-rect))))


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

(defmethod texture-size :before ((tex texture))
  (setf (slot-value tex 'size) (sf-texture-get-size (texture-pointer tex))))
  
(defcfun ("sfTexture_copyToImage" sf-texture-copy-to-image) :pointer
  (texture :pointer))

(defmethod texture-copy-to-image ((tex texture))
  (make-image :pointer (sf-texture-copy-to-image (texture-pointer tex))))

(defcfun ("sfTexture_updateFromPixels" sf-texture-update-from-pixels) :void
  (texture :pointer)
  (pixels (:pointer sf-uint-8))
  (width :unsigned-int)
  (height :unsigned-int))

(defmethod texture-update-from-pixels ((tex texture) (pixels list))
  (when (every #'(lambda (pix) (typep pix 'pixel)) pixels)
    (let* ((max-x (apply #'max (mapcar #'pixel-x pixels)))
	   (min-x (apply #'min (mapcar #'pixel-x pixels)))
	   (max-y (apply #'max (mapcar #'pixel-y pixels)))
	   (min-y (apply #'min (mapcar #'pixel-y pixels)))
	   (width (- max-x min-x))
	   (height (- max-y min-y))
	   (pixel-ptr (foreign-alloc 'sf-uint-8)))
      ;; each pixel is just an RGBA value, so it takes up 4 slots in the pointer
      ;; the asusmption is that the list of pixels is in row-major form, if this
      ;; is not the case, all bets are off.
      (loop
	 for pixel in pixels
	 for i upfrom 0
	 do
	   (loop
	      with color = (pixel-color pixel)
	      with color-slots = (slot-names color)
	      for slot in color-slots
	      for j upfrom 0
	      do
		(setf (mem-aref pixel-ptr 'sf-uint-8 (+ j (* 4 i)))
		      (slot-value color slot))))
      (sf-texture-update-from-pixels (texture-pointer tex) pixel-ptr width height)
      (foreign-free pixel-ptr))))

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

(defmethod texture-update-from-render-window ((tex texture) (rw window) (x integer) (y integer))
  (sf-texture-update-from-window (texture-pointer tex)
				 (window-pointer rw)
				 x y))

(defcfun ("sfTexture_isSmooth" sf-texture-is-smooth) sf-bool
  (texture :pointer))

(defmethod texture-smooth :before ((tex texture))
  (setf (slot-value tex 'smooth) (sf-texture-is-smooth (texture-pointer tex))))

(defcfun ("sfTexture_setSmooth" sf-texture-set-smooth) :void
  (texture :pointer)
  (smooth sf-bool))

(defmethod (setf texture-smooth) :after (smooth (tex texture))
  (sf-texture-set-smooth (texture-pointer tex) smooth))

(defcfun ("sfTexture_isRepeated" sf-texture-is-repeated) sf-bool
  (texture :pointer))

(defmethod texture-repeated :before ((tex texture))
  (setf (slot-value tex 'repeated) (sf-texture-is-repeated (texture-pointer tex))))

(defcfun ("sfTexture_setRepeated" sf-texture-set-repeated) :void
  (texture :pointer)
  (repeated sf-bool))

(defmethod (setf texture-repeated) :after (repeated (tex texture))
  (sf-texture-set-repeated (texture-pointer tex) repeated))

;; these two functions give an "undefined alien" warning

(defcfun ("sfTexture_isSrgb" sf-texture-is-srgb) sf-bool
  (texture :pointer))

(defmethod texture-srgb :before ((tex texture))
  (setf (slot-value tex 'srgb) (sf-texture-is-srgb (texture-pointer tex))))

(defcfun ("sfTexture_setSrgb" sf-texture-set-srgb) :void
  (texture :pointer)
  (srgb sf-bool))

(defmethod (setf texture-srgb) :after (srgb (tex texture))
  (sf-texture-set-srgb (texture-pointer tex) srgb))


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

;; undefined alien, probably not implemented in version 2.3

(defcfun ("sfTexture_generateMipmap" sf-texture-generate-mipmap) sf-bool
  (texture :pointer))

(defmethod texture-generate-mipmap ((tex texture))
  (sf-texture-generate-mipmap (texture-pointer tex)))

(defcfun ("sfTexture_getMaximumSize" sf-texture-get-maximum-size) :unsigned-int)

(defun texture-max-size ()
  (sf-texture-get-maximum-size))
