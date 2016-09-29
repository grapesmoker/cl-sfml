(in-package :sfml)

(defclass image ()
  ((pointer :initarg :pointer :initform nil :accessor image-pointer)
   (width :initarg :width :initform 0 :accessor image-width)
   (height :initarg :height :initform 0 :accessor image-height)
   (color :initarg :color :initform (make-color) :accessor image-color)
   (filename :initarg :filename :initform "" :accessor image-filename)))

(defcfun ("sfImage_create" sf-image-create) :pointer
  (width :unsigned-int)
  (height :unsigned-int))

(defcfun ("sfImage_createFromColor" sf-image-create-from-color) :pointer
  (width :unsigned-int)
  (height :unsigned-int)
  (color (:struct sf-color)))

;; (defmethod initialize-instance :after ((im image) &key)
;;   (setf (image-pointer im) (sf-image-create-from-color (image-width im)
;; 						       (image-height im)
;; 						       (image-color im))))

(defcfun ("sfImage_createFromFile" sf-image-create-from-file) :pointer
  (filename :string))

(defun make-image (&key (width 0) (height 0)
		     (color (make-color) color-p)
		     (filename "" filename-p)
		     (pointer nil pointer-p))
  
  (let ((im (make-instance 'image
			   :width width
			   :height height
			   :color color
			   :filename filename)))
    (cond (pointer-p
	   (setf (image-pointer im) pointer))
	  (filename-p
	   (with-foreign-string (fn filename)
	     (setf (image-pointer im) (sf-image-create-from-file fn))))
	  (color-p
	   (setf (image-pointer im) (sf-image-create-from-color (image-width im)
								(image-height im)
								(image-color im))))
	  (t
	   (setf (image-pointer im) (sf-image-create (image-width im)
						     (image-height im)))))
    im))
			   

;; Not sure how useful this is because you'd have to translate
;; the data into a foreign-alloc'ed pointer first, but hey, for completeness.

(defcfun ("sfImage_createFromMemory" sf-image-create-from-memory) :pointer
  (data :pointer)
  (size :unsigned-int))

;; Not implementing the sfImage_createFromStream because there doesn't
;; seem much point in replicating this functionality on the Lisp side.

(defcfun ("sfImage_copy" sf-image-copy) :pointer
  (image :pointer))

(defmethod image-copy ((im image))
  (let ((new-pointer (sf-image-copy (image-pointer im))))
    (make-image :pointer new-pointer)))

(defcfun ("sfImage_destroy" sf-image-destroy) :void
  (image :pointer))

(defmethod image-destroy ((im image))
  (sf-image-destroy (image-pointer im)))

(defcfun ("sfImage_saveToFile" sf-image-save-to-file) sf-bool
  (image :pointer)
  (filename :string))

(defcfun ("sfImage_getSize" sf-image-get-size) (:struct sf-vector-2u)
  (image :pointer))

(defcfun ("sfImage_createMaskFromColor" sf-image-create-mask-from-color) :void
  (image :pointer)
  (color (:struct sf-color))
  (alpha sf-uint-8))

(defmethod image-mask-from-color ((im image) (c color) alpha)
  (sf-image-create-mask-from-color (image-pointer im) c alpha))

(defcfun ("sfImage_copyImage" sf-image-copy-image) :void
  (image :pointer)
  (source :pointer)
  (dest-x :unsigned-int)
  (dest-y :unsigned-int)
  (source-rect (:struct sf-int-rect))
  (apply-alpha sf-bool))

(defcfun ("sfImage_setPixel" sf-image-set-pixel) :void
  (image :pointer)
  (x :unsigned-int)
  (y :unsigned-int)
  (color (:struct sf-color)))

(defmethod image-set-pixel ((im image) x y (c color))
  (sf-image-set-pixel (image-pointer im) x y c))

(defcfun ("sfImage_getPixel" sf-image-get-pixel) (:struct sf-color)
  (image :pointer)
  (x :unsigned-int)
  (y :unsigned-int))

(defmethod image-get-pixel ((im image) (x integer) (y integer))
  (sf-image-get-pixel (image-pointer im) x y))

;; pointer returned is a pointer to sf-uint-8, i.e. unsigned char

(defcfun ("sfImage_getPixelsPtr" sf-image-get-pixels-ptr) :pointer
  (image :pointer))

(defcfun ("sfImage_flipHorizontally" sf-image-flip-horizontally) :void
  (image :pointer))

(defcfun ("sfImage_flipVertically" sf-image-flip-vertically) :void
  (image :pointer))
