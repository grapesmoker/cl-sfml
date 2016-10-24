(in-package :sfml)

(defclass image ()
  ((pointer :initarg :pointer :initform nil :accessor image-pointer)
   (pixels-pointer :initform nil :accessor image-pixels-pointer)
   (width :initarg :width :initform 0 :accessor image-width)
   (height :initarg :height :initform 0 :accessor image-height)
   (size :reader image-size)
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
		     (pointer nil pointer-p)
		     (pixels nil pixels-p))
  
  (let ((im (make-instance 'image
			   :width width
			   :height height
			   :color color
			   :filename filename)))
    (cond (pointer-p
	   (setf (image-pointer im) pointer))
	  (pixels-p
	   (with-foreign-pointer (pixel-ptr (length pixels))
	     (loop
		for pixel in pixels
		for i upfrom 0
		do
		  (loop
		     for slot in '(r g b a)
		     for j upfrom 0
		     with color = (pixel-color pixel)
		     do
		       (setf (mem-aref pixel-ptr 'sf-uint-8 (+ (* 4 i) j)) (slot-value color slot))))
	     (sf-image-create-from-pixels width height pixel-ptr)))
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

(defcfun ("sfImage_createFromPixels" sf-image-create-from-pixels) :pointer
  (width :unsigned-int)
  (height :unsigned-int)
  (pixels :pointer))

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

(defmethod image-save-to-file ((im image) (filename string))
  (with-foreign-string (fn filename)
    (sf-image-save-to-file (image-pointer im) fn)))

(defcfun ("sfImage_getSize" sf-image-get-size) (:struct sf-vector-2u)
  (image :pointer))

(defmethod image-size :before ((im image))
  (setf (slot-value im 'size) (sf-image-get-size (image-pointer im))))

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

(defmethod image-copy-image ((im image) (src image) (dest-x integer) (dest-y integer)
			     (source-rect rect) apply-alpha)
  (sf-image-copy-image (image-pointer im)
		       (image-pointer src)
		       dest-x dest-y source-rect apply-alpha))

(defclass pixel ()
  ((x :initarg :x :initform nil :accessor pixel-x)
   (y :initarg :y :initform nil :accessor pixel-y)
   (color :initarg :color :initform nil :accessor pixel-color)))

(defun make-pixel (&optional x y (color (make-color)))
  (make-instance 'pixel :x x :y y :color color))

(defmethod print-object ((p pixel) stream)
  (format stream "<PIXEL: [(~D, ~D), ~A]>" (pixel-x p) (pixel-y p) (pixel-color p)))

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
  (make-instance 'pixel :x x :y y :color (sf-image-get-pixel (image-pointer im) x y)))

;; pointer returned is a pointer to sf-uint-8, i.e. unsigned char

(defcfun ("sfImage_getPixelsPtr" sf-image-get-pixels-ptr) :pointer
  (image :pointer))

(defmethod image-pixels-pointer :before ((im image))
  (setf (slot-value im 'pixels-pointer) (sf-image-get-pixels-ptr (image-pointer im))))

(defcfun ("sfImage_flipHorizontally" sf-image-flip-horizontally) :void
  (image :pointer))

(defmethod image-flip-h ((im image))
  (sf-image-flip-horizontally (image-pointer im))
  nil)

(defcfun ("sfImage_flipVertically" sf-image-flip-vertically) :void
  (image :pointer))

(defmethod image-flip-v ((im image))
  (sf-image-flip-vertically (image-pointer im)))

