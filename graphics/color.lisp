(in-package :sfml)

(defcstruct (sf-color :class color-type)
  (r sf-uint-8)
  (g sf-uint-8)
  (b sf-uint-8)
  (a sf-uint-8))

(defclass color ()
  ((r :initarg :red :initform 0 :accessor color-red)
   (b :initarg :blue :initform 0 :accessor color-blue)
   (g :initarg :green :initform 0 :accessor color-green)
   (a :initarg :alpha :initform 0 :accessor color-alpha)))

(defun make-color (&optional (r 0) (g 0) (b 0) (a 0))
  (make-instance 'color
		 :red r
		 :green g
		 :blue b
		 :alpha a))

(defmethod print-object ((c color) stream)
  (format stream "<COLOR: RGBA (~D,~D,~D,~D)>"
	  (color-red c)
	  (color-green c)
	  (color-blue c)
	  (color-alpha c)))

(defmethod translate-from-foreign (p (type color-type))
 (copy-from-foreign 'color p '(:struct sf-color)))

(defmethod translate-into-foreign-memory ((c color) (type color-type) p)
    (copy-to-foreign c p '(:struct sf-color) 
		     '(sf-uint-8 sf-uint-8 sf-uint-8)))

(defcfun ("sfColor_fromRGB" sf-color-from-rgb) (:struct sf-color)
  (red sf-uint-8)
  (green sf-uint-8)
  (blue sf-uint-8))

(defcfun ("sfColor_fromRGBA" sf-color-from-rgba) (:struct sf-color)
  (red sf-uint-8)
  (green sf-uint-8)
  (blue sf-uint-8)
  (alpha sf-uint-8))

(defcfun ("sfColor_fromInteger" sf-color-from-integer) (:struct sf-color)
  (color sf-uint-32))

(defcfun ("sfColor_toInteger" sf-color-to-integer) sf-uint-32
  (color (:struct sf-color)))

(defcfun ("sfColor_add" sf-color-add) (:struct sf-color)
  (color1 (:struct sf-color))
  (color2 (:struct sf-color)))

(defcfun ("sfColor_subtract" sf-color-subtract) (:struct sf-color)
  (color1 (:struct sf-color))
  (color2 (:struct sf-color)))

(defcfun ("sfColor_modulate" sf-color-modulate) (:struct sf-color)
  (color1 (:struct sf-color))
  (color2 (:struct sf-color)))

