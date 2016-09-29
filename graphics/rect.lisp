(in-package :sfml)

(defcstruct (sf-float-rect :class float-rect-type)
  (left :float)
  (top :float)
  (width :float)
  (height :float))

(defcstruct (sf-int-rect :class int-rect-type)
  (left :int)
  (top :int)
  (width :int)
  (height :int))

(defclass rect ()
  ((left :initarg :left :initform nil :accessor rect-left)
   (top :initarg :top :initform nil :accessor rect-top)
   (width :initarg :width :initform nil :accessor rect-width)
   (height :initarg :height :initform nil :accessor rect-height)))

(defmethod translate-from-foreign (p (type float-rect-type))
 (copy-from-foreign 'rect p '(:struct sf-float-rect)))

(defmethod translate-into-foreign-memory ((r rect) (type float-rect-type) p)
  (copy-to-foreign r p '(:struct sf-float-rect)
		   '(:float :float :float)))

(defmethod translate-from-foreign (p (type int-rect-type))
 (copy-from-foreign 'rect p '(:struct sf-int-rect)))

(defmethod translate-into-foreign-memory ((r rect) (type int-rect-type) p)
  (copy-to-foreign r p '(:struct sf-int-rect)
		   '(:int :int :int)))

(defcfun ("sfFloatRect_contains" sf-float-rect-contains) sf-bool
  (rect (:pointer (:struct sf-float-rect)))
  (x :float)
  (y :float))

(defcfun ("sfIntRect_contains" sf-int-rect-contains) sf-bool
  (rect (:pointer (:struct sf-int-rect)))
  (x :int)
  (y :int))

(defcfun ("sfFloatRect_intersects" sf-float-rect-intersects) sf-bool
  (rect1 (:pointer (:struct sf-float-rect)))
  (rect2 (:pointer (:struct sf-float-rect)))
  (intersection (:pointer (:struct sf-float-rect))))

(defcfun ("sfIntRect_intersects" sf-float-int-intersects) sf-bool
  (rect1 (:pointer (:struct sf-int-rect)))
  (rect2 (:pointer (:struct sf-int-rect)))
  (intersection (:pointer (:struct sf-int-rect))))

