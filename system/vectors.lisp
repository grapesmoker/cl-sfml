(in-package :sfml)

(defcstruct (sf-vector-2i :class vector-2i-type)
  (x :int)
  (y :int))

(defcstruct (sf-vector-2u :class vector-2u-type)
  (x :unsigned-int)
  (y :unsigned-int))

(defcstruct (sf-vector-2f :class vector-2f-type)
  (x :float)
  (y :float))

;; a root vector class so that we can specialize
;; matrix mult methods on it

(defclass vect ()
  ())

(defclass vector2 (vect)
  ((x :initarg :x :initform nil :accessor vector2-x)
   (y :initarg :y :initform nil :accessor vector2-y)))

(defmethod vec-size ((v vector2))
  2)

(defun make-vector2 (x y &optional (coerce-to 'float))
  (make-instance 'vector2 :x (coerce x coerce-to) :y (coerce y coerce-to)))

(defmethod print-object ((v vector2) stream)
  (format stream "<VECTOR (~A, ~A)>" (vector2-x v) (vector2-y v)))

;;(defmethod translate-from-foreign (p (type vector-2i-type))
;;  (print "converting vector from struct")
;;  (print p)
  ;; (print (mem-ref p '(:struct sf-vector-2i)))
;;  (let ((plist (call-next-method)))
;;    (make-instance 'vector2 :x (getf plist 'x) :y (getf plist :y))))

(defmethod translate-from-foreign (p (type vector-2i-type))
  (copy-from-foreign 'vector2 p '(:struct sf-vector-2i)))

(defmethod translate-into-foreign-memory ((v vector2) (type vector-2i-type) p)
  (copy-to-foreign v p '(:struct sf-vector-2i) '(:int :int)))

(defmethod translate-from-foreign (p (type vector-2u-type))
  (copy-from-foreign 'vector2 p '(:struct sf-vector-2u)))

(defmethod translate-into-foreign-memory ((v vector2) (type vector-2u-type) p)
  (copy-to-foreign v p '(:struct sf-vector-2u) '(:unsigned-int :unsigned-int)))

(defmethod translate-from-foreign (p (type vector-2f-type))
  (copy-from-foreign 'vector2 p '(:struct sf-vector-2f)))

(defmethod translate-into-foreign-memory ((v vector2) (type vector-2f-type) p)
  (copy-to-foreign v p '(:struct sf-vector-2f) '(:float :float)))

;; 3d vectors

(defcstruct (sf-vector-3f :class vector-3f-type)
  (x :float)
  (y :float)
  (z :float))


(defclass vector3 (vect)
  ((x :initarg :x :initform nil :accessor vector3-x)
   (y :initarg :y :initform nil :accessor vector3-y)
   (z :initarg :z :initform nil :accessor vector3-z)))

(defmethod vec-size ((v vector3))
  3)

(defmethod translate-from-foreign (p (type vector-3f-type))
  (copy-from-foreign 'vector2 p '(:struct sf-vector-3f)))

(defmethod translate-into-foreign-memory ((v vector3) (type vector-3f-type) p)
  (copy-to-foreign v p '(:struct sf-vector-3f) '(:float :float :float)))
