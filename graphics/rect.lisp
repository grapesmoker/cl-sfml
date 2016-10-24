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
  ((left :type number :initarg :left :initform nil :accessor rect-left)
   (top :initarg :top :initform nil :accessor rect-top)
   (width :initarg :width :initform nil :accessor rect-width)
   (height :initarg :height :initform nil :accessor rect-height)))

(defmethod print-object ((r rect) stream)
  (format stream "<RECT (~A, ~A) x (~A, ~A)>"
	  (rect-left r)
	  (rect-top r)
	  (rect-width r)
	  (rect-height r)))

(defmethod translate-from-foreign (p (type float-rect-type))
 (copy-from-foreign 'rect p '(:struct sf-float-rect)))

(defmethod translate-into-foreign-memory ((r rect) (type float-rect-type) p)
  (copy-to-foreign r p '(:struct sf-float-rect)
		   '(:float :float :float :float)))

(defmethod translate-from-foreign (p (type int-rect-type))
 (copy-from-foreign 'rect p '(:struct sf-int-rect)))

(defmethod translate-into-foreign-memory ((r rect) (type int-rect-type) p)
  (copy-to-foreign r p '(:struct sf-int-rect)
		   '(:int :int :int :int)))

(defun make-rect (&key (left 0) (top 0) (width 0) (height 0) (coerce-to 'float))
  (make-instance 'rect
		 :left (coerce left coerce-to)
		 :top (coerce top coerce-to)
		 :width (coerce width coerce-to)
		 :height (coerce height coerce-to)))

(defmethod rect-type ((r rect))
  (let* ((slots (slot-names r))
	 (values (loop for slot in slots collect (slot-value r slot))))
    (cond ((every #'floatp values)
	   :float)
	  ((every #'integerp values)
	   :int))))
	  
(defcfun ("sfFloatRect_contains" sf-float-rect-contains) sf-bool
  (rect (:pointer (:struct sf-float-rect)))
  (x :float)
  (y :float))

(defcfun ("sfIntRect_contains" sf-int-rect-contains) sf-bool
  (rect (:pointer (:struct sf-int-rect)))
  (x :int)
  (y :int))

(defmethod rect->pointer ((r rect) foreign-type)
  (let* ((ptr (foreign-alloc foreign-type))
	 (type (rect-type r))
	 (values
	  (case type
	    (:float (mapcar #'(lambda (x) (coerce x 'float))
			    (list (rect-left r) (rect-top r) (rect-width r) (rect-height r))))
	    (:int (mapcar #'(lambda (x) (coerce x 'integer))
			  (list (rect-left r) (rect-top r) (rect-width r) (rect-height r)))))))
    (loop
       for v in values
       for i upfrom 0
       do
	 (setf (mem-aref ptr type i) v))
    ptr))

(defmethod rect-contains ((r rect) (x number) (y number))
  (case (rect-type r)
    (:float
     (let* ((ptr (rect->pointer r '(:struct sf-float-rect)))
	    (contains
	     (sf-float-rect-contains ptr (coerce x 'float) (coerce y 'float))))
       (foreign-free ptr)
       contains))
    (:int
     (let* ((ptr (rect->pointer r '(:struct sf-int-rect)))
	    (contains
	     (sf-int-rect-contains ptr (coerce x 'integer) (coerce y 'integer))))
       (foreign-free ptr)
       contains))))
    
(defcfun ("sfFloatRect_intersects" sf-float-rect-intersects) sf-bool
  (rect1 (:pointer (:struct sf-float-rect)))
  (rect2 (:pointer (:struct sf-float-rect)))
  (intersection (:pointer (:struct sf-float-rect))))

(defcfun ("sfIntRect_intersects" sf-int-rect-intersects) sf-bool
  (rect1 (:pointer (:struct sf-int-rect)))
  (rect2 (:pointer (:struct sf-int-rect)))
  (intersection (:pointer (:struct sf-int-rect))))


(defmethod rect-intersection ((r1 rect) (r2 rect))
  (let* ((type (cond ((and (eq :float (rect-type r1))
			   (eq :float (rect-type r2)))
		      :float)
		     ((and (eq :int (rect-type r1))
			   (eq :int (rect-type r2)))
		      :int)
		     (t (error :error-message "Rectangle type mismatch!"))))
	 (result-ptr
	  (case type
	    (:float (foreign-alloc '(:struct sf-float-rect)))
	    (:int (foreign-alloc '(:struct sf-int-rect)))))
	 (r1-ptr
	  (case type
	    (:float (rect->pointer r1 '(:struct sf-float-rect)))
	    (:int (rect->pointer r1 '(:struct sf-int-rect)))))
	 (r2-ptr
	  (case type
	    (:float (rect->pointer r2 '(:struct sf-float-rect)))
	    (:int (rect->pointer r2 '(:struct sf-int-rect))))))
    (case type
      (:float
       (let* ((does-intersect
	       (sf-float-rect-intersects r1-ptr r2-ptr result-ptr))
	      (result-rect
	       (convert-from-foreign result-ptr '(:struct sf-float-rect))))
	 (foreign-free result-ptr)
	 (values does-intersect result-rect)))
      (:int
       (let* ((does-intersect
	       (sf-int-rect-intersects r1-ptr r2-ptr result-ptr))
	      (result-rect
	       (convert-from-foreign result-ptr '(:struct sf-int-rect))))
	 (foreign-free result-ptr)
	 (values does-intersect result-rect))))))
