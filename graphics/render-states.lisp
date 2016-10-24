(in-package :sfml)

(defcstruct (sf-render-states :class render-states-type)
  (blend-mode (:struct sf-blend-mode))
  (transform (:struct sf-transform))
  (texture :pointer)
  (shader :pointer))

(defclass render-state ()
  ((blend-mode :initarg :blend-mode :initform nil :accessor render-state-blend-mode)
   (transform :initarg :transform :initform (make-identity) :accessor render-state-transform)
   (texture :initarg :texture :initform (make-texture) :accessor render-state-texture)
   ;; shader not currently implemented
   (shader :initarg :shader :initform (null-pointer) :accessor render-state-shader)))

;; Because of the compound natue of the sf-render-states struct, I couldn't figure
;; out a better way of translating it to foreign memory than by explicitly reading
;; and copying all the values from the class. It seems to be a one-off in the API
;; so I'm ok with that.

(defmethod translate-to-foreign ((rs render-state) (type render-states-type))
  (let* ((ptr (foreign-alloc '(:struct sf-render-states)))
	 (slots (slot-names rs))
	 (slot-types (list '(:struct sf-blend-mode)
			   '(:struct sf-transform)
			   :pointer :pointer)))
    (loop
       for slot in slots
       for slot-type in slot-types
       do
	 (let ((slot-ptr (foreign-slot-pointer ptr '(:struct sf-render-states) slot))
	       (value-to-set (slot-value rs slot)))
	   (typecase value-to-set
	     (blend-mode
	      (loop
		 for value-slot in (slot-names value-to-set)
		 do
		   (setf (foreign-slot-value slot-ptr '(:struct sf-blend-mode) value-slot)
			 (slot-value value-to-set value-slot))))
	     (transform
	      (loop
		 with value-list = (pointer->matrix (transform-pointer value-to-set))
		 for value in value-list
		 for i upto (- (length value-list) 1) 
		 do
		   (setf (mem-aref slot-ptr :float i) value)))
	     (texture
	      (setf (foreign-slot-value ptr '(:struct sf-render-states) 'texture)
	      	    (if (typep (texture-pointer value-to-set) 'sb-sys:system-area-pointer)
	      		(texture-pointer value-to-set)
	      		(null-pointer))))
	     (t
	      (setf (foreign-slot-value ptr '(:struct sf-render-states) 'shader)
		    (null-pointer)))
	     )))
    ptr))
