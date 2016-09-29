(in-package :sfml)

(defclass view (entity)
  ((pointer :initarg :pointer :initform nil :accessor view-pointer)
   (center :initarg :center :initform nil :accessor view-center)
   (viewport :initarg :viewport :initform nil :accessor view-viewport)))

(defcfun ("sfView_create" sf-view-create) :pointer)

(defcfun ("sfView_createFromRect" sf-view-create-from-rect) :pointer
  (rectangle (:struct sf-float-rect)))

(defun make-view (&optional (rectangle nil rectangle-p))
  (cond (rectangle-p
	 (make-instance 'view
			:pointer (sf-view-create-from-rect rectangle)))
	(t
	 (make-instance 'view
			:pointer (sf-view-create)))))

(defcfun ("sfView_copy" sf-view-copy) :pointer
  (view :pointer))

(defmethod view-copy ((v view))
  (make-instance 'view
		 :pointer (sf-view-copy (view-pointer v))))

(defcfun ("sfView_destroy" sf-view-destroy) :void
  (view :pointer))

(defmethod view-destroy ((v view))
  (sf-view-destroy (view-pointer v)))

(defcfun ("sfView_getCenter" sf-view-get-center) (:struct sf-vector-2f)
  (view :pointer))

(defmethod view-center :before ((v view))
  (setf (slot-value v 'center) (sf-view-get-center (view-pointer v))))

(defcfun ("sfView_setCenter" sf-view-set-center) :void
  (view :pointer)
  (center (:struct sf-vector-2f)))

(defmethod (setf view-center) :after ((center vect) (v view))
  (sf-view-set-center (view-pointer v) center))

(defcfun ("sfView_getSize" sf-view-get-size) (:struct sf-vector-2f)
  (view :pointer))

(defmethod entity-size :before ((v view))
  (setf (slot-value v 'size) (sf-view-get-size (view-pointer v))))

(defcfun ("sfView_setSize" sf-view-set-size) :void
  (view :pointer)
  (size (:struct sf-vector-2f)))

(defmethod (setf entity-size) :after ((size vect) (v view))
  (sf-view-set-size (view-pointer v) size))

(defcfun ("sfView_getRotation" sf-view-get-rotation) :float
  (view :pointer))

(defmethod entity-rotation :before ((v view))
  (setf (slot-value v 'rotation) (sf-view-get-rotation (view-pointer v))))

(defcfun ("sfView_setRotation" sf-view-set-rotation) :void
  (view :pointer)
  (angle :float))

(defmethod (setf entity-rotation) :after ((angle number) (v view))
  (sf-view-set-rotation (view-pointer v) (coerce angle 'single-float)))

(defcfun ("sfView_getViewport" sf-view-get-viewport) (:struct sf-vector-2f)
  (view :pointer))

(defmethod view-viewport :before ((v view))
  (setf (slot-value v 'viewport) (sf-view-get-viewport v)))

(defcfun ("sfView_setViewport" sf-view-set-viewport) :void
  (view :pointer)
  (viewport (:struct sf-float-rect)))

(defmethod (setf view-viewport) :after ((viewport rect) (v view))
  (sf-view-set-viewport (view-pointer v) viewport))

(defcfun ("sfView_reset" sf-view-reset) :void
  (view :pointer)
  (rectangle (:struct sf-float-rect)))

(defmethod view-reset ((v view) (r rect))
  (sf-view-reset (view-pointer v) r))

(defcfun ("sfView_move" sf-view-move) :void
  (view :pointer)
  (offset (:struct sf-vector-2f)))

(defmethod entity-move ((v view) (offset rect))
  (sf-view-move (view-pointer v) offset))

(defcfun ("sfView_rotate" sf-view-rotate) :void
  (view :pointer)
  (angle :float))

(defmethod entity-rotate ((v view) (angle number))
  (sf-view-rotate (view-pointer v) (coerce angle 'float)))

(defcfun ("sfView_zoom" sf-view-zoom) :void
  (view :pointer)
  (factor :float))

(defmethod view-zoom ((v view) (factor number))
  (sf-view-zoom (view-pointer v) (coerce factor 'float)))
