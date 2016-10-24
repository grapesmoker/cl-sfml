(in-package :sfml)

;; SFML implements shapes in a really bizarre way. It asks for you
;; to pass it a callback which it can call upon initialization
;; to get the number of points, followed by a callback which can
;; get a specific point, followed by the actual data of the points.
;; So for now I'm not going to implement the interface to drawing
;; arbitrary shapes, which are likely of limited utility anyway.

;; Still create the top-level class for the shape hierarchy though.

;; Override the accessors for the scale, position, rotation, and origin
;; for semantic convenience.

(defclass shape (entity)
  ((pointer :initarg :pointer :initform nil :accessor shape-pointer)
   (changed-slots :initform '() :accessor shape-changed-slots)
   ;; type should be one of :convex-shape, :rectangle, or :circle
   (type :initarg :type :initform nil :accessor shape-type)
   ;; origin should store a vector
   (origin :accessor shape-origin)
   ;; position should store a vector
   (position :accessor shape-position)
   (rotation :accessor shape-rotation)
   (scale :accessor shape-scale)
   (size :accessor shape-size)
   (texture :accessor shape-texture)
   (texture-rect :accessor shape-texture-rect)
   (point-count :accessor shape-point-count)
   ;; bounding boxes are rect classes
   (local-bbox :reader shape-local-bbox)
   (global-bbox :reader shape-global-bbox)
   ;; this should store a color class
   (fill-color :initarg :fill-color
	       :initform (make-color)
	       :accessor shape-fill-color)
   (outline-color :initarg :outline-color
		  :initform (make-color)
		  :accessor shape-outline-color)
   (outline-thickness :initarg :outline-thickness
		      :initform nil
		      :accessor shape-outline-thickness)))
