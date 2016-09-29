(in-package :sfml)

;; The entity class is actually going to be the root class for
;; all the different drawable entities that can exist in SFML.
;; Rather than calling into SFML's geometry routines, we can
;; just do the geometry manipulation on the Lisp side. This might
;; be non-ideal from the standpoint of speed, but Lisp can be
;; optimized pretty nicely and the actual math isn't terribly
;; complex.

(defclass entity ()
  ((origin :initarg :origin :initform nil :accessor entity-origin)
   ;; position should store a vector
   (position :initarg :position
	     :initform (make-vector2 0 0)
	     :accessor entity-position)
   (rotation :initarg :rotation :initform 0 :accessor entity-rotation)
   (scale :initarg :scale :initform 1 :accessor entity-scale)
   (size :initarg :size
	 :initform (make-vector2 1 1)
	 :accessor entity-size)
   (texture :initarg :texture :initform nil :accessor entity-texture)
   (local-bbox :initarg :local-bbox :initform nil :accessor entity-local-bbox)
   (global-bbox :initarg :global-bbox :initform nil :accessor entity-global-bbox)))

(defgeneric entity-move (entity offset))
(defgeneric entity-rotate (entity angle))
(defgeneric entity-do-scale (entity scale))
