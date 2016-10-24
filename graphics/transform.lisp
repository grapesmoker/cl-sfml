(in-package :sfml)

;; For reasons that probably make sense in C but not so much
;; in Lisp, the authors of the CSFML wrapper decided to return
;; the transform as a wrapped array. Although the declarations
;; below *seem* to work, they don't actually result in anything
;; but garbage data, which leads me to suspect I'm not understanding
;; something about the CFFI-appropriate strategy for calling
;; such functions. Therefore, I'm mothballing this bit for the
;; moment, and substituting in my own geometry library which
;; implements all the relevant transformations. This should work,
;; since the point of this is not so much to integrate with
;; the C-based SFML geometry as it is to just do the geometry
;; correctly in the first place.

;; UPDATE: 09/30/2016 - well, fuck, I guess it does work?!

(defcstruct (sf-transform :class transform-type)
  (matrix :pointer :count 9))

(defclass transform ()
  ((pointer :initarg :pointer :initform nil :accessor transform-pointer)
   (matrix :initarg :matrix :initform nil :accessor transform-matrix)
   (opengl-matrix :initform nil :accessor transform-opengl-matrix)))

(defun make-transform (&optional (matrix nil matrix-p))
  (cond (matrix-p
	 (transform-from-matrix (nth 0 matrix) (nth 1 matrix) (nth 2 matrix)
				(nth 3 matrix) (nth 4 matrix) (nth 5 matrix)
				(nth 6 matrix) (nth 7 matrix) (nth 8 matrix)))
	(t (make-identity))))

(defun make-identity ()
  (sf-transform-from-matrix 1s0 0s0 0s0
			    0s0 1s0 0s0
			    0s0 0s0 1s0))

(defmethod print-object ((tr transform) stream)
  (let ((matrix
	 (loop
	    for i upto 8 by 3
	    collect
	      (subseq (transform-matrix tr) i (+ i 3)))))
    (format stream "<TRANSFORM:~%~{~{~4F~^ ~}~^~%~}>" matrix)))

(defun pointer->matrix (tr-pointer &optional (size 9))
  (when (or (= size 9) (= size 16))
    (loop
       for i upto (- size 1)
       collect
	 (mem-aref tr-pointer :float i))))
     

(defmethod translate-from-foreign (p (type transform-type))
  (let ((plist (call-next-method)))
    (make-instance 'transform
		   :pointer (cadr plist)
		   :matrix (pointer->matrix (cadr plist)))))

(defcfun ("sfTransform_fromMatrix" sf-transform-from-matrix) (:struct sf-transform)
  (a00 :float) (a01 :float) (a02 :float)
  (a10 :float) (a11 :float) (a12 :float)
  (a20 :float) (a21 :float) (a22 :float))

(defmethod transform-from-matrix ((a00 number) (a01 number) (a02 number)
				  (a10 number) (a11 number) (a12 number)
				  (a20 number) (a21 number) (a22 number))

  (sf-transform-from-matrix (coerce a00 'float) (coerce a01 'float) (coerce a02 'float)
			    (coerce a10 'float) (coerce a11 'float) (coerce a12 'float)
			    (coerce a20 'float) (coerce a21 'float) (coerce a22 'float)))

(defcfun ("sfTransform_getInverse" sf-transform-get-inverse) (:struct sf-transform)
  (transform (:pointer (:struct sf-transform))))

(defmethod transform-inverse ((tr transform))
  (sf-transform-get-inverse (transform-pointer tr)))

(defcfun ("sfTransform_transformPoint" sf-transform-point) (:struct sf-vector-2f)
  (transform (:pointer (:struct sf-transform)))
  (point (:struct sf-vector-2f)))

(defmethod transform-point ((tr transform) (pt vector2))
  (sf-transform-point (transform-pointer tr) pt))

(defcfun ("sfTransform_getMatrix" sf-transform-get-matrix) :void
  (transform (:pointer (:struct sf-transform)))
  (matrix :pointer))

;; this method should only be used if you're manipulating OpenGL contexts directly

(defmethod transform-opengl-matrix :before ((tr transform))
  (cond ((null (slot-value tr 'opengl-matrix))
	 (setf (slot-value tr 'opengl-matrix)
	       (foreign-alloc :float :count 16))))
  (sf-transform-get-matrix (transform-pointer tr)
			   (slot-value tr 'opengl-matrix))
  nil)

(defcfun ("sfTransform_transformRect" sf-transform-rect) (:struct sf-float-rect)
  (transform (:pointer (:struct sf-transform)))
  (rectange (:struct sf-float-rect)))

(defmethod transform-rect ((tr transform) (r rect))
  (sf-transform-rect (transform-pointer tr) r))

;; note that BOTH matrices are destructively modified in this call!

(defcfun ("sfTransform_combine" sf-transform-combine) :void
  (transform (:pointer (:struct sf-transform)))
  (other (:pointer (:struct sf-transform))))

(defmethod transform-combine ((tr transform) (other transform))
  (sf-transform-combine (transform-pointer tr) (transform-pointer other))
  (setf (transform-matrix tr) (pointer->matrix (transform-pointer tr)))
  (setf (transform-matrix other) (pointer->matrix (transform-pointer other)))
  nil)

(defcfun ("sfTransform_translate" sf-transform-translate) :void
  (transform (:pointer (:struct sf-transform)))
  (x :float)
  (y :float))

(defmethod transform-translate ((tr transform) (x number) (y number))
  (sf-transform-translate (transform-pointer tr) (coerce x 'float) (coerce y 'float))
  (setf (transform-matrix tr)
	(pointer->matrix (transform-pointer tr)))
  nil)

(defcfun ("sfTransform_rotate" sf-transform-rotate) :void
  (transform (:pointer (:struct sf-transform)))
  (angle :float))

(defmethod transform-rotate ((tr transform) (angle number))
  (sf-transform-rotate (transform-pointer tr) (coerce angle 'float))
  (setf (transform-matrix tr)
	(pointer->matrix (transform-pointer tr)))
  nil)

(defcfun ("sfTransform_scale" sf-transform-scale) :void
  (transform (:pointer (:struct sf-transform)))
  (scale-x :float)
  (scale-y :float))

(defmethod transform-scale ((tr transform) (scale-x number) (scale-y number))
  (sf-transform-scale (transform-pointer tr) (coerce scale-x 'float) (coerce scale-y 'float))
  (setf (transform-matrix tr)
	(pointer->matrix (transform-pointer tr)))
  nil)

(defcfun ("sfTransform_rotateWithCenter" sf-transform-rotate-with-center) :void
  (transform (:pointer (:struct sf-transform)))
  (angle :float)
  (center-x :float)
  (center-y :float))

(defmethod transform-rotate-with-center ((tr transform) (angle number)
					 (center-x number) (center-y number))
  (sf-transform-rotate-with-center (transform-pointer tr) (coerce angle 'float)
				   (coerce center-x 'float) (coerce center-y 'float))
  (setf (transform-matrix tr)
	(pointer->matrix (transform-pointer tr)))
  nil)


(defcfun ("sfTransform_scaleWithCenter" sf-transform-scale-with-center) :void
  (transform (:pointer (:struct sf-transform)))
  (scale-x :float)
  (scale-y :float)
  (center-x :float)
  (center-y :float))

(defmethod transform-scale-with-center ((tr transform) (scale-x number) (scale-y number)
					 (center-x number) (center-y number))
  (sf-transform-scale-with-center (transform-pointer tr)
				  (coerce scale-x 'float) (coerce scale-y 'float)
				  (coerce center-x 'float) (coerce center-y 'float))
  (setf (transform-matrix tr)
	(pointer->matrix (transform-pointer tr)))
  nil)
