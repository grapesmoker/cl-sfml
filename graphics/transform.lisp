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

;; (defcstruct (sf-transform :class transform-type)
;;   (matrix :pointer :count 9))

;; (defcfun ("sfTransform_fromMatrix" sf-transform-from-matrix) (:struct sf-transform)
;;   (a00 :float) (a01 :float) (a02 :float)
;;   (a10 :float) (a11 :float) (a12 :float)
;;   (a20 :float) (a21 :float) (a22 :float))

;; (defcfun ("sfTransform_getInverse" sf-transform-get-inverse) (:struct sf-transform)
;;   (transform (:pointer (:struct sf-transform))))

;;=================================
;; CUSTOM GEOMETRY CODE BEGINS HERE
;;=================================


;; General `nice' matrix class that does things like check validity for you before
;; you use the matrix and provides lots of nice functionality 

(defclass matrix ()
  ;; the underlying array representation
  ((matrix-array :initarg :array
		 :accessor matrix-array)))

(defmethod print-object ((m matrix) stream)
  (format stream "<#MATRIX ~A>" (matrix-array m)))

(defmethod m-ref ((m matrix) (i fixnum) (j fixnum))
  "Accessor function for matrices."
  (aref (matrix-array m) i j))

(defmethod update-m-ref ((m matrix) (i fixnum) (j fixnum) (new-val number))
  (setf (aref (matrix-array m) i j) new-val))

(defsetf m-ref update-m-ref)

(defmethod matrix-rows ((m matrix))
  (array-dimension (matrix-array m) 0))

(defmethod matrix-cols ((m matrix))
  (array-dimension (matrix-array m) 1))

  
;; We subclass matrix generally for a few specific purposes. Of course arbitrary matrices
;; can be any size, but matrices specifically for geometric use are either going to be
;; 3x3 or 4x4.

(defclass mat3 (matrix)
  ())

(defclass mat4 (matrix)
  ())

(defun list-of-lists-of-numbers? (thing)
  "Check to see if thing is a list of lists of numbers."
  (if (listp thing)
      (and (every #'listp thing)
	   (every #'(lambda (x)
		      (every #'numberp x))
		  thing))
      nil))

(defun array->list (a)
  "Convert a 2D array into a list."
  (loop
     for i below (array-dimension a 0)
     collect
       (loop
	  for j below (array-dimension a 1)
	  collect (aref a i j))))


(defun make-matrix (rows columns &optional content)
  (cond ((and (not (null content))
	      (list-of-lists-of-numbers? content)
	      (not (every #'(lambda (x) (= columns (length x))) content)))
	 (error :error-text "Content has unequal columns!"))
	((not (and (numberp rows)
		   (numberp columns)
		   (or (null content)
		       (list-of-lists-of-numbers? content))))
	 (error :error-text "Invalid content or rows/columns specified!"))
	((and (numberp rows)
	      (numberp columns)
	      (null content))
	 (setf content (loop
			  for i below rows
			  collect
			    (loop
			       for j below columns
			       collect 0)))
	 (make-instance 'matrix
			:array (make-array `(,rows ,columns)
					   :element-type 'number
					   :initial-contents content)))
	(t
	 (make-instance 'matrix
			:array (make-array `(,rows ,columns)
					   :element-type 'number
					   :initial-contents content)))))


(defun ident-matrix (size)
  (make-matrix size
	       size
	       (loop
		  for i below size
		  collect
		    (loop
		       for j below size
		       collect
			 (if (= i j) 1 0)))))


(defmacro with-valid-matrices ((matrices) &rest body)
  (let ((g1 (gensym))
	(g2 (gensym))
	(valid-flag (gensym)))
  `(progn
     (let ((,valid-flag
	    (loop 
	       for m in ,matrices
	       do
		 (let ((,g1 (matrix-rows m))
		       (,g2 (matrix-cols m)))
		   (loop
		      for i below ,g1
		      do
			(loop
			   for j below ,g2
			   if (not (numberp (m-ref m i j)))
			   do (return nil))))
	       finally (return t))))
       (if (not ,valid-flag)
	   (error :error-text "Invalid matrices!")
	   (progn
	     ,@body))))))

(defmethod m+ ((m1 matrix) (m2 matrix))
  (with-valid-matrices ((list m1 m2))
    (let* ((dim1 (matrix-rows m1))
	   (dim2 (matrix-cols m1))
	   (result (make-matrix dim1 dim2)))
      (print 'foo)
      (loop
	 for i below dim1
	 do
	   (loop
	      for j below dim2
	      do
		(setf (m-ref result i j) (+ (m-ref m1 i j) (m-ref m2 i j)))))
      result)))

(defmethod m* ((m matrix) (s number))
  (let* ((dim1 (matrix-rows m))
	 (dim2 (matrix-cols m))
	 (result (make-matrix dim1 dim2)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (m-ref result i j) (* (m-ref m i j) s))))
    result))

(defmethod transpose-matrix ((m matrix))
    (let* ((dim1 (matrix-rows m))
	   (dim2 (matrix-cols m))
	   (result (make-matrix dim1 dim2)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (m-ref result i j) (m-ref m j i))))
    result))

(defmethod matrix-trace ((m matrix))
  (let* ((dim1 (matrix-rows m))
	 (dim2 (matrix-cols m)))
    (assert (= dim1 dim2))
    (loop
       for i from 0 to (- dim1 1)
       summing (m-ref m i i) into result
       finally (return result))))

(defmethod m* ((m matrix) (v vector2))
  (assert (= (matrix-cols m) (vec-size v)))
  (make-instance 'vector2
		 :x (+ (* (m-ref m 0 0) (vector2-x v))
		       (* (m-ref m 0 1) (vector2-y v)))
		 :y (+ (* (m-ref m 1 0) (vector2-x v))
		       (* (m-ref m 1 1) (vector2-y v)))))

