(in-package :sfml)

(defcstruct (sf-vertex :class vertex-type)
  (position (:struct sf-vector-2f))
  (color (:struct sf-color))
  (tex-coords (:struct sf-vector-2f)))

(defclass vertex ()
  ((position :initarg :position :initform nil :accessor vertex-position)
   (color :initarg :color :initform nil :accessor vertex-color)
   (tex-coords :initarg :tex-coords :initform nil :accessor vertex-tex-coords)))

;; not sure how to handle aggregate types right now

;; (defmethod translate-from-foreign (p (type vertex-type))
;;   (copy-from-foreign 'key-event p '(:struct sf-vertex)))

;; (defmethod translate-into-foreign-memory ((v vertex) (type vertex-type) p)
;;   (copy-to-foreign v p '(:struct sf-vertex)
;; 		   '((:struct sf-vector-2f)
;; 		     (:struct sf-color)
;; 		     (:struct sf-vector-2f))))
