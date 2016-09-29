(in-package :sfml)

(defcstruct (sf-glyph :class glyph-type)
  (advance :float)
  (bounds (:struct sf-float-rect))
  (texture-rect (:struct sf-int-rect)))

(defclass glyph ()
  ((advance :initarg :advance :initform 0 :accessor glyph-advance)
   (bounds :initarg :bounds :initform nil :accessor glyph-bounds)
   (texture-rect :initarg :texture-rect :initform nil :accessor glyph-texture-rect)))

(defmethod translate-from-foreign (p (type glyph-type))
 (copy-from-foreign 'glyph p '(:struct sf-glyph)))

(defmethod translate-into-foreign-memory ((g glyph) (type glyph-type) p)
    (copy-to-foreign g p '(:struct sf-glyph) 
		     '(:float (:struct sf-float-rect) (:struct sf-int-rect))))
