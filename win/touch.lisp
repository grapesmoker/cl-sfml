(in-package :sfml)

(defclass touch ()
  ((down :initform nil :reader touch-down)
   (position :initform nil :reader touch-position)))

(defcfun ("sfTouch_isDown" sf-touch-is-down) sf-bool
  (finger :unsigned-int))

(defmethod touch-finger-down ((tt touch) (finger integer))
  (let ((finger-is-down (sf-touch-is-down finger)))
    (setf (slot-value tt 'down) finger-is-down)
    finger-is-down))

(defcfun ("sfTouch_getPosition" sf-touch-get-position) (:struct sf-vector-2i)
  (finger :unsigned-int)
  (relative-to :pointer))
