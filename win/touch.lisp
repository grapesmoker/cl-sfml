(in-package :sfml)

(defcfun ("sfTouch_isDown" sf-touch-is-down) sf-bool
  (finger :unsigned-int))

(defcfun ("sfTouch_getPosition" sf-touch-get-position) (:struct sf-vector-2i)
  (finger :unsigned-int)
  (relative-to :pointer))
