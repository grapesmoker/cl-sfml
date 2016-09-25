(in-package :sfml)

(defcenum sf-mouse-button
  (:sf-mouse-left 0)
  (:sf-mouse-right 1)
  (:sf-mouse-middle 2)
  (:sf-mouse-x-button-1 3)
  (:sf-mouse-x-button-2 4)
  (:sf-mouse-button-count 5))

(defcenum sf-mouse-wheel
  (:sf-mouse-vertical-wheel 0)
  (:sf-mouse-horizontal-wheel 1))

(defcfun ("sfMouse_isButtonPressed" sf-mouse-is-button-pressed) sf-bool
  (button sf-mouse-button))

(defcfun ("sfMouse_getPosition" sf-mouse-get-position) (:struct sf-vector-2i)
  (window :pointer))

(defcfun ("sfMouse_setPosition" sf-mouse-set-position) :void
  (position (:struct sf-vector-2i))
  (window :pointer))
  
(defclass mouse ()
  ((x :initarg :x :initform nil :accessor mouse-x)
   (y :initarg :y :initform nil :accessor mouse-y)))

(defmethod print-object ((m mouse) stream)
  (format stream "<MOUSE [X: ~D, Y:~D]>" (mouse-x m) (mouse-y m)))
