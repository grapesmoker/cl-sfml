(in-package :sfml)

(defclass clock ()
  ((pointer :initarg :pointer :initform nil :accessor clock-pointer)
   (elapsed-time-since-restart :initform 0 :accessor clock-elapsed-time-since-restart)
   (elapsed-time :initform 0 :accessor clock-elapsed-time)))

(defcfun ("sfClock_create" sf-clock-create) :pointer)

(defun make-clock ()
  (make-instance 'clock :pointer (sf-clock-create)))

(defcfun ("sfClock_copy" sf-clock-copy) :pointer
  (clock :pointer))

(defcfun ("sfClock_destroy" sf-clock-destroy) :void
  (clock :pointer))

(defmethod clock-destroy ((c clock))
  (sf-clock-destroy (clock-pointer c)))

(defcfun ("sfClock_getElapsedTime" sf-clock-get-elapsed-time) (:struct sf-time)
  (clock :pointer))

(defmethod clock-elapsed-time :before ((c clock))
  (setf (slot-value c 'elapsed-time) (sf-clock-get-elapsed-time (clock-pointer c))))

(defcfun ("sfClock_restart" sf-clock-restart) (:struct sf-time)
  (clock :pointer))

(defmethod clock-restart ((c clock))
  (let ((elapsed-time (sf-clock-restart (clock-pointer c))))
    (setf (clock-elapsed-time-since-restart c) elapsed-time)))

(defmethod clock-time-as-seconds ((c clock))
  (sf-time-as-seconds (clock-elapsed-time c)))

(defcfun ("sfSleep" sf-sleep) :void
  (duration (:struct sf-time)))
