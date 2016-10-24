(in-package :sfml)

(defcenum sf-sensor-type
  (:sf-sensor-accelerometer 0)
  (:sf-sensor-gyroscope 1)
  (:sf-sensor-magnetometer 2)
  (:sf-sensor-gravity 3)
  (:sf-sensor-user-acceleration 4)
  (:sf-sensor-orientation 5)
  (:sf-sensor-count 6))

(defclass sensor ()
  ((type :initarg :type :initform nil :accessor sensor-type)
   (available :initform nil :reader sensor-available)
   (enabled :initform nil :accessor sensor-enabled)
   (value :initform nil :accessor sensor-value)))

(defun make-sensor (type)
  (make-instance 'sensor :type type))

(defcfun ("sfSensor_isAvailable" sf-sensor-available) sf-bool
  (sensor sf-sensor-type))

(defmethod sensor-available :before ((s sensor))
  (setf (slot-value s 'available) (sf-sensor-available (sensor-type s))))

(defcfun ("sfSensor_setEnabled" sf-sensor-set-enabled) :void
  (sensor sf-sensor-type)
  (enabled sf-bool))

(defmethod (setf sensor-enabled) :after (enabled (s sensor))
  (sf-sensor-set-enabled (sensor-type s) enabled))

(defcfun ("sfSensor_getValue" sf-sensor-get-value) (:struct sf-vector-3f)
  (sensor sf-sensor-type))

(defmethod sensor-value :before ((s sensor))
  (setf (slot-value s 'value) (sf-sensor-get-value (sensor-type s))))


