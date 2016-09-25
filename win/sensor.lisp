(in-package :sfml)

(defcenum sf-sensor-type
  (:sf-sensor-accelerometer 0)
  (:sf-sensor-gyroscope 1)
  (:sf-sensor-magnetometer 2)
  (:sf-sensor-gravity 3)
  (:sf-sensor-user-acceleration 4)
  (:sf-sensor-orientation 5)
  (:sf-sensor-count 6))

(defcfun ("sfSensor_isAvailable" sf-sensor-type) sf-bool
  (sensor sf-sensor-type))

(defcfun ("sfSensor_setEnabled" sf-sensor-set-enabled) :void
  (sensor sf-sensor-type)
  (enabled sf-bool))

(defcfun ("sfSensor_getValue" sf-sensor-get-value) (:struct sf-vector-3f)
  (sensor sf-sensor-type))
