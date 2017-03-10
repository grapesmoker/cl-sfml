(in-package :sfml)

(defcstruct sf-time
  (microseconds :long-long))

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(use-foreign-library libcsfml-system)

(defcfun ("sfSeconds" sf-seconds) (:struct sf-time)
  (amount :float))

(defcfun ("sfMilliseconds" sf-milliseconds) (:struct sf-time)
  (amount :float))

(defcfun ("sfMicroseconds" sf-microseconds) (:struct sf-time)
  (amount :float))

(defcfun ("sfTime_asSeconds" sf-time-as-seconds) :float
  (time (:struct sf-time)))

(defcfun ("sfTime_asMilliseconds" sf-time-as-milliseconds) :float
  (time (:struct sf-time)))

(defcfun ("sfTime_asMicroseconds" sf-time-as-microseconds) :float
  (time (:struct sf-time)))

