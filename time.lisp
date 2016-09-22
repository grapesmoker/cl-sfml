(in-package :sfml)

(defcstruct sf-time
  (microseconds :long-long))

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(use-foreign-library libcsfml-system)

(defcfun ("sfTime_asSeconds" sf-time-as-seconds) :float
  (time (:struct sf-time)))

(defcfun ("sfSeconds" sf-seconds) (:struct sf-time)
  (amount :float))

(defcfun ("gettimeofday" get-time-of-day) :int
  (tp :pointer)
  (tzp :pointer))

(defcfun ("sqrtf" square-root) :float
  (num :float))
