(in-package :sfml)

(defcstruct sf-joystick-identification
  (name :string)
  (vendor-id :unsigned-int)
  (product-id :unsigned-int))

(defcenum sf-joystick-global
  (:sf-joystick-count 8)
  (:sf-joystick-button-count 32)
  (:sf-joystick-axis-count 8))

(defcenum sf-joystick-axis
  (:sf-joystick-x 0)
  (:sf-joystick-y 1)
  (:sf-joystick-z 2)
  (:sf-joystick-r 3)
  (:sf-joystick-u 4)
  (:sf-joystick-v 5)
  (:sf-joystick-pov-x 6)
  (:sf-joystick-pov-y 7))
  

(defcfun ("sfJoystick_isConnected" sf-joystick-is-connected) sf-bool
  (joystick :unsigned-int))

(defcfun ("sfJoystick_getButtonCount" sf-joystick-get-button-count) :unsigned-int
  (joystick :unsigned-int))

(defcfun ("sfJoystick_hasAxis" sf-joystick-has-access) sf-bool
  (joystick :unsigned-int)
  (axis sf-joystick-axis))

(defcfun ("sfJoystick_isButtonPressed" sf-joystick-is-button-pressed) sf-bool
  (joystick :unsigned-int)
  (button :unsigned-int))

(defcfun ("sfJoystick_getAxisPosition" sf-joystick-get-axis-position) :float
  (joystick :unsigned-int)
  (axis sf-joystick-axis))

(defcfun ("sfJoystick_getIdentification" sf-joystick-get-identification)
    (:struct sf-joystick-identification)
  (joystick :unsigned-int))

(defcfun ("sfJoystick_update" sf-joystick-update) :void)
