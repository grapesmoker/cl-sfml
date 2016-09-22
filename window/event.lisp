(in-package :sfml)

(defcenum sf-event-type
  (:sf-evt-closed 0)
  (:sf-evt-resized 1)
  (:sf-evt-lost-focus 2)
  (:sf-evt-text-entered 3)
  (:sf-evt-key-pressed 4)
  (:sf-evt-key-released 5)
  (:sf-evt-mouse-wheel-moved 6)
  (:sf-evt-mouse-wheel-scrolled 7)
  (:sf-evt-mouse-button-pressed 8)
  (:sf-evt-mouse-button-released 9)
  (:sf-evt-mouse-moved 10)
  (:sf-evt-mouse-entered 11)
  (:sf-evt-mouse-left 12)
  (:sf-evt-joystick-button-pressed 13)
  (:sf-evt-joystick-button-released 14)
  (:sf-evt-joystick-moved 15)
  (:sf-evt-joystick-connected 16)
  (:sf-evt-joystick-disconnected 17)
  (:sf-evt-touch-began 18)
  (:sf-evt-touch-moved 19)
  (:sf-evt-touch-ended 20)
  (:sf-evt-sensor-changed 21)
  (:sf-evt-count 22))

(defcstruct sf-key-event
  (type sf-event-type)
  (key-code sf-key-code)
  (alt sf-bool)
  (control sf-bool)
  (shift sf-bool)
  (system sf-bool))

(defcstruct sf-text-event
  (type sf-event-type)
  (unicode sf-uint-32))

(defcstruct sf-mouse-move-event
  (type sf-event-type)
  (x :int)
  (y :int))

(defcstruct sf-mouse-button-event
  (type sf-event-type)
  (button sf-mouse-button)
  (x :int)
  (y :int))

(defcstruct sf-mouse-wheel-event
  (type sf-event-type)
  (delta :int)
  (x :int)
  (y :int))

(defcstruct sf-mouse-wheel-scroll-event
  (type sf-event-type)
  (wheel sf-mouse-wheel)
  (delta :float)
  (x :int)
  (y :int))

(defcstruct sf-joystick-move-event
  (type sf-event-type)
  (joystick-id :unsigned-int)
  (axis sf-joystick-axis)
  (position :float))

(defcstruct sf-joystick-button-event
  (type sf-event-type)
  (joystick-id :unsigned-int)
  (button :unsigned-int))

(defcstruct sf-joystick-connect-event
  (type sf-event-type)
  (joystick-id :unsigned-int))

(defcstruct sf-size-event
  (type sf-event-type)
  (width :unsigned-int)
  (height :unsigned-int))

(defcstruct sf-touch-event
  (type sf-event-type)
  (finger :unsigned-int)
  (x :int)
  (y :int))

(defcstruct sf-sensor-event
  (type sf-event-type)
  (sensor-type sf-sensor-type)
  (x :float)
  (y :float)
  (z :float))

(defcunion sf-event
  (type sf-event-type)
  (size (:struct sf-size-event))
  (key (:struct sf-key-event))
  (text (:struct sf-text-event))
  (mouse-move (:struct sf-mouse-move-event))
  (mouse-button (:struct sf-mouse-button-event))
  (mouse-wheel (:struct sf-mouse-wheel-event))
  (mouse-wheel-scroll (:struct sf-mouse-wheel-scroll-event))
  (joystick-move (:struct sf-joystick-move-event))
  (joystick-button (:struct sf-joystick-button-event))
  (joystick-connect (:struct sf-joystick-connect-event))
  (touch (:struct sf-touch-event))
  (sensor (:struct sf-sensor-event)))
