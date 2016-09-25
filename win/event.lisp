(in-package :sfml)

;; base class for events

(defclass root-event ()
  ((type :initarg :type :initform nil :accessor event-type)))

;; print-object method that encompasses all events, since their text
;; representation is all going to be the same anyway

(defmethod print-object ((ev root-event) stream)
  (let* ((slots
	  (mapcar #'slot-definition-name (class-slots (find-class (type-of ev)))))
	 (slot-values (mapcar #'(lambda (slot)
				  (slot-value ev slot))
			      slots))
	 (formatted-names-and-values
	  (loop
	     for slot in slots
	     for value in slot-values
	     collect
	       (format nil "~A: ~A" slot value))))
    (format stream
	    "<~A [~{~A~^, ~}]>" (type-of ev) formatted-names-and-values)))


(defcenum sf-event-type
  ;; there seems to be some issue with event management so this dummy
  ;; code ensures that when the pointer to the event union is created
  ;; it's filled with something that indicates there's no event
  (:sf-evt-none -1)
  (:sf-evt-closed 0)
  (:sf-evt-resized 1)
  (:sf-evt-lost-focus 2)
  (:sf-evt-gained-focus 3)
  (:sf-evt-text-entered 4)
  (:sf-evt-key-pressed 5)
  (:sf-evt-key-released 6)
  (:sf-evt-mouse-wheel-moved 7)
  (:sf-evt-mouse-wheel-scrolled 8)
  (:sf-evt-mouse-button-pressed 9)
  (:sf-evt-mouse-button-released 10)
  (:sf-evt-mouse-moved 11)
  (:sf-evt-mouse-entered 12)
  (:sf-evt-mouse-left 13)
  (:sf-evt-joystick-button-pressed 14)
  (:sf-evt-joystick-button-released 15)
  (:sf-evt-joystick-moved 16)
  (:sf-evt-joystick-connected 17)
  (:sf-evt-joystick-disconnected 18)
  (:sf-evt-touch-began 19)
  (:sf-evt-touch-moved 20)
  (:sf-evt-touch-ended 21)
  (:sf-evt-sensor-changed 22)
  (:sf-evt-count 23))

;; Keyboard Events

;; foreign struct for key events

(defcstruct (sf-key-event :class key-event-type)
  (type sf-event-type)
  (code :char)
  (alt sf-bool)
  (control sf-bool)
  (shift sf-bool)
  (system sf-bool))

;; class for key events

(defclass key-event (root-event)
  ((code :initarg :code :initform nil :accessor key-event-code)
   (alt :initarg :alt :initform nil :accessor key-event-alt)
   (control :initarg :control :initform nil :accessor key-event-control)
   (shift :initarg :shift :initform nil :accessor key-event-shift)
   (system :initarg :system :initform nil :accessor key-event-system)))

;; transformation functions

(defmethod translate-from-foreign (p (type key-event-type))
 (copy-from-foreign 'key-event p '(:struct sf-key-event)))

(defmethod translate-into-foreign-memory ((ev key-event) (type key-event-type) p)
    (copy-to-foreign ev p '(:struct sf-key-event) '
		     (sf-event-type :int sf-bool sf-bool sf-bool sf-bool)))


;; Text Events

;; foreign struct for text events

(defcstruct (sf-text-event :class text-event-type)
  (type sf-event-type)
  (unicode sf-uint-32))

;; class for text events

(defclass text-event (root-event)
  ((unicode :initarg :unicode :initform nil :accessor text-event-unicode)))

;; transformations 

(defmethod translate-from-foreign (p (type text-event-type))
  (copy-from-foreign 'text-event p '(:struct sf-text-event)))

(defmethod translate-into-foreign-memory ((ev text-event) (type text-event-type) p)
    (copy-to-foreign ev p '(:struct sf-text-event) '
		     (sf-event-type sf-unit-32)))

;; Mouse Events

;; class that encompasses all mouse events, since they all share
;; x and y coordinates

(defclass mouse-event (root-event)
  ((x :initarg :x :initform nil :accessor mouse-event-x)
   (y :initarg :y :initform nil :accessor mouse-event-y)))

;; struct for mouse-move-event

(defcstruct (sf-mouse-move-event :class mouse-move-event-type)
  (type sf-event-type)
  (x :int)
  (y :int))

;; class for mouse-move-event

;; the mouse-move-event is literally identical to mouse-event proper
;; but is created just for the purposes of having a "nice" event
;; hierarchy

(defclass mouse-move-event (mouse-event)
  ())

(defmethod translate-from-foreign (p (type mouse-move-event-type))
  (copy-from-foreign 'mouse-move-event p '(:struct sf-mouse-move-event)))

(defmethod translate-into-foreign-memory ((ev mouse-move-event) (type mouse-move-event-type) p)
    (copy-to-foreign ev p '(:struct sf-mouse-move-event) '
		     (sf-event-type :int :int)))

;; struct for mouse-button-event

(defcstruct (sf-mouse-button-event :class mouse-button-event-type)
  (type sf-event-type)
  (button sf-mouse-button)
  (x :int)
  (y :int))

;; class for mouse-button-event

(defclass mouse-button-event (mouse-event)
  ((button :initarg :button :initform nil :accessor mouse-button-event-button)))

;; transformations

(defmethod translate-from-foreign (p (type mouse-button-event-type))
  (copy-from-foreign 'mouse-button-event p '(:struct sf-mouse-button-event)))

(defmethod translate-into-foreign-memory ((ev mouse-button-event) (type mouse-button-event-type) p)
    (copy-to-foreign ev p '(:struct sf-mouse-button-event) '
		     (sf-event-type :int :int sf-mouse-button)))

;; struct for mouse-wheel-event

(defcstruct (sf-mouse-wheel-event :class mouse-wheel-event-type)
  (type sf-event-type)
  (delta :int)
  (x :int)
  (y :int))

;; class for mouse-wheel-event

(defclass mouse-wheel-event (mouse-event)
  ((delta :initarg :delta :initform nil :accessor mouse-wheel-event-delta)))

;; transformations

(defmethod translate-from-foreign (p (type mouse-wheel-event-type))
  (copy-from-foreign 'mouse-wheel-event p '(:struct sf-mouse-wheel-event)))

(defmethod translate-into-foreign-memory ((ev mouse-wheel-event) (type mouse-wheel-event-type) p)
    (copy-to-foreign ev p '(:struct sf-mouse-wheel-event) '
		     (sf-event-type :int :int :int)))

;; struct for mouse-wheel-scroll-event

(defcstruct (sf-mouse-wheel-scroll-event :class mouse-wheel-scroll-event-type)
  (type sf-event-type)
  (wheel sf-mouse-wheel)
  (delta :float)
  (x :int)
  (y :int))

;; class for mouse-wheel-scroll-event

(defclass mouse-wheel-scroll-event (mouse-event)
  ((wheel :initarg :wheel :initform nil :accessor mouse-wheel-scroll-event-wheel)
   (delta :initarg :delta :initform nil :accessor mouse-wheel-scroll-event-delta)))

;; transformations

(defmethod translate-from-foreign (p (type mouse-wheel-scroll-event-type))
  (copy-from-foreign 'mouse-wheel-scroll-event p '(:struct sf-mouse-wheel-scroll-event)))

(defmethod translate-into-foreign-memory ((ev mouse-wheel-scroll-event) (type mouse-wheel-scroll-event-type) p)
  (copy-to-foreign ev p '(:struct sf-mouse-wheel-scroll-event)
		   '(sf-event-type :int :int sf-mouse-wheel :float)))

;; Joystick Events

;; root event for joystick events

(defclass joystick-event (root-event)
  ((joystick-id :initarg :joystick-id :initform nil :accessor joystick-event-joystick-id)))

;; struct for joystick-move-event

(defcstruct (sf-joystick-move-event :class joystick-move-event-type)
  (type sf-event-type)
  (joystick-id :unsigned-int)
  (axis sf-joystick-axis)
  (position :float))

;; class for joystick-move-event

(defclass joystick-move-event (joystick-event)
  ((axis :initarg :axis :initform nil :accessor joystick-move-event-axis)
   (position :initarg :position :initform nil :accessor joystick-move-event-position)))

(defmethod translate-from-foreign (p (type joystick-move-event-type))
  (copy-from-foreign 'joystick-move-event p '(:struct sf-joystick-move-event)))

(defmethod translate-into-foreign-memory ((ev joystick-move-event) (type joystick-move-event-type) p)
  (copy-to-foreign ev p '(:struct sf-joystick-move-event)
		   '(sf-event-type :unsigned-int sf-joystick-axis :float)))
		   
;; struct for joystick-button-event

(defcstruct (sf-joystick-button-event :class joystick-button-event-type)
  (type sf-event-type)
  (joystick-id :unsigned-int)
  (button :unsigned-int))

;; class for joystick-move-event

(defclass joystick-button-event (joystick-event)
  ((button :initarg :button :initform nil :accessor joystick-button-event-button)))

(defmethod translate-from-foreign (p (type joystick-button-event-type))
  (copy-from-foreign 'joystick-button-event p '(:struct sf-joystick-button-event)))

(defmethod translate-into-foreign-memory ((ev joystick-button-event) (type joystick-button-event-type) p)
  (copy-to-foreign ev p '(:struct sf-joystick-button-event)
		   '(sf-event-type :unsigned-int :unsigned-int)))

;; struct for joystick-connect-event

(defcstruct (sf-joystick-connect-event :class joystick-connect-event-type)
  (type sf-event-type)
  (joystick-id :unsigned-int))

;; class for joystick-connect-event

(defclass joystick-connect-event (joystick-event)
  ())

(defmethod translate-from-foreign (p (type joystick-connect-event-type))
  (copy-from-foreign 'joystick-connect-event p '(:struct sf-joystick-connect-event)))

(defmethod translate-into-foreign-memory ((ev joystick-connect-event) (type joystick-connect-event-type) p)
  (copy-to-foreign ev p '(:struct sf-joystick-connect-event)
		   '(sf-event-type :unsigned-int)))

;; struct for resize event

(defcstruct (sf-size-event :class size-event-type)
  (type sf-event-type)
  (width :unsigned-int)
  (height :unsigned-int))

;; class for resize event

(defclass size-event (root-event)
  ((width :initarg :width :initform nil :accessor size-event-width)
   (height :initarg :height :initform nil :accessor size-event-height)))

(defmethod translate-from-foreign (p (type size-event-type))
  (copy-from-foreign 'size-event p '(:struct sf-size-event)))

(defmethod translate-into-foreign-memory ((ev size-event) (type size-event-type) p)
  (copy-to-foreign ev p '(:struct sf-size-event)
		   '(sf-event-type :unsigned-int :unsigned-int)))

;; struct for touch-event

(defcstruct (sf-touch-event :class touch-event-type)
  (type sf-event-type)
  (finger :unsigned-int)
  (x :int)
  (y :int))

;; class for touch-event

(defclass touch-event (root-event)
  ((finger :initarg :finger :initform nil :accessor touch-event-finger)
   (x :initarg :x :initform nil :accessor touch-event-x)
   (y :initarg :y :initform nil :accessor touch-event-y)))

(defmethod translate-from-foreign (p (type touch-event-type))
  (copy-from-foreign 'touch-event p '(:struct sf-touch-event)))

(defmethod translate-into-foreign-memory ((ev touch-event) (type touch-event-type) p)
  (copy-to-foreign ev p '(:struct sf-touch-event)
		   '(sf-event-type :unsigned-int :int :int)))

;; struct for sensor-event

(defcstruct (sf-sensor-event :class sensor-event-type)
  (type sf-event-type)
  (sensor-type sf-sensor-type)
  (x :float)
  (y :float)
  (z :float))

(defclass sensor-event (root-event)
  ((sensor-type :initarg :finger :initform nil :accessor sensor-event-sensor-type)
   (x :initarg :x :initform nil :accessor sensor-event-x)
   (y :initarg :y :initform nil :accessor sensor-event-y)
   (z :initarg :z :initform nil :accessor sensor-event-z)))

(defmethod translate-from-foreign (p (type sensor-event-type))
  (copy-from-foreign 'sensor-event p '(:struct sf-sensor-event)))

(defmethod translate-into-foreign-memory ((ev sensor-event) (type sensor-event-type) p)
  (copy-to-foreign ev p '(:struct sf-sensor-event)
		   '(sf-event-type sf-sensor-type :float :float :float)))

;; union for the main event type

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

(defclass event (root-event)
  ((pointer :initarg :pointer :initform (foreign-alloc '(:union sf-event)) :accessor event-pointer)
   (event-struct :initarg :event :initform nil :accessor event-struct)))

(defmethod initialize-instance :after ((ev event) &key)
  (setf (foreign-slot-value (event-pointer ev) '(:union sf-event) 'type) -1))

(defun get-event-struct (ptr event-type)
  (foreign-slot-value ptr '(:union sf-event) event-type))

;; can't seem to read out mouse position properly for some reason
;; so for now here's an auxiliary function that just updates the
;; mouse position from the event

(defmethod update-mouse-position ((m mouse) (ev event))
  (let* ((ev-struct (event-struct ev))
	 (x (mouse-event-x ev-struct))
	 (y (mouse-event-y ev-struct)))
    (setf (mouse-x m) x)
    (setf (mouse-y m) y)))
