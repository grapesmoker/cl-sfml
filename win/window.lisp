(in-package :sfml)

(defcenum sf-window-style
  (:sf-none 0)
  (:sf-titlebar 1)
  (:sf-resize 2)
  (:sf-close 4)
  (:sf-fullscreen 8)
  (:sf-default-style 7))

(defclass window ()
  ((pointer :initarg :pointer :initform nil :accessor window-pointer)
   (width :initarg :width :initform nil :accessor window-width)
   (height :initarg :height :initform nil :accessor window-height)
   (title :initarg :title :initform nil :accessor window-title)
   (video-mode :initarg :video-mode :initform nil :accessor window-video-mode)
   (style :initarg :style :initform nil :accessor window-style)
   (context :initarg :context :initform nil :accessor window-context)
   (is-open? :reader window-is-open?)
   (focus :accessor window-focus)
   (size :accessor window-size)
   (position :accessor window-position)
   (visible :accessor window-visible)
   (active :accessor window-active)
   (vertical-sync :accessor window-vertical-sync)
   (key-repeat :accessor window-key-repeat)
   (framerate-limit :accessor window-framerate-limit)
   (joystick-threshold :accessor window-joystick-threshold)
   (mouse-cursor-visible :accessor window-mouse-cursor-visible)
   (mouse-cursor-grabbed :accessor window-mouse-cursor-grabbed)
   (icon :accessor window-icon)
   (x :initarg :x :initform nil :accessor window-x)
   (y :initarg :x :initform nil :accessor window-y)
   (prev-event-code :initarg :prev-event-code
		    :initform :sf-evt-closed
		    :accessor window-prev-event-code)))

(defcfun ("sfWindow_create" sf-window-create) :pointer
  (video-mode (:struct sf-video-mode))
  (title :string)
  (style sf-uint-32)
  (settings :pointer))

(defun make-window (video-mode title style context)
  (make-instance 'window
		 :video-mode video-mode
		 :title title
		 :style style
		 :context context))

(defmethod initialize-instance :after ((w window) &key)
  (with-foreign-string (title (window-title w))
    (setf (window-pointer w)
	  (sf-window-create (slot-value w 'video-mode)
			    title
			    (slot-value w 'style)
			    (context-pointer
			     (slot-value w 'context))))))

(defcfun ("sfWindow_isOpen" sf-window-is-open) sf-bool
  (window :pointer))

(defmethod window-is-open? :before ((w window))
  (setf (slot-value w 'is-open?)
	(sf-window-is-open (window-pointer w))))


(defcfun ("sfWindow_pollEvent" sf-window-poll-event) sf-bool
  (window :pointer)
  (event :pointer))

(defcfun ("sfWindow_close" sf-window-close) :void
  (window :pointer))

(defcfun ("sfWindow_destroy" sf-window-destroy) :void
  (window :pointer))

(defcfun ("sfWindow_getSettings" sf-window-get-settings) (:struct sf-context-settings)
  (window :pointer))

(defmethod window-context :before ((w window))
  (let ((context-settings (sf-window-get-settings (window-pointer w))))
    (setf (slot-value (slot-value w 'context) 'settings)
	  context-settings)))

(defcfun ("sfWindow_waitEvent" sf-window-wait-event) sf-bool
  (window :pointer)
  (event :pointer))

(defcfun ("sfWindow_getSize" sf-window-get-size) (:struct sf-vector-2u)
  (window :pointer))

(defmethod window-size :before ((w window))
  (setf (slot-value w 'size) (sf-window-get-size (window-pointer w))))

(defcfun ("sfWindow_setSize" sf-window-set-size) :void
  (window :pointer)
  (size (:struct sf-vector-2u)))

(defmethod (setf window-size) :after ((v vect) (w window))
  (sf-window-set-size (window-pointer w) v))

(defcfun ("sfWindow_getPosition" sf-window-get-position) (:struct sf-vector-2i)
  (window :pointer))

(defmethod window-position :before ((w window))
  (setf (slot-value w 'position) (sf-window-get-position (window-pointer w))))

(defcfun ("sfWindow_setPosition" sf-window-set-position) :void
  (window :pointer)
  (position (:struct sf-vector-2i)))

(defmethod (setf window-position) :after ((v vect) (w window))
  (sf-window-set-position (window-pointer w) v))

(defcfun ("sfWindow_setTitle" sf-window-set-title) :void
  (window :pointer)
  (title :pointer))

(defmethod (setf window-title) :after ((new-title string) (w window))
  (with-foreign-string (title new-title)
    (sf-window-set-title (window-pointer w) title)))

(defcfun ("sfWindow_setUnicodeTitle" sf-window-set-unicode-title) :void
  (window :pointer)
  (title :pointer))

(defcfun ("sfWindow_setIcon" sf-window-set-icon) :void
  (window :pointer)
  (width :unsigned-int)
  (height :unsigned-int)
  (pixels :pointer))


(defcfun ("sfWindow_setVisible" sf-window-set-visible) :void
  (window :pointer)
  (visible sf-bool))

(defmethod (setf window-visible) :after (visible (w window))
  (sf-window-set-visible (window-pointer w) visible))

(defcfun ("sfWindow_setVerticalSyncEnabled" sf-window-set-vertical-sync-enabled) :void
  (window :pointer)
  (enabled sf-bool))

(defmethod (setf window-vertical-sync) :after (vertical-sync (w window))
  (sf-window-set-vertical-sync-enabled (window-pointer w) vertical-sync))

(defcfun ("sfWindow_setKeyRepeatEnabled" sf-window-set-key-repeat-enabled) :void
  (window :pointer)
  (enabled sf-bool))

(defmethod (setf window-key-repeat) :after (key-repeat (w window))
  (sf-window-set-key-repeat-enabled (window-pointer w) key-repeat))

(defcfun ("sfWindow_setActive" sf-window-set-active) sf-bool
  (window :pointer)
  (active sf-bool))

(defmethod (setf window-active) :after (active (w window))
  (sf-window-set-active (window-pointer w) active))

(defcfun ("sfWindow_hasFocus" sf-window-has-focus) sf-bool
  (window :pointer))

(defmethod window-focus :before ((w window))
  (setf (slot-value w 'focus) (sf-window-has-focus (window-pointer w))))

(defcfun ("sfWindow_requestFocus" sf-window-request-focus) :void
  (window :pointer))

(defmethod (setf window-focus) :after (focus (w window))
  (sf-window-request-focus (window-pointer w))
  (let ((has-focus (sf-window-has-focus (window-pointer w))))
    (when (not (eq focus has-focus))
      (setf (slot-value w 'focus) has-focus))))

(defcfun ("sfWindow_display" sf-window-display) :void
  (window :pointer))

(defcfun ("sfWindow_setFramerateLimit" sf-window-set-framerate-limit) :void
  (window :pointer)
  (limit :unsigned-int))

(defmethod (setf window-framerate-limit) :after ((limit integer) (w window))
  (sf-window-set-framerate-limit (window-pointer w) limit))

(defcfun ("sfWindow_setJoystickThreshold" sf-window-set-joystick-threshold) :void
  (window :pointer)
  (threshold :float))

(defmethod (setf window-joystick-threshold) :after ((threshold number) (w window))
  (sf-window-set-joystick-threshold (window-pointer w) (coerce threshold 'float)))

(defcfun ("sfWindow_setMouseCursorVisible" sf-window-set-mouse-cursor-visible) :void
  (window :pointer)
  (visible sf-bool))

(defmethod (setf window-mouse-cursor-visible) (visible (w window))
  (sf-window-set-mouse-cursor-visible (window-pointer w) visible))

(defcfun ("sfWindow_setMouseCursorGrabbed" sf-window-set-mouse-cursor-grabbed) :void
  (window :pointer)
  (grabbed sf-bool))



(defmethod window-close ((w window))
  (sf-window-close (window-pointer w)))

(defmethod window-destroy ((w window))
  (sf-window-destroy (window-pointer w))
  (free-converted-object (window-pointer w) :pointer nil))

;; This is a bit awkward because you'd think the window-poll-event
;; function would return the event code, but because of the way that
;; events are managed and because of some weirdness that I can't quite
;; track down, I'm leaving it up to the event class itself to manage
;; its own internals. So this function actually sets the internals
;; of the event passed to it so that the caller can figure out what
;; to do with it.

(defparameter event-struct-mapping
  '(:sf-evt-key-pressed 'key
    :sf-evt-key-released 'key
    :sf-evt-closed nil
    :sf-evt-resized 'size
    :sf-evt-lost-focus 'nil
    :sf-evt-mouse-moved 'mouse-move
    :sf-evt-mouse-entered nil ;; 'mouse
    :sf-evt-mouse-left nil ;; 'mouse
    :sf-evt-mouse-button-pressed 'mouse-button
    :sf-evt-mouse-button-released 'mouse-button
    :sf-evt-mouse-wheel-moved 'mouse-wheel
    :sf-evt-mouse-wheel-scrolled 'mouse-wheel-scroll
    :sf-evt-joystick-button-pressed 'joystick-button
    :sf-evt-joystick-button-released 'joystick-button
    :sf-evt-joystick-moved 'joystick-move
    :sf-evt-joystick-connected 'joystick-connect
    :sf-evt-joystick-disconnected 'joystick-connect
    :sf-evt-touch-began 'touch
    :sf-evt-touch-moved 'touch
    :sf-evt-touch-ended 'touch
    :sf-evt-sensor-changed 'sensor))


(defmethod window-poll-event ((w window) (ev event))
  (sf-window-poll-event (window-pointer w) (event-pointer ev))
  (let* ((event-keyword
	  (foreign-slot-value (event-pointer ev) '(:union sf-event) 'type)))
    (setf (event-type ev) event-keyword)
    (let ((struct-type
    	   (cadr (getf event-struct-mapping event-keyword))))
      (when struct-type
      	(setf (event-struct ev)
	      (foreign-slot-value (event-pointer ev) '(:union sf-event) struct-type))))))
	      ;; (get-event-struct (event-pointer ev) struct-type))))))		   

(defmethod mouse-get-position ((w window) (m mouse))
  (let ((pos-vector (sf-mouse-get-position (window-pointer w))))
    (setf (mouse-x m) (vector2-x pos-vector)
	  (mouse-y m) (vector2-y pos-vector))))
