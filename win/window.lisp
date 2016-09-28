(in-package :sfml)

(defcenum sf-window-style
  (:sf-none 0)
  (:sf-titlebar 1)
  (:sf-resize 2)
  (:sf-close 4)
  (:sf-fullscreen 8)
  (:sf-default-style 7))

(defcfun ("sfWindow_create" sf-window-create) :pointer
  (video-mode (:struct sf-video-mode))
  (title :string)
  (style sf-uint-32)
  (settings :pointer))

(defcfun ("sfWindow_isOpen" sf-window-is-open) sf-bool
  (window :pointer))

(defcfun ("sfWindow_pollEvent" sf-window-poll-event) sf-bool
  (window :pointer)
  (event :pointer))

(defcfun ("sfWindow_close" sf-window-close) :void
  (window :pointer))

(defcfun ("sfWindow_destroy" sf-window-destroy) :void
  (window :pointer))

(defcfun ("sfWindow_getSettings" sf-window-get-settings) (:struct sf-context-settings)
  (window :pointer))

(defcfun ("sfWindow_waitEvent" sf-window-wait-event) sf-bool
  (window :pointer)
  (event :pointer))

(defcfun ("sfWindow_getPosition" sf-window-get-position) (:struct sf-vector-2i)
  (window :pointer))

(defcfun ("sfWindow_getSize" sf-window-get-size) (:struct sf-vector-2u)
  (window :pointer))

(defcfun ("sfWindow_setPosition" sf-window-set-position) :void
  (window :pointer)
  (position (:struct sf-vector-2i)))

(defcfun ("sfWindow_setSize" sf-window-set-size) :void
  (window :pointer)
  (size (:struct sf-vector-2u)))

(defcfun ("sfWindow_setTitle" sf-window-set-title) :void
  (window :pointer)
  (title :pointer))

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

(defcfun ("sfWindow_setVerticalSyncEnabled" sf-window-set-vertical-sync-enabled) :void
  (window :pointer)
  (enabled sf-bool))

(defcfun ("sfWindow_setKeyRepeatEnabled" sf-window-set-key-repeat-enabled) :void
  (window :pointer)
  (enabled sf-bool))

(defcfun ("sfWindow_setActive" sf-window-set-active) sf-bool
  (window :pointer)
  (active sf-bool))

(defcfun ("sfWindow_requestFocus" sf-window-request-focus) :void
  (window :pointer))

(defcfun ("sfWindow_hasFocus" sf-window-has-focus) sf-bool
  (window :pointer))

(defcfun ("sfWindow_display" sf-window-display) :void
  (window :pointer))

(defcfun ("sfWindow_setFramerateLimit" sf-window-set-framerate-limit) :void
  (window :pointer)
  (limit :unsigned-int))

(defcfun ("sfWindow_setJoystickThreshold" sf-window-set-joystick-threshold) :void
  (window :pointer)
  (threshold :float))


(defclass window ()
  ((pointer :initarg :pointer :initform nil :accessor window-pointer)
   (width :initarg :width :initform nil :accessor window-width)
   (height :initarg :height :initform nil :accessor window-height)
   (x :initarg :x :initform nil :accessor window-x)
   (y :initarg :x :initform nil :accessor window-y)
   (prev-event-code :initarg :prev-event-code
		    :initform :sf-evt-closed
		    :accessor window-prev-event-code)))

(defmethod window-is-open? ((w window))
  (sf-window-is-open (window-pointer w)))

(defmethod window-close ((w window))
  (sf-window-close (window-pointer w)))

(defmethod window-destroy ((w window))
  (sf-window-destroy (window-pointer w))
  (free-converted-object (window-pointer w) :pointer nil))

(defmethod window-get-size ((w window))
  (let ((size-vector (sf-window-get-size (window-pointer w))))
    (setf (window-width w) (vector2-x size-vector)
	  (window-height w) (vector2-y size-vector))))

(defmethod window-get-position ((w window))
  (let ((pos-vector (sf-window-get-position (window-pointer w))))
    (setf (window-x w) (vector2-x pos-vector)
	  (window-y w) (vector2-y pos-vector))))

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

    ;; (setf (mouse-x m) (vector2-x pos)
    ;; 	  (mouse-y m) (vector2-y pos))))
   
    ;; (case event-keyword
    ;;   (:sf-evt-closed
    ;;    (progn
    ;; 	 ()))
    ;;   (:sf-evt-key-pressed
    ;;    (setf (event-struct ev)
    ;; 	     (foreign-slot-value (event-pointer ev) '(:union sf-event) 'key)))
    ;;   (:sf-evt-key-released
    ;;    (setf (event-struct ev)
    ;; 	     (foreign-slot-value (event-pointer ev) '(:union sf-event) 'key)))
    ;;   (:sf-evt-text-entered
    ;;    (setf (event-struct ev)
    ;; 	     (get-event-struct (event-pointer ev) 'text)))
    ;;   (:sf-evt-mouse-moved
    ;;    (setf (event-struct ev)
    ;; 	     (get-event-struct (event-pointer ev) 'mouse-move)))
    ;;   (:t
    ;;    ())))))


      
