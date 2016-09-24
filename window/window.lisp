(in-package :sfml)

(use-foreign-library libcsfml-window)

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

(defclass window ()
  ((pointer :initarg :pointer :initform nil :accessor window-pointer)
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

;; This is a bit awkward because you'd think the window-poll-event
;; function would return the event code, but because of the way that
;; events are managed and because of some weirdness that I can't quite
;; track down, I'm leaving it up to the event class itself to manage
;; its own internals. So this function actually sets the internals
;; of the event passed to it so that the caller can figure out what
;; to do with it.

(defmethod window-poll-event ((w window) (ev event))
  (sf-window-poll-event (window-pointer w) (event-pointer ev))
  (let* ((event-keyword
	  (foreign-slot-value (event-pointer ev) '(:union sf-event) 'type)))
    (setf (event-type ev) event-keyword)
    (case event-keyword
      (:sf-evt-closed
       (progn
	 ()))
      (:sf-evt-key-pressed
       (setf (event-struct ev)
	     (foreign-slot-value (event-pointer ev) '(:union sf-event) 'key)))
      (:sf-evt-key-released
       (setf (event-struct ev)
	     (foreign-slot-value (event-pointer ev) '(:union sf-event) 'key)))
      (:t
       ()))))

