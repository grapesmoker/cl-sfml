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
  ((pointer :initarg :pointer :initform nil :accessor window-pointer)))

(defmethod window-is-open? ((w window))
  (sf-window-is-open (window-pointer w)))

(defmethod window-close ((w window))
  (sf-window-close (window-pointer w)))

(defmethod window-destroy ((w window))
  (sf-window-destroy (window-pointer w))
  (free-converted-object (window-pointer w) :pointer nil))

(defmethod window-poll-event ((w window) (ev event))
  (with-foreign-object (event '(:union sf-event))
    (sf-window-poll-event (window-pointer w) event)
    (let* (;;(lisp-event (convert-from-foreign event '(:pointer (:union sf-event))))
	   (event-code
	    (mem-ref (foreign-slot-pointer event '(:union sf-event) 'type) :int)))
      (format t "event code: ~A~%" event-code)
      (cond ((eq event-code (foreign-enum-value 'sf-event-type :sf-evt-closed))
	     (make-instance 'event :type :sf-evt-closed))))))
