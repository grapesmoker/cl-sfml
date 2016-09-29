(in-package :sfml)

(defclass render-window (window)
  ((view :accessor render-window-view)
   (viewport :accessor render-window-viewport)))

(defcfun ("sfRenderWindow_create" sf-render-window-create) :pointer
  (mode (:struct sf-video-mode))
  (title :string)
  (style sf-uint-32)
  (settings :pointer))

(defun make-render-window (mode title style context)
  (make-instance 'render-window
		 :video-mode mode
		 :title title
		 :style style
		 :context context))

(defmethod initialize-instance :after ((rw render-window) &key)
  (with-foreign-string (title (window-title rw))
    (setf (window-pointer rw)
	  (sf-render-window-create (slot-value rw 'video-mode)
				   title
				   (slot-value rw 'style)
				   (context-pointer
				    (slot-value rw 'context))))))

(defcfun ("sfRenderWindow_close" sf-render-window-close) :void
  (window :pointer))

(defcfun ("sfRenderWindow_destroy" sf-render-window-destroy) :void
  (window :pointer))

(defcfun ("sfRenderWindow_getSettings" sf-render-window-get-settings)
    (:struct sf-context-settings)
  (window :pointer))

(defcfun ("sfRenderWindow_clear" sf-render-window-clear) :void
  (render-window :pointer)
  (color (:struct sf-color)))

(defmethod render-window-clear ((rw render-window) (c color))
  (sf-render-window-clear (window-pointer rw) c))

(defcfun ("sfRenderWindow_isOpen" sf-render-window-is-open) sf-bool
  (window :pointer))

(defmethod window-is-open? :before ((rw render-window))
  (setf (slot-value rw 'is-open?)
	(sf-render-window-is-open (window-pointer rw))))

(defcfun ("sfRenderWindow_getSettings" sf-render-window-get-settings)
    (:struct sf-context-settings)
  (render-window :pointer))

(defmethod window-context :before ((rw render-window))
  (let ((context-settings (sf-render-window-get-settings (window-pointer rw))))
    (setf (slot-value (slot-value rw 'context) 'settings)
	  context-settings)))

(defcfun ("sfRenderWindow_getSize" sf-render-window-get-size) (:struct sf-vector-2u)
  (window :pointer))

(defmethod window-size :before ((rw render-window))
  (setf (slot-value rw 'size) (sf-render-window-get-size (window-pointer rw))))

(defcfun ("sfRenderWindow_setSize" sf-render-window-set-size) :void
  (window :pointer)
  (size (:struct sf-vector-2u)))

(defmethod (setf window-size) :after ((v vect) (rw render-window))
  (sf-render-window-set-size (window-pointer rw) v))

(defcfun ("sfRenderWindow_getPosition" sf-render-window-get-position)
    (:struct sf-vector-2i)
  (window :pointer))

(defmethod window-position :before ((rw render-window))
  (setf (slot-value rw 'position) (sf-render-window-get-position (window-pointer rw))))

(defcfun ("sfRenderWindow_setPosition" sf-render-window-set-position) :void
  (window :pointer)
  (position (:struct sf-vector-2i)))

(defmethod (setf window-position) :after ((v vect) (rw render-window))
  (sf-render-window-set-position (window-pointer rw) v))

(defcfun ("sfRenderWindow_setTitle" sf-render-window-set-title) :void
  (window :pointer)
  (title :pointer))

(defmethod (setf window-title) :after ((new-title string) (rw render-window))
  (with-foreign-string (title new-title)
    (sf-render-window-set-title (window-pointer rw) title)))

(defcfun ("sfRenderWindow_setUnicodeTitle" sf-render-window-set-unicode-title) :void
  (window :pointer)
  (title :pointer))

(defcfun ("sfRenderWindow_setIcon" sf-render-window-set-icon) :void
  (window :pointer)
  (width :unsigned-int)
  (height :unsigned-int)
  (pixels :pointer))


(defcfun ("sfRenderWindow_setVisible" sf-render-window-set-visible) :void
  (window :pointer)
  (visible sf-bool))

(defmethod (setf window-visible) :after (visible (rw render-window))
  (sf-render-window-set-visible (window-pointer rw) visible))

(defcfun ("sfRenderWindow_setVerticalSyncEnabled"
	  sf-render-window-set-vertical-sync-enabled) :void
  (window :pointer)
  (enabled sf-bool))

(defmethod (setf window-vertical-sync) :after (vertical-sync (rw render-window))
  (sf-render-window-set-vertical-sync-enabled (window-pointer rw) vertical-sync))

(defcfun ("sfRenderWindow_setKeyRepeatEnabled" sf-render-window-set-key-repeat-enabled)
    :void
  (window :pointer)
  (enabled sf-bool))

(defmethod (setf window-key-repeat) :after (key-repeat (rw render-window))
  (sf-render-window-set-key-repeat-enabled (window-pointer rw) key-repeat))

(defcfun ("sfRenderWindow_setActive" sf-render-window-set-active) sf-bool
  (window :pointer)
  (active sf-bool))

(defmethod (setf window-active) :after (active (rw render-window))
  (sf-render-window-set-active (window-pointer rw) active))

(defcfun ("sfRenderWindow_hasFocus" sf-render-window-has-focus) sf-bool
  (window :pointer))

(defmethod window-focus :before ((rw render-window))
  (setf (slot-value rw 'focus) (sf-render-window-has-focus (window-pointer rw))))

(defcfun ("sfRenderWindow_requestFocus" sf-render-window-request-focus) :void
  (window :pointer))

(defmethod (setf window-focus) :after (focus (rw render-window))
  (sf-render-window-request-focus (window-pointer rw))
  (let ((has-focus (sf-render-window-has-focus (window-pointer rw))))
    (when (not (eq focus has-focus))
      (setf (slot-value rw 'focus) has-focus))))

(defcfun ("sfRenderWindow_display" sf-render-window-display) :void
  (window :pointer))

(defmethod window-display ((rw render-window))
  (sf-render-window-display (window-pointer rw)))

(defcfun ("sfRenderWindow_setFramerateLimit" sf-render-window-set-framerate-limit) :void
  (window :pointer)
  (limit :unsigned-int))

(defmethod (setf window-framerate-limit) :after ((limit integer) (rw render-window))
  (sf-render-window-set-framerate-limit (window-pointer rw) limit))

(defcfun ("sfRenderWindow_setJoystickThreshold" sf-render-window-set-joystick-threshold)
    :void
  (window :pointer)
  (threshold :float))

(defmethod (setf window-joystick-threshold) :after ((threshold number) (rw render-window))
  (sf-render-window-set-joystick-threshold (window-pointer rw)
					   (coerce threshold 'float)))

(defcfun ("sfRenderWindow_setMouseCursorVisible"
	  sf-render-window-set-mouse-cursor-visible) :void
  (render-window :pointer)
  (visible sf-bool))

(defmethod (setf window-mouse-cursor-visible) (visible (rw render-window))
  (sf-render-window-set-mouse-cursor-visible (window-pointer rw) visible))

(defcfun ("sfRenderWindow_setMouseCursorGrabbed"
	  sf-render-window-set-mouse-cursor-grabbed) :void
  (window :pointer)
  (grabbed sf-bool))

(defmethod (setf window-mouse-cursor-grabbed) (grabbed (rw render-window))
  (sf-window-set-mouse-cursor-grabbed (window-pointer rw) grabbed))

(defcfun ("sfRenderWindow_getView" sf-render-window-get-view) :pointer
  (render-window :pointer))

(defmethod render-window-view :before ((rw render-window))
  (setf (slot-value rw 'view)
	(make-instance 'view
		       :pointer (sf-render-window-get-view (window-pointer rw)))))

(defcfun ("sfRenderWindow_setView" sf-render-window-set-view) :void
  (render-window :pointer)
  (view :pointer))

(defmethod (setf render-window-view) :after ((v view) (rw render-window))
  (sf-render-window-set-view (window-pointer rw) (view-pointer v)))

(defcfun ("sfRenderWindow_getViewport" sf-render-window-get-viewport)
    (:struct sf-int-rect)
  (render-window :pointer))

(defmethod render-window-viewport :before ((rw render-window))
  (setf (slot-value rw 'viewport) (sf-render-window-get-viewport (window-pointer rw))))

(defcfun ("sfRenderWindow_setViewport" sf-render-window-set-viewport) :void
  (render-window :pointer)
  (viewport (:struct sf-int-rect)))

(defmethod (setf render-window-viewport) :after ((v rect) (rw render-window))
  (sf-render-window-set-viewport (window-pointer rw) v))

(defcfun ("sfRenderWindow_mapPixelToCoords" sf-render-window-map-pixel-to-coords)
    (:struct sf-vector-2f)
  (render-window :pointer)
  (point (:struct sf-vector-2i))
  (view :pointer))

(defmethod render-window-pixel->coords ((rw render-window) (pt vect) (v view))
  (sf-render-window-map-pixel-to-coords (window-pointer rw) pt
					(view-pointer v)))

(defcfun ("sfRenderWindow_mapCoordsToPixel" sf-render-window-map-coords-to-pixel)
    (:struct sf-vector-2i)
  (render-window :pointer)
  (point (:struct sf-vector-2f))
  (view :pointer))

(defmethod render-window-coords->pixel ((rw render-window) (pt vect) (v view))
  (sf-render-window-map-coords-to-pixel (window-pointer rw) pt
					(view-pointer v)))


(defmethod window-close ((rw render-window))
  (sf-render-window-close (window-pointer rw)))

(defmethod window-destroy ((rw render-window))
  ;; apparently trying to destroy the window pointer causes
  ;; some kind of core dump in the nvidia drivers, not sure
  ;; what's up because it works for the normal window.
  ;;(sf-render-window-destroy (window-pointer rw))
  (free-converted-object (window-pointer rw) :pointer nil))


(defmethod window-poll-event ((rw window) (ev event))
  (sf-window-poll-event (window-pointer rw) (event-pointer ev))
  (let* ((event-keyword
	  (foreign-slot-value (event-pointer ev) '(:union sf-event) 'type)))
    (setf (event-type ev) event-keyword)
    (let ((struct-type
    	   (cadr (getf event-struct-mapping event-keyword))))
      (when struct-type
      	(setf (event-struct ev)
	      (foreign-slot-value (event-pointer ev) '(:union sf-event) struct-type))))))

(defcfun ("sfRenderWindow_drawSprite" sf-render-window-draw-sprite) :void
  (render-window :pointer)
  (sprite :pointer)
  (states :pointer))

(defcfun ("sfRenderWindow_drawText" sf-render-window-draw-text) :void
  (render-window :pointer)
  (text :pointer)
  (states :pointer))

(defcfun ("sfRenderWindow_drawCircleShape" sf-render-window-draw-circle-shape) :void
  (render-window :pointer)
  (circle-shape :pointer)
  (states :pointer))

(defcfun ("sfRenderWindow_drawConvexShape" sf-render-window-draw-convex-shape) :void
  (render-window :pointer)
  (convext-shape :pointer)
  (states :pointer))

(defcfun ("sfRenderWindow_drawRectangleShape"
	  sf-render-window-draw-rectangle-shape) :void
  (render-window :pointer)
  (rectangle-shape :pointer)
  (states :pointer))

(defmethod entity-draw ((spr sprite) (rw render-window) render-states)
  (sf-render-window-draw-sprite (window-pointer rw)
				(sprite-pointer spr)
				render-states))

(defmethod entity-draw ((tex text) (rw render-window) render-states)
  (sf-render-window-draw-text (window-pointer rw)
			      (text-pointer tex)
			      render-states))

(defmethod entity-draw ((circ circle) (rw render-window) render-states)
  (sf-render-window-draw-circle-shape (window-pointer rw)
				      (shape-pointer circ)
				      render-states))

(defmethod entity-draw ((conv convex) (rw render-window) render-states)
  (sf-render-window-draw-convex-shape (window-pointer rw)
				      (shape-pointer conv)
				      render-states))

(defmethod entity-draw ((rect rectangle) (rw render-window) render-states)
  (sf-render-window-draw-rectangle-shape (window-pointer rw)
					 (shape-pointer rect)
					 render-states))



