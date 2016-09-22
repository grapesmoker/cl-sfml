(in-package :sfml)

(use-foreign-library libcsfml-window)

(defcenum sf-window-style
  (:sf-none 0)
  (:sf-titlebar 1)
  (:sf-resize 2)
  (:sf-close 4)
  (:sf-fullscreen 8)
  (:sf-default-style 7))

;; video mode type

(defcstruct (sf-video-mode :class video-mode-type)
  (width :unsigned-int)
  (height :unsigned-int)
  (bits-per-pixel :unsigned-int))

;; lisp-side class

(defclass video-mode-class ()
  ((width
    :initarg :width
    :initform 0
    :accessor video-mode-width)
   (height
    :initarg :height
    :initform 0
    :accessor video-mode-height)
   (bits-per-pixel
    :initarg :bits-per-pixel
    :initform 0
    :accessor video-mode-bits-per-pixel)))

(defmethod print-object ((vm video-mode-class) stream)
  (format stream
	  "<VIDEO-MODE (~Dx~D), ~Dbpp>"
	  (video-mode-width vm)
	  (video-mode-height vm)
	  (video-mode-bits-per-pixel vm)))

(defstruct video-mode (width 0) (height 0) (bits-per-pixel 0))

;; conversion methods

(defmethod translate-from-foreign (p (type video-mode-type))
  (copy-from-foreign 'video-mode-class p '(:struct sf-video-mode)))

(defmethod translate-into-foreign-memory ((vm video-mode-class) (type video-mode-type) p)
  (copy-to-foreign vm p '(:struct sf-video-mode) '(:unsigned-int :unsigned-int :unsigned-int)))


;; context settings

(defcstruct (sf-context-settings :class context-settings-type)
  (depth-bits :unsigned-int)
  (stencil-bits :unsigned-int)
  (antialiasing-level :unsigned-int)
  (major-version :unsigned-int)
  (minor-version :unsigned-int)
  (attribute-flags sf-uint-32))

;; lisp side struct

(defclass context-settings ()
  ((depth-bits
    :initarg :depth-bits
    :initform 0
    :accessor context-settings-depth-bits)
   (stencil-bits
    :initarg :stencil-bits
    :initform 0
    :accessor context-settings-stencil-bits)
   (antialiasing-level
    :initarg :antialiasing-level
    :initform 0
    :accessor context-settings-antialiasing-level)
   (major-version
    :initarg :major-version
    :initform 0
    :accessor context-settings-major-version)
   (minor-version
    :initarg :minor-version
    :initform 0
    :accessor context-settings-minor-version)
   (attribute-flags
    :initarg :attribute-flags
    :initform 0
    :accessor context-settings-attribute-flags)))

(defmethod translate-from-foreign (p (type context-settings-type))
  (copy-from-foreign 'context-settings p '(:struct sf-context-settings)))

(defmethod translate-into-foreign-memory ((context context-settings) (type context-settings-type) p)
  (copy-to-foreign context p '(:struct sf-context-settings)
		   '(:unsigned-int :unsigned-int :unsigned-int :unsigned-int :unsigned-int sf-uint-32)))

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
  
