(in-package :sfml)

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

;; functions

(defcfun ("sfVideoMode_getDesktopMode" sf-video-mode-get-desktop-mode) (:struct sf-video-mode))

(defcfun ("sfVideoMode_getFullscreenModes" sf-video-mode-get-fullscreen-modes) :pointer
  (count :int))

(defcfun ("sfVideoMode_isValid" sf-video-mode-is-valid) :pointer
  (mode (:struct sf-video-mode)))
