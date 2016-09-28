(in-package :sfml)

;; video mode type

(defcstruct (sf-video-mode :class video-mode-type)
  (width :unsigned-int)
  (height :unsigned-int)
  (bits-per-pixel :unsigned-int))

;; lisp-side class

(defclass video-mode ()
  ((width :initarg :width :initform 0 :accessor video-mode-width)
   (height :initarg :height :initform 0 :accessor video-mode-height)
   (bits-per-pixel :initarg :bits-per-pixel :initform 0
		   :accessor video-mode-bits-per-pixel)))

(defmethod print-object ((vm video-mode) stream)
  (format stream
	  "<VIDEO-MODE (~Dx~D), ~Dbpp>"
	  (video-mode-width vm)
	  (video-mode-height vm)
	  (video-mode-bits-per-pixel vm)))

;; conversion methods

(defmethod translate-from-foreign (p (type video-mode-type))
  (copy-from-foreign 'video-mode p '(:struct sf-video-mode)))

(defmethod translate-into-foreign-memory ((vm video-mode) (type video-mode-type) p)
  (copy-to-foreign vm p '(:struct sf-video-mode) '(:unsigned-int :unsigned-int :unsigned-int)))

;; functions

(defcfun ("sfVideoMode_getDesktopMode" sf-video-mode-get-desktop-mode) (:struct sf-video-mode))

(defcfun ("sfVideoMode_getFullscreenModes" sf-video-mode-get-fullscreen-modes)
    (:pointer (:struct sf-video-mode))
  (count :pointer))

(defcfun ("sfVideoMode_isValid" sf-video-mode-is-valid) :pointer
  (mode (:struct sf-video-mode)))


(defun video-mode-get-desktop-mode ()
  (sf-video-mode-get-desktop-mode))

(defun video-mode-get-fullscreen-modes ()
  (with-foreign-object (count-ptr :unsigned-long)
    (let* ((video-modes-pointer (sf-video-mode-get-fullscreen-modes count-ptr))
	   (count (mem-aref count-ptr :unsigned-long)))
      (loop
	 for i upto count
	 collect
	   (mem-aref video-modes-pointer '(:struct sf-video-mode) i)))))
