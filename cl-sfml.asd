(asdf:defsystem #:cl-sfml
  :description "Common Lisp bindinds for the CSFML library"
  :author "Jerry Vinokurov <grapesmoker@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-autowrap #:cffi #:cffi-libffi #:cl-mop #:cl-ppcre)
  :components ((:file "package")
	       (:file "sfml")
	       (:file "utils")
	       (:module window
			:components ((:file "types")
				     (:file "video-mode")
				     (:file "keyboard")
				     (:file "mouse")
				     (:file "joystick")
				     (:file "touch")
				     (:file "sensor")
				     (:file "event")
				     (:file "window")))
	       (:file "time")
	       (:file "test" :depends-on (window))))
