(asdf:defsystem #:cl-sfml
  :description "Common Lisp bindinds for the CSFML library"
  :author "Jerry Vinokurov <grapesmoker@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-autowrap #:cffi #:cffi-libffi #:cl-mop #:cl-ppcre)
  :components ((:file "package")
	       (:file "sfml")
	       (:file "utils")
	       (:module system
			:components ((:file "time")
				     (:file "vectors")))
	       (:module win
			;; :depends-on (system)
			:components ((:file "types")
				     (:file "video-mode")
				     (:file "keyboard")
				     (:file "mouse")
				     (:file "joystick")
				     (:file "touch")
				     (:file "sensor")
				     (:file "event")
				     (:file "window"))))) ;; :depends-on (mouse))))))
