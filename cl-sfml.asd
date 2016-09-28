(asdf:defsystem #:cl-sfml
  :description "Common Lisp bindinds for the CSFML library"
  :author "Jerry Vinokurov <grapesmoker@gmail.com>"
  :license "MIT"
  :serial t
  ;; :defsystem-depends-on ("cffi-grovel")
  :depends-on (#:cl-autowrap #:cffi #:cffi-libffi #:cl-mop #:cl-ppcre #:cffi-grovel)
  :components ((:file "package")
	       (:file "sfml")
	       ;; (:cffi-grovel-file "sfml-grovel")
	       (:file "utils")
	       (:module system
			:components ((:file "time")
				     (:file "vectors")))
	       (:module win
			;; :depends-on (system)
			:components ((:file "types")
				     (:file "video-mode")
				     (:file "context")
				     (:file "keyboard")
				     (:file "mouse")
				     (:file "joystick")
				     (:file "touch")
				     (:file "sensor")
				     (:file "event")
				     (:file "window")))
	       (:module graphics
			:components ((:file "blend-mode")
				     (:file "color")
				     (:file "vertex")
				     (:file "primitive-type")
				     (:file "rect")
				     (:file "transform")
				     (:file "entity")
				     (:file "shape")
				     (:file "rectangle-shape")
				     (:file "circle-shape")
				     (:file "convex-shape")
				     ))))
