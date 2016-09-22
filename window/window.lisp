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
  
