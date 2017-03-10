#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-sfml))

(in-package :sfml)

(defun test-window ()
  ;;(with-foreign-string (title "hello world")
    (let* ((video-mode
	    (make-instance 'video-mode
			   :width 640 :height 480 :bits-per-pixel 32))
	   (style 7)
	   (title "hello world")
	   (context (make-context))
	   (window (make-render-window video-mode title style context))
	    ;; (make-instance
	    ;;  'render-window
	    ;;  :pointer (sf-render-window-create video-mode title style
	    ;; 				       (null-pointer))))
	    ;;(make-render-window video-mode title style (null-pointer)))
	   (event (make-instance 'event))
	   (mouse (make-instance 'mouse))
	   (font (make-font "arial.ttf"))
	   (text (make-text :string "hello world"
			    :size 28
			    :color (make-color 0 255 0 255)
			    :font font))
	   (circle (make-circle 25)))
      (setf (shape-position circle) (make-vector2 320.0 240.0))
      (setf (shape-fill-color circle) (make-color 255 0 0 255))
      (setf (entity-position text) (make-vector2 320.0 40.0))
      (setf (text-font text) font)
      (loop
	 while (window-is-open? window)
	 with prev-event = :sf-evt-none
	 with framerate = 60.0
	 with dt = (/ 1.0 framerate)
	 with clock = (make-clock)
	 with start-time = (clock-time-as-seconds clock)
	 if (let ((elapsed-time (clock-time-as-seconds clock)))
	      (> (- elapsed-time start-time) dt))
	 do
	   (setf start-time (clock-time-as-seconds clock))
	   (render-window-clear window (make-color 255 255 255 0))
	   (entity-draw circle window (null-pointer))
	   (entity-draw text window (null-pointer))
	   (window-display window)
	   (window-poll-event window event)
	   (when (not (eq prev-event (event-type event)))
	     (format t "prev event: ~A, current event: ~A~%"
		     prev-event (event-struct event))

	     (setf prev-event (event-type event)))
	   (cond ((is-key-pressed? :sf-key-left)
	   	  (entity-move circle (make-vector2 -1.0 0.0)))
	   	 ((is-key-pressed? :sf-key-right)
	   	  (entity-move circle (make-vector2 1.0 0.0)))
	   	 ((is-key-pressed? :sf-key-up)
	   	  (entity-move circle (make-vector2 0.0 -1.0)))
	   	 ((is-key-pressed? :sf-key-down)
	   	  (entity-move circle (make-vector2 0.0 1.0))))
	   (case (event-type event)
	     (:sf-evt-closed
	      (window-close window)
	      (window-destroy window)
	      (format t "received close signal, exiting~%")
	      )
	     (:sf-evt-key-pressed
	      ;; (format t "~A~%" (event-struct event)))
	      )
	     (:sf-evt-key-released
	      )
	     (:sf-evt-mouse-moved
	      )
	     (:sf-evt-mouse-button-pressed
	      (mouse-get-position window mouse)
	      (format t "mouse clicked at ~D,~D~%" (mouse-x mouse) (mouse-y mouse))
	      )
	     (:sf-evt-resized
	      )
	     (:t
	      ()))
	   (setf (event-type event) :sf-evt-none))))

(test-window)
