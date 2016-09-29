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
	   (circle (make-circle 10)))
      (setf (shape-position circle) (make-vector2 320.0 240.0))
      (setf (shape-fill-color circle) (make-color 255 0 0 255))
      (update-circ circle)
      (loop
	 while (window-is-open? window)
	 with prev-event = :sf-evt-none
	 do
	   ;; (render-window-clear window (make-color 128 128 128 0))
	   (entity-draw circle window (null-pointer))
	   (window-display window)
	   (window-poll-event window event)
	   (when (not (eq prev-event (event-type event)))
	     (format t "prev event: ~A, current event: ~A~%"
		     prev-event (event-struct event))
	     (format t "circle position is: ~A~%" (shape-position circle))
	     (setf prev-event (event-type event)))
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
