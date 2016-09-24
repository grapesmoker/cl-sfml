#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-sfml))

(in-package :sfml)

(defun test-window ()
  (with-foreign-string (title "hello world")
    (let* ((video-mode
	    (make-instance 'video-mode
			   :width 640 :height 480 :bits-per-pixel 32))
	   (style 7)
	   (window (make-instance
		    'window
		    :pointer (sf-window-create video-mode title style (null-pointer))))
	   (event (make-instance 'event))
	   (mouse (make-instance 'mouse)))
      (loop
	 while (window-is-open? window)
	 with prev-event = :sf-evt-none
	 do
	   (window-poll-event window event)
	   (when (not (eq prev-event (event-type event)))
	     (format t "prev event: ~A, current event: ~A~%" prev-event (event-struct event))
	     (setf prev-event (event-type event)))
	   (case (event-type event)
	     (:sf-evt-closed
	      (progn
		(window-close window)
		(window-destroy window)
		(format t "event: ~A~%" (event-type event))
		(format t "received close signal, exiting~%")))
	     (:sf-evt-key-pressed
	      ;; (format t "~A~%" (event-struct event)))
	      )
	     (:sf-evt-key-released
	      (progn ()))
	     (:sf-evt-mouse-moved
	      ;; (sf-mouse-get-position (window-pointer window))
	      ;;(format t "~A~%" (event-struct event))
	      )
	     (:sf-evt-mouse-button-pressed
	      ;;(format t "~A~%" (event-struct event))
	      )
	     (:t
	      ()))
	   (setf (event-type event) :sf-evt-none)))))

(test-window)
