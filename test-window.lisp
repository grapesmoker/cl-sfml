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
	   (event (make-instance 'event)))
      (loop
	 while (window-is-open? window)
	 do
	   (window-poll-event window event)
	   (case (event-type event)
	     (:sf-evt-closed
	      (progn
		(window-close window)
		(window-destroy window)
		(format t "event: ~A~%" (event-type event))
		(format t "received close signal, exiting~%")))
	     (:sf-evt-key-pressed
	      (progn ()))
		;; (format t "~C was pressed~%" (code-char (key-event-code (event-struct event))))))
	     (:sf-evt-key-released
	      (progn ()))
		;; (format t "~C was released~%" (code-char (key-event-code (event-struct event))))))
	     (:t
	      ()))
	   (setf (event-type event) :sf-evt-none)))))

(test-window)
