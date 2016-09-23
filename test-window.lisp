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
		    :pointer (sf-window-create video-mode title style (null-pointer)))))
      (loop
	 while (window-is-open? window)
	 do
	   (let ((event (window-poll-event window (make-instance 'event))))
	     (when event
	       (if (eq (event-type event) :sf-evt-closed)
		   (progn
		     (window-close window)
		     (window-destroy window)
		     (format t "event: ~A~%" event)
		     (format t "received close signal, exiting~%")))))))))

(test-window)
