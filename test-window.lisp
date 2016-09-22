#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-sfml))

(in-package :sfml)

(defun test-window ()
  (with-foreign-string (title "hello world")
    (let* ((video-mode
	    (make-instance 'video-mode-class
			   :width 640 :height 480 :bits-per-pixel 32))
	   (style 7)
	   (window (sf-window-create video-mode title style (null-pointer))))
      (loop
	 while (sf-window-is-open window)
	 do
	   (with-foreign-object (event '(:union sf-event))
	     (sf-window-poll-event window event)
	     (let ((lisp-event (convert-from-foreign
				event
				'(:pointer (:union sf-event))))
		   (event-code
		    (mem-ref
		     (foreign-slot-pointer event '(:union sf-event) 'type)
		     :int)))
	       (if (= event-code (foreign-enum-value 'sf-event-type :sf-evt-closed))
		   (progn
		     (sf-window-close window)
		     (sf-window-destroy window)
		     (format t "event: ~A~%" lisp-event)
		     (format t "received close signal, exiting~%")))))))))   

(test-window)
