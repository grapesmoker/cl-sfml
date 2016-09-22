(in-package :sfml)

;; (defmacro copy-to-foreign (value ptr ptr-type slot-types)
;;   `(let* ((class-alist (to-alist ,value))
;; 	  (slot-values
;; 	   (mapcar #'(lambda (slot-value)
;; 		       (cdr slot-value))
;; 		   class-alist))
;; 	  (slot-pointers
;; 	   (mapcar #'(lambda (slot-value)
;; 		       (list :pointer (car slot-value)))
;; 		   class-alist)))
;;      `(loop
;; 	for slot-name in ',(mapcar #'cadr slot-pointers)
;; 	for slot-value in ',slot-values
;; 	for slot-type in ',,slot-types
;; 	do
;; 	   (let ((slot-pointer (foreign-slot-pointer ,,ptr ',,ptr-type slot-name)))
;; 	     (setf (mem-aref slot-pointer slot-type) slot-value)))))

(defun copy-to-foreign (value ptr ptr-type slot-types)
  (let* ((class-alist (to-alist value))
	 (slot-values (mapcar #'cdr class-alist))
	 (slot-names (mapcar #'car class-alist)))
    (loop
       for slot-name in slot-names
       for slot-value in slot-values
       for slot-type in slot-types
       do
	 (setf (foreign-slot-value ptr ptr-type slot-name) slot-value))))

(defun copy-from-foreign (value-class ptr ptr-type)
  (let* ((slot-names
	  (mapcar #'slot-definition-name (class-slots (find-class value-class))))
	 (foreign-slot-values
	  (loop
	     for slot-name in slot-names
	     collect
	       (foreign-slot-value ptr ptr-type slot-name)))
	 (new-value (make-instance value-class)))
    (loop
       for slot-name in slot-names
       for foreign-value in foreign-slot-values
       do
	 (setf (slot-value new-value slot-name) foreign-value))
    new-value))
