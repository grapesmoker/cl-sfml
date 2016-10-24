(in-package :sfml)

(defcenum sf-blend-factor
  (:sf-blend-factor-zero 0)
  (:sf-blend-factor-one 1)
  (:sf-blend-factor-src-color 2)
  (:sf-blend-factor-one-minus-src-color 3)
  (:sf-blend-factor-dst-color 4)
  (:sf-blend-factor-one-minus-dst-color 5)
  (:sf-blend-factor-src-alpha 6)
  (:sf-blend-factor-one-minus-src-alpha 7)
  (:sf-blend-factor-dst-alpha 8)
  (:sf-blend-factor-one-minus-dst-alpha 9))

(defcenum sf-blend-equation
  (:sf-blend-equation-add 0)
  (:sf-blend-equation-subtract 1))

(defcstruct (sf-blend-mode :class blend-mode-type)
  (color-src-factor sf-blend-factor)
  (color-dst-factor sf-blend-factor)
  (color-equation sf-blend-equation)
  (alpha-src-factor sf-blend-factor)
  (alpha-dst-factor sf-blend-factor)
  (alpha-equation sf-blend-equation))

(defclass blend-mode ()
  ((color-src-factor :initarg :color-src-factor :initform 0 :accessor blend-mode-color-src-factor)
   (color-dst-factor :initarg :color-dst-factor :initform 0 :accessor blend-mode-color-dst-factor)
   (color-equation :initarg :color-equation :initform 0 :accessor blend-mode-color-equation)
   (alpha-src-factor :initarg :alpha-src-factor :initform 0 :accessor blend-mode-alpha-src-factor)
   (alpha-dst-factor :initarg :alpha-dst-factor :initform 0 :accessor blend-mode-alpha-dst-factor)
   (alpha-equation :initarg :alpha-equation :initform 0 :accessor blend-mode-alpha-equation)))

(defmethod print-object ((bm blend-mode) stream)
  (let* ((names (slot-names bm))
	 (values (mapcar #'(lambda (name) (slot-value bm name)) names))
	 (names-and-values (nreverse (pairlis names values))))
  (format stream "<BLEND-MODE [~{~A~^~%~}]>" names-and-values)))

(defun make-blend-mode (&optional
			  (color-src-factor :sf-blend-factor-zero)
			  (color-dst-factor :sf-blend-factor-zer)
			  (color-equation :sf-blend-equation-add)
			  (alpha-src-factor :sf-blend-factor-zero)
			  (alpha-dst-factor :sf-blend-factor-zero)
			  (alpha-equation :sf-blend-equation-add))
  (make-instance 'blend-mode
		 :color-src-factor color-src-factor
		 :color-dst-factor color-dst-factor
		 :color-equation color-equation
		 :alpha-src-factor alpha-src-factor
		 :alpha-dst-factor alpha-dst-factor
		 :alpha-equation alpha-equation))

(defmethod translate-from-foreign (p (type blend-mode-type))
  (copy-from-foreign 'blend-mode p '(:struct sf-blend-mode)))

(defmethod translate-into-foreign-memory ((bm blend-mode) (type blend-mode-type) p)
  (copy-to-foreign bm p '(:struct sf-blend-mode) '(:unsigned-int :unsigned-int :unsigned-int
						   :unsigned-int :unsigned-int :unsigned-int)))
