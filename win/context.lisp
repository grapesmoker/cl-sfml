(in-package :sfml)

;; context settings

(defcstruct (sf-context-settings :class context-settings-type)
  (depth-bits :unsigned-int)
  (stencil-bits :unsigned-int)
  (antialiasing-level :unsigned-int)
  (major-version :unsigned-int)
  (minor-version :unsigned-int)
  (attribute-flags sf-uint-32))

;; lisp side struct

(defclass context-settings ()
  ((depth-bits
    :initarg :depth-bits
    :initform 0
    :accessor context-settings-depth-bits)
   (stencil-bits
    :initarg :stencil-bits
    :initform 0
    :accessor context-settings-stencil-bits)
   (antialiasing-level
    :initarg :antialiasing-level
    :initform 0
    :accessor context-settings-antialiasing-level)
   (major-version
    :initarg :major-version
    :initform 0
    :accessor context-settings-major-version)
   (minor-version
    :initarg :minor-version
    :initform 0
    :accessor context-settings-minor-version)
   (attribute-flags
    :initarg :attribute-flags
    :initform 0
    :accessor context-settings-attribute-flags)))

(defmethod translate-from-foreign (p (type context-settings-type))
  (copy-from-foreign 'context-settings p '(:struct sf-context-settings)))

(defmethod translate-into-foreign-memory ((context context-settings) (type context-settings-type) p)
  (copy-to-foreign context p '(:struct sf-context-settings)
		   '(:unsigned-int :unsigned-int :unsigned-int :unsigned-int :unsigned-int sf-uint-32)))


(defcfun ("sfContext_create" sf-context-create) :pointer)

(defcfun ("sfContext_destroy" sf-context-destroy) :void
  (context :pointer))

(defcfun ("sfContext_setActive" sf-context-set-active) :void
  (context :pointer)
  (active sf-bool))
