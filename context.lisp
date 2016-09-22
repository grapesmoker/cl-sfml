(in-package :sfml)

(defcfun ("sfContext_create" sf-context-create) :pointer)

(defcfun ("sfContext_destroy" sf-context-destroy) :void
  (context :pointer))

(defcfun ("sfContext_setActive" sf-context-set-active) :void
  (context :pointer)
  (active sf-bool))
