(in-package :sfml)

(defcstruct sf-font-info
  (family :pointer))

;; You can't really modify the font in any way after it's loaded, so all
;; this does is wrap the pointer in a lispy interface.

(defclass font ()
  ((pointer :initarg :pointer :initform nil :accessor font-pointer)))

(defcfun ("sfFont_createFromFile" sf-font-create-from-file) :pointer
  (filename :string))

(defun make-font (filename)
  (with-foreign-string (fn filename)
    (make-instance 'font :pointer
		   (sf-font-create-from-file fn))))

;; same caveat as applies to the analogous function for sfImage holds here

(defcfun ("sfFont_createFromMemory" sf-font-create-from-memory) :pointer
  (data :pointer)
  (size-in-bytes :unsigned-int))

(defcfun ("sfFont_copy" sf-font-copy) :pointer
  (font :pointer))

(defmethod font-copy ((f font))
  (let ((fp (sf-font-copy (font-pointer f))))
    (make-instance 'font :pointer fp)))

(defcfun ("sfFont_destroy" sf-font-destroy) :void
  (font :pointer))

(defmethod font-destroy ((f font))
  (sf-font-destroy (font-pointer f)))

(defcfun ("sfFont_getGlyph" sf-font-get-glyph) (:struct sf-glyph)
  (font :pointer)
  (code-point sf-uint-32)
  (character-size :unsigned-int)
  (bold sf-bool))

(defmethod font-get-glyph ((f font) (code-point integer)
			   (character-size integer) bold)
  (sf-font-get-glyph (font-pointer f)
		     code-point
		     character-size
		     bold))

(defcfun ("sfFont_getKerning" sf-font-get-kerning) :float
  (font :pointer)
  (first sf-uint-32)
  (second sf-uint-32)
  (character-size :unsigned-int))

(defmethod font-kerning ((f font) (first integer)
			     (second integer) (character-size integer))
    (sf-font-get-kerning (font-pointer f) first second character-size))

(defcfun ("sfFont_getLineSpacing" sf-font-get-line-spacing) :float
  (font :pointer)
  (character-size :unsigned-int))

(defmethod font-line-spacing ((f font) (character-size integer))
  (sf-font-get-line-spacing (font-pointer f) character-size))

(defcfun ("sfFont_getUnderlinePosition" sf-font-get-underline-position) :float
  (font :pointer)
  (character-size :unsigned-int))

(defmethod font-underline-position ((f font) (character-size integer))
  (sf-font-get-underline-position (font-pointer f) character-size))

(defcfun ("sfFont_getUnderlineThickness" sf-font-get-underline-thickness) :float
  (font :pointer)
  (character-size :unsigned-int))

(defmethod font-underline-thickness ((f font) (character-size integer))
  (sf-font-get-underline-thickness (font-pointer f) character-size))

(defcfun ("sfFont_getTexture" sf-font-get-texture) :pointer
  (font :pointer)
  (character-size :unsigned-int))

;; (defmethod font-texture ((f font) (character-size integer))
;;  (sf-font-get-texture 
