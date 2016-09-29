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


  
