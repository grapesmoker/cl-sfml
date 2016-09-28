(defpackage #:cl-sfml
  (:nicknames sfml)
  (:use #:cl #:cffi #:cl-mop #:cl-ppcre))

(in-package :sfml)

(define-foreign-library libcsfml-system
  (:unix (:or "libcsfml-system.so"))
  (:t (:default "libcsfml-system")))

(define-foreign-library libcsfml-window
  (:unix (:or "libcsfml-window.so"))
  (:t (:default "libcsfml-window")))

(define-foreign-library libcsfml-graphics
  (:unix (:or "libcsfml-graphics.so"))
  (:t (:default "libcsfml-graphics")))

(define-foreign-library libcsfml-network
  (:unix (:or "libcsfml-network.so"))
  (:t (:default "libcsfml-network")))

(define-foreign-library libcsfml-audio
  (:unix (:or "libcsfml-audio.so"))
  (:t (:default "libcsfml-audio")))

(use-foreign-library libcsfml-window)
(use-foreign-library libcsfml-graphics)

(defpackage #:cl-sfml-grovel
  (:nicknames sfml-grovel)
  (:use #:cl #:cffi))
