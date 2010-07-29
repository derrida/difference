(defpackage #:difference-system
  (:use #:common-lisp #:asdf))

(in-package #:difference-system)

(asdf:defsystem #:difference
  :description "A functional drawing API for Common Lisp. Artists rejoice!"
  :version "0.0.3"
  :depends-on (#:lispbuilder-sdl #:lispbuilder-sdl-gfx)
  :components ((:file "package")
	       (:file "environment" :depends-on ("package"))
	       (:file "turtle" :depends-on ("environment"))
	       (:file "color" :depends-on ("environment"))
	       (:file "difference" :depends-on ("environment"))
	       (:file "primitives" :depends-on ("difference"))
	       (:file "main" :depends-on ("difference" "primitives"))))
