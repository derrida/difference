(defpackage #:difference-system
  (:use #:common-lisp #:asdf))

(in-package #:difference-system)

(asdf:defsystem #:difference
  :description "A functional drawing API for Common Lisp. Artists rejoice!"
  :version "0.0.3"
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
	       (:file "difference" :depends-on ("package"))))