(defpackage #:difference-system
  (:use #:cl #:asdf))

(in-package #:difference-system)

(asdf:defsystem #:difference
  :description "A functional drawing API for Common Lisp. Artists rejoice!"
  :version "0.0.3"
  :depends-on (#:lispbuilder-sdl #:bordeaux-threads)
  :components ((:file "package")
	       (:file "difference" :depends-on ("package"))
	       (:file "turtle" :depends-on ("difference"))
	       (:file "utilities" :depends-on ("turtle"))
	       (:file "matrix" :depends-on ("turtle"))))
