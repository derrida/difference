(in-package #:cl-user)

(defpackage #:difference
  (:use #:cl)
  (:nicknames #:d5)
  (:documentation "drawing api for common-lisp using lispbuilder-sdl.")
  (:export "main"
	   "setup"
	   "draw"))
