(in-package #:cl-user)

(defpackage #:difference
  (:use #:cl #:iterate)
  (:nicknames #:d5)
  (:documentation "drawing api for common-lisp using lispbuilder-sdl.")
  (:export "main"
	   "setup"
	   "draw"))