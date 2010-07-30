(in-package :difference)

;;; Environment
(defparameter *width* 800)
(defparameter *height* 800)
(defparameter *frame-rate* 60)
(defparameter *show-turtle* t)
(defparameter *dashboard* t)
(defparameter *rainbow* nil)

;;; Surface Parameters
(defparameter *current-surface* sdl:*default-surface*)
(defparameter *canvas-surface* nil)

;;; Color Parameters
(defparameter *background-color* sdl:*black*)
(defparameter *fill-color* sdl:*green*)
(defparameter *stroke-color* sdl:*white*)
(defparameter *font-color* sdl:*blue*)
(defparameter *last-color* nil)

;;; Bezier Parameters
(defparameter *bezier-segments* 20)
(defparameter *bezier-style* :solid)

;;; Font
(defparameter *last-font* nil)
(defparameter *unicode* t)
(defparameter *anti-aliasing* t)
(defparameter *current-font* sdl:*FONT-5X7*)
(defparameter *random-color* sdl:*black*)

;;; Numbers
(defparameter *random-number* (random 1000))

;;; Sample Data
(defparameter *sample-vertices* (list 10 10 20 20 30 30 50 50 80 60 80 40 70 50))