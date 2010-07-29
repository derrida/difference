(in-package :difference)
;;(defclass environment ()
;; (width         :accessor x            :initarg :x         :reader check :initform (/ *width*  2))
;; (height        :accessor y            :initarg :y         :reader check :initform (/ *height* 2))
;; (frame-rate    :accessor px           :initarg :px        :reader check :initform      nil      )
;; (dashboard     :accessor py           :initarg :py        :reader check :initform      nil      )
;; (background-color :accessor direction    :initarg :direction :reader check :initform (/ *width*  2))
;; (anti-aliasing :accessor pen-state    :initarg :pen-state :reader check :initform (/ *height* 2)))

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
(defparameter *background-color* sdl:*white*)
(defparameter *fill-color* sdl:*green*)
(defparameter *stroke-color* sdl:*black*)
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