(in-package :difference)

;;; Environment
(defparameter *width* 800)
(defparameter *height* 800)
(defparameter *frame-rate* 60)
(defparameter *show-turtle* t)
(defparameter *dashboard* t)
(defparameter *rainbow* nil)
(defparameter *keystack* nil)
(defparameter *dirty* nil)

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


(defclass turtle ()
  ((x-coordinate  :accessor turtle-x         :initarg :x         :reader check :initform (/ *width*  2))
   (y-coordinate  :accessor turtle-y         :initarg :y         :reader check :initform (/ *height* 2))
   (px-coordinate :accessor turtle-px        :initarg :px        :reader check :initform (/ *width*  2))
   (py-coordinate :accessor turtle-py        :initarg :py        :reader check :initform (/ *height* 2))
   (direction     :accessor turtle-direction :initarg :direction :reader check :initform       0       )
   (pen-state     :accessor pen-state        :initarg :pen-state :reader check :initform (/ *height* 2))
   (color         :accessor turtle-color     :initarg :color     :reader check :initform *stroke-color*)
   (surface       :accessor turtle-surface   :initarg :surface   :reader check :initform *canvas-surface*)))

;;; Initialize a turtle to start
(defparameter *turtle* (make-instance 'turtle
				      :x (/ *width* 2) :y (/ *height* 2)
				      :px 0 :py 0 :direction 0
				      :pen-state nil
				      :color *stroke-color*
				      :surface *canvas-surface*))

