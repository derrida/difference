;;;;;;;;;;;;;;;;;;;;;;;;;
;;     difference      ;;
;;    a drawing api    ;;
;;.....................;;
;;   difference.lisp   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:difference)

;;; Structures
(defstruct turtle
  (x 0) (px 0)
  (y 0) (py 0)
  (direction 0)
  (pen-state nil)
  (poly-state nil)
  (color sdl:*black*))

;;; Environment
(defparameter *width* 400)
(defparameter *height* 400)
(defparameter *frame-rate* 30)

;;; Initialize a turtle to start
(defparameter *turtle*
  (make-turtle :x (/ *width* 2)
	       :y (/ *height* 2)
	       :direction 0
	       :pen-state nil
	       :poly-state nil
	       :color sdl:*black*))

;;; Colors
(defparameter *background-color* sdl:*black*)
(defparameter *fill-color* sdl:*white*)
(defparameter *stroke-color* sdl:*blue*)
(defparameter *font-color* sdl:*white*)
(defparameter *last-color* nil)

;;; Font
(defparameter *last-font* nil)
(defparameter *current-font* sdl:*FONT-5X7*)
(defparameter *random-color* sdl:*black*)

;;; Numbers
(defparameter *random-number* (random 1000))

;;; Getters
(defun get-x ()
  (round (turtle-x *turtle*)))

(defun get-y ()
  (round (turtle-y *turtle*)))

(defun get-direction ()
  (round (turtle-direction *turtle*)))

(defun get-pen-state ()
  (turtle-x *turtle*))

;;; Predicates
(defun nilp (object)
  (eq object nil))

;;; Variables
(defparameter *x* (round (get-x)))
(defparameter *y* (round (get-y)))
(defparameter *direction* (round (get-direction)))
(defparameter *pen-state* (get-pen-state))

;;; Surfaces
(defparameter *current-surface* sdl:*default-surface*)
(defvar *canvas-surface*)
(defvar *turtle-surface*)

;;; Turtle Functions
(defun left (&optional (degrees 1))
  (mod (decf (turtle-direction *turtle*) degrees) 360)
  (update))

(defun right (&optional (degrees 1))
  (mod (incf (turtle-direction *turtle*) degrees) 360)
  (update))

(defun forward (steps)
  (let ((angle (* (turtle-direction *turtle*) (/ pi 180.0))))
    (let ((dx (* (sin angle) steps))
	  (dy (* (cos angle) steps))
	  (x (get-x))
	  (y (get-y)))
      (incf (turtle-x *turtle*) dx)
      (incf (turtle-y *turtle*) dy)
      (when *pen-state*
	(sdl:draw-line-* x y (get-x) (get-y) :surface *canvas-surface*))))
  (update))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))

(defun update ()
  (sdl:blit-surface *canvas-surface*)
  (setf *x* (round (turtle-x *turtle*)))
  (setf *y* (round (turtle-y *turtle*)))
  (setf *direction* (round (turtle-direction *turtle*)))
  (let ((angle (* (turtle-direction *turtle*) (/ pi 180.0))))
    (let ((sin1 (sin angle))
          (cos1 (cos angle)))
      (sdl:draw-trigon (sdl:point :x (get-x) :y (get-y))
                       (sdl:point :x (+ (get-x) (* 5 cos1) (* -5 sin1))
                                  :y (+ (get-y) (* -5 sin1) (* -5 cos1)))
                       (sdl:point :x (+ (get-x) (* -5 cos1) (* -5 sin1))
                                  :y (+ (get-y) (* 5 sin1) (* -5 cos1))))))
  *turtle*)


(defun pen-down ()
  (setf (turtle-pen-state *turtle*) t)
  (setf *pen-state* (turtle-pen-state *turtle*)))

(defun pen-up ()
  (setf (turtle-pen-state *turtle*) nil)
  (setf *pen-state* (turtle-pen-state *turtle*)))

(defun draw-turtle ()
  (pen-down)
  (forward 20)
  (right (random 90))
  (right (random 30)))

;;; Main
(defmethod main ()
  (sdl:with-init ()
    (sdl:window *width* *height* :title-caption "difference")
    (setf (sdl:frame-rate) *frame-rate*)
    ;; Setup Block
					; (setup)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (if (sdl:key-pressed-p :SDL-KEY-ESCAPE) (sdl:push-quit-event))
		       (if (sdl:key-pressed-p :SDL-KEY-UP) (move-north))
		       (if (sdl:key-pressed-p :SDL-KEY-DOWN) (move-south))
		       (if (sdl:key-pressed-p :SDL-KEY-LEFT) (left))
		       (if (sdl:key-pressed-p :SDL-KEY-RIGHT) (right))
		       (if (sdl:key-pressed-p :SDL-KEY-0) (setf *current-weapon* 'fist))
      (:idle ()
	     ;; Draw Block
	     (draw-turtle)
	     (sdl:update-display)))))

;; Setup
(defun setup ()
  (sdl:enable-key-repeat 100 50)
  (clear))

;;; Drawing Functions
(defun clear ()
  (sdl:clear-display *background-color*))

;;; Setters
(defmacro frame-rate (fps)
  `(setf *frame-rate* ,fps))

;;; Shape Primitives

(defun pixel (x y
	      &key (color *stroke-color*) (surface sdl:*default-surface*))
  (sdl:draw-pixel-* x y :color color :surface surface))

(defun point (x y)
  (sdl:point :x x :y y))

(defun circle (x y r
	       &key (surface *current-surface* ) (color *stroke-color*) (alpha nil) (aa nil))
  (sdl:draw-circle-* x y r :surface surface :color color :alpha alpha :aa aa))

(defun rect (x y w h
	     &key (surface sdl:*default-surface*) (color *stroke-color*))
  (sdl:draw-rectangle-* x y w h :surface surface :color color))

(defun triangle (x1 y1 x2 y2 x3 y3
		 &key (surface sdl:*default-surface*) (color *stroke-color*))
  (sdl:draw-trigon (sdl:point :x (round (+ 0 x1))  :y (round (+ 0 y1)))
		   (sdl:point :x (round (+ 0 x2))  :y (round (+ 10 y2)))
		   (sdl:point :x (round (+ 10 x3)) :y (round (+ 0 y3)))
		   :surface surface 
		   :color color))

(defun ellipse (x y rx ry)
  (sdl:draw-ellipse-* x y rx ry 
		      :surface sdl:*default-surface* 
		      :color *stroke-color*))

(defun line (x0 y0 x1 y1
	     &key (color *stroke-color*) (surface sdl:*default-surface*) (clipping nil))
  (sdl:draw-line-* x0 y0 x1 y1 
		   :surface surface 
		   :color color 
		   :clipping clipping))

(defun bezier (vertices
	       &key (color *stroke-color*) (segments 20) (style :solid))
  (sdl:draw-bezier vertices 
		   :color color
		   :segments segments
		   :style style))

(defun polygon (vertices
		&key (color *stroke-color*) (surface sdl:*default-surface*) (clipping t))
  (sdl:draw-polygon vertices 
		    :surface surface 
		    :color color 
		    :clipping clipping))


(defun letter (index)
  (let ((alphabet (loop for i from 97 to 122
		     collecting (code-char i))))
    (format nil "~a"
	    (nth (- index 1) alphabet))))

(defun random-element (lst)
  (nth (random (length lst)) lst))

(defun random-number (&key (base 100))
  (random base))

(defun random-color (&key (red (random 255)) (green (random 255)) (blue (random 255)))
  (sdl:color :r red
	     :g green
	     :b blue))

;;; Color
(defun stroke (&key (r 0) (g 0) (b 0))
  (setf *stroke-color* (sdl:color :r r :g g)))

;; Iteration
;; requires iterate package
	
(defun range (&optional (start 0) (end 100))
  (loop for i from start to end collecting i))


;; OS Integration
(defun exec (name
	     &rest args)
  (sb-ext:run-program name args
		      :output *standard-output*
		      :search t))

;; I/O
(defun cat (filename)
  (with-open-file (str filename :direction :input)
    (do ((line (read-line str nil 'eof)
	       (read-line str nil 'eof)))
	((eql line 'eof))
      (format t "~A~%" line))))

;; Common Inputs
(defun ask (question expected-type)
  (format t "~A " question)
  (let ((response (read)))
    (if (typep response expected-type)
	response
	(ask (format t "~A " question) expected-type))))