;;;;;;;;;;;;;;;;;;;;;;;;;
;;     difference      ;;
;;    a drawing api    ;;
;;.....................;;
;;   difference.lisp   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:difference)

;;; Structures
(defstruct turtle
  (x  0) (y  0)
  (px 0) (py 0)
  (x1 0) (y1 0)
  (direction 0)
  (pen-state nil)
  (poly-state nil)
  (color sdl:*black*))

;;; Environment
(defparameter *width* 400)
(defparameter *height* 400)
(defparameter *frame-rate* 30)

;;; Surfaces
(defparameter *current-surface* sdl:*default-surface*)
(defparameter *canvas-surface* nil)

;;; Colors
(defparameter *background-color* sdl:*black*)
(defparameter *fill-color* sdl:*white*)
(defparameter *stroke-color* sdl:*blue*)
(defparameter *font-color* sdl:*red*)
(defparameter *last-color* nil)

;;; Font
(defparameter *last-font* nil)
(defparameter *current-font* sdl:*FONT-5X7*)
(defparameter *random-color* sdl:*black*)

;;; Numbers
(defparameter *random-number* (random 1000))

;;; Polygons
; Draw list of 3D convex polygons
(defparameter *polygons* '(
  ((:r 78 :g 114 :b 114)
   (0 -1 0)
   (0 -1 1)
   (-1 -2 1)
   (-1 -2 0))
  ((:r 78 :g 114 :b 114)
   (3 0 0)
   (3 0 1)
   (1 -2 1)
   (1 -2 0))
  ((:r 78 :g 114 :b 114)
   (0 3 0)
   (0 3 1)
   (3 0 1)
   (3 0 0))
  ((:r 255 :g 85 :b 125)
   (0 3 0)
   (0 -1 0)
   (1 -2 0)
   (3 0 0))
  ((:r 255 :g 85 :b 125)
   (0 3 0)
   (-3 0 0)
   (-1 -2 0)
   (0 -1 0))))


;;; Initialize a turtle to start
(defparameter *turtle*
  (make-turtle :x (/ *width* 2)
	       :y (/ *height* 2)
	       :px 0
	       :py 0
	       :direction 0
	       :pen-state nil
	       :poly-state nil
	       :color *stroke-color*))

;;; Getters
(defun get-x ()
  (round (turtle-x *turtle*)))

(defun get-y ()
  (round (turtle-y *turtle*)))

(defun get-x1 ()
  (round (turtle-x1 *turtle*)))

(defun get-y1 ()
  (round (turtle-y1 *turtle*)))

(defun get-px ()
  (round (turtle-px *turtle*)))

(defun get-py ()
  (round (turtle-py *turtle*)))

(defun get-direction ()
  (round (turtle-direction *turtle*)))

(defun get-pen-state ()
  (turtle-pen-state *turtle*))

(defun get-poly-state ()
  (turtle-poly-state *turtle*))

;;; Predicates
(defun nilp (object)
  (eq object nil))

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
      (when (and (turtle-poly-state *turtle*) (or (/= x (get-px)) (/= y (get-y1))))
	(sdl-gfx:draw-filled-trigon (sdl:point :x (get-px)
					       :y (get-py))
				    (sdl:point :x x
					       :y y)
				    (sdl:point :x (get-x)
					       :y (get-y))
				    :surface *canvas-surface*))
      (when *pen-state*
	(sdl:draw-line-* x y (get-x) (get-y) :surface *canvas-surface*))))
  (update))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))


(defun update ()
  (sdl:blit-surface *canvas-surface*)
  ;; (setf *x* (round (turtle-x *turtle*)))
  ;; (setf *y* (round (turtle-y *turtle*)))
  ;; (setf *direction* (round (turtle-direction *turtle*)))
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
  (setf (turtle-pen-state *turtle*) t))

(defun pen-up ()
  (setf (turtle-pen-state *turtle*) nil))

(defun poly-down ()
  (setf (turtle-px *turtle*) (turtle-x *turtle*))
  (setf (turtle-py *turtle*) (turtle-y *turtle*))
  (setf (turtle-poly-state *turtle*) t))

(defun poly-up ()
  (setf (turtle-poly-state *turtle*) nil))

(defun draw-turtle ()
  (poly-up)
  (dolist (poly *polygons*)
    (setf sdl:*default-color* (apply #'sdl:color (car poly)))
    (apply #'move-to-3d (second poly))
    (poly-down)
    (dolist (vert (cddr poly))
      (apply #'move-to-3d vert))
    (poly-up))
  (setf sdl:*default-color* (sdl:color :r 0 :g 0 :b 0 :a 40))
  (move-to 0 0)
  (poly-down)
  (move-to 0 *height*)
  (move-to *width* *height*)
  (move-to *width* 0)
  (poly-up))

(defun move-to (x y)
  (let ((dx (- x (get-x)))
        (dy (- y (get-y))))
    (right (- (* (atan dx dy) (/ 180 pi)) (get-direction)))
    (forward (sqrt (+ (* dx dx) (* dy dy))))))

(defun move-to-3d (x y z)
  ; 3D transform
  (incf z -0.5)
  (let ((rotx 0) (roty (- (* (sin (* (sdl:sdl-get-ticks) 0.0035)) (/ pi 10)) (/ pi 6))) (rotz 0))
    (let ((cosx (cos rotx)) (sinx (sin rotx))
          (cosy (cos roty)) (siny (sin roty))
          (cosz (cos rotz)) (sinz (sin rotz)))
      (let ((d (+ (* sinz y) (* cosz x)))
            (f (+ (* cosz y) (* sinz x))))
        (let ((e (+ (* cosy z) (* siny d))))
          (setf x (- (* cosy d) (* siny z)))
          (setf y (+ (* sinx e) (* cosx f)))
          (setf z (- (* cosx e) (* sinx f)))))))
  (incf x -0.3)
  (incf y (+ -0.5 (* (sin (* (sdl:sdl-get-ticks) 0.006)) 0.12)))
  (incf z 5)
  ; Projection
  (let ((dx (/ (* x *width*) (* z 2.0)))
        (dy (/ (* y *height*) (* z 2.0))))
    ; Move turtle by dx,dy
    (move-to (+ dx (/ *width* 2)) (+ dy (/ *height* 2)))))


;;; Drawing Functions
(defun clear ()
  (sdl:clear-display *background-color*))

;;; Setters
(defmacro frame-rate (fps)
  `(setf *frame-rate* ,fps))

;;; Shape Primitives
(defun pixel (x y
	      &key (color *stroke-color*) (surface sdl:*default-surface*))
  (sdl:draw-pixel-* x y
		    :color color
		    :surface surface))

(defun point (x y)
  (sdl:point :x x
	     :y y))

(defun circle (x y r
	       &key (surface *current-surface* ) (color *stroke-color*) (alpha nil) (aa nil))
  (sdl:draw-circle-* x y r
		     :surface surface
		     :color color
		     :alpha alpha
		     :aa aa))

(defun rect (x y w h
	     &key (surface sdl:*default-surface*) (color *stroke-color*))
  (sdl:draw-rectangle-* x y w h
			:surface surface
			:color color))

(defun triangle (x1 y1 x2 y2 x3 y3
		 &key (surface sdl:*default-surface*) (color *stroke-color*))
  (sdl:draw-trigon (sdl:point :x (round (+ 0 x1))
			      :y (round (+ 0 y1)))
		   (sdl:point :x (round (+ 0 x2))
			      :y (round (+ 10 y2)))
		   (sdl:point :x (round (+ 10 x3))
			      :y (round (+ 0 y3)))
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
(defun range (&optional (start 0) (end 100))
  (loop for i from start to end collecting i))

;; OS Integration ; Runs a shell function, sbcl dependant. eg. (exec "ls" "/etc/")
(defun exec (name &rest args)
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

;;; Main
(defmethod main ()
  (sdl:with-init ()
    (sdl:window *width* *height* :title-caption "difference")
    (setf (sdl:frame-rate) *frame-rate*)
    (setf *canvas-surface*
          (sdl:convert-to-display-format
	   :surface (sdl:create-surface *width* *height*)
	   :free t))
    (sdl:enable-key-repeat 100 50)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (if (sdl:key-pressed-p :SDL-KEY-ESCAPE) (sdl:push-quit-event))
		       (if (sdl:key-pressed-p :SDL-KEY-UP) (forward 5))
		       (if (sdl:key-pressed-p :SDL-KEY-DOWN) (backward 5))
		       (if (sdl:key-pressed-p :SDL-KEY-LEFT) (left))
		       (if (sdl:key-pressed-p :SDL-KEY-RIGHT) (right)))
      (:idle ()
	     ;; Draw Block
	     (pen-down)
	     (forward (random 20))
	     (right (random 9))
	     (right (random 5))
	     (draw-turtle)
	     (sdl:update-display)))))

;; Setup
(defun setup ()
  (clear))
