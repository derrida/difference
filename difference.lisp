;; difference                                          ;;
;; a drawing api                                       ;;
;;                                                     ;;
;; difference.lisp                                     ;;
;;                                                     ;;
;; Goal:                                               ;;
;; (defun main()                                       ;;
;;   (setup(setup your sketch's initial runtime here)) ;;
;;   (draw(adapt a drawing loop)))                     ;;
;;                                                     ;;
;; enjoy! \o/ rage!  jabbar: mgs@informalcode.com      ;;
;;-----------------------------------------------------;;
(in-package #:difference)

;;; Environment
(defparameter *window-height*  800)
(defparameter *window-width*  800)
(defparameter *frame-rate*  60)

;;; Toggles (Booleans)
(defvar *smoothing* nil)
(defvar *anti-aliasing* nil)
(defvar *cursor-visible* nil)
(defvar *unicode* nil)
(defvar *show-fps* nil)

;;; Colors
(defparameter *background-color* sdl:*black*)
(defparameter *fill-color* sdl:*white*)
(defparameter *stroke-color* sdl:*blue*)
(defparameter *font-color* sdl:*white*)
(defparameter *last-color* nil)

;;; Font
(defparameter *current-font* sdl:*FONT-5X7*)
(defparameter *last-font* nil)
(defparameter *random-color* sdl:*black*)

;;; Numbers
(defparameter *random-number* (random 1000))

;;; Surfaces
(defparameter *current-surface*  sdl:*default-surface*)

;;; Structures
(defstruct point 
  x
  y
  z)

;;; Cursor
(defstruct cursor
  x
  y)

;;; Turtle
(defstruct turtle
  x
  y
  z
  heading)

;;; TVector
(defstruct tvector
  x y)

;;; 3x2 Affine Matrix
(defstruct matrix
  m00 m01 m02
  m10 m11 m12)


;;; Main
(defmethod main ()
  (sdl:with-init ()
    (sdl:window *window-width* *window-height* :title-caption "difference")
    (setf (sdl:frame-rate) *frame-rate*)
    ;; Setup Block
    (setup)
    (sdl:with-events ()
      (:quit-event () t)      
      (:key-down-event ()		 
		       (bind-movement-keys)
		       (bind-numeric-keys)
		       (bind-fire-key)
		       (bind-quit-key))
      (:idle ()
	     ;; Draw Block
	     (draw) 
	     (sdl:update-display)))))

;; Setup
(defun setup ()
  (sdl:enable-key-repeat 100 50)
  (clear))
  
;; Draw
(defun draw (&aux (x (round (get-turtle-x))) (y (round (get-turtle-y))))
  (clear)
  (draw-turtle :x x :y y))

;;; Keybindings
(defun bind-quit-key ()
  "This defines the key that will cause a running program to stop."
   (if (sdl:key-pressed-p  :SDL-KEY-ESCAPE) (sdl:push-quit-event)))

(defun bind-movement-keys ()
  "This function specifies the keys that will be used for player/cursor movement."
  (if (sdl:key-pressed-p :SDL-KEY-UP) (forward))
  (if (sdl:key-pressed-p :SDL-KEY-DOWN) (back))
  (if (sdl:key-pressed-p :SDL-KEY-LEFT) (left))
  (if (sdl:key-pressed-p :SDL-KEY-RIGHT) (right)))

(defun bind-numeric-keys ()
  "These keys define how the user will change weapons within the game."
   (if (sdl:key-pressed-p :SDL-KEY-0) (setf *current-weapon* 'fist))
   (if (sdl:key-pressed-p :SDL-KEY-1) (setf *current-weapon* 'pistol))
   (if (sdl:key-pressed-p :SDL-KEY-2) (setf *current-weapon* 'shotgun))
   (if (sdl:key-pressed-p :SDL-KEY-3) (setf *current-weapon* 'machine-gun))
   (if (sdl:key-pressed-p :SDL-KEY-4) (setf *current-weapon* 'laser-gun))
   (if (sdl:key-pressed-p :SDL-KEY-5) (setf *current-weapon* 'nail-gun))
   (if (sdl:key-pressed-p :SDL-KEY-6) (setf *current-weapon* 'assault-rifle))
   (if (sdl:key-pressed-p :SDL-KEY-7) (setf *current-weapon* 'dicer-rifle))
   (if (sdl:key-pressed-p :SDL-KEY-8) (setf *current-weapon* 'rice-cannon))
   (if (sdl:key-pressed-p :SDL-KEY-9) (setf *current-weapon* 'samurai-sword)))

(defun bind-fire-key ()
  (if (sdl:key-pressed-p  :SDL-KEY-SPACE) (fire-weapon *current-weapon*)))

(defun which (key)
  (case key
    (help (print "help fucker!"))))

;; Cursor Movement
(defun move-north (&optional (steps 5))
  "Move cursor north on a 2D Grid. Returns the resulting Y-Coordinate."
  (setf (point-y *position*) (- (point-y *position*) steps)))

(defun move-south (&optional (steps 5))
  "Move cursor south on a 2D Grid. Returns the resulting Y-Coordinate."
  (setf (point-y *position*) (+ steps (point-y *position*))))

(defun move-east (&optional (steps 5))
  "Move cursor east on a 2D Grid. Returns the resulting Y-Coordinate."
  (setf (point-x *position*) (+ steps (point-x *position*))))

(defun move-west (&optional (steps 5))
  "Move cursor west on a 2D Grid. Returns the resulting Y-Coordinate."
  (setf (point-x *position*) (- (point-x *position*) steps)))

(defun fire-weapon (&optional weapon)
  (cond
    ((equal weapon 'fist)
     (format t "~A" "pof! *you punch*"))
    ((equal weapon 'pistol)
     (format t "~A" "pff! *fired pistol*"))
    ((equal weapon 'shotgun)
     (format t "~A" "poof! *fired shotgun*"))
    ((equal weapon 'machine-gun)
     (format t "~A" "poof! *fired machinegun*"))
    ((equal weapon 'laser-gun)
     (format t "~A" "poof! *you punch*"))
    ((equal weapon 'rice-cannon)
     (format t "~A" "pif! pif! *fired rice-cannon*"))
    ((equal weapon 'assault-rifle)
     (format t "~A" "poof! *fired assault rifle*"))
    ((equal weapon 'samurai-sword)
     (format t "~A" "poof! *you slice*"))))

(defun punch ()
    (format t "~A" '("hi-yah! *you punch*")))

(defun fire-pistol ()
    (format t "~A" '("pfffaw! *pistol fired*")))

(defun fire-shotgun ()
    (format t "~A" "blao! *shotgun fired*"))

(defun fire-laser ()
    (format t "~A" "pew! *laser fired*"))

;;; Drawing Functions
(defun clear ()
  (sdl:clear-display *background-color*))

(defun background (&key (red 0) (green 0) (blue 0))
  (sdl:fill-surface-* red green blue :surface *current-surface*))
;  (setf *background-color* (sdl:color :r red :g green :b blue))
;  (sdl:color-* *background-color*)
;  (sdl:clear-display *background-color*))

;;; Environmental Functions
(defun get-cursor-position ()
  *position*)

(defun get-x-position ()
  (point-x *position*))

(defun get-y-position ()
  (point-y *position*))

(defun get-frame-rate ()
  (format t "~a fps" *frame-rate*))

(defun frame-rate (fps)
  (setf *frame-rate* fps)
  *frame-rate*)

;;; Toggles

(defun show-cursor ()
  (setf *cursor-visible* t))

(defun show-frame-rate ()
  (setf *show-fps* t))

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
  (sdl:draw-trigon (sdl:point :x (round (+ 0 x1))
			      :y (round (+ 0 y1)))
		   (sdl:point :x (round (+ 0 x2))
			      :y (round (+ 10 y2)))
		   (sdl:point :x (round (+ 10 x3))
			      :y (round (+ 0 y3)))
		   :surface surface :color color))

(defun ellipse (x y rx ry)
  (sdl:draw-ellipse-* x y rx ry :surface sdl:*default-surface* :color *stroke-color*))

(defun line (x0 y0 x1 y1
	     &key (color *stroke-color*) (surface sdl:*default-surface*) (clipping nil))
  (sdl:draw-line-* x0 y0 x1 y1 :surface surface :color color :clipping clipping))

(defun bezier (vertices
	       &key (color *stroke-color*) (segments 20) (style :solid))
  (sdl:draw-bezier vertices :color color
  		            :segments segments
		            :style style))

(defun polygon (vertices
		&key (color *stroke-color*) (surface sdl:*default-surface*) (clipping t))
  (sdl:draw-polygon vertices :surface surface :color color :clipping clipping))


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
  (sdl:color :r red :g green :b blue))

;;; Color
(defun stroke (&key (r 0) (g 0) (b 0))
  (setf *stroke-color* (sdl:color :r r :g g :b b)))
  
;;; Typography
(defun text (string x y
	     &key (surface *current-surface*) (font *current-font*) (color *font-color*))
  (sdl:draw-string-solid-* string x y :surface surface :font font :color color))

;; Iteration
(defun range (&key (start 0) (end (+ start 100)))
  (iter (for i from start to end)
	(collect i)))

;; OS Integration
(defun exec (name
	     &rest args)
  (sb-ext:run-program name args :output *standard-output*
		                :search t))

;;; try/catch

;(defun try ((try-statements) catch-exception (catch-statements)) ())

;;; Benchmarking

;(defun benchmark ()
;  (format t "~a" (sdl:average-fps)))

;(defun pcolor (r g b)
;  (setf *last-color* *current-color*)
;  (setf *current-color* (sdl:color r g b)
;	`(sdl:color-* *current-color*)))

;; Maths

;  (draw-dash x y))
;; (stroke :r (random 200) :g (random 200) :b (random 100))
;; (loop for i from 1 to 200 by 25 do
;;      (text "hello" (+ i x) y)
;;      (circle 400 i i)))
