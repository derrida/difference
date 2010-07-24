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

;;; Implementation of Turtle Graphics using lispbuilder-sdl.
;;; This is based on the implementation discussed in Turtle Geometry.


; Turtle struct
;; Keys:
;;; x           - integer - x-coordinate of the turtle
;;; y           - integer - y-coordinate of the turtle 
;;; direction   - integer - angle the turtle's heading
;;; pen-state   - boolean - defines whether or not the pen is down
;;; color       - color   - specifies the color the turtle will draw using
(defstruct turtle
  (x 0)
  (y 0)
  (direction 0)
  (pen-state nil)
  (color sdl:*black*))

; Environment
(defparameter *width* 400)
(defparameter *height* 400)
(defparameter *frame-rate* 60)

; Initialize a turtle
(defparameter *turtle*
  (make-turtle :x         (/ *width* 2)
	       :y         (/ *height* 2)
	       :direction 0
	       :pen-state nil
	       :color sdl:*black*))

;;; Colors
(defparameter *background-color* sdl:*black*)
(defparameter *fill-color* sdl:*white*)
(defparameter *stroke-color* sdl:*blue*)
(defparameter *font-color* sdl:*white*)
(defparameter *last-color* nil)

;; ;;; Toggles (Booleans)
(defvar *smoothing*)
(defvar *anti-aliasing*)
(defvar *cursor-visible*)
(defvar *unicode*)
(defvar *show-fps*)

;;; Font
(defparameter *current-font* sdl:*FONT-5X7*)
(defparameter *last-font* nil)
(defparameter *random-color* sdl:*black*)

;;; Numbers
(defparameter *random-number* (random 1000))

;;; Surfaces
(defparameter *current-surface*  sdl:*default-surface*)


; "Getters"
(defun get-x ()
  (round (turtle-x *turtle*)))

(defun get-y ()
  (round (turtle-y *turtle*)))

(defun get-direction ()
  (round (turtle-direction *turtle*)))

(defun get-pen-state ()
  (turtle-x *turtle*))

; Variables
(defparameter *x* (round (get-x)))
(defparameter *y* (round (get-y)))
(defparameter *direction* (round (get-direction)))
(defparameter *pen-state* (get-pen-state))
(defparameter *turtle-surface* nil)

; Turtle Geometry Functions
(defun pen-down ()
  (setf (turtle-pen-state *turtle*) t)
  (setf *pen-state* (turtle-pen-state *turtle*)))

(defun pen-up ()
  (setf (turtle-pen-state *turtle*) nil)
  (setf *pen-state* (turtle-pen-state *turtle*)))

(defun left (degrees)
  (decf (turtle-direction *turtle*) degrees)
  (update))

(defun right (degrees)
  (incf (turtle-direction *turtle*) degrees)
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
	(sdl:draw-line-* x y (get-x) (get-y)))))
  (update))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))

(defun update ()
  (setf *x* (truncate (turtle-x *turtle*) 2))
  (setf *y* (truncate (turtle-y *turtle*) 2))
  (setf *direction* (truncate  (turtle-direction *turtle*) 2))
  (let ((dx (* (sin (turtle-direction *turtle*))))
	(dy (* (sin (turtle-direction *turtle*)))))
    (sdl:draw-trigon (sdl:point :x (* dx (+ 0 (get-x))) :y (* dy (+ 0 (get-y))))
		     (sdl:point :x (* dx (+ (get-x))) :y (* dy (+ 6 (get-y))))
		     (sdl:point :x (* dx (+ 6 (get-x))) :y (* dy (+ 0 (get-y))))))
  *turtle*)

(defmethod turtle ()
  (sdl:with-init ()
    (sdl:window *width* *height* :title-caption "Rolled My Own Fucking Turtle")
    (setf (sdl:frame-rate) *frame-rate*)
    (setf sdl:*default-color* sdl:*black*)
    (setf (turtle-x *turtle*) 200)
    (setf (turtle-y *turtle*) 200)
    (setf (turtle-direction *turtle*) 200)
    (setf *turtle-surface* (sdl:create-surface *width* *height*))
    (sdl:clear-display sdl:*white*)
    (sdl:update-display)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (when (sdl:key-pressed-p :SDL-KEY-ESCAPE)
			 (sdl:push-quit-event)))
      (:idle ()
	     (draw-turtle)
	     (sdl:update-display)))))

(defun draw-turtle ()
	     (pen-down)
	     (forward 20)
	     (right (random 90))
	     (right (random 30)))

;; ;;; Environment
;; (defparameter *window-height*  800)
;; (defparameter *window-width*  800)
;; (defparameter *frame-rate*  60)

;;; Structures
(defstruct point 
  x
  y
  z)

;;; Cursor
(defstruct cursor
  x
  y)

;;; TVector
(defstruct tvector
  x y)

;;; 3x2 Affine Matrix
(defstruct matrix
  "3x2 Affine Matrix"
  (m00 0.0) (m01 0.0) (m02 0.0)
  (m10 0.0) (m11 0.0) (m12 0.0))

;;; Main
(defmethod main ()
  (sdl:with-init ()
    (sdl:window *width* *height* :title-caption "difference")
    (setf (sdl:frame-rate) *frame-rate*)
    ;; Setup Block
;    (setup)
    (sdl:with-events ()
      (:quit-event () t)      
      (:key-down-event ()		 
		       (bind-movement-keys)
		       (bind-numeric-keys)
		       (bind-fire-key)
		       (bind-quit-key))
      (:idle ()
	     ;; Draw Block
	     (draw-turtle) 
	     (sdl:update-display)))))

;; Setup
(defun setup ()
  (sdl:enable-key-repeat 100 50)
  (clear))
  
;; Draw
;(defun draw (&aux (x (round (get-turtle-x))) (y (round (get-turtle-y))))
;  (clear))

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
     (format t "~A" "pff!pff! *fired machinegun*"))
    ((equal weapon 'laser-gun)
     (format t "~A" "pew! pew! *fired lazor*"))
    ((equal weapon 'rice-cannon)
     (format t "~A" "pif! pif! *fired rice-cannon*"))
    ((equal weapon 'assault-rifle)
     (format t "~A" "poof! *fired assault rifle*"))
    ((equal weapon 'samurai-sword)
     (format t "~A" "poof! *you slice*"))))

;;; Drawing Functions
(defun clear ()
  (sdl:clear-display *background-color*))

;(defun background (&key (red 0) (green 0) (blue 0))
;  (sdl:fill-surface-* red green blue :surface *current-surface*)
;  (setf *background-color* (sdl:color :r red :g green :b blue)))
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

(defun fill-color (&key (r 0) (g 0) (b 0))
  (setf *fill-color* (sdl:color :r r :g g :b b)))

(defun background (&key (r 0) (g 0) (b 0))
  (setf *background-color* (sdl:color :r r :g g :b b)))
  
;;; Typography
(defun text (string x y
	     &key (surface *current-surface*) (font *current-font*) (color *font-color*))
  (sdl:draw-string-solid-* string x y :surface surface :font font :color color))

;; Iteration
;; requires iterate package
;(defun range (&key (start 0) (end (+ start 100)))
  ;(iter (for i from start to end)
	;(collect i)))

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


;;; Matrix

(defmethod get-matrix (target)
  (list (matrix-m00 target)
	(matrix-m01 target)
	(matrix-m02 target)
	(matrix-m10 target)
	(matrix-m11 target)
	(matrix-m12 target)))

(defun multX (multi target)
  (list (* (matrix-m00 target) multi)
	(* (matrix-m01 target) multi)
	(* (matrix-m02 target) multi)))

(defun multY (multi target)
  (list (* (matrix-m10 target) multi)
	(* (matrix-m11 target) multi)
	(* (matrix-m12 target) multi)))

(defun mult (matrix1 matrix2)
  (let ((m00 (matrix-m00 matrix1)) (m01 (matrix-m01 matrix1)) (m02 (matrix-m02 matrix1))
	(m10 (matrix-m10 matrix1)) (m11 (matrix-m11 matrix1)) (m12 (matrix-m02 matrix1))
	(s00 (matrix-m00 matrix2)) (s01 (matrix-m01 matrix2)) (s02 (matrix-m02 matrix2))
	(s10 (matrix-m10 matrix2)) (s11 (matrix-m11 matrix2)) (s12 (matrix-m12 matrix2)))
    (list (* m00 s00) (* m01 s01) (* m02 s02)
	  (* m10 s10) (* m11 s11) (* m12 s12))))

(defun determinant (target)
  (let* ((m00 (matrix-m00 target))
	 (m01 (matrix-m01 target))
	 (m10 (matrix-m10 target))
	 (m11 (matrix-m11 target)))
    (- (* m00 m11) (* m01 m10))))

(defun invert (target)
  (let* ((m00 (matrix-m00 target))
	(m01 (matrix-m01 target))
	(m02 (matrix-m02 target))
	(m10 (matrix-m10 target))
	(m11 (matrix-m11 target))
	(m12 (matrix-m02 target)))
    (let ((t00 m00)
	  (t01 m01)
	  (t02 m02)
	  (t10 m10)
	  (t11 m11)
	  (t12 m12))
      (setf m00 (/ t11 (determinant target)))
      (setf m10 (/ (- 0 t10) (determinant target)))
      (setf m01 (/ (- 0 t01) (determinant target)))
      (setf m11 (/ t00 (determinant target)))
      (setf m02 (/ (- (* t01 t12) (* t11 t02)) (determinant target))))
    (list m00 m10 m01 m11 m02)))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

;;; Utility Functions
;(defmacro def (name args &body body)
;  "Synonym for defun"
;  `(defun ,name ,args
;     ,@body))

;(defun combiner (x)
;  (typecase x
;    (number #'+)
;    (list #'append)
;    (t #'list)))

;(defun combine (&rest args)
;  (apply (combiner (car args))
;	 args))

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

(defun get-number ()
  (ask "Please enter a number:" 'number))

(defun get-string (category)
  (ask (format t "Please enter a ~A" category) 'string))

(defun nilp (object)
  (eq object nil))