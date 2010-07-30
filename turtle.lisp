(in-package :difference)

(defclass turtle ()
  ((x-coordinate  :accessor turtle-x         :initarg :x         :reader check :initform (/ *width*  2))
   (y-coordinate  :accessor turtle-y         :initarg :y         :reader check :initform (/ *height* 2))
   (px-coordinate :accessor turtle-px        :initarg :px        :reader check :initform      nil      )
   (py-coordinate :accessor turtle-py        :initarg :py        :reader check :initform      nil      )
   (direction     :accessor turtle-direction :initarg :direction :reader check :initform (/ *width*  2))
   (pen-state     :accessor pen-state        :initarg :pen-state :reader check :initform (/ *height* 2))
   (color         :accessor turtle-color     :initarg :color     :reader check :initform *stroke-color*)))

;;; Initialize a turtle to start
(defparameter *turtle* (make-instance 'turtle
				      :x (/ *width* 2)
				      :y (/ *height* 2)
				      :px 0
				      :py 0
				      :direction 0
				      :pen-state nil
				      :color *stroke-color*))

;;; Turtle Functions
(defmacro to (name args &body body)
  `(defun ,name ,args
     ,@body))

(defun left (&optional (degrees 1))
  (setf (turtle-direction *turtle*) (round (mod (+ (turtle-direction *turtle*) degrees) 360)))
  (update))

(defun right (&optional (degrees 1))
  (setf (turtle-direction *turtle*) (round (mod (- (turtle-direction *turtle*) degrees) 360)))
  (update))

(defun forward (steps)
  (let* ((px (turtle-x *turtle*))
	 (py (turtle-y *turtle*))
	 (theta (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s (* (sin theta) steps))
	 (c (* (cos theta) steps))
	 (x (+ (turtle-x *turtle*) s))
	 (y (+ (turtle-y *turtle*) c)))
    (setf (turtle-px *turtle*) px)
    (setf (turtle-py *turtle*) py)
    (setf (turtle-x  *turtle*) (bound x :min 0 :max *width*))    
    (setf (turtle-y  *turtle*) (bound y :min 0 :max *height*)))
  (when (pen-state *turtle*)
    (let ((px (turtle-px *turtle*))
	  (py (turtle-py *turtle*))
	  (x (turtle-x *turtle*))
	  (y (turtle-y *turtle*)))
      (let ((p0 (sdl:point :x px
			   :y py))
	    (p1 (sdl:point :x x
			   :y y)))
	(sdl:draw-line p0 p1 :color *stroke-color* :surface *canvas-surface*))))
  (when *rainbow*
    (random-stroke!))
  (update))

(defun bound (num &key min max)
  (cond 
    ((< num min) min)
    ((< max num) max)
    ((< num max) num)))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))

(defun update ()
  (sdl:blit-surface *canvas-surface*)
  (let* ((angle (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s     (sin angle))
	 (c     (cos angle))
	 (x     (round (turtle-x *turtle*)))
	 (y     (round (turtle-y *turtle*))))
    (when *show-turtle*
      (sdl:draw-trigon (sdl:point :x x
				  :y y)
		       (sdl:point :x (round (+ x (*  5 c) (* -5 s)))
				  :y (round (+ y (* -5 s) (* -5 c))))
		       (sdl:point :x (round (+ x (* -5 c) (* -5 s)))
				  :y (round (+ y (*  5 s) (* -5 c))))
		       :color *stroke-color*
		       :surface *current-surface*))))

(defun pen ()
  (setf (pen-state *turtle*) (not (pen-state *turtle*)))
  (update))

(defun pen-down ()
  (setf (pen-state *turtle*) t))

(defun pen-up ()
  (setf (pen-state *turtle*) nil))

(defun turtle-home ()
  (setf (turtle-x *turtle*) (/ *width* 2))
  (setf (turtle-y *turtle*) (/ *height* 2))
  (update))

(defun dashboard ()
  (setf *dashboard* (not *dashboard*))
  (update))

(defun draw-dashboard ()
  (text 10 10 "x:~A" (round (turtle-x *turtle*)))
  (text 10 20 "y:~A" (round (turtle-y *turtle*)))
  (text 10 30 "heading:~A" (round (turtle-direction *turtle*)))
  (text 10 40 "pen-state: ~A" (pen-state *turtle*))
  (text 10 50 "stroke-color: ~A" (stroke?)))

(defun clear-canvas ()
  (sdl:clear-display *background-color* :surface *canvas-surface*))

;;; Cartesian Turtle
(defun move-to (x y)
  (let ((dx (- x (turtle-x *turtle*)))
	(dy (- y (turtle-y *turtle*))))
    (right (- (* (atan dx dy) (/ 180 pi)) (turtle-direction *turtle*)))
    (forward (sqrt (+ (* dx dx) (* dy dy))))))
