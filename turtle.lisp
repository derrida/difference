(in-package :difference)

;;; Turtle Functions
(defmacro to (name args
	      &body body)
  `(defun ,name ,args
     ,@body))

(defun left (&optional (degrees 1))
  (setf (turtle-direction *turtle*) (mod (+ (turtle-direction *turtle*) degrees) 360))
  (make-dirty))

(defun right (&optional (degrees 1))
  (setf (turtle-direction *turtle*) (mod (- (turtle-direction *turtle*) degrees) 360))
  (make-dirty))

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
    (setf (turtle-x  *turtle*) (bound x 0 *width*))    
    (setf (turtle-y  *turtle*) (bound y 0 *height*)))
  (when (pen-state *turtle*)
    (let ((px (turtle-px *turtle*))
	  (py (turtle-py *turtle*))
	  (x (turtle-x *turtle*))
	  (y (turtle-y *turtle*)))
      (let ((p0 (sdl:point :x px :y py))
	    (p1 (sdl:point :x  x :y  y)))
	(sdl:draw-line p0 p1 :color *stroke-color* :surface *canvas-surface*))))
  (when *rainbow*
    (random-stroke!))
  (when (not *dirty*)
    (make-dirty)))

(declaim (inline bound))
(defun bound (number min max)
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps)))

(defun render-turtle ()
  (let* ((angle (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s (sin angle))
	 (c (cos angle)))
    (when *show-turtle*
      (sdl:draw-trigon (sdl:point :x (turtle-x *turtle*)
				  :y (turtle-y *turtle*))
		       (sdl:point :x (+ (turtle-x *turtle*) (*  5 c) (* -5 s))
				  :y (+ (turtle-y *turtle*) (* -5 s) (* -5 c)))
		       (sdl:point :x (+ (turtle-x *turtle*) (* -5 c) (* -5 s))
				  :y (+ (turtle-y *turtle*) (*  5 s) (* -5 c)))
		       :color *stroke-color*
		       :surface *current-surface*))))

(defun make-dirty ()
  (setf *dirty* t))

(defun make-clean ()
  (setf *dirty* nil))

(defun pen ()
  (setf (pen-state *turtle*) (not (pen-state *turtle*)))
  (make-dirty))

(defun pen-down ()
  (setf (pen-state *turtle*) t)
  (make-dirty))

(defun pen-up ()
  (setf (pen-state *turtle*) nil)
  (make-dirty))

(defun turtle-home ()
  (setf (turtle-x *turtle*) (/ *width* 2))
  (setf (turtle-y *turtle*) (/ *height* 2))
  (make-dirty))

(defun clear-canvas ()
  (sdl:clear-display *background-color* :surface *canvas-surface*)
  (make-dirty))

;;; Cartesian Turtle
(defun move-to (x y)
  (let ((dx (- x (turtle-x *turtle*)))
	(dy (- y (turtle-y *turtle*))))
    (right (- (* (atan dx dy) (/ 180 pi)) (turtle-direction *turtle*)))
    (forward (sqrt (+ (* dx dx) (* dy dy)))))
  (make-dirty))


(defun dashboard ()
  (setf *dashboard* (not *dashboard*))
  (make-dirty))

(defun draw-dashboard ()
  (text 10 10 "x:~A" (turtle-x *turtle*))
  (text 10 20 "y:~A" (turtle-y *turtle*))
  (text 10 30 "heading:~A" (turtle-direction *turtle*))
  (text 10 40 "pen-state: ~A" (pen-state *turtle*))
  (text 10 50 "stroke-color: ~A" (stroke?))
  (make-dirty))