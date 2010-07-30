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
  (setf (turtle-direction *turtle*) (mod (incf (turtle-direction *turtle*) degrees) 360))
  (update))

(defun right (&optional (degrees 1))
  (setf (turtle-direction *turtle*) (mod (decf (turtle-direction *turtle*) degrees) 360))
  (update))

(defun forward (steps)
  (let* ((angle (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s (* (sin angle) steps))
	 (c (* (cos angle) steps)))
    (setf (turtle-px *turtle*) (round (turtle-x *turtle*)))
    (setf (turtle-py *turtle*) (round (turtle-y *turtle*)))
    (setf (turtle-x *turtle*) (and (> (turtle-x *turtle*) 0)
				(if (<= (+ (turtle-x *turtle*) (round s)) *width*)
				    (setf (turtle-x *turtle*) (+ (turtle-x *turtle*) (round s)))
				    (setf (turtle-x *turtle*) *width*))))
    (setf (turtle-y *turtle*) (and (> (turtle-y *turtle*) 0)
				(if (<= (+ (turtle-y *turtle*) (round c)) *height*)		      
				    (setf (turtle-y *turtle*) (+ (turtle-y *turtle*) (round c)))
				    (setf (turtle-y *turtle*) *height*))))
    (when (pen-state *turtle*)
      (sdl:draw-line-* (round (turtle-px *turtle*)) (round (turtle-py *turtle*)) (round (turtle-x *turtle*)) (round (turtle-y *turtle*)) :color *stroke-color* :surface *canvas-surface*))
    (when *rainbow*
      (random-stroke!))
    (update)))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))

(defun update ()
  (sdl:blit-surface *canvas-surface*)
  (let* ((angle (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s     (sin angle))
	 (c     (cos angle))
	 (x     (turtle-x *turtle*))
	 (y     (turtle-y *turtle*)))
    (when *show-turtle*
      (sdl:draw-trigon (sdl:point :x x :y y)
		       (sdl:point :x (+ x (*  5 c) (* -5 s))
				  :y (+ y (* -5 s) (* -5 c)))
		       (sdl:point :x (+ x (* -5 c) (* -5 s))
				  :y (+ y (*  5 s) (* -5 c)))
		       :color *stroke-color*
		       :surface *current-surface*))))


(defun pen-down ()
  (setf (pen-state *turtle*) t))

(defun pen-up ()
  (setf (pen-state *turtle*) nil))

(defun pen ()
  (setf (pen-state *turtle*) (not (pen-state *turtle*))))

(defun turtle-home ()
  (setf (turtle-x *turtle*) (/ *width* 2))
  (setf (turtle-y *turtle*) (/ *height* 2)))

(defun dashboard ()
  (setf *dashboard* (not *dashboard*)))

(defun draw-dashboard ()
  (text 10 10 "x:~A"             (round (turtle-x *turtle*)))
  (text 10 20 "y:~A"             (round (turtle-y *turtle*)))
  (text 10 30 "heading:~A"       (round (turtle-direction *turtle*)))
  (text 10 40 "pen-state: ~A"    (pen-state *turtle*))
  (text 10 50 "stroke-color: ~A" (stroke?)))

(defun clear-canvas ()
  (sdl:clear-display *background-color* :surface *canvas-surface*))

(defun move-to (x y)
  (let ((dx (- x (turtle-x *turtle*)))
	(dy (- y (turtle-y *turtle*))))
    (right (- (* (atan dx dy) (/ 180 pi)) (turtle-direction *turtle*)))
    (forward (sqrt (+ (* dx dx) (* dy dy))))))
