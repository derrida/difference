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
  "Synonym for defun"
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
	 (dx (* (sin angle) steps))
	 (dy (* (cos angle) steps)))
    (incf (turtle-x *turtle*) dx)
    (incf (turtle-y *turtle*) dy)
    (update)))

(defun backward (steps)
  (let ((backward-steps (- 0 steps)))
    (forward backward-steps))
  (update))

(defun update ()
  (sdl:blit-surface *canvas-surface*)
  (setf (turtle-px *turtle*) (turtle-x *turtle*))
  (setf (turtle-py *turtle*) (turtle-y *turtle*))
  (draw-turtle))

(defun draw-turtle ()
  (let* ((angle (* (turtle-direction *turtle*) (/ pi 180.0)))
	 (s (sin angle))
	 (c (cos angle)))
    (when (eq *show-turtle* t)
      (render-turtle s c (+ 5 (turtle-x *turtle*)) (turtle-y *turtle*)))
    (when (pen-state *turtle*)
      (line (turtle-px *turtle*) (turtle-px *turtle*) (turtle-x *turtle*) (turtle-y *turtle*)))))

(defun render-turtle (s c x y)
      (sdl:draw-trigon (sdl:point :x x :y y)
                       (sdl:point :x (+ x (*  5 c) (* -5 s))
				  :y (+ y (* -5 s) (* -5 c)))
		       (sdl:point :x (+ x (* -5 c) (* -5 s))
				  :y (+ y (*  5 s) (* -5 c)))))

(defun pen-down ()
  (setf (pen-state *turtle*) t))

(defun pen-up ()
  (setf (pen-state *turtle*) nil))

(defun turtle-home ()
  (setf (turtle-x *turtle*) (/ *width* 2))
  (setf (turtle-y *turtle*) (/ *height* 2)))

(defun toggle-pen ()
  (if (pen-state *turtle*)
      (setf (pen-state *turtle*) nil)
      (setf (pen-state *turtle*) t)))

(defun toggle-dashboard ()
  (if (eq *dashboard* t)
      (setf *dashboard* nil)
      (setf *dashboard* t)))

(defun draw-dashboard ()
  (text 10 10 "x:~A"             (round (turtle-x *turtle*)))
  (text 10 20 "y:~A"             (round (turtle-y *turtle*)))
  (text 10 30 "heading:~A"       (round (turtle-direction *turtle*)))
  (text 10 40 "pen-state: ~A"    (princ (pen-state *turtle*)))
  (text 10 50 "stroke-color: ~A" (stroke?)))

(defun move-to (x y)
  (let ((dx (- x (turtle-x *turtle*)))
	(dy (- y (turtle-y *turtle*))))
    (right (- (* (atan dx dy) (/ 180 pi)) (turtle-direction *turtle*)))
    (forward (sqrt (+ (* dx dx) (* dy dy))))))
