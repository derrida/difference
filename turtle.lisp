(in-package :difference)

;;; Implementation of Turtle Graphics using lispbuilder-sdl.
;;; This is based on the implementation discussed in Turtle Geometry.


;; (defun rotate-turtle (angle
;;  		      &aux (s (sin angle)) (c (cos angle)))
;;    (let ((x (get-turtle-x)) (y (get-turtle-y)))
;;      (setf (turtle-x *turtle*) (+ (* c x) (* s y)))
;;      (setf (turtle-y *turtle*) (+ (* -s x) (* c y)))
;;      (let ((x ())))))

;; (defun draw-dash ()
;;   (text (format nil "X: ~A~%Y: ~A~%" (turtle-x *turtle*) (turtle-y *turtle*)) 100 100))

(defparameter *turtle*
  (make-turtle :x (/ *window-width* 2)
	       :y (/ *window-height* 2)
	       :z 0
	       :heading 0))

(defparameter *matrix*
  (make-matrix :m00 0 :m01 0 :m02 0
	       :m10 0 :m11 0 :m12 0))

(defparameter *pen-color* *stroke-color*)

(defparameter *dashboard* t)
(defparameter *pen-up* )

(defparameter *heading* 0)

;;; Functions
(defmethod rotate-x (angle &aux (x (turtle-x *turtle*)) (y (turtle-y *turtle*)))
  (setf (turtle-x *turtle*) (- (* x (cos angle)) (* y (sin angle)))))

(defmethod rotate-y (angle &aux (x (turtle-x *turtle*)) (y (turtle-y *turtle*)))
  (setf (turtle-y *turtle*) (+ (* x (sin angle)) (* y (cos angle)))))

(defmethod rotate (angle)
  (rotate-x angle)
  (rotate-y angle))

;(defun rotate (&key (degrees 1) (turtle *turtle*))
;  (setf (turtle-heading turtle)	(mod (+ (turtle-heading turtle) degrees) 360))
;  (format t "Heading: ~S~%" (get-turtle-heading)))

;; (defun distance-from-origin (&optional (turtle *turtle*))
;;   (with-slots (x y z)
;;       turtle
;;     (sqrt (+ (* x x) (* y y) (* z z)))))

;; (defun rotate (angle 
;; 	       &key (turtle *turtle*))
;;   "This function rotates the turtle on it's centroid."
;;   (setf (turtle-heading turtle) (let* ((s (sin angle))
;; 				       (c (cos angle))
;; 				       (temp1 (turtle-x turtle))
;; 				       (temp2 (turtle-y turtle)))
;; 				  (setf (turtle-x turtle) (mod (+ (* c temp1) (* s temp2)) 360))
;; 				  (setf (turtle-y turtle) (mod (+ (* (- 0 s) temp1) (* c temp2)) 360))
;; 				  (setf temp1 (turtle-x turtle))
;; 				  (setf temp2 (turtle-y turtle))
;; 				  (setf (turtle-x turtle) (mod (+ (* c temp1) (* s temp2)) 360))
;; 				  (setf (turtle-y turtle) (mod (+ (* (- 0 s) temp1) (* c temp2)) 360)))))

;;; Macros
(defmacro TO (name args &body body)
  "Synonym for defun. This is the define function from Turtle Geometry"
  `(defun ,name ,args ,@body))

;; Movement
(defun FORWARD (&optional (steps 1) (turtle *turtle*))
  (let ((x (turtle-x turtle))
	(y (turtle-y turtle))
	(x-modifier (cos (turtle-heading turtle)))
	(y-modifier (sin (turtle-heading turtle))))
    (setf (turtle-x turtle) (+ x (* steps (+ 1 x-modifier))))
    (setf (turtle-y turtle) (+ y (* steps (+ 1 y-modifier))))))

(defun BACK (&optional (steps 1) (turtle *turtle*))
  (forward (- 0 steps) turtle))

(defun LEFT (&optional (angle -1))      
  (rotate angle))

(defun RIGHT (&optional (angle 1))      
  (rotate angle))

(defun PEN-DOWN ()
  (setf *pen-color* *stroke-color*))

(defun PEN-UP ()
  (setf *pen-color* *background-color*))

;; (defun UP (&optional (steps 1) (turtle *turtle*))
;;   (setf (turtle-z turtle) (+ steps (turtle-z turtle))))

;; (defun DOWN (&optional (steps 1) (turtle *turtle*))
;;   (setf (turtle-z turtle) (- (turtle-z turtle) steps)))


;; Some Getter Functions
(defun x (&optional (turtle *turtle*))
  (turtle-x turtle))

(defun y (&optional (turtle *turtle*))
  (turtle-y turtle))

(defun z (&optional (turtle *turtle*))
  (turtle-z turtle))

(defun heading (&optional (turtle *turtle*))
  (turtle-heading turtle))

(defun draw-turtle (&key (x (get-turtle-x)) (y (get-turtle-y)))
  ;; (rect (round  x) (round y) 5 5)
  (triangle (- x 20) (- y 5)
	    x     y
	    (+ x 5) (- y 5)))

(defun dashboard ()
  (format nil "Turtle Location:~%X: ~S~%Y: ~S~%Heading: ~S"
	  (round (get-turtle-x))
	  (round (get-turtle-y))
	  (round (get-turtle-heading))))



;; (defun move-turtle (x y)
;;   (setf (turtle-x *turtle*) x)
;;   (setf (turtle-y *turtle*) y))

;; ;;; Shapes
;; (defmacro make-shape (&rest points)
;;   `(list
;;     ,@(loop for p in points
;;           collect `(cons ,(car p)
;;                          ,(cadr p)))))

;; (defmacro deftrans (name args x y)
;;   (let ((shape (gensym))
;;         (point (gensym)))
;;     `(defmethod ,name (,@args (,shape list))
;;        (mapcar (lambda (,point)
;; 		 (let ((t-x (turtle-x *turtle*))
;; 		       (t-y (turtle-y *turtle*)))
;; 		   (cons ,x ,y)))
;; 	       ,shape))))

;; (deftrans translate (x y)
;;   (+ t-x x)
;;   (+ t-y y))

;; (deftrans rotate (angle)
;;   (- (* (cos angle) t-x) (* (sin angle) t-y))
;;   (+ (* (sin angle) t-x) (* (cos angle) t-y)))

;; (deftrans scale (factor)
;;   (* t-x factor)
;;   (* t-y *turtle*))
