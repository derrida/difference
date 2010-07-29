;; (defclass environment ()
;;   (width            :accessor x            :initarg :x         :reader check :initform (/ *width*  2))
;;   (height           :accessor y            :initarg :y         :reader check :initform (/ *height* 2))
;;   (frame-rate       :accessor px           :initarg :px        :reader check :initform      nil      )
;;   (dashboard        :accessor py           :initarg :py        :reader check :initform      nil      )
;;   (background-color :accessor direction    :initarg :direction :reader check :initform (/ *width*  2))
;;   (anti-aliasing    :accessor pen-state    :initarg :pen-state :reader check :initform (/ *height* 2)))

;; Wild Version ...
;; (defun update ()
;;   (setf *x* (truncate (turtle-x *turtle*) 2))
;;   (setf *y* (truncate (turtle-y *turtle*) 2))
;;   (setf *direction* (truncate  (turtle-direction *turtle*) 2))
;;   (let ((dx (* (sin (turtle-direction *turtle*))))
;; 	(dy (* (sin (turtle-direction *turtle*)))))
;;     (sdl:draw-trigon (sdl:point :x (* dx (+ 0 (get-x))) :y (* dy (+ 0 (get-y))))
;; 		     (sdl:point :x (* dx (+ (get-x))) :y (* dy (+ 6 (get-y))))
;; 		     (sdl:point :x (* dx (+ 6 (get-x))) :y (* dy (+ 0 (get-y))))))
;;   *turtle*)

;; ;;; Environment
;; (defparameter *window-height*  800)
;; (defparameter *window-width*  800)
;; (defparameter *frame-rate*  60)


;;; Polygons
; Draw list of 3D convex polygons
(defparameter *tan* (sdl:color :r 78 :g 114 :b 114))
(defparameter *mauve* (sdl:color :r 255 :g 85 :b 125))

(defparameter -m3 -3)
(defparameter -m2 -2)
(defparameter -m1 -1)
(defparameter m0  0)
(defparameter m1  1)
(defparameter m2  2)
(defparameter m3  3)



(defun move-to-3d (x y z)
  ;; 3D transform
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
  ;; Projection
  (let ((dx (/ (* x *width*) (* z 2.0)))
        (dy (/ (* y *height*) (* z 2.0))))
    ;; Move turtle by dx,dy
    (move-to (+ dx (/ *width* 2)) (+ dy (/ *height* 2)))))

(defun poly-down ()
  (setf (turtle-poly-state *turtle*) t))

(defun poly-up ()
  (setf (turtle-poly-state *turtle*) nil))

(defun draw-grid (x-interval y-interval &optional (width *width*) (height *height*))
  (let ((acc 0))
    (loop for i from 0 to width by x-interval do
	 (push "bang" acc)(loop for j from 0 to height by y-interval do (push j acc)) (print acc))))

(defun get-poly-state ()
  (turtle-poly-state *turtle*))

(defun letter (index)
  (let ((alphabet (loop for i from 97 to 122
		     collecting (code-char i))))
    (format nil "~a"
	    (nth (- index 1) alphabet))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun random-number (&key (highest-number 100))
  (random highest-number))

(defun random-color (&key (red (random 255)) (green (random 255)) (blue (random 255)))
  (sdl:color :r red
	     :g green
	     :b blue))
;
;     (when (and (turtle-poly-state *turtle*) (or (/= x (get-px)) (/= y (get-y1))))
;	(sdl-gfx:draw-filled-trigon (sdl:point :x (get-px)
;					       :y (get-py))
;				    (sdl:point :x x
;					       :y y)
;				    (sdl:point :x (get-x)
;					       :y (get-y))
;				    :surface *canvas-surface*))
;
