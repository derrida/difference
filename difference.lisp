;;;;;;;;;;;;;;;;;;;;;;;;;
;;     difference      ;;
;;    a drawing api    ;;
;;.....................;;
;;   difference.lisp   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:difference)

;; (defclass environment ()
;;   (width            :accessor x            :initarg :x         :reader check :initform (/ *width*  2))
;;   (height           :accessor y            :initarg :y         :reader check :initform (/ *height* 2))
;;   (frame-rate       :accessor px           :initarg :px        :reader check :initform      nil      )
;;   (dashboard        :accessor py           :initarg :py        :reader check :initform      nil      )
;;   (background-color :accessor direction    :initarg :direction :reader check :initform (/ *width*  2))
;;   (anti-aliasing    :accessor pen-state    :initarg :pen-state :reader check :initform (/ *height* 2)))

;;; Predicates
(defun nilp (object)
  (eq object nil))

;;; Setters
(defun frame-rate (fps)
  (setf (sdl:frame-rate) fps))

;; Iteration
(defun range (&optional (start 0) (end 100))
  (loop for i from start to end collecting i))

;; OS Integration
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

;;; Drawing Functions
(defun text (x y string &rest expr)
  (sdl:draw-string-solid-* (format nil string expr) x y :color *font-color* :font sdl:*default-font*))

(defun clear ()
  (sdl:clear-display *background-color*))