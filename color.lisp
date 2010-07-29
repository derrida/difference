(in-package :difference)

;;; Color
(defmacro defcolor (color-name r g b &optional (a 255))
  `(setf ,color-name (sdl:color :r ,r
				:g ,g
				:b ,b
				:a ,a)))

(defun stroke! (r g b a)
  (setf *stroke-color* (sdl:color :r r
				  :g g
				  :b b
				  :a a)))

(defun random-stroke! ()
  (setf *stroke-color* (sdl:color :r (random 255)
				  :b (random 255)
				  :g (random 255))))

(defun fill! (r g b a)
  (setf *fill-color* (sdl:color :r r
				:g g
				:b b
				:a a)))

(defun stroke? (&optional isolate)
  (multiple-value-bind (r g b a)
      (sdl:color-* *stroke-color*)
    (case isolate
      (:red (list r))
      (:green (list g))
      (:blue (list b))
      (:alpha (list b))
      (otherwise (list r g b a)))))