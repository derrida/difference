(in-package :difference)

(defstruct matrix
  "3x2 Affine Matrix"
  (m00 0.0) (m01 0.0) (m02 0.0)
  (m10 0.0) (m11 0.0) (m12 0.0))

(defmethod get-matrix (target)
  (list (matrix-m00 target)
	(matrix-m01 target)
	(matrix-m02 target)
	(matrix-m10 target)
	(matrix-m11 target)
	(matrix-m12 target)))

(defmethod scale (amount)
  )


;; (defun matrix-multiply (a b)
;;   (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
;;          (row (mat i) (elt mat i)))
;;     (loop for row from 0 below (length a)
;;           collect (loop for col from 0 below (length (row b 0))
;;                         collect (apply #'+ (mapcar #'* (row a row) (col b col)))))))
 
;; example use:
;(matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
; 
 
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

;; (defun mult (matrix1 matrix2)
;;  (mapcar
;;   (lambda (row)
;;    (apply #'mapcar
;;     (lambda (&rest column)
;;      (apply #'+ (mapcar #'* row column))) matrix2)) matrix1))

(defmethod translate (tx ty)
  (format t "~A ~A" tx ty))

(defun transpose (matrix)
  (apply #'mapcar #'list matrix))

(defmethod invert-matrix ()
  )