;; Dumb Functions
;   (sdl:initialise-default-font)
(defmacro def (name args &body body)
  "Synonym for defun"
  `(defun ,name ,args
     ,@body))

(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
	 args))

;;; I/O
(defun cat (filename)
      (with-open-file (str filename :direction :input)
	(do ((line (read-line str nil 'eof)
		   (read-line str nil 'eof)))
	    ((eql line 'eof))
	  (format t "~A~%" line))))

;;; Common Inputs
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