(in-package :difference)

;;; Main
(defmethod main ()
  (sdl:with-init ()
    ;;
    ;; Setup Block
    ;;
    (sdl:window *width* *height* :title-caption "difference" :resizable t)
    (frame-rate *frame-rate*)
    (setf *canvas-surface* (sdl:convert-to-display-format :surface (sdl:create-surface *width* *height*) :free t))
    (setf *keystack* '())
    (sdl:clear-display *background-color*)
    (when (eq *unicode* t)
      (sdl:enable-unicode))
    (sdl:initialise-default-font)
    ;;
    ;; End Setup Block
    ;;
    
    (sdl:with-events ()
      (:quit-event () t)
      (:active-event () )
      (:resize-event (:W w :H h)
		     (print w)
		     (print h)
		     (setf *width* w)
		     (setf *height* h)
		     (sdl:resize-window w h :title-caption "difference (resized)"))
      (:key-down-event (:KEY key)
		       (push key *keystack*)
		       (mapcar #'process-non-repeating-key *keystack*))
      (:key-up-event (:KEY key)
		     (setf *keystack* (delete key *keystack*)))
      (:idle ()
	     (when *keystack*
	       (mapcar #'process-repeating-key *keystack*))
	     (when *dirty*
	       (sdl:blit-surface *canvas-surface*)
	       (render-turtle))
	     (when *dashboard*
	       (draw-dashboard))
	     (sdl:update-display)))))