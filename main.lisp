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
    (sdl:enable-key-repeat 10 10)
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
      (:resize-event (:W w :H h) )
      (:key-down-event (:KEY key)
		       (case  key
			 (:SDL-KEY-ESCAPE (sdl:push-quit-event))
			 (:SDL-KEY-UP (forward 2))
			 (:SDL-KEY-DOWN (backward 2))
			 (:SDL-KEY-LEFT (left 2))
			 (:SDL-KEY-RIGHT (right 2))
			 (:SDL-KEY-C (clear-canvas))
			 (:SDL-KEY-D (dashboard))
			 (:SDL-KEY-R (random-stroke!))
			 (:SDL-KEY-H (turtle-home))
			 (:SDL-KEY-SPACE (pen))
			 (:SDL-KEY-S (sdl:save-image *canvas-surface* `(scrn ,(gensym))))))
      (:idle ()
	     ;; Game
	     (when *dashboard*
	       (draw-dashboard))
	     (sdl:update-display)))))
