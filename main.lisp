(in-package :difference)

;;; Main
(defmethod main ()
  (sdl:with-init ()
    ;;
    ;; Setup Block
    ;;
    (sdl:window *width* *height* :title-caption "difference")
    (frame-rate *frame-rate*)
    (setf *canvas-surface* (sdl:convert-to-display-format :surface (sdl:create-surface *width* *height*) :free t))
    (sdl:enable-key-repeat 20 20)
    (sdl:clear-display *background-color*)
    (when (eq *unicode* t)
      (sdl:enable-unicode))
    (sdl:initialise-default-font)
    ;;
    ;; End Setup Block
    ;;
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (case  key
			 (:SDL-KEY-ESCAPE (sdl:push-quit-event))
			 (:SDL-KEY-UP (forward 10))
			 (:SDL-KEY-DOWN (backward 10))
			 (:SDL-KEY-LEFT (left 5))
			 (:SDL-KEY-RIGHT (right 5))
			 (:SDL-KEY-D (dashboard))
			 (:SDL-KEY-R (random-stroke!))
			 (:SDL-KEY-H (turtle-home))
			 (:SDL-KEY-SPACE (pen))))
      (:idle ()
					;	     (sdl:update-display)
	     ;;
	     ;; Draw Block (Game Loop?)
	     ;;		
	     (when *dashboard*
	       (draw-dashboard))
	     ;; (update)
	     ;;
	     ;; End Draw Block
	     ;;
	     (sdl:update-display)))))