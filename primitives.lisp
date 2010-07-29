(in-package :difference)

;;; Shape Primitives
(defun pixel (x y &key (color *stroke-color*) (surface sdl:*default-surface*))
  (sdl:draw-pixel-* x y :color color :surface surface))

(defun point (x y)
  (sdl:point :x x :y y))

(defun circle (x y r &key (surface *current-surface* ) (color *stroke-color*) (alpha nil) (aa nil))
  (sdl:draw-circle-* x y r :surface surface :color color :alpha alpha :aa aa))

(defun rect (x y w h &key (surface sdl:*default-surface*) (color *stroke-color*))
  (sdl:draw-rectangle-* x y w h :surface surface :color color))

(defun triangle (x1 y1 x2 y2 x3 y3
		 &key (surface sdl:*default-surface*) (color *stroke-color*) (clipping t) (aa *anti-aliasing*))
  (let ((p1 (sdl:point :x (+  0 x1) :y (+  0 y1)))
	(p2 (sdl:point :x (+  0 x2) :y (+ 10 y2)))
	(p3 (sdl:point :x (+ 10 x3) :y (+  0 y3))))
    (sdl:draw-trigon p1
		     p2
		     p3 :surface surface :color color :clipping clipping :aa aa)))

(defun ellipse (x y rx ry
		&key (surface sdl:*default-surface*) (color *stroke-color*) (aa *anti-aliasing*))
  (if (eq *anti-aliasing* t)
      (sdl-gfx:draw-aa-ellipse-* x y rx ry :surface surface :color color)
      (sdl-gfx:draw-ellipse-* x y rx ry :surface surface :color color :aa aa)))

(defun line (x0 y0 x1 y1)
  (if (eq *anti-aliasing* t)
      (sdl-gfx:draw-aa-line-* x0 y0 x1 y1
			      :surface *current-surface*
			      :color *stroke-color*)
      (sdl-gfx:draw-line-* x0 y0 x1 y1
			   :surface *current-surface*
			   :color *stroke-color*
			   :aa *anti-aliasing*)))

(defun bezier (vertices)
  (sdl-gfx:draw-bezier vertices
		       :surface *current-surface*
		       :color *stroke-color*
		       :segments *bezier-segments*
		       :style *bezier-style*))

(defun polygon (vertices)
  (if (eq *anti-aliasing* t)
      (sdl-gfx:draw-aa-polygon vertices
			    :surface *current-surface*
			    :color *stroke-color*)
      (sdl-gfx:draw-polygon vertices
			    :surface *current-surface*
			    :color *stroke-color*
			    :aa *anti-aliasing*)))
