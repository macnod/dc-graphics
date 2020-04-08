(in-package :dc-graphics)

(defun line-chart (point-series &key (width 640) 
                                  (height 400)
                                  (margin-left 25)
                                  (margin-right 25)
                                  (margin-top 25)
                                  (margin-bottom 25)
                                  series-labels)
  (let* ((file "/tmp/chart.png")
         (x-min (point-series-extreme point-series :x #'<))
         (x-max (point-series-extreme point-series :x #'>))
         (y-min (point-series-extreme point-series :y #'<))
         (y-max (point-series-extreme point-series :y #'>))
         (w (- x-max x-min))
         (h (- y-max y-min))
         (a-width (- width margin-left margin-right))
         (a-height (- height margin-top margin-bottom))
         (actual (lambda (x y)
                   (list
                    (+ (* (/ (- x x-min) w) a-width) margin-left)
                    (+ (* (/ (- y y-min) h) a-height) margin-bottom))))
         (colors (loop for index from 1 to (length point-series)
                    collect (get-color index))))
      (with-canvas (:width width :height height)
        (set-line-width 2)
        (set-rgb-stroke 0.5 0.5 0.5)
        (rectangle 0 0 width height)
        (stroke)
        (set-rgb-stroke 0.5 0.5 1.0)
        (rectangle margin-left margin-bottom a-width a-height)
        (stroke)
        (when series-labels (render-color-legend series-labels colors))
        (loop for s in point-series
           for color in colors
           do
             (apply #'set-rgb-stroke color)
             (loop for x-old = nil then (car xy)
                for y-old = nil then (second xy)
                for (x y) in s
                for xy = (funcall actual x y)
                when x-old do
                  (move-to x-old y-old)
                  (line-to (car xy) (second xy))
                  (stroke)))
        (save-png file))
      (list :file file :x-min x-min :x-max x-max :y-min y-min :y-max y-max)))

(defun render-color-legend (

(defun point-series-extreme (series point-dimension function)
  (loop with x = nil
     with dim = (if (eql point-dimension :x) 0 1)
     for s in series do
       (loop for p in s
          for value = (elt p dim)
          when (or (null x) (funcall function value x))
          do (setf x value))
     finally (return x)))

(defun make-point-series (&key (begin 0) 
                            (end (* 2 pi))
                            (step (/ (* 2 pi) 100.0))
                            (function #'sin)
                            (max 5) (min -5))
  (let ((functions (if (listp function) function (list function))))
    (loop for function in functions collect
         (loop for x from begin to end by step
            for y = (max min (min max (funcall function x)))
            collect (list x y)))))

(defun list-to-point-series (list)
  (list (loop for y in list 
           for x = 0 then (1+ x)
           collect (list x y))))

(defun lists-to-point-series (lists)
  (loop for list in lists collect (car (list-to-point-series list))))

(defun get-color (i)
  (case i
    (1 '(1 0 0))
    (2 '(0 1 0))
    (3 '(0 0 1))
    (t (mapcar (lambda (x) (random (float x))) '(1 1 1)))))

(defun line (&key (width 600) (height 200))
  (let ((file "/tmp/chart.png"))
    (with-canvas (:width width :height height)
      (set-line-width 2)
      (set-rgb-stroke 1 0 0)
      (move-to 0.0 0.0)
      (line-to 300 100)
      (stroke)
      (save-png file))))

(defun radiant-lambda ()
  (with-canvas (:width 90 :height 90)
    (let ((font (get-font "/usr/share/fonts/truetype/freefont/FreeSerif.ttf"))
          (step (/ pi 7))
          (file "/tmp/chart.png"))
      (set-font font 40)
      (translate 45 45)
      (set-rgb-fill 1 0 0)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 0 0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (save-png file))))
