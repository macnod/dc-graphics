(in-package :dc-graphics)

(defun line-chart (width height series)
  (let* ((file "/tmp/chart.png")
         (x-min (series-point-extreme series 1 #'<))
         (x-max (series-point-extreme series 1 #'>))
         (y-min (series-point-extreme series 2 #'<))
         (y-max (series-point-extreme series 2 #'>))
         (w (- x-max x-min))
         (h (- y-max y-min))
         (a-margin-left 50)
         (a-margin-right 50)
         (a-margin-top 50)
         (a-margin-bottom 50)
         (a-width (- width a-margin-left a-margin-right))
         (a-height (- height a-margin-top a-margin-bottom))
         (dx (/ a-width w))
         (dy (/ a-height h)))
      (with-canvas (:width width :height height)
        (set-line-width 2)
        (set-rgb-stroke 0.5 0.5 0.5)
        (rectangle 0 0 width height)
        (stroke)
        (set-rgb-stroke 0.5 0.5 1.0)
        (rectangle a-margin-left a-margin-bottom a-width a-height)
        (stroke)
        (loop for s in series
           for row = 1 then (1+ row)
           for color = (get-color row)
           do
             (apply #'set-rgb-stroke color)
             (loop for x-old = nil then x-new
                for y-old = nil then y-new
                for (x y) in s
                for x-new = (+ (* x dx) a-margin-left)
                for y-new = (+ (* y dy) a-margin-bottom)
                when x-old do
                  (move-to x-old y-old)
                  (line-to x-new y-new)
                  (stroke)))
        (save-png file))
      file))

(defun series-point-extreme (series point-dimension function)
  (loop with x = nil
     with dim = (if (eql point-dimension :x) 0 1)
     for s in series do
       (loop for p in s
          for value = (elt p dim)
          when (or (null x) (funcall function value x))
          do (setf x value))
     finally (return x)))

(defun bogus-series (&key (row-count 3) (row-length 10) 
                       (min-value 0.0) (max-value 10.0))
  (loop with dx = (/ pi (float row-length))
     with value-range = (/ (- max-value min-value) 2)
     for row from 1 to row-count
     collect (loop for column from 0 below row-length
                for x = (/ pi row) then (+ x dx)
                for value = (truncate (+ (* (sin x) value-range) value-range))
                collect (list column value))))

(defun list-to-series (list)
  (loop for x = 0 then (1+ x)
     for y in list
     collect (list x y)))

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
