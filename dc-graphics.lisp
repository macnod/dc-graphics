(in-package :dc-graphics)

(defun line-chart (width height series 
                   &key
                     (margin-left 50)
                     (margin-right 50)
                     (margin-bottom 50)
                     (margin-top 50)
                     (background '(0.0 0.0 0.0 1.0)))
  (declare (ignore background))
  (let ((x-min (apply #'min (mapcar (lambda (s) (second (car s)))  series)))
        (x-max (apply #'max (mapcar (lambda (s) (second (car (last s))))  series)))
        (y-min nil)
        (y-max nil)
        (file "/tmp/chart.png"))
    (loop for s in series do
         (loop for p in s
            when (or (null y-min) (< (second p) y-min))
            do (setf y-min (second p))
            when (or (null y-max) (> (second p) y-max))
            do (setf y-max (second p))))
    (let* ((canvas-width (1+ (- x-max x-min)))
           (canvas-height (1+ (- y-max y-min)))
           (dx (/ width canvas-width))
           (dy (/ height canvas-height)))
      (format t "~{~(~a~)=~a~^; ~}~%"
              (list :x-min x-min
                    :x-max x-max
                    :y-min y-min
                    :y-max y-max
                    :v-width v-width
                    :v-height v-height
                    :dx dx
                    :xy dy
                    :file file))
      (with-canvas (:width width :height height)
        (set-line-width 2)
        (loop for s in series
           for row = 1 then (1+ row)
           for color = (get-color row)
           do
             (apply #'set-rgb-stroke color)
             (loop for x-old = nil then x-new
                for y-old = nil then y-new
                for (x y) in s
                for x-new = (* x dx)
                for y-new = (* y dy)
                when x-old do
                  (move-to x-old y-old)
                  (line-to x-new y-new)
                  (stroke)))
        (save-png file)))
    file))

(defun bogus-series (&optional (n 10))
  (loop for rows from 1 to 3
     collect (mapcar (lambda (x) (list x (random n))) (range 1 n))))

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
