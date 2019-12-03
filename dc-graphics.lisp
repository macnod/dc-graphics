(in-package :dc-graphics)

(defun line-chart (width height series 
                   &key
                     (background '(0.0 0.0 0.0 1.0)))
  (declare (ignore background))
  (let ((x-min (apply #'min (mapcar #'car series)))
        (x-max (apply #'max (mapcar (lambda (s) (car (last s))) series)))
        (y-min nil)
        (y-max nil)
        (file "/tmp/chart.png"))
    (loop for s in series do
         (loop for n in s
            when (or (null y-min) (< n y-min))
            do (setf y-min n)
            when (or (null y-max) (> n y-max))
            do (setf y-max n)))
    (let* ((v-width (1+ (- x-max x-min)))
           (v-height (1+ (- y-max y-min)))
           (dx (/ width v-width))
           (dy (* height v-height)))
    (with-canvas (:width width :height height)
      (set-line-width 2)
      (set-rgba-stroke 1.0 1.0 1.0 0.5)
      (loop for s in series do
           (move-to 0.0 (car s))
           (loop
              for y in (cdr s)
              for x = dx then (+ x dx)
              do (line-to (float x) (float (/ y dy)))
                (setf x (+ x (+ x 0.1)))
                (stroke)))
           
      (save-png file)))
    file))

(defun line (&key (width 600) (height 200))
  (let ((file "/tmp/chart.png"))
    (with-canvas (:width width :height height)
      (set-line-width 2)
      (set-rgba-stroke 1.0 1.0 1.0 0.5)
      (with-graphics-state
        (move-to 0.0 0.0)
        (line-to 100 200)
        (save-png file)))))

