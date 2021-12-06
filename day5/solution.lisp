(require :asdf)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line 
          collect (remove-if #'uiop:emptyp (uiop:split-string line :separator " -> ") ))))

(defparameter grid (make-array '(1000 1000)))

(defun make-point (point-str)
  (mapcar #'parse-integer (uiop:split-string point-str :separator ",")))

(defun is-horizontal (point1 point2)
  (= (car point1) (car point2)))

(defun is-vertical (point1 point2)
  (= (cadr point1) (cadr point2)))

(defun is-diagonal (point1 point2)
  (= (abs (- (car point1) (car point2))) (abs (- (cadr point1) (cadr point2)))))

(defun draw-hline (grid point1 point2)
  (let ((x-coord (car point1)) (start (if (> (cadr point2) (cadr point1)) (cadr point1) (cadr point2))) (end (if (> (cadr point1) (cadr point2)) (cadr point1) (cadr point2))))
    (loop for y from start to end 
          do (setf (aref grid x-coord y) (+ 1 (aref grid x-coord y))))
    ))

(defun draw-vline (grid point1 point2)
  (let ((y-coord (cadr point1)) (start (if (> (car point2) (car point1)) (car point1) (car point2))) (end (if (> (car point1) (car point2)) (car point1) (car point2))))
    (loop for x from start to end 
          do (setf (aref grid x y-coord) (+ 1 (aref grid x y-coord))))
    ))

(defun draw-dline (grid point1 point2)
  (let ((x-inc (if (> (- (car point1) (car point2)) 0) -1 1)) (y-inc (if (> (- (cadr point1) (cadr point2)) 0) -1 1)))
    (loop for i from 0 to (abs (- (car point1) (car point2))) do
        (let ((x-pos (+ (car point1) (* i x-inc))) (y-pos (+ (cadr point1) (* i y-inc))))
          (setf (aref grid x-pos y-pos) (incf (aref grid x-pos y-pos) 1))))))

(defun solve (lines)
  (defvar gt2 0)
  (loop for line in lines
        do (progn
             (let ((point1 (make-point (car line))) (point2 (make-point (cadr line))))
               (if (is-horizontal point1 point2)
                 (draw-hline grid point1 point2))
               (if (is-vertical point1 point2)
                 (draw-vline grid point1 point2))
               (if (is-diagonal point1 point2)
                 (draw-dline grid point1 point2))
               )))
  (destructuring-bind (n m) (array-dimensions grid)
    (loop for i from 0 below n do
      (loop for j from 0 below m do (if (> (aref grid i j) 1) (incf gt2 1)))))
  (write gt2))

(solve (parse-input "./in.txt"))
