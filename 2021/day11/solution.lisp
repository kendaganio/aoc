(require :asdf)

(defun parse-line (line)
  (mapcar 'parse-integer (mapcar #'string (concatenate 'list line))))

(defun parse-input (filename &aux (lines nil))
  (with-open-file (stream filename)
    (defparameter lines nil)
    (setf lines (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))
  (make-array (list (length lines) (length (car lines))) :initial-contents lines))

(defun safe-aref (grid i j)
  (destructuring-bind (n m) (array-dimensions grid)
    (if (or (< i 0) (< j 0) (>= i n) (>= j m))
      (return-from safe-aref nil)
      (return-from safe-aref (aref grid i j)))))

(defvar flash-count 0)

(defun flash (grid i j)
  (incf flash-count)
  (setf (aref grid i j) -1000)
  (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1) (1 1) (1 -1) (-1 1) (-1 -1)) do 
    (if (safe-aref grid (+ i dx) (+ j dy)) 
      (progn
        (incf (aref grid (+ i dx) (+ j dy)))
        (if (> (aref grid (+ i dx) (+ j dy)) 9) (flash grid (+ i dx) (+ j dy)))))))

(defun tick (input)
    (destructuring-bind (n m) (array-dimensions input)
      (loop for i from 0 below n do
            (loop for j from 0 below m do 
                  (incf (aref input i j))
                  (if (> (aref input i j) 9) (flash input i j)))))
    (identity input))

(defun cleanup (input)
    (destructuring-bind (n m) (array-dimensions input)
      (loop for i from 0 below n do
            (loop for j from 0 below m do 
                  (if (< (aref input i j) 0) (setf (aref input i j) 0)))))
    (identity input))

(defun solve1 (input &aux (steps 100))
  (loop for i from 0 below steps do
        (setf input (cleanup (tick input))))
  (identity flash-count))

(defun is-mega-flash (input &aux (sum 0))
  (destructuring-bind (n m) (array-dimensions input)
    (loop for i from 0 below n do
          (loop for j from 0 below m do 
                (incf sum (aref input i j)))))
  (eq sum 0))

(defun solve2 (input &aux (mega-flash nil))
  (loop for i from 1 below 999999999 do
        (setf input (cleanup (tick input)))
        (if (is-mega-flash input)
        (return-from solve2 i))))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
