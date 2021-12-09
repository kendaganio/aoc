(require :asdf)

(defun parse-line (line)
  (mapcar #'parse-integer (mapcar #'string (concatenate 'list line))))

(defun parse-input (filename)
  (with-open-file (stream filename)
    (defparameter lines nil)
    (setf lines (loop for line = (read-line stream nil)
          while line
          collect (parse-line line)))
    (make-array (list (length lines) (length (car lines))) :initial-contents lines)))

(defun safe-aref (grid i j)
  (destructuring-bind (n m) (array-dimensions grid)
    (if (or (< i 0) (< j 0) (>= i n) (>= j m))
      (return-from safe-aref 10)
      (return-from safe-aref (aref grid i j)))))

(defun is-low-point (input val i j) 
  (every #'identity (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)) 
                          collect (< val (safe-aref input (+ i dx) (+ j dy))))))

(defun solve1 (input)
  (let ((low-points (list '())))
    (destructuring-bind (n m) (array-dimensions input)
      (loop for i from 0 below n do
            (loop for j from 0 below m do 
                  (let ((val (aref input i j)))
                    (if (is-low-point input val i j) 
                      (push val (car low-points)))))))
    (+ (length (car low-points)) (reduce #'+ (car low-points)))))

(defun find-basin (grid i j)
  (let ((val (safe-aref grid i j)) (nodes 1))
    (if (>= val 9) (return-from find-basin 0))

    (destructuring-bind (n m) (array-dimensions grid)
      (if (or (< i 0) (< j 0) (>= i n) (>= j m))
        (write "lol")
        (setf (aref grid i j) 99)))

    (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)) 
          if (> (safe-aref grid (+ i dx) (+ j dy)) val) do
          (incf nodes (find-basin grid (+ i dx) (+ j dy))))

    (return-from find-basin nodes)))

(defun solve2 (input)
  (let ((basins (list '())))
    (destructuring-bind (n m) (array-dimensions input)
      (loop for i from 0 below n do
            (loop for j from 0 below m do 
                  (let ((val (aref input i j)))
                    (if (is-low-point input val i j) 
                      (push (find-basin (parse-input "./in.txt") i j) (car basins)))))))
    (setf sorted (sort (car basins) #'>))
    (* (car sorted) (cadr sorted) (caddr sorted))))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
