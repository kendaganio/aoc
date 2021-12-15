(require :asdf)

(defun safe-aref (grid i j)
  (destructuring-bind (n m) (array-dimensions grid)
    (if (or (< i 0) (< j 0) (>= i n) (>= j m))
      (return-from safe-aref nil)
      (return-from safe-aref (aref grid i j)))))

(defun parse-line (line)
  (mapcar 'parse-integer (mapcar #'string (concatenate 'list line))))

(defvar lowest-score 99999)

(defun traverse (grid i j visited &aux current-score)
  (setf visited (cons (list i j) visited))
  (setf current-score (- (loop for k in visited sum (aref grid (car k) (cadr k))) (aref grid 0 0)))

  (if (>= current-score lowest-score)
    (return-from traverse nil))

  ;(format t "i: ~a j: ~a~%" i j)
  ;(format t "v: ~a~%" visited)
  ;(terpri)

  ; stop if end point
  (destructuring-bind (n m) (array-dimensions grid)
    (if (find (list (1- n) (1- m)) visited :test 'equal)
      (progn
        (if (< current-score lowest-score)
          (setf lowest-score current-score))
        (return-from traverse nil))
    )
  )

  (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1)) ;(1 1) (1 -1) (-1 1) (-1 -1)) 
        when (and 
               (safe-aref grid (+ i dx) (+ j dy))
               (not (find (list (+ i dx) (+ j dy)) visited :test 'equal))
                  ) do
        (traverse grid (+ i dx) (+ j dy) (copy-seq visited))))

(defun parse-input (filename &aux (lines nil))
  (with-open-file (stream filename)
    (setf lines (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))
  (make-array (list (length lines) (length (car lines))) :initial-contents lines))

(defun solve1 (input &aux out)
  (traverse input 0 0 nil)
  (identity lowest-score))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./example.txt"))))
;(time (format t "[PART2]: ~a" (solve1 (parse-input "./in.txt") 40)))
