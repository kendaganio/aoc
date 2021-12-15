(require :asdf)

(defun safe-aref (grid i j)
  (destructuring-bind (n m) (array-dimensions grid)
    (if (or (< i 0) (< j 0) (>= i n) (>= j m))
      (return-from safe-aref nil)
      (return-from safe-aref (aref grid i j)))))

(defun wot (n)
  (if (= n 9) 1 (1+ n)))

(defun x5 (line &aux next)
  (setf next line)
  (dotimes (i 4)
    (setf next (mapcar #'wot next))
    (setf line (concatenate 'list line next)))
  (identity line))

(defun y5 (lines &aux next)
  (setf next lines)
  (dotimes (i 4)
    (setf next (loop for line in next 
                     collect (mapcar #'wot line)))
    (setf lines (concatenate 'list lines next)))
  (identity lines))

(defun parse-line (line)
  (mapcar 'parse-integer (mapcar #'string (concatenate 'list line))))

(defun parse-input (filename &aux (lines nil))
  (with-open-file (stream filename)
    (setf lines (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))
  (cons
    (make-array (list (length lines) (length (car lines))) :initial-contents lines)
    (make-array (list (length lines) (length (car lines))) :initial-element 99)
  ))

(defun parse-input2 (filename &aux (lines nil))
  (with-open-file (stream filename)
    (setf lines (loop for line = (read-line stream nil)
          while line
          collect (x5 (parse-line line)))))
  (setf lines (y5 lines))
  (cons
    (make-array (list (length lines) (length (car lines))) :initial-contents lines)
    (make-array (list (length lines) (length (car lines))) :initial-element 99)))

(defun manhattan (x y i j)
  (+ (abs (- x i)) (abs (- y j))))

(defun djikstra (grid scores &aux queue x y (visited (make-hash-table :test 'equalp)))
  ; set-up queue with 0,0
  (setf queue (cons (list 0 0 0 0) queue))

  (destructuring-bind (n m) (array-dimensions grid)
    (setf x (1- n))
    (setf y (1- m)))

  (loop while (> (length queue) 0) do
        (destructuring-bind (i j score h) (pop queue)
          ;(format t "i: ~a j: ~a q: ~a~%" i j (length queue))
          (if (not (gethash (list i j) visited))
            (progn
              (setf (gethash (list i j) visited) 't)
              (setf (aref scores i j) score)

              (if (and (= i x) (= j y))
                (return-from djikstra (aref scores X Y)))

              (loop for (dx dy) in '((0 1) (1 0) (0 -1) (-1 0)) 
                    when (and 
                           (safe-aref grid (+ i dx) (+ j dy))
                           (not (gethash (list (+ i dx) (+ j dy)) visited))) 
                    do (setf queue (cons (list (+ i dx) 
                                               (+ j dy)
                                               (+ score (aref grid (+ i dx) (+ j dy)))
                                               (+ score (aref grid (+ i dx) (+ j dy))  (manhattan X Y (+ i dx) (+ j dy))))
                                         queue)))
              (setf queue (sort queue #'< :key #'cadddr))))))
  (aref scores X Y))

(defun solve1 (input &aux out)
  (djikstra (car input) (cdr input)))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
(time (format t "[PART2]: ~a" (solve1 (parse-input2 "./in.txt"))))
