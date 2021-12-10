(require :asdf)

(defun parse-line (line)
  (mapcar #'string (concatenate 'list line)))

(defun parse-input (filename)
  (with-open-file (stream filename)
    (defparameter lines nil)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

(defvar pairs (make-hash-table :test 'equal))
(setf (gethash "(" pairs) ")")
(setf (gethash "{" pairs) "}")
(setf (gethash "<" pairs) ">")
(setf (gethash "[" pairs) "]")

(defvar openers (loop for key being the hash-keys of pairs collect key))
(defvar closers (loop for key being the hash-keys of pairs collect (gethash key pairs)))

(defvar part1-scores (make-hash-table :test 'equal))
(setf (gethash ")" part1-scores) 3)
(setf (gethash "]" part1-scores) 57)
(setf (gethash "}" part1-scores) 1197)
(setf (gethash ">" part1-scores) 25137)

(defvar part2-scores (make-hash-table :test 'equal))
(setf (gethash "(" part2-scores) 1)
(setf (gethash "[" part2-scores) 2)
(setf (gethash "{" part2-scores) 3)
(setf (gethash "<" part2-scores) 4)

(defun walk (line &aux (stack nil))
  (loop for ch in line do
        (if (find ch openers :test 'equal)
          (setf stack (cons ch stack))
          (if (equal (gethash (car stack) pairs) ch) 
            (setf stack (cdr stack))
            (return-from walk (gethash ch part1-scores)))))
  (return-from walk 0))

(defun solve1 (input)
  (loop for line in input sum (walk line)))

(defun find-incomplete (lines)
  (loop for line in lines when (equal 0 (walk line)) collect line))

(defun close-and-score (line &aux (stack nil) (score 0))
  (loop for ch in line do
        (if (find ch openers :test 'equal)
          (setf stack (cons ch stack))
          (if (equal (gethash (car stack) pairs) ch) 
            (setf stack (cdr stack)))))
  (loop for s in stack do (setf score (+ (* score 5) (gethash s part2-scores))))
  (identity score))

(defun solve2 (input &aux (scores nil))
  (setf scores (sort (mapcar #'close-and-score (find-incomplete input)) #'>))
  (nth (/ (- (length scores) 1) 2) scores))
  

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
