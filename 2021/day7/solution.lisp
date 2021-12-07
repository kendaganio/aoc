(require :asdf)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-integer (uiop:split-string (read-line stream nil) :separator ","))))

(defun solve1 (input)
  (let ((len (length input)) (highest (apply #'max input)) (lowest (apply #'min input)))
    (apply #'min (loop for pos from lowest to highest collect (reduce #'+ (loop for i in input collect (abs (- i pos))))))))

(defun sum-of-seq (n)
  (/ (* n (1+ n)) 2))

(defun solve2 (input)
  (let ((len (length input)) (highest (apply #'max input)) (lowest (apply #'min input)))
    (apply #'min (loop for pos from lowest to highest collect (reduce #'+ (loop for i in input collect (sum-of-seq (abs (- i pos)))))))))

(time (format t "part 1: ~a" (solve1 (parse-input "in.txt"))))
(time (format t "part 2: ~a" (solve2 (parse-input "in.txt"))))
