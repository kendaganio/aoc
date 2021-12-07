(require :asdf)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-integer (uiop:split-string (read-line stream nil) :separator ","))))

(defun solve1 (input)
  (let ((len (length input)) (highest (apply #'max input)) (lowest (apply #'min input)))
    (format t "min ~a~%" (apply #'min (loop for pos from lowest to highest collect (reduce #'+ (loop for i in input collect (abs (- i pos)))))))))

(defun sum-of-seq (n start end)
  (/ (* n (+ start end)) 2))

(defun solve2 (input)
  (let ((len (length input)) (highest (apply #'max input)) (lowest (apply #'min input)))
    (format t "min ~a~%" (apply #'min (loop for pos from lowest to highest
          collect (reduce #'+ (loop for i in input collect (sum-of-seq (abs (- i pos)) 1 (abs (- i pos))))))))))

(time (solve1 (parse-input "in.txt")))
(time (solve2 (parse-input "in.txt")))
