(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defvar readings (get-file-contents "in.txt"))

(defun part1 (nums)
  (loop for (prev curr) on nums while curr count (> curr prev)))

(defun part2 (nums)
  (loop for (a b c d) on nums while d count (> (+ b c d) (+ a b c))))

(format t "part1: ~a~%" (part1 readings))
(format t "part2: ~a~%" (part2 readings))
