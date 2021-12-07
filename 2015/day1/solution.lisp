(require :asdf)

(defun parse-input (filename)
  (with-open-file (stream filename) 
    (mapcar #'to-step (loop for c across (read-line stream nil)
          collect c))))

(defun to-step (ch)
  (ecase ch (#\( 1) (#\) -1)))

(defun solve1 (input)
  (reduce #'+ input))

(defun solve2 (input &aux (current-floor 0))
  (loop for d in input and pos from 1
        do (incf current-floor d)
        when (= current-floor -1) return pos))

(time (format t "[PART 1]: ~a~%" (solve1 (parse-input "in.txt"))))
(time (format t "[PART 2]: ~a~%" (solve2 (parse-input "in.txt"))))
