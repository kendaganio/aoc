(require :asdf)

(defun split-line (line)
  (uiop:split-string line :separator "|"))

(defun parse-tokens (tokens)
  (remove-if #'uiop:emptyp (uiop:split-string tokens :separator " ")))

(defun parse-line (line)
  (let ((splitted (split-line line)))
    (list (parse-tokens (car splitted)) (parse-tokens (cadr splitted)))))

(defun parse-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

(defun one-p (segment)
  (equal (length segment) 2))
(defun four-p (segment)
  (equal (length segment) 4))
(defun seven-p (segment)
  (equal (length segment) 3))
(defun eight-p (segment)
  (equal (length segment) 7))

(defun lol (segment)
  (or (one-p segment) (four-p segment) (seven-p segment) (eight-p segment)))

(defun solve1 (segments)
  (loop for line in segments
        sum (count 't (mapcar #'lol (cadr line)))))

(defun sort-by-length (line)
  (mapcar #'sort-chars (sort line (lambda (a b) (< (length a) (length b))))))

(defun sort-chars (str)
  (sort str #'char-lessp))

(defun subtract-str (a b)
  (set-difference (concatenate 'list '() a) (concatenate 'list '() b)))

(defun union-str (a b)
  (concatenate 'string (union (concatenate 'list '() a) (concatenate 'list '() b))))

(defun do-the-thing (input)
  (let ((bits (make-list 7)) (segments (sort-by-length (car input))) (to-decode (cadr input)))
    (setf one (car segments))
    (setf seven (cadr segments))
    (setf four (caddr segments))
    (setf eight (nth 9 segments))

    ; find 0th bit from #1 and #7
    (setf (car bits) (car (subtract-str seven one)))

    ; find 3rd bit from #9 by adding #4 and #7
    (setf nine-finder (union-str seven four))
    (setf nine (find-if 
                 (lambda (n) (eq (length (subtract-str n nine-finder)) 1)) 
                 (remove-if (lambda (n) (not (eq (length n) 6))) segments)))
    (setf (nth 3 bits) (car (subtract-str nine nine-finder)))

    ; find 6th bit from #3 by adding #7 and the 3rd bit
    (setf three-finder (union-str seven (concatenate 'string (list (nth 3 bits)))))
    (setf three (find-if 
                 (lambda (n) (eq (length (subtract-str n three-finder)) 1)) 
                 (remove-if (lambda (n) (not (eq (length n) 5))) segments)))
    (setf (nth 6 bits) (car (subtract-str three three-finder)))

    ; find 4th bit by subtracting #9 and #8
    (setf (nth 4 bits) (car (subtract-str eight nine)))

    ; find #2 from known bits
    (setf two-finder (concatenate 'string (remove nil bits)))
    (setf two (find-if 
                 (lambda (n) (eq (length (subtract-str n two-finder)) 1)) 
                 (remove-if (lambda (n) (not (eq (length n) 5))) segments)))
    (setf (nth 1 bits) (car (subtract-str two two-finder)))

    ; get 2nd bit by subtracting 1st bit to #1
    (setf (nth 2 bits) (car (subtract-str one (concatenate 'string (list (nth 1 bits))))))

    ; #8 - 6th bit is 0
    (setf (nth 5 bits) (car (subtract-str eight (concatenate 'string (remove nil bits)))))

    (setf five (concatenate 'string (subtract-str nine (concatenate 'string (list (nth 1 bits))))))
    (setf zero (concatenate 'string (subtract-str eight (concatenate 'string (list (nth 6 bits))))))
    (setf six (concatenate 'string (subtract-str eight (concatenate 'string (list (nth 1 bits))))))

    (defvar decoder (make-hash-table :test 'equal))
    (setf (gethash (sort-chars zero) decoder) "0")
    (setf (gethash (sort-chars one) decoder) "1")
    (setf (gethash (sort-chars two) decoder) "2")
    (setf (gethash (sort-chars three) decoder) "3")
    (setf (gethash (sort-chars four) decoder) "4")
    (setf (gethash (sort-chars five) decoder) "5")
    (setf (gethash (sort-chars six) decoder) "6")
    (setf (gethash (sort-chars seven) decoder) "7")
    (setf (gethash (sort-chars eight) decoder) "8")
    (setf (gethash (sort-chars nine) decoder) "9")

    (return-from do-the-thing (parse-integer (format nil "~{~a~^~}" (loop for s in to-decode collect (gethash (sort-chars s) decoder)))))))

(defun solve2 (segments)
  (loop for line in segments sum (do-the-thing line))
  )

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
