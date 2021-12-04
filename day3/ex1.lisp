(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defvar input (get-file-contents "in.txt"))

(defun to-nums (nums)
  (loop for num across nums
        collect (parse-integer (string num))))

(defun add-in-place (total num-array)
  (loop for tt in total
       for n in num-array
       collect (+ tt n)))

(defun compute-gamma (out total)
  (if (> total 500)
    (concatenate 'string out "1" )
    (concatenate 'string out "0" )))

(defun compute-epsilon (out total)
  (if (< total 500)
    (concatenate 'string out "1" )
    (concatenate 'string out "0" )))

(defun part1 (input)
  (defparameter added-values 
    (reduce #'add-in-place (mapcar #'to-nums input)))
  (defparameter gamma 
    (reduce #'compute-gamma added-values :initial-value ""))
  (defparameter epsilon
    (reduce #'compute-epsilon added-values :initial-value ""))
  (format t "gamma: ~a epsilon: ~a~%"  gamma epsilon)
  (format t "power: ~a~%" (* (parse-integer gamma :radix 2) (parse-integer epsilon :radix 2))))

(defun find-oxygen-rating (input)
  (defun num-in-idx-eq (updated-input binary num idx)
    (defparameter bit (if (>= num (/ (length updated-input) 2)) 1 0))
    (eq (parse-integer (string (char binary idx))) bit))
  (loop for index from 0 to 11
        do 
        (defparameter max-bits (reduce #'add-in-place (mapcar #'to-nums input)))
        (defparameter bit-count (car (nthcdr index max-bits)))
        (setf input (remove-if-not (lambda (i) (num-in-idx-eq input i bit-count index)) input))
        (if (eq (length input) 1)
          (return (car input)))))

(defun find-co2-rating (input)
  (defun num-in-idx-eq (updated-input binary num idx)
    (defparameter bit (if (>= num (/ (length updated-input) 2)) 0 1))
    (eq (parse-integer (string (char binary idx))) bit))
  (loop for index from 0 to 11
        do 
        (defparameter max-bits (reduce #'add-in-place (mapcar #'to-nums input)))
        (defparameter bit-count (car (nthcdr index max-bits)))
        (setf input (remove-if-not (lambda (i) (num-in-idx-eq input i bit-count index)) input))
        (if (eq (length input) 1)
          (return (car input)))))

(defun part2 (input)
  (defparameter oxygen 
    (find-oxygen-rating input))
  (defparameter co2
    (find-co2-rating input))
  (format t "oxygen: ~a co2: ~a~%"  oxygen co2)
  (format t "life support: ~a~%" (* (parse-integer oxygen :radix 2) (parse-integer co2 :radix 2))))

(part1 input)
(part2 input)
