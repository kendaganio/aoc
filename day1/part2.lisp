(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun first-n (list n)
  (when (not (zerop n))
    (cons (parse-integer (first list)) (first-n (rest list) (1- n)))))

(defun sum-first-3 (list)
  (reduce '+ (first-n list 3)))

(defvar readings (get-file-contents "in.txt"))
(defvar higher 0)
(defvar prev-val 0)
(defvar current-val 0)

(loop while (> (length readings) 2) 
  do (progn 
       (setq current-val (sum-first-3 readings))
       (format t "~a > ~a~%" current-val prev-val)
       (if (> current-val prev-val)
         (setq higher (+ higher 1)))
       (setq prev-val current-val)
       (pop readings)))

(format t "~a~%" (1- higher))

