(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defvar readings (get-file-contents "in.txt"))
(defvar higher 0)
(defvar prev-val 0)
(defvar current-val 0)

(loop while (> (length readings) 0) 
  do (progn 
       (setq current-val (parse-integer (pop readings)))
       (format t "~a > ~a~%" current-val prev-val)
       (if (> current-val prev-val)
         (setq higher (+ higher 1)))
       (setq prev-val current-val)))

(format t "~a~%" (- higher 1))

