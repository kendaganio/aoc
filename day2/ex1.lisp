(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defvar input (get-file-contents "in.txt"))
(defparameter hash (make-hash-table))
(setf (gethash 'x hash) 0)
(setf (gethash 'y hash) 0)
(setf (gethash 'aim hash) 0)

(defun adjust-pos (coords value)
  (case (char (car (split-str value " ")) 0)
    (#\f (progn
     (setf (gethash 'x coords) (+ (gethash 'x coords) (parse-integer (car (cdr (split-str value " "))))))
     (setf (gethash 'y coords) 
           (+ (gethash 'y coords)
           (* (gethash 'aim coords) (parse-integer (car (cdr (split-str value " ")))))))))
    (#\d (setf (gethash 'aim coords) (+ (gethash 'aim coords) (parse-integer (car (cdr (split-str value " ")))))))
    (#\u (setf (gethash 'aim coords) (- (gethash 'aim coords) (parse-integer (car (cdr (split-str value " "))))))))
  (identity coords))

(defvar pos (reduce 'adjust-pos input :initial-value hash))
(format t "~a~%" pos)
