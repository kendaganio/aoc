(defun flatten (L)
"Converts a list to single level."
    (if (null L)
        nil
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))

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

(defun parse-input (filename)
  (with-open-file (f filename)
    (setf calls (mapcar #'parse-integer (split-str (read-line f nil) ",")))
    (setf cards (loop for line = (read-line f nil)
          while line
          collect (mapcar #'parse-integer (flatten (loop for i from 0 to 4
                        collect (remove-if (lambda (s) (string= "" s)) 
                                   (split-str (string-trim 
                                                '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) 
                                                (read-line f nil)) " ")))))))))
    

; 00 01 02 03 04 
; 05 06 07 08 09 
; 10 11 12 13 14 
; 15 16 17 18 19 
; 20 21 22 23 24 
(defvar win-lines '(
  ; horizontal
  (00 01 02 03 04) (05 06 07 08 09) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24)
  ; vertical
  (00 05 10 15 20) (01 06 11 16 21) (02 07 12 17 22) (03 08 13 18 23) (04 09 14 19 24)
  ; diagonal
  (00 06 12 18 24) (20 16 12 08 04)))

(defun check-win (card)
  (loop for line in win-lines do (if (check-win-line line card) (return T))))

(defun check-win-line (line card)
  (eq 0 (reduce #'+ (loop for num in line collect (car (nthcdr num card))))))

(defun tantos (num card)
  (if (position num card)
    (setf (nth (position num card) card) 0))
  (return-from tantos card))

(defun part1 (input)
  (loop for bola in calls
    do (loop for card in cards
          do (tantos bola card)
             (if (check-win card)
               (progn
                 (write "part 1")
                 (terpri)
                 (write card)
                 (terpri)
                 (write bola)
                 (terpri)
                 (write (* bola (reduce #'+ card)))
                 (exit))))))

(defun part2 (input)
  (loop for bola in calls
    do (loop for card in cards
          do (tantos bola card)
             (if (eq (length cards) 1)
               (progn
                 (write "part 2")
                 (terpri)
                 (write card)
                 (terpri)
                 (write bola)
                 (terpri)
                 (write (* bola (reduce #'+ card)))
                 (terpri)))
             (if (check-win card)
               (progn
                 (setf cards (remove card cards)))))))

(part2 (parse-input "./in.txt"))
(part1 (parse-input "./in.txt"))
