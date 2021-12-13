(require :asdf)

(defvar graph nil)
(defvar points nil)
(defvar folds nil)
(defvar max-x nil)
(defvar max-y nil)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do 
          (progn
            (if (search "," line)
              (setf points (cons (uiop:split-string line :separator ",") points)))
            (if (search "fold" line)
              (setf folds (cons (uiop:split-string (caddr (uiop:split-string line :separator " ")) :separator "=") folds))))))
  (setf max-y (1+ (apply 'max (loop for p in points collect (parse-integer (car p))))))
  (setf max-x (1+ (apply 'max (loop for p in points collect (parse-integer (cadr p))))))
  (setf graph (make-array (list max-x max-y) :initial-element "."))
  (loop for p in points do
        (setf (aref graph (parse-integer (cadr p)) (parse-integer (car p))) "#")))

(defun fold-y (graph mid &aux (new-graph nil) (dims nil))
  (setf dims (array-dimensions graph))
  (setf new-graph (make-array (list mid (cadr dims)) :initial-element "."))

  (loop for i from 0 below (cadr dims) do
  (loop for j from 0 below mid do
          (if (or
                (equal "#" (aref graph (1- (- (car dims) j)) i))
                (equal "#" (aref graph j i)))
            (setf (aref new-graph j i) "#")
            (setf (aref new-graph j i) "."))))
  (identity new-graph))

(defun fold-x (graph mid &aux (new-graph nil) (dims nil))
  (setf dims (array-dimensions graph))
  (setf new-graph (make-array (list (car dims) mid) :initial-element "."))

  (loop for i from 0 below mid do
  (loop for j from 0 below (car dims) do
        (if (or
              (equal "#" (aref graph j (1- (- (cadr dims) i))))
              (equal "#" (aref graph j i)))
          (setf (aref new-graph j i) "#")
          (setf (aref new-graph j i) "."))))
  (identity new-graph))

(defun solve1 (input &aux (steps 100))
  (setf first-fold (car (reverse folds)))

  (if (equalp (car first-fold) "y")
    (setf graph (fold-y graph (parse-integer (cadr first-fold)))))
  (if (equalp (car first-fold) "x")
    (setf graph (fold-x graph (parse-integer (cadr first-fold)))))

  (destructuring-bind (n m) (array-dimensions graph)
    (loop for i from 0 below n
      sum (loop for j from 0 below m 
            count (equal "#" (aref graph i j))))))

(defun solve2 (input &aux (mega-flash nil))
  (loop for f in (reverse folds) do
        (if (equalp (car f) "y")
          (setf graph (fold-y graph (parse-integer (cadr f)))))
        (if (equalp (car f) "x")
          (setf graph (fold-x graph (parse-integer (cadr f))))))
  (destructuring-bind (n m) (array-dimensions graph)
    (loop for i from 0 below n do
      (loop for j from 0 below m do
            (format t "~a" (aref graph i j)))
      (terpri))))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))

; need to reset the vars for part 2
(setf graph nil)
(setf points nil)
(setf folds nil)
(setf max-x nil)
(setf max-y nil)
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
