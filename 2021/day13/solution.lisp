(require :asdf)

(defvar graph nil)
(defvar folds nil)

(defun parse-input (filename &aux (max-x 0) (max-y 0) (points nil))
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do 
          (if (search "," line)
            (setf points (cons (uiop:split-string line :separator ",") points)))
          (if (search "fold" line)
            (setf folds (cons (uiop:split-string (caddr (uiop:split-string line :separator " ")) :separator "=") folds)))))
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

(defun fold (graph command)
  (if (equalp (car command) "y")
    (setf graph (fold-y graph (parse-integer (cadr command))))
    (setf graph (fold-x graph (parse-integer (cadr command))))))

(defun solve1 (input &aux (first-fold nil))
  (setf first-fold (car (reverse folds)))
  (setf graph (fold graph first-fold))
  (destructuring-bind (n m) (array-dimensions graph)
    (loop for i from 0 below n
      sum (loop for j from 0 below m 
            count (equal "#" (aref graph i j))))))

(defun solve2 (input &aux (mega-flash nil))
  (loop for f in (reverse folds) do (setf graph (fold graph f)))
  (destructuring-bind (n m) (array-dimensions graph)
    (loop for i from 0 below n do
      (loop for j from 0 below m do
            (format t "~a" (aref graph i j)))
      (terpri))))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt"))))
; need to reset the vars for part 2
(setf graph nil)
(setf folds nil)
(time (format t "[PART2]: ~a" (solve2 (parse-input "./in.txt"))))
