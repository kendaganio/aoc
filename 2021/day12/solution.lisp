(require :asdf)

(defun add-node (graph line &aux (node nil) (neighbors-from nil) (neighbors-to nil))
  (setf node (uiop:split-string line :separator "-"))
  (setf neighbors-from (gethash (car node) graph))
  (setf neighbors-to (gethash (cadr node) graph))
  (setf (gethash (car node) graph) (cons (cadr node) neighbors-from))
  (setf (gethash (cadr node) graph) (cons (car node) neighbors-to)))

;; define these 2 outside to not pass it around as much
(defvar graph (make-hash-table :test 'equal))
(defvar all-paths nil)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line do
          (add-node graph line)))
  (identity graph))

(defun occurrences (lst &aux result)
  (mapc (lambda (item &aux (pair (assoc item result :test 'equal)))
          (if pair (incf (cdr pair)) (push (cons item 1) result))) 
        lst)
  (sort result #'> :key #'cdr))

(defun small-cave-p (node)
  (every 'lower-case-p node))

(defun only-1-double (n path &aux (lst nil))
  (setf lst (remove "start" (remove-if-not 'small-cave-p (cons n path)) :test 'equal))
  (<= (loop for o in (occurrences (copy-seq lst))
            count (> (cdr o) 1)
            do (if (> (cdr o) 2) (return-from only-1-double nil))) 1))

(defun walk (start end path &aux (neighbors nil))
  (setf path (cons start path))
  (if (equal start end) (progn
    (setf all-paths (cons path all-paths))
    (return-from walk path)))
  (setf neighbors (gethash start graph))
  (loop for n in (remove "start" neighbors :test 'equal) 
        ;;; part1
        ;when (not (and (small-cave-p n) (find n path :test 'equal)))
        ;;; part2
        when (only-1-double n (copy-seq path))
        do (walk n end path)))

(defun find-paths (start-node end-node)
  (walk start-node end-node nil)
  (length all-paths))

(defun solve (input)
  (find-paths "start" "end"))

(time (format t "[PART1]: ~a" (solve (parse-input "./in.txt"))))
