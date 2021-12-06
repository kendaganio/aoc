(require :asdf)

(defun parse-input (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-integer (uiop:split-string (read-line stream nil) :separator ","))))

(defun tick (lst)
  (let ((spawned (loop for e in lst count (eq 0 e))))
    (setf lst (append lst (make-list spawned :initial-element 9)))
    (loop for e in lst collect (if (eq 0 e) 6 (1- e))
        )))

; memo
(defvar memo (make-hash-table :test 'equalp))

(defun spawn-count (life days)
  (let ((fish-count 0) (new-fish 0) (cached (gethash (format nil "~a/~a" life days) memo)))
    (if (> (1+ life) days)
      (return-from spawn-count 1))
    (if cached (return-from spawn-count cached))
    (setf fish-count (spawn-count 6 (- (1- days) life)))
    (setf new-fish (spawn-count 8 (- (1- days) life)))
    (setf (gethash (format nil "~a/~a" life days) memo) (+ fish-count new-fish))
    (return-from spawn-count (+ fish-count new-fish))))

(defun solve2 (input days)
  (let ((fishes (copy-seq input)) (count-all 0))
    (loop for fish in fishes do
          (incf count-all (spawn-count fish days)))
    (format t "part2 fishes ~a~%" count-all)))

(defun solve1 (input days)
  (let ((fishes (copy-seq input)))
    (loop for i from 0 below days do
          (setf fishes (tick fishes)))
    (format t "part1 fishes ~a~%" (length fishes))))

(time (solve2 (parse-input "./in.txt") 256))
