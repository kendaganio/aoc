(require :asdf)

(defun advance-probe (probe)
  (destructuring-bind (xv yv xpos ypos) probe
    (setf xpos (+ xpos xv))
    (setf ypos (+ ypos yv))
    (if (>= xv 1) (decf xv) (setf xv 0))
    (decf yv 1)
    (list xv yv xpos ypos)))

(defun hits-target-p (probe target-x target-y &aux (max-y 0) )
  (loop while 't do
        (setf probe (advance-probe probe))
        (destructuring-bind (xv yv xpos ypos) probe
          (setf max-y (max max-y ypos))
          (if (or (> xpos (cdr target-x)) (< ypos (car target-y)))
              (return-from hits-target-p nil))
           ; hit
          (if (and 
                (>= xpos (car target-x)) (<= xpos (cdr target-x))
                (>= ypos (car target-y)) (<= ypos (cdr target-y)))
              (return-from hits-target-p max-y)))))

(defun solve1 (target-x target-y &aux l temp-res) 
  (loop for i from 0 to 500 do
        (loop for j from -100 to 500 do
              (setf temp-res (hits-target-p (list i j 0 0) target-x target-y))
              (if temp-res (setf l (cons temp-res l)))))
  (format t "[PART1]: ~a~%" (apply #'max l))
  (format t "[PART2]: ~a~%" (length l)))

; puzzle input
; target area: x=253..280, y=-73..-46
(time (solve1 (cons 253 280) (cons -73 -46)))
