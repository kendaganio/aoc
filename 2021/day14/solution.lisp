(require :asdf)

(defun trim (s)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))

(defun split (s separator)
  (remove-if 'uiop:emptyp (uiop:split-string s :separator separator)))

(defun parse-input (filename &aux template rule (rules (make-hash-table :test 'equal)))
  (with-open-file (stream filename)
    (setf template (read-line stream nil))
    (read-line stream nil)
    (loop for line = (read-line stream nil)
          while line do
          (setf rule (mapcar 'trim (split line "->")))
          (setf (gethash (car rule) rules) (cadr rule))))
  (cons template rules))
  

(defun print-hash (r)
  (loop for k being the hash-keys of r do
        (format t "~a: ~a~%" k (gethash k r))))

(defun do-step (template rules &aux out)
  (loop for (a b) on (mapcar 'string (concatenate 'list template)) 
        while b 
        do (setf out
                 (concatenate 'string 
                              out 
                              (concatenate 'string (gethash (concatenate 'string a b) rules) b))))
  (concatenate 'string (string (char template 0)) out))

(defun polymerize (polymer freqs rules &aux child splitted left right (np (make-hash-table :test 'equal)))
  (loop for pair being each hash-keys of polymer 
        when (not (equal pair 0)) do
        (setf splitted (uiop:split-string pair :separator  ":"))
        (setf child (gethash (format nil "~a~a" (car splitted) (cadr splitted)) rules))

        (if (gethash child freqs)
          (incf (gethash child freqs) (gethash pair polymer)) 
          (setf (gethash child freqs) (gethash pair polymer)) )

        (setf left (format nil "~a:~a" (car splitted) child))
        (setf right (format nil "~a:~a" child (cadr splitted)))

        (if (gethash left np)
          (incf (gethash left np) (gethash pair polymer))
          (setf (gethash left np) (gethash pair polymer)))

        (if (gethash right np)
          (incf (gethash right np) (gethash pair polymer))
          (setf (gethash right np) (gethash pair polymer))))
  (return-from polymerize np))

(defun solve1 (input iterations &aux 
                     out 
                     big
                     smol
                     (template (car input)) 
                     (rules (cdr input)) 
                     (polymer (make-hash-table :test 'equal)) (freqs (make-hash-table :test 'equal)))

  (loop for a in (concatenate 'list template) do
        (setf a (string a))
        (if (gethash a freqs)
          (incf (gethash a freqs))
          (setf (gethash a freqs) 1)))

  (loop for (a b) on (concatenate 'list template) while b do
        (if (gethash (format nil "~a:~a" a b) polymer)
          (incf (gethash (format nil "~a:~a" a b) polymer))
          (setf (gethash (format nil "~a:~a" a b) polymer) 1)))

  (loop for i from 0 below iterations do 
        (setf polymer (polymerize polymer freqs rules)))

  (setf big (apply 'max (loop for k being each hash-keys of freqs collect (gethash k freqs))))
  (setf smol (apply 'min (loop for k being each hash-keys of freqs collect (gethash k freqs))))
  (- big smol))

(time (format t "[PART1]: ~a" (solve1 (parse-input "./in.txt") 10)))
(time (format t "[PART2]: ~a" (solve1 (parse-input "./in.txt") 40)))
