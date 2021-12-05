(require :asdf)
(defparameter test (uiop:read-file-lines "README.md"))

(defun hello ()
  (write-line "gago")
  (write test))

(hello)
