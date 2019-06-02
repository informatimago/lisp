(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defun find-in-tree (item tree)
  (cond
    ((null tree) nil)
    ((atom tree) (eql item tree))
    (t (or (find-in-tree item (car tree))
           (find-in-tree item (cdr tree))))))
