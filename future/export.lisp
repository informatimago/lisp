(in-package "COMMON-LISP-USER")

(defun process-defuns (function form)
  (cond
    ((atom form))
    ((member (car form) '(defun defmacro defgeneric defmethod))
     (funcall function form))
    (t
     (process-defuns function (car form))
     (process-defuns function (cdr form)))))

(defun defined-symbols (form)
  (let ((result '()))
    (flet ((collect (symbol) (push symbol result)))
      (process-defuns (lambda (form)
                        (cond
                          ((symbolp (second form))
                           (collect (second form)))
                          ((and (consp (second form))
                                (eql 'setf (first (second form))))
                           (collect (second (second form))))))
                      (macroexpand-1 form))
      (remove-duplicates result))))

(defun export-symbols (syms)
  `(:export ,@(sort (mapcar (function string) syms) (function string<))))


