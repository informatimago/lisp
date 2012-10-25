

(defparameter bf$symbols '())

(defun bf$intern (name)
  (let ((ass (assoc name bf$symbols :test (function equal))))
    (if ass
        (cdr ass)
        (let ((sym (cons :$symbol (list :pname name))))
          (setf bf$symbols (cons (cons name sym) bf$symbols))
          sym))))

(defun bf$set (sym value)
  (let ((slot (member :value (cdr sym))))
    (if slot
        (setf (cadr slot) value)
        (setf (cdr sym) (list* :value value (cdr sym))))
    value))

(defun bf$symbol-value (sym)
  (let ((slot (member :value (cdr sym))))
    (when slot
        (cadr slot))))



(bf$intern "CAR")
(bf$intern "ASSOC")
(bf$set (bf$intern "X") 42)
(bf$symbol-value (bf$intern "X"))
(bf$symbol-value (bf$intern "ASSOC"))
(bf$symbol-value (bf$intern "Y"))
(bf$set (bf$intern "X") 24)
(bf$symbol-value (bf$intern "X"))
bf$symbols


