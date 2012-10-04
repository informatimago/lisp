
(shadow '(trace untrace))
(defun trace   (functions) (eval `(cl:trace   ,@functions)))
(defun untrace (functions) (eval `(cl:untrace ,@functions)))

(defun define (definitions)
  (dolist (def definitions)
    (eval (if (and (consp (second def)) (eq 'lambda (car (second def))))
              `(progn (defun        ,(first def) ,@(cdr (second def)))
                      (defparameter ,(first def) ,(second def)))
              `(defparameter ,(first def) ,(second def))))))

(defun stop (arguments) (throw 'driver-end-of-deck nil))
(defun fin  (arguments) (throw 'driver-end-of-deck nil))
(defun test (arguments) (princ arguments) (terpri))


(defun driver (path)
  (with-open-file (cards path)
    (catch 'driver-end-of-deck
      (loop (let ((first-char (read-char cards)))
              (if (char= #\* first-char)
                  (read-line cards)     ; comment
                  (progn
                    (unread-char first-char cards)
                    (let* ((command   (read cards))
                           (arguments (if (member command '(stop fin test))
                                          (list (read-line cards))
                                          (read cards))))
                      (print (apply command arguments))))))))))

(driver "wang.job")
