(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER.TEST")

(defun date (&optional (date (get-universal-time)))
  "Prints the date."
  (format t "~&~{~5*~4,'0D-~2:*~2,'0D-~2:*~2,'0D ~2:*~2,'0D:~2:*~2,'0D:~2:*~2,'0D~8*~}~%"
          (multiple-value-list (decode-universal-time date)))
  date)


(assert (eq (ADD-PROMPT-FUNCTION 'date)
            'date))
(assert (eq (REMOVE-PROMPT-FUNCTION 'date)
            'date))

(let ((old  (LIST-PROMPT-FUNCTIONS)))
  (dolist (fun old)
    (REMOVE-PROMPT-FUNCTION fun))

  (unwind-protect
       (progn
         (assert (null (LIST-PROMPT-FUNCTIONS)))
         (assert (eq (ADD-PROMPT-FUNCTION 'date)
                     'date))
         (assert (equal (LIST-PROMPT-FUNCTIONS)
                        '(date))))

    (dolist (fun  (LIST-PROMPT-FUNCTIONS))
      (REMOVE-PROMPT-FUNCTION fun))
    (dolist (fun old)
      (ADD-PROMPT-FUNCTION fun))))

;;;; THE END ;;;;
