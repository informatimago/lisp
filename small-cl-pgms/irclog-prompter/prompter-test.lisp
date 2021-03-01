(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER.TEST")

(assert (eq (ADD-PROMPT-FUNCTION 'com.informatimago.common-lisp.interactive.interactive:date)
            'com.informatimago.common-lisp.interactive.interactive:date))
(assert (eq (REMOVE-PROMPT-FUNCTION 'com.informatimago.common-lisp.interactive.interactive:date)
            'com.informatimago.common-lisp.interactive.interactive:date))
(assert (null (LIST-PROMPT-FUNCTIONS)))
(assert (eq (ADD-PROMPT-FUNCTION 'com.informatimago.common-lisp.interactive.interactive:date)
            'com.informatimago.common-lisp.interactive.interactive:date))
(assert (equal (LIST-PROMPT-FUNCTIONS)
               '(com.informatimago.common-lisp.interactive.interactive:date)))

;;;; THE END ;;;;
