;; -*- mode:emacs-lisp -*-


(defun cl-indent (symbol num-forms)
  "
Put on the SYMBOL and its lower case and upper case variants
a 'lisp-indent-function property set to NUM-FORMS.
"
  (dolist (property '(lisp-indent-function common-lisp-indent-function))
    (put symbol property num-forms)
    (put (intern (string-downcase (symbol-name symbol))) property num-forms)
    (put (intern (string-upcase   (symbol-name symbol))) property num-forms)))


(dolist (table (list lisp-mode-syntax-table lisp-interaction-mode-syntax-table))
  (modify-syntax-entry ?\[ "(]  " table)
  (modify-syntax-entry ?\] ")[  " table))

(cl-indent 'class 2)
(cl-indent 'method 3)
(cl-indent 'class-method 3)


;;;; THE END ;;;;
