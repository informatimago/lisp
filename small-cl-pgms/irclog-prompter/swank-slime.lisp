(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.SLIME"
  (:use "COMMON-LISP")
  (:documentation "This package exports functions to evaluate expressions in emacs thru swank/slime.")
  (:export "EVAL-IN-EMACS"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.SLIME")

(defparameter *emacs-readtable*
  (let ((rt (copy-readtable)))
    
    (setf (readtable-case rt) :preserve)
    (set-syntax-from-char #\> #\) rt)
    (set-dispatch-macro-character
     #\# #\<
     (lambda (stream subchar dispchar)
       `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
     rt)
    ;; Probably more readtable patching would be in order.
    rt))


;; We could define CLOS proxies for emacs objects for a more seamless
;; integration. swank::eval-in-emacs process the CL form to make it
;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; to emacs lisp forms returning the corresponding emacs object.

(defun eval-in-emacs (form &optional nowait)
  (let ((result #+swank (swank::eval-in-emacs `(format "%S" ,form) nowait)
                #-swank (error "Swank not available"))
        (*readtable* *emacs-readtable*))
    (with-input-from-string (in result)
      (let ((result (read in nil in)))
        result))))

;;;; THE END ;;;;
