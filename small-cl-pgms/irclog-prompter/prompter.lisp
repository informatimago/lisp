(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER"
  (:use "COMMON-LISP")
  (:use "COM.INFORMATIMAGO.SMALL-CL-PGMS.SLIME")
  (:documentation "This package installs functions to be called before the REPL prompt is displayed.")
  (:export "ADD-PROMPT-FUNCTION"
           "REMOVE-PROMPT-FUNCTION"
           "LIST-PROMPT-FUNCTIONS"
           "PROMPT-FUNCTION-ERRORS"
           "INSTALL-PROMPT-FUNCTIONS"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER")

(defvar *prompt-functions* '()
  "A list of functions to be run before printing the promtp.")

(defvar *prompt-function-errors* '())

(defun ADD-PROMPT-FUNCTION (function)
  "Add at the end of the prompt functions list, a new function to be called each time before the prompt is printed."
  (setf *prompt-functions* (nconc *prompt-functions* (list function)))
  function)

(defun REMOVE-PROMPT-FUNCTION (function)
  "Remove an old function from the list of prompt functions."
  (setf *prompt-functions* (delete function *prompt-functions*))
  function)

(defun LIST-PROMPT-FUNCTIONS ()
  "A fresh-list of prompt functions, in the order they're called."
  (copy-list *prompt-functions*))

(defun run-prompt-functions (stream)
  "Calls each prompt function in turn, with *STANDARD-OUTPUT* bound to STREAM..
Errors are handled and printed.
STREAM is flushed."
  (let ((*standard-output* stream))
    (dolist (pfun *prompt-functions*)
      (handler-case
          (funcall pfun)
        (error (err)
          (force-output *standard-output*)
          (format *error-output* "Error while running prompt function ~S:~%  ~A~%" pfun err)
          (force-output *error-output*)
          (remove-prompt-function pfun)
          (push (list pfun err) *prompt-function-errors*))))
    (finish-output))
  (values))

(defun prompt-function-errors ()
  (prog1 *prompt-function-errors*
    (setf *prompt-function-errors* nil)))

(defvar *prompt-functions-installed* nil)

(defun install-prompt-functions ()
  "Installs the prompt functions hook into the implamentation."
  (unless *prompt-functions-installed*

    ;; Note: when using slime/swank, the REPL is implemented by emacs,
    ;; and the prompt is displayed by emacs.
    ;; Therefore this feature must be implemented in emacs.
    #+swank (progn
              (eval-in-emacs
               (quote
                (defadvice slime-repl-insert-prompt
                    (before slime-repl-insert-prompt/run-prompt-functions last () activate)
                  (let ((form (ignore-errors
                               (car (read-from-string
                                     "(cl:let ((rpf (cl:ignore-errors (cl:find-symbol \"RUN-PROMPT-FUNCTIONS\" \"COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER\")))) (cl:when rpf (cl:funcall rpf cl:*standard-output*)))"
                                     )))))
                    (when form
                      (slime-eval form))))
                )))

    ;; We still hook in the prompt for *inferior-lisp*,
    ;; and for the normal case (terminal):
    
    ;; For ccl, we cannot use the *read-loop-function* since once we're
    ;; inside the loop, this hook is not used anymore (it's used when
    ;; starting a new REPL).  Therefore we have to patch print-listener-prompt.
    #+ccl  (let ((ccl::*warn-if-redefine-kernel* nil))
             (eval
              '(defun ccl::print-listener-prompt (stream &optional (force t))
                (unless ccl::*quiet-flag*
                  (when (or force (not (eq ccl::*break-level* ccl::*last-break-level*)))
                    (run-prompt-functions stream)
                    (let ((ccl::*listener-indent* nil))
                      (fresh-line stream)
                      (format stream ccl::*listener-prompt-format* ccl::*break-level*))
                    (setf ccl::*last-break-level* ccl::*break-level*)))
                (force-output stream))))

    #+sbcl (let ((old-prompt-fun sb-int:*repl-prompt-fun*))
             (setf sb-int:*repl-prompt-fun*
                   (lambda (stream)
                     (if (null stream)
                         old-prompt-fun
                         (progn
                           (run-prompt-functions stream)
                           (force-output stream)
                           (funcall old-prompt-fun stream)
                           (finish-output stream))))))
    
    #-(or ccl sbcl)
    (error "~S is not implemented yet for ~A"
           'install-prompt-functions (lisp-implementation-type))
    (setf *prompt-functions-installed* t)))

#-(and)
(progn
  (setf *prompt-functions-installed* nil
        #+sbcl sb-int:*repl-prompt-fun* #+sbcl (function com.informatimago.pjb::prompt))
  (install-prompt-functions))
#-(and)
(progn
  (pop (LIST-PROMPT-FUNCTIONS))
  (REMOVE-PROMPT-FUNCTION 'com.informatimago.small-cl-pgms.irclog.main::display-new-irc-messages)
  (REMOVE-PROMPT-FUNCTION 'common-lisp-user::date)
  (add-PROMPT-FUNCTION 'com.informatimago.small-cl-pgms.irclog.main::display-new-irc-messages)
  (add-PROMPT-FUNCTION 'common-lisp-user::date))

;;;; THE END ;;;;
