(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.IRCLOG.MAIN"
  (:use "COMMON-LISP")
  (:use "COM.INFORMATIMAGO.SMALL-CL-PGMS.IRCLOG"
        "COM.INFORMATIMAGO.SMALL-CL-PGMS.PROMPTER")
  (:documentation "This package fetches new lines from irclogs,
and displays them before the next prompt.")
  (:export "START"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.IRCLOG.MAIN")

(defun display-new-irc-messages ()
  (let ((messages  (get-new-messages)))
    (when messages
      (fresh-line)
      (loop :for (channel messages) :in messages
            :do (loop :for message :in messages
                      :do (format t "#~A: ~A~%" channel message)))
      (force-output))))

(defun start ()
  (install-prompt-functions)
  (add-prompt-function 'display-new-irc-messages)
  (values))

;;;; THE END ;;;;
