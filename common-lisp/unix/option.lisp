;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               option.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See defpackage documentation string.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-10 <PJB> Extracted from ~/bin/script.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.UNIX.OPTION"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "PNAME" "*PROGRAM-NAME*" "*DEBUG*"
           "REDIRECTING-STDOUT-TO-STDERR"
           "DEFINE-OPTION"
           "CALL-OPTION-FUNCTION"
           "*DOCUMENTATION-TEXT*"
           "*BASH-COMPLETION-HOOK*"
           "PARSE-OPTIONS" "PARSE-OPTIONS-FINISH"
           ;; Exit codes:
           "EX-OK" "EX--BASE" "EX-USAGE" "EX-DATAERR" "EX-NOINPUT"
           "EX-NOUSER" "EX-NOHOST" "EX-UNAVAILABLE" "EX-SOFTWARE"
           "EX-OSERR" "EX-OSFILE" "EX-CANTCREAT" "EX-IOERR"
           "EX-TEMPFAIL" "EX-PROTOCOL" "EX-NOPERM" "EX-CONFIG"
           "EX--MAX"
           ;; 
           "OPTION-KEYS" "OPTION-ARGUMENTS" "OPTION-DOCUMENTATION" "OPTION-FUNCTION"
           "OPTION-LIST")
  (:documentation "

This package processes command line options.

Example:

        (defvar *force-execution* nil)

        (define-option (\"-f\" \"--force\" \"force\") ()
           \"Force execution\"
           (setf *force-execution* t))

        (defun main (arguments)
            (parse-options arguments)
            (when *force-execution* (do-it))
            (ext:exit ex-ok))

        (main ext:*args*)

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.UNIX.OPTION")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *program-name* "unnamed"
  "Name of the program.
If available we use the actual command line program name,
otherwise we fallback to *PROGRAM-NAME*.")


(defvar *debug* nil
  "Errors break into the debugger.")

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition parse-options-finish (condition)
  ((status-code :initarg :status-code :reader parse-options-finish-status-code))
  (:report "PARSE-OPTIONS-FINISH must be called only in the dynamic context of a call to PARSE-OPTIONS")
  (:documentation "Condition signaled to finish the parsing of options early."))

(defun parse-options-finish (status-code)
  "Signals the PARSE-OPTIONS-FINISH condition, which terminates option parsing early."
  (error 'parse-options-finish :status-code status-code))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defmacro redirecting-stdout-to-stderr (&body body)
  "
Execute BODY with the *standard-output*, *error-output*, and
*trace-output* redirected.  If an error occurs in BODY, then all the
redirected output is sent to *error-output*.
"
  (let ((verror  (gensym))
        (voutput (gensym)))
   `(let* ((,verror  nil)
           (,voutput (with-output-to-string (stream)
                       (let ((*standard-output* stream)
                             (*error-output*    stream)
                             (*trace-output*    stream))
                         (handler-case (progn ,@body)
                           (error (err) (setf ,verror err)))))))
      (when ,verror
        (terpri *error-output*)
        (princ ,voutput *error-output*)
        (terpri *error-output*)
        (princ ,verror *error-output*)
        (terpri *error-output*)
        (terpri *error-output*)
        #-testing-script (parse-options-finish ex-software)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From /usr/include/sysexits.h (Linux)
;;;

(defconstant ex-ok            0   "successful termination")

(defconstant ex--base         64  "base value for error messages")

(defconstant ex-usage         64  "command line usage error
The command was used incorrectly, e.g., with
the wrong number of arguments, a bad flag, a bad
syntax in a parameter, or whatever.")

(defconstant ex-dataerr       65  "data format error
The input data was incorrect in some way.
This should only be used for user's data & not
system files.")

(defconstant ex-noinput       66  "cannot open input
An input file (not a system file) did not
exist or was not readable.  This could also include
errors like \"No message\" to a mailer (if it cared
to catch it).")

(defconstant ex-nouser        67  "addressee unknown
The user specified did not exist.  This might
be used for mail addresses or remote logins.
")

(defconstant ex-nohost        68  "host name unknown
The host specified did not exist.  This is used
in mail addresses or network requests.")

(defconstant ex-unavailable   69  "service unavailable
A service is unavailable.  This can occur
if a support program or file does not exist.  This
can also be used as a catchall message when something
you wanted to do doesn't work, but you don't know
why.")

(defconstant ex-software      70  "internal software error
An internal software error has been detected.
This should be limited to non-operating system related
errors as possible.")

(defconstant ex-oserr         71  "system error (e.g., can't fork)
An operating system error has been detected.
This is intended to be used for such things as \"cannot
fork\", \"cannot create pipe\", or the like.  It includes
things like getuid returning a user that does not
exist in the passwd file.")

(defconstant ex-osfile        72  "critical OS file missing
Some system file (e.g., /etc/passwd, /etc/utmp,
etc.) does not exist, cannot be opened, or has some
sort of error (e.g., syntax error).")

(defconstant ex-cantcreat     73  "can't create (user) output file
A (user specified) output file cannot be created.")

(defconstant ex-ioerr         74  "input/output error
An error occurred while doing I/O on some file.")

(defconstant ex-tempfail      75  "temp failure; user is invited to retry
temporary failure, indicating something that
is not really an error.  In sendmail, this means
that a mailer (e.g.) could not create a connection,
and the request should be reattempted later.")

(defconstant ex-protocol      76  "remote error in protocol
the remote system returned something that
was \"not possible\" during a protocol exchange.")

(defconstant ex-noperm        77  "permission denied
You did not have sufficient permission to
perform the operation.  This is not intended for
file system problems, which should use NOINPUT or
CANTCREAT, but rather for higher level permissions.")

(defconstant ex-config        78  "configuration error")

(defconstant ex--max          78  "maximum listed value")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIONS PROCESSING
;;;


(defun pname ()
  "This function can be used to set *program-name* in
the main script  (setf script:*program-name* (script:pname))
"
  (file-namestring *program-name*))


(defstruct option
  "An option structure."
  keys arguments documentation function)

(setf (documentation 'option-keys 'function)
      "A list of option keys."
      (documentation 'option-arguments 'function)
      "A lambda-list of option arguments."
      (documentation 'option-documentation 'function)
      "The documentation of the option (a string)."
      (documentation 'option-function 'function)
      "The option function.")


(defun q&d-parse-parameters (parameters)
  "Parses (mandatory &optional optionals... &rest rest &key key...)"
  (loop
     :with mandatories = '()
     :with optionals   = '()
     :with rest        = nil
     :with keys        = '()
     :with state       = :mandatory
     :with params      = parameters
     :for param = (first params)
     :while params
     :do (ecase state
           ((:mandatory)
            (case param
              ((&optional) (setf state :optional))
              ((&rest)     (setf state :rest))
              ((&key)      (setf state :key))
              (otherwise (push param mandatories)))
            (pop params))
           ((:optional)
            (case param
              ((&optional) (error "&OPTIONAL given more than once in ~S" parameters))
              ((&rest)     (setf state :rest))
              ((&key)      (setf state :key))
              (otherwise (push param optionals)))
            (pop params))
           ((:rest)
            (case param
              ((&optional) (error "&OPTIONAL given after &REST in ~S" parameters))
              ((&rest)     (error "&REST given twice in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (setf state :after-rest
                                 rest param)))
            (pop params))
           ((:after-rest)
            (case param
              ((&optional) (error "&OPTIONAL given after &REST in ~S" parameters))
              ((&rest)     (error "&REST given after &REST in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (error "Several &REST parameters given in ~S" parameters)))
            (pop params))
           ((:key)
            (case param
              ((&optional) (error "&OPTIONAL given after &KEY in ~S" parameters))
              ((&rest)     (error "&REST given after &KEY in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (push param keys)))
            (pop params)))
     :finally (return (values (nreverse mandatories)
                              (nreverse optionals)
                              rest
                              (nreverse keys)))))


(defun keywordize (string-designator)
  (intern (string string-designator) (load-time-value (find-package "KEYWORD"))))


(defun q&d-arguments (mandatories optionals rest keys)
  "
BUG: when the optionals or keys have a present indicator,
     we just ignore it and build a list that will pass
     the default value anyways...
" 
  (assert (every (function symbolp) mandatories))
  (append mandatories
          (mapcar (lambda (opt)
                    (etypecase opt
                      (cons   (first opt))
                      (symbol opt)))
                  optionals)
          (when rest (list rest))
          (mapcan (lambda (key)
                    (etypecase key
                      (cons  (etypecase (first key)
                               (symbol (list (keywordize (first key)) (first key)))
                               (cons   (list (second (first key)) (first (first key))))))
                      (symbol (list (keywordize key) key))))
                  keys)))


(defun wrap-option-function (keys option-arguments docstring option-function)
  (let ((vargs (gensym)))
    (multiple-value-bind (mandatories optionals rest keys-args) (q&d-parse-parameters option-arguments)
      (setf *print-circle* nil)
      (make-option
       :keys keys
       :arguments option-arguments
       :function (compile (make-symbol (format nil "~:@(~A-WRAPPER~)" (first keys)))
                          `(lambda (,vargs)
                             (if (<= ,(length mandatories) (length ,vargs))
                                 ,(cond
                                   (rest
                                    `(destructuring-bind ,option-arguments ,vargs
                                       (funcall ',option-function ,@(q&d-arguments mandatories
                                                                                   optionals
                                                                                   rest
                                                                                   keys-args))
                                       nil))
                                   (keys-args
                                    (error "An option cannot have &key parameters without a &rest parameter. ~@
                                            Invalid option parameters: ~S" option-arguments))
                                   (t
                                    (let ((vremaining (gensym)))
                                      `(destructuring-bind (,@option-arguments &rest ,vremaining) ,vargs
                                         (funcall ',option-function ,@(q&d-arguments mandatories
                                                                                     optionals
                                                                                     rest
                                                                                     keys-args))
                                         ,vremaining))))
                                 (error "Missing arguments: ~{~A ~}"
                                        (subseq ',option-arguments (length ,vargs))))))
       :documentation (split-string docstring (string #\newline))))))




;;; ---

;;; The public API is:
;;; (register-option option warn-on-conflicts)
;;; (get-option key case-sensitive)


(defparameter *options* '()
  "A list of all registered options.")

(defparameter *case-sensitive-options-map* nil
  "A cached dictionary of options.")
(defparameter *case-insensitive-options-map* nil
  "A cached dictionary of options.")

(defun find-option (keys)
  (let* ((options (loop
                    :for option :in *options*
                    :append (loop
                              :for key :in keys
                              :if (member key (option-keys option))
                                :collect (list option "case sensitive" key)
                              :else
                                :if (member key (option-keys option) :test (function equalp))
                                  :collect (list option "case insensitive" key))))
         (option (remove-duplicates (mapcar (function first) options))))
    (values option options)))

(defun fill-option-map (table)
  (loop
    :for option :in *options*
    :do (loop :for key :in (option-keys option)
              :do (setf (gethash key table) option)))
  table)

(defun register-option (option warn-on-conflicts)
  (when warn-on-conflicts
    (multiple-value-bind (old-option conflicts) (find-option (option-keys option))
      (when old-option
        (let ((*print-circle* nil) (*print-escape* nil))
          (warn "There are already options for ~:{the ~A key ~A~^, ~}."
                (mapcar (function rest) conflicts))))))
  (push option *options*))

(defun get-option (key case-sensitive)
  (let ((table (if case-sensitive
                   *case-sensitive-options-map*
                   *case-insensitive-options-map*)))
    (gethash key
             (or table
                 (fill-option-map 
                  (if case-sensitive
                      (setf *case-sensitive-options-map*   (make-hash-table :test (function equal)))
                      (setf *case-insensitive-options-map* (make-hash-table :test (function equalp)))))))))




;;; ---

(defgeneric call-option-function (option arguments undefined-argument case-sensitive)
  (:documentation  "
DO:                  Call the option function with the ARGUMENTS.
RETURN:              The remaining list of arguments.
UNDEFINED-ARGUMENT:  A function taking an option key and the remaining
                     list of arguments, called if an undefined
                     argument is found in ARGUMENTS.  It should return
                     the new remaining list of arguments.
")
  (:method ((key string) arguments undefined-argument case-sensitive)
    (let* ((funopt (get-option key case-sensitive)))
      (cond
        (funopt             (call-option-function funopt arguments undefined-argument case-sensitive))
        (undefined-argument (funcall undefined-argument key arguments))
        (t                  (error "Unknown option ~A ; try: ~A help" key (pname))))))
  (:method ((option option) arguments undefined-argument case-sensitive)
    (declare (ignore undefined-argument case-sensitive))
    (funcall (option-function option) arguments)))


(defmacro define-option (names parameters &body body)
  "
DO:         Define a new option for the scirpt.
NAMES:      A list designator of option names (strings
            such as \"-a\" \"--always\").
PARAMETERS: A list of option parameters.  The names of
            these parameters must be descriptive as they
            are used to build the usage help text.
BODY:       The code implementing this option.
RETURN:     The lisp-name of the option (this is a symbol
            named for the first option name).
"
  (let* ((main-name   (if (listp names)
                          (first names)
                          names))
         (other-names (if (listp names)
                          (rest names)
                          '()))
         (lisp-name   (intern (string-upcase main-name)))
         (docstring   (if (and (stringp (first body)) (rest body))
                          (first body)
                          nil))
         (body        (if (and (stringp (first body)) (rest body))
                          (rest body)
                          body)))
    `(progn
       (register-option (wrap-option-function ',(cons main-name other-names)
                                              ',parameters
                                              ',docstring
                                              (lambda ,(remove '&rest parameters)
                                                ,docstring
                                                (block ,lisp-name
                                                  ,@body)))
                        t)
       ',lisp-name)))


(defvar *documentation-text* ""
  "Some general documentation text issued by the --help command.")



(defun option-list ()
  "
RETURN: The list of options defined.
"
  (copy-list *options*))


(define-option ("help" "-h" "--help") ()
  "Give this help."
  (let ((options (option-list)))
    (format t "~2%~A options:~2%" (pname))
    (dolist (option (sort options (function string<)
                          :key (lambda (option) (first (option-keys option)))))
      (format t "    ~{~A~^ | ~}  ~:@(~{~A ~}~)~%~@[~{~%        ~A~}~]~2%"
              (option-keys option)
              (option-arguments option)
              (option-documentation option)))
    (format t "~A~%" *documentation-text*)))




;; TODO: See if we couldn't simplify it, perhaps with complete -C.

(defun list-all-option-keys ()
  (let ((keys '()))
    (dolist (option (option-list))
      (dolist (key (option-keys option))
        (push key keys)))
    keys))


(defun completion-option-prefix (prefix)
  (dolist (key (remove-if-not (lambda (key)
                                (and (<= (length prefix) (length key))
                                     (string= prefix key :end2 (length prefix))))
                              (list-all-option-keys)))
    (format t "~A~%" key))
  (finish-output))


(defun completion-all-options ()
  (dolist (key (list-all-option-keys))
    (format t "~A~%" key))
  (finish-output))


(defvar *bash-completion-hook* nil
  "A function (lambda (index words) ...)
that will print the completion and return true, or do nothing and return nil.")


(define-option ("--bash-completions") (index &rest words)
  "Implement the auto-completion of arguments.
This option is designed to be invoked from the function generated by
the '--bash-completion-function' option.  There should be no need to
use directly.
"
  (let ((index (parse-integer index :junk-allowed t)))
    (unless (and *bash-completion-hook*
                 (funcall *bash-completion-hook* index words))
      (if index
          (completion-option-prefix (elt words index))
          (completion-all-options))))
  (parse-options-finish ex-ok))


(define-option ("--bash-completion-function") ()
  "Write two bash commands (separated by a semi-colon) to create a
bash function used to do auto-completion of command arguments.
Use it with:

       eval $($COMMAND  --bash-completion-function) 

and then typing TAB on the command line after the command name will
autocomplete argument prefixes.
"
  (format t "function completion_~A(){ ~
COMPREPLY=( $(~:*~A --bash-completions \"$COMP_CWORD\" \"${COMP_WORDS[@]}\") ) ; } ;~
complete -F completion_~:*~A ~:*~A~%"
          *program-name*)
  (parse-options-finish ex-ok))



(defun parse-options (arguments &optional default undefined-argument (case-sensitive t))
  "
DO:                 Parse the options in the ARGUMENTS list.
DEFAULT:            Thunk called if ARGUMENTS is empty.
UNDEFINED-ARGUMENT: Thunk called if an undefined option is present in
                    the ARGUMENTS.
"
  (handler-case
      (flet ((process-arguments ()
               (cond
                 (arguments
                  (loop
                    :while arguments
                    :do (setf arguments (call-option-function (pop arguments)
                                                              arguments
                                                              undefined-argument
                                                              case-sensitive)))
                  nil)
                 (default
                  (funcall default)))))
        (if *debug*
            (process-arguments)
            (handler-case
                (process-arguments)
              ;; Somewhat arbitrary dispatching of lisp conditions to
              ;; linux sysexits:
              ((or arithmetic-error parse-error print-not-readable type-error) (err)
                (format *error-output* "~%ERROR: ~A~%" err)
               (parse-options-finish ex-dataerr))
              ((or cell-error control-error package-error program-error) (err)
                (format *error-output* "~%ERROR: ~A~%" err)
               (parse-options-finish ex-software))
              (file-error (err)
                (format *error-output* "~%ERROR: ~A~%" err)
                (parse-options-finish ex-osfile))
              (stream-error (err)
                (format *error-output* "~%ERROR: ~A~%" err)
                (parse-options-finish ex-ioerr))
              (error (err)
                (format *error-output* "~%ERROR: ~A~%" err)
                (parse-options-finish ex-software)))))
    (parse-options-finish (condition)
      (return-from parse-options (parse-options-finish-status-code condition)))))


;;;; THE END ;;;;
