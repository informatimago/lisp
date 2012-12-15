;;;; -*- coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:             script.lisp
;;;;LANGUAGE:         Common-Lisp
;;;;SYSTEM:           clisp
;;;;USER-INTERFACE:   clisp
;;;;DESCRIPTION
;;;;
;;;;    This module exports some functions usefull when writting clisp scripts.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-01-29 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
;;;;*****************************************************************************
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "SYS" "EXT"))
(defpackage "COM.INFORMATIMAGO.CLISP.SCRIPT"
  (:documentation
   "This package exports script functions.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLISP.STRING")
  (:export  "INITIALIZE"
            "PERROR" "PMESSAGE" "PQUERY"
            "*INITIAL-WORKING-DIRECTORY*" "IS-RUNNING"
            "*PATH*" "*NAME*" "*ARGUMENTS*" "*TESTING*" "PID"
            "SHELL" "SHELL-QUOTE-ARGUMENT" "EXECUTE"
            "MAKE-DIRECTORY" "MAKE-SYMBOLIC-LINK" "COPY-FILE"
            "EXIT" "EX-OK" "EX--BASE" "EX-USAGE"
            "EX-DATAERR" "EX-NOINPUT" "EX-NOUSER" "EX-NOHOST"
            "EX-UNAVAILABLE" "EX-SOFTWARE" "EX-OSERR" "EX-OSFILE"
            "EX-CANTCREAT" "EX-IOERR" "EX-TEMPFAIL" "EX-PROTOCOL"
            "EX-NOPERM" "EX-CONFIG" "EX--MAX" ))
(in-package "COM.INFORMATIMAGO.CLISP.SCRIPT")

;; egrep '([d]efun' pjb-script.lisp | sed -e 's/(defun/;;/' | sort


;;----------------------------------------------------------------------

(defparameter *initial-working-directory*  nil
  "
The path to the initial working directory.
BUG: This is the value of (EXT:CD) when INITIALIZE is called.
")


(defparameter *path*     nil
  "
The *path* of the script.  Possibly this is not the absolute *path*, but only a
relative *path* from the INITIAL-WORKING-DIRECTORY.
BUG: This is the value of *LOAD-PATHNAME* when INITIALIZE is called.
")


(defparameter *name*     nil
  "
The name of the script.
BUG: It's derived from the value of *LOAD-PATHNAME* when INITIALIZE is called.
")


(defparameter *arguments* ext:*args*
  "
The list of strings containing the arguments passed to the script.
")


(defparameter *testing*   nil
  "
Whether we're only testing the script.
In this package, this will make END-WITH-STATUS THROW :EXIT instead of exiting.
NOTE:   This variable can be set by the client script (for example,
        from a --test option).
")


(defun initialize ()
  "
DO:     Initialize this package.
        This function MUST be called from the  script itself to get the
        correct PNAME.
"
  (setq *initial-working-directory* (ext:cd)
        *path* *load-pathname*
        *name* (file-namestring *load-pathname*)
        *arguments* (copy-seq ext:*args*)))
  




(defun perror (format-string &rest args)
  "
DO:     Writes a message on the error output in the name of the script.
"
  (format *error-output* "~&~A: " *name*)
  (apply (function format) *error-output* format-string args)
  (finish-output *error-output*))


(defun pmessage (format-string &rest args)
  "
DO:     Writes a message on the standard output in the name of the script.
"
  (format *standard-output* "~&~A: " *name*)
  (apply (function format) *standard-output* format-string args)
  (finish-output *standard-output*))


(defun pquery (format-string &rest args)
  "
DO:     Writes a message on the query I/O in the name of the script, and
        read a response line.
RETURN: A string containing the response line.
"
  (format *query-io* "~&~A: " *name*)
  (apply (function format) *query-io* format-string args)
  (finish-output *query-io*)
  (read-line *query-io*))




;; Awfull trick for pjb-script:is-running; put this in ~/.clisprc.lisp
;; (DEFUN EXECUTABLE-READER (A B C) (SYS::UNIX-EXECUTABLE-READER A B C))
;; (SET-DISPATCH-MACRO-CHARACTER #\# #\! #EXECUTABLE-READER)

(defun is-running ()
  "
RETURN:  Whether we're running as a script. (Otherwise, we're just loading).
"
  (eq (get-dispatch-macro-character #\# #\!) #'sys::unix-executable-reader))


(defun pid ()
  (linux:|getpid|))



(defun shell-quote-argument (argument)
  "
DO:      Quote an argument for passing as argument to an inferior shell.
RETURN:  A string containing the quoted argument.
"
  (do ((i 0 (1+ i))
       (ch)
       (result '()))
      ((<= (length argument) i) (coerce (nreverse result) 'string))
    (setq ch (char argument i))
    (unless (or (char= (character "-") ch)
                (char= (character ".") ch)
                (char= (character "/") ch)
                (and (char<= (character "A") ch) (char<= ch (character "Z")))
                (and (char<= (character "a") ch) (char<= ch (character "z")))
                (and (char<= (character "0") ch) (char<= ch (character "9"))))
      (push (character "\\") result))
    (push ch result)))


(defun shell   (command)
  "
SEE ALSO:    EXECUTE.
"
  (ext:shell command))




(defun execute (&rest command)
  "
RETURN:     The status returned by the command.
SEE ALSO:   SHELL
"
  (ext:run-program (car command)
    :arguments (cdr command)
    :input :terminal :output :terminal))



(defun copy-file (file newname &optional ok-if-already-exists keep-time)
  "
IMPLEMENTATION: The optional argument is not implemented.

Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME names a directory, copy FILE there.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil.
"
  (declare (ignore ok-if-already-exists keep-time))
  (execute "cp" (shell-quote-argument file)  (shell-quote-argument newname)))


(defun make-symbolic-link (filename linkname &optional ok-if-already-exists)
  "
IMPLEMENTATION: The optional argument is not implemented.

Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
"
  (declare (ignore ok-if-already-exists))
  (/= 0 (linux:|symlink| filename linkname)))


(defun make-directory (*path* &optional (parents nil))
  "
Create the directory *PATH* and any optionally nonexistent parents dirs.
The second (optional) argument PARENTS says whether
to create parents directories if they don't exist.
"
  (if parents
      (ensure-directories-exist (concatenate 'string *path* "/.") :verbose nil)
      (linux:|mkdir| *path*  511 #| #o777 |# ))
  (ext:probe-directory (if (char= (char *path* (1- (length *path*)))
                                  (character "/"))
                           *path* (concatenate 'string *path* "/"))))




;; From /usr/include/sysexists.h (Linux)

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



(defun exit (&optional (status 0))
  "
DO:      Exit the script.
         If we are testing, then use throw to jump back to the caller.
"
  (when (is-running)
    (if *testing*
        (throw :exit status)
        (ext:exit status))))
;; when loading, we don't exit, could we?

;;;; THE END ;;;;
