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
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;
;;;;    This file is part of PJB Clisp Scripts.
;;;;
;;;;    This  program is  free software;  you can  redistribute  it and/or
;;;;    modify it  under the  terms of the  GNU General Public  License as
;;;;    published by the Free Software Foundation; either version 2 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    This program  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU General Public License
;;;;    along with  this program; see the  file COPYING; if  not, write to
;;;;    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;*****************************************************************************

;; (UNUSE-PACKAGE "EXT") ;; actually, unexport its symbols.
(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.SCRIPT"
  (:DOCUMENTATION
   "This package exports script functions.")
  (:FROM COMMON-LISP :IMPORT :ALL)
  (:USE  EXT) ;; ALSO uses EXT, but does not import its symbols...
  (:USE  SYS)
  (:FROM COM.INFORMATIMAGO.CLISP.STRING :IMPORT :ALL)
  (:EXPORT  "INITIALIZE"
            "PERROR" "PMESSAGE" "PQUERY"
            "*INITIAL-WORKING-DIRECTORY*" "IS-RUNNING"
            "*PATH*" "*NAME*" "*ARGUMENTS*" "*TESTING*" "PID"
            "SHELL" "SHELL-QUOTE-ARGUMENT" "EXECUTE"
            "MAKE-DIRECTORY" "MAKE-SYMBOLIC-LINK" "COPY-FILE" ";;" "MISPLACED!"
            "EXIT" "EX-OK" "EX--BASE" "EX-USAGE"
            "EX-DATAERR" "EX-NOINPUT" "EX-NOUSER" "EX-NOHOST"
            "EX-UNAVAILABLE" "EX-SOFTWARE" "EX-OSERR" "EX-OSFILE"
            "EX-CANTCREAT" "EX-IOERR" "EX-TEMPFAIL" "EX-PROTOCOL"
            "EX-NOPERM" "EX-CONFIG" "EX--MAX" )
  ) ;;COM.INFORMATIMAGO.CLISP.SCRIPT
;; (IN-PACKAGE "COM.INFORMATIMAGO.CLISP.SCRIPT")

;; egrep '([d]efun' pjb-script.lisp | sed -e 's/(defun/;;/' | sort


;;----------------------------------------------------------------------

(DEFPARAMETER *INITIAL-WORKING-DIRECTORY*  NIL
  "
The path to the initial working directory.
BUG: This is the value of (EXT:CD) when INITIALIZE is called.
") ;;*INITIAL-WORKING-DIRECTORY*


(DEFPARAMETER *PATH*     NIL
  "
The *path* of the script.  Possibly this is not the absolute *path*, but only a
relative *path* from the INITIAL-WORKING-DIRECTORY.
BUG: This is the value of *LOAD-PATHNAME* when INITIALIZE is called.
") ;;*PATH*


(DEFPARAMETER *NAME*     NIL
  "
The name of the script.
BUG: It's derived from the value of *LOAD-PATHNAME* when INITIALIZE is called.
") ;;*NAME*


(DEFPARAMETER *ARGUMENTS* EXT:*ARGS*
  "
The list of strings containing the arguments passed to the script.
") ;;*ARGUMENTS*


(DEFPARAMETER *TESTING*   NIL
  "
Whether we're only testing the script.
In this package, this will make END-WITH-STATUS THROW :EXIT instead of exiting.
NOTE:   This variable can be set by the client script (for example,
        from a --test option).
") ;;*TESTING*


(DEFUN INITIALIZE ()
  "
DO:     Initialize this package.
        This function MUST be called from the  script itself to get the
        correct PNAME.
"
  (SETQ *INITIAL-WORKING-DIRECTORY* (EXT:CD)
        *PATH* *LOAD-PATHNAME*
        *NAME* (FILE-NAMESTRING *LOAD-PATHNAME*)
        *ARGUMENTS* (COPY-SEQ EXT:*ARGS*))
  ) ;;INITIALIZE
  




(DEFUN PERROR (FORMAT-STRING &REST ARGS)
  "
DO:     Writes a message on the error output in the name of the script.
"
  (FORMAT *ERROR-OUTPUT* "~&~A: " *NAME*)
  (APPLY (FUNCTION FORMAT) *ERROR-OUTPUT* FORMAT-STRING ARGS)
  (FINISH-OUTPUT *ERROR-OUTPUT*)
  ) ;;PERROR


(DEFUN PMESSAGE (FORMAT-STRING &REST ARGS)
  "
DO:     Writes a message on the standard output in the name of the script.
"
  (FORMAT *STANDARD-OUTPUT* "~&~A: " *NAME*)
  (APPLY (FUNCTION FORMAT) *STANDARD-OUTPUT* FORMAT-STRING ARGS)
  (FINISH-OUTPUT *STANDARD-OUTPUT*)
  ) ;;PMESSAGE


(DEFUN PQUERY (FORMAT-STRING &REST ARGS)
  "
DO:     Writes a message on the query I/O in the name of the script, and
        read a response line.
RETURN: A string containing the response line.
"
  (FORMAT *QUERY-IO* "~&~A: " *NAME*)
  (APPLY (FUNCTION FORMAT) *QUERY-IO* FORMAT-STRING ARGS)
  (FINISH-OUTPUT *QUERY-IO*)
  (READ-LINE *QUERY-IO*)
  ) ;;PQUERY




;; Awfull trick for pjb-script:is-running; put this in ~/.clisprc.lisp
;; (DEFUN EXECUTABLE-READER (A B C) (SYS::UNIX-EXECUTABLE-READER A B C))
;; (SET-DISPATCH-MACRO-CHARACTER #\# #\! #EXECUTABLE-READER)

(DEFUN IS-RUNNING ()
  "
RETURN:  Whether we're running as a script. (Otherwise, we're just loading).
"
  (EQ (GET-DISPATCH-MACRO-CHARACTER #\# #\!) #'SYS::UNIX-EXECUTABLE-READER)
  ) ;;IS-RUNNING



(DEFUN PID ()
  (LINUX:|getpid|)) ;;PID



(DEFUN SHELL-QUOTE-ARGUMENT (ARGUMENT)
  "
DO:      Quote an argument for passing as argument to an inferior shell.
RETURN:  A string containing the quoted argument.
"
  (DO ((I 0 (1+ I))
       (CH)
       (RESULT '()))
      ((<= (LENGTH ARGUMENT) I) (COERCE (NREVERSE RESULT) 'STRING))
    (SETQ CH (CHAR ARGUMENT I))
    (UNLESS (OR (CHAR= (CHARACTER "-") CH)
                (CHAR= (CHARACTER ".") CH)
                (CHAR= (CHARACTER "/") CH)
                (AND (CHAR<= (CHARACTER "A") CH) (CHAR<= CH (CHARACTER "Z")))
                (AND (CHAR<= (CHARACTER "a") CH) (CHAR<= CH (CHARACTER "z")))
                (AND (CHAR<= (CHARACTER "0") CH) (CHAR<= CH (CHARACTER "9"))))
      (PUSH (CHARACTER "\\") RESULT))
    (PUSH CH RESULT))
  ) ;;SHELL-QUOTE-ARGUMENT


(DEFUN SHELL   (COMMAND)
  "
SEE ALSO:    EXECUTE.
"
  (EXT:SHELL COMMAND)) ;;SHELL




(DEFUN EXECUTE (&REST COMMAND)
  "
RETURN:     The status returned by the command.
SEE ALSO:   SHELL
"
  (EXT:RUN-PROGRAM (CAR COMMAND)
    :ARGUMENTS (CDR COMMAND)
    :INPUT :TERMINAL :OUTPUT :TERMINAL)) ;;EXECUTE



(DEFUN COPY-FILE (FILE NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME)
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
  (declare (ignore OK-IF-ALREADY-EXISTS KEEP-TIME))
  (EXECUTE "cp" (SHELL-QUOTE-ARGUMENT FILE)  (SHELL-QUOTE-ARGUMENT NEWNAME))
  ) ;;COPY-FILE


(DEFUN MAKE-SYMBOLIC-LINK (FILENAME LINKNAME &optional OK-IF-ALREADY-EXISTS)
  "
IMPLEMENTATION: The optional argument is not implemented.

Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
"
  (declare (ignore OK-IF-ALREADY-EXISTS))
  (/= 0 (LINUX:|symlink| FILENAME LINKNAME))
  ) ;;MAKE-SYMBOLIC-LINK


(DEFUN MAKE-DIRECTORY (*PATH* &OPTIONAL (PARENTS NIL))
  "
Create the directory *PATH* and any optionally nonexistent parents dirs.
The second (optional) argument PARENTS says whether
to create parents directories if they don't exist.
"
  (IF PARENTS
      (ENSURE-DIRECTORIES-EXIST (CONCATENATE 'STRING *PATH* "/.") :VERBOSE NIL)
      (LINUX:|mkdir| *PATH*  511 #| #o777 |# ))
  (EXT:PROBE-DIRECTORY (IF (CHAR= (CHAR *PATH* (1- (LENGTH *PATH*)))
                                  (CHARACTER "/"))
                           *PATH* (CONCATENATE 'STRING *PATH* "/")))
  ) ;;MAKE-DIRECTORY




;; From /usr/include/sysexists.h (Linux)

(DEFCONSTANT EX-OK            0   "successful termination")


(DEFCONSTANT EX--BASE         64  "base value for error messages")


(DEFCONSTANT EX-USAGE         64  "command line usage error
    The command was used incorrectly, e.g., with
    the wrong number of arguments, a bad flag, a bad
    syntax in a parameter, or whatever.") ;;EX-USAGE

(DEFCONSTANT EX-DATAERR       65  "data format error
    The input data was incorrect in some way.
    This should only be used for user's data & not
    system files.") ;;EX-DATAERR

(DEFCONSTANT EX-NOINPUT       66  "cannot open input
    An input file (not a system file) did not
    exist or was not readable.  This could also include
    errors like \"No message\" to a mailer (if it cared
    to catch it).") ;;EX-NOINPUT

(DEFCONSTANT EX-NOUSER        67  "addressee unknown
    The user specified did not exist.  This might
    be used for mail addresses or remote logins.
    ") ;;EX-NOUSER

(DEFCONSTANT EX-NOHOST        68  "host name unknown
    The host specified did not exist.  This is used
    in mail addresses or network requests.") ;;EX-NOHOST

(DEFCONSTANT EX-UNAVAILABLE   69  "service unavailable
    A service is unavailable.  This can occur
    if a support program or file does not exist.  This
    can also be used as a catchall message when something
    you wanted to do doesn't work, but you don't know
    why.") ;;EX-UNAVAILABLE

(DEFCONSTANT EX-SOFTWARE      70  "internal software error
    An internal software error has been detected.
    This should be limited to non-operating system related
    errors as possible.") ;;EX-SOFTWARE

(DEFCONSTANT EX-OSERR         71  "system error (e.g., can't fork)
    An operating system error has been detected.
    This is intended to be used for such things as \"cannot
    fork\", \"cannot create pipe\", or the like.  It includes
    things like getuid returning a user that does not
    exist in the passwd file.") ;;EX-OSERR

(DEFCONSTANT EX-OSFILE        72  "critical OS file missing
    Some system file (e.g., /etc/passwd, /etc/utmp,
    etc.) does not exist, cannot be opened, or has some
    sort of error (e.g., syntax error).") ;;EX-OSFILE

(DEFCONSTANT EX-CANTCREAT     73  "can't create (user) output file
    A (user specified) output file cannot be created.") ;;EX-CANTCREAT

(DEFCONSTANT EX-IOERR         74  "input/output error
     An error occurred while doing I/O on some file.") ;;EX-IOERR

(DEFCONSTANT EX-TEMPFAIL      75  "temp failure; user is invited to retry
    temporary failure, indicating something that
    is not really an error.  In sendmail, this means
    that a mailer (e.g.) could not create a connection,
    and the request should be reattempted later.") ;;EX-TEMPFAIL

(DEFCONSTANT EX-PROTOCOL      76  "remote error in protocol
    the remote system returned something that
    was \"not possible\" during a protocol exchange.") ;;EX-PROTOCOL

(DEFCONSTANT EX-NOPERM        77  "permission denied
    You did not have sufficient permission to
    perform the operation.  This is not intended for
    file system problems, which should use NOINPUT or
    CANTCREAT, but rather for higher level permissions.") ;;EX-NOPERM

(DEFCONSTANT EX-CONFIG        78  "configuration error")


(DEFCONSTANT EX--MAX          78  "maximum listed value")



(DEFUN EXIT (&OPTIONAL (STATUS 0))
  "
DO:      Exit the script.
         If we are testing, then use throw to jump back to the caller.
"
  (WHEN (IS-RUNNING)
    (IF *TESTING*
        (THROW :EXIT STATUS)
        (EXT:EXIT STATUS))
    ;; else, when loading, we don't exit, could we?
    )
  ) ;;EXIT



;;;; script.lisp                      --                     --          ;;;;
