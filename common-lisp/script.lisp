;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               script.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     CLI
;;;;DESCRIPTION
;;;;    
;;;;    This file defines utilities for lisp scripts.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-11-29 <PJB> Extracted from logs scripts.
;;;;    2009-07-27 <PJB> Merged log-to-script in here.
;;;;    2009-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2009 - 2009
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.SCRIPT"
  (:nicknames "SCRIPT")
  (:use "COMMON-LISP")
  (:export "PROGRAM-NAME" "*PROGRAM-NAME*" "*VERBOSE*"
           "REDIRECTING-STDOUT-TO-STDERR"
           "DEFINE-OPTION" "CALL-OPTION-FUNCTION"  "MAIN" 
           ;; Utilities:
           "FIND-DIRECTORIES"
           "CONCAT" "MAPCONCAT"
           ;; Exit codes:
           "EX-OK" "EX--BASE" "EX-USAGE" "EX-DATAERR" "EX-NOINPUT"
           "EX-NOUSER" "EX-NOHOST" "EX-UNAVAILABLE" "EX-SOFTWARE"
           "EX-OSERR" "EX-OSFILE" "EX-CANTCREAT" "EX-IOERR"
           "EX-TEMPFAIL" "EX-PROTOCOL" "EX-NOPERM" "EX-CONFIG"
           "EX--MAX" "EX-OK" "EX--BASE" "EX-USAGE" "EX-DATAERR"
           "EX-NOINPUT" "EX-NOUSER" "EX-NOHOST" "EX-UNAVAILABLE"
           "EX-SOFTWARE" "EX-OSERR" "EX-OSFILE" "EX-CANTCREAT"
           "EX-IOERR" "EX-TEMPFAIL" "EX-PROTOCOL" "EX-NOPERM"
           "EX-CONFIG" "EX--MAX"
           ;;
           "*SHELL-OUTPUT*" "*SHELL-ERROR*" "SHELL" "UNAME" "PARSE-OPTIONS"
           ))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.SCRIPT")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *program-name* "PROGRAM-NAME"
  "Name of the program.
If available we use the actual program name (from *LOAD-PATHNAME*),
otherwise we fallback to *PROGRAM-NAME*.")

(defvar *verbose* nil
  "Adds some trace output.")

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defmacro redirecting-stdout-to-stderr (&body body)
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
        #-testing-script (ext:exit 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIONS PROCESSING
;;;


(defun program-name ()
  "This function can be used to set *program-name* in the main script.
"
  (setf *program-name*
        (file-namestring (or *load-pathname* *program-name*))))


(defparameter *options*
  (make-hash-table :test (function equal))
  "The dictionary of options.")


(defstruct option
  keys arguments documentation function)


(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (let ((string     (if (simple-string-p string)
                        string
                        (copy-seq string)))
        (separators (if (simple-string-p separators)
                        separators
                        (copy-seq separators))))
    (declare (type simple-string string separators))
    (let ((chunks  '())
          (position 0)
          (nextpos  0)
          (strlen   (length string)) )
      (loop :while (< position strlen) :do
         (loop :while (and (< nextpos strlen)
                           (not (position (char string nextpos) separators))) :do
            (setq nextpos (1+ nextpos)))
         (push (subseq string position nextpos) chunks)
         (setq position (1+ nextpos))
         (setq nextpos  position))
      (nreverse chunks))))


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



(defgeneric call-option-function (option arguments)
  (:method ((key string) arguments)
    (let* ((funopt  (gethash key *options*)))
      (if funopt
          (call-option-function funopt arguments)
          (error "Unknown option ~A ; try: ~A help" key (program-name)))))
  (:method ((option option) arguments)
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
       (setf (gethash ',main-name *options*)
             (wrap-option-function ',(cons main-name other-names)
                                   ',parameters
                                   ',docstring
                                   (lambda ,(remove '&rest parameters)
                                     ,docstring
                                     (block ,lisp-name
                                       ,@body))))
       ,@(mapcar (lambda (other-name)
                   `(setf (gethash ',other-name *options*) (gethash ',main-name *options*)))
                 other-names)
       ',lisp-name)))


(define-option ("help" "-h" "--help") ()
  "Give this help."
  (let ((options '()))
    (maphash (lambda (key option)
               (declare (ignore key))
               (pushnew option options))
             *options*)
    (format t "~2%~A options:~2%" (program-name))
    (dolist (option (sort options (function string<)
                          :key (lambda (option) (first (option-keys option)))))
      (format t "    ~{~A~^ | ~}  ~:@(~{~A ~}~)~%~@[~{~%        ~A~}~]~2%"
              (option-keys option)
              (option-arguments option)
              (option-documentation option)))
    (format t "~%")))


(defun parse-options (arguments)
  (handler-case
      (loop
         :while arguments
         :do (setf arguments (call-option-function  (pop arguments) arguments)))
    (error (err)
      (format *error-output* "~%ERROR: ~A~%" err)
      ;; TODO: return a LINUX sysexit.h error codes.
      (return-from parse-options 1)))
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From /usr/include/sysexists.h (Linux)
;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun not-implemented-here (function-name)
  (error "How to implement ~S in ~S"
         function-name
         (lisp-implementation-type)))


(defun prepare-options (options)
  (mapcar (lambda (option)
            (typecase option
              (keyword (format nil "-~(~A~)" option))
              (symbol  (string-downcase option))
              (string  option)
              (t       (prin1-to-string option))))
          options))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SHELL
;;;


(defvar *shell-output* (make-synonym-stream '*standard-output*)
  "The stream where the output stream of the shell commands is set to.")

(defvar *shell-error*  (make-synonym-stream '*error-output*)
  "The stream where the error  stream of the shell commands is set to.")



;; From stream.lisp (to be stand alone):

(defun contents-from-stream (stream &key length (min-size 256) max-extend)
  "
STREAM:     May be a binary or character, file or non-file stream.
LENGTH:     NIL, or the number of stream elements to read.
MIN-SIZE:   Minimum pre-allocated buffer size. If LENGTH is given, or STREAM
            has a FILE-LENGTH, then the MIN-SIZE is ignored.
MAX-EXTEND: NIL ==> double the buffer size, or double the buffer size until
            it's greater than MAX-EXTEND, and then increment by MAX-EXTEND.
RETURN:     A vector containing the elements read from the STREAM.
"
  (let* ((busize (or length (ignore-errors (file-length stream)) min-size))
         (eltype (stream-ELEMENT-TYPE stream))
         (initel (if (subtypep eltype 'integer) 0 #\Space))
         (buffer (make-ARRAY busize 
                             :ELEMENT-TYPE eltype
                             :INITIAL-ELEMENT initel
                             :adjustable t :fill-pointer t))
         (start 0))
    (loop
       (let ((end (read-sequence buffer stream :start start)))
         (when (or (< end busize) (and length (= length end)))
           ;; we got eof, or have read enough
           (setf (fill-pointer buffer) end)
           (return-from contents-from-stream buffer))
         ;; no eof; extend the buffer
         (setf busize
               (if (or (null max-extend) (<= (* 2 busize) max-extend))
                   (* 2 busize)
                   (+ busize max-extend))
               start end))
       (adjust-array buffer busize :initial-element initel :fill-pointer t))))


;; From file.lisp (to be stand alone):

(defun text-file-contents (path &key (if-does-not-exist :error)
                           (external-format :default))
  "
RETURN: The contents of the file at PATH as a LIST of STRING lines.
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (if (and (streamp in) (not (eq in if-does-not-exist)))
        (contents-from-stream in :min-size 16384)
        in)))



#+clisp
(defun clisp-shell (command)
  (let ((command (format nil "( ( ( ~A ) | while read line ; do echo \":output $line\" 1>&3 ; done ) |& while read line ; do echo \":error $line\" 1>&3 ; done && echo :status 0 || echo :status $? ) 3>&1" command))
        (out)
        (err)
        (status))
    (setf out (with-output-to-string (stdout)
                (setf err (with-output-to-string (stderr)
                            (with-open-stream (cmd (ext:run-shell-command command :output :stream))
                              (with-standard-io-syntax 
                                (loop
                                   :for line = (read-line cmd nil nil)
                                   :while line
                                   :do (multiple-value-bind (kind remainder) (read-from-string line)
                                         (ecase kind
                                           ((:status) (setf status (read-from-string line :start remainder)))
                                           ((:output) (write-line (subseq line remainder) stdout))
                                           ((:error)  (write-line (subseq line remainder) stderr)))))))))))
    (values status out err)))


#+(or ecl gcl)
(defun system-shell (command system-function output-file error-file status-file)
  "
Execute the command using a Bourne shell.
Return the command status;
       a string containing the standard output of the command ;
       a string containing the error output of the command.
OUTPUT-FILE:      The pathname of a temporary file.
ERROR-FILE:       The pathname of a temporary file.
STATUS-FILE:      The pathname of a temporary file.
SYSTEM-FUNCTION:  A function used to execute a sh command.
"
  (let ((command (format nil "( ( ~A ) > ~A 2> ~A && echo 0 > ~A || echo $? > ~:*~A )"
                         command output-file error-file status-file)))
    (funcall system-function command)
    (multiple-value-prog1
        (values
         (parse-integer (text-file-contents status-file))
         (text-file-contents output-file)
         (text-file-contents error-file))
      (delete-file output-file)
      (delete-file error-file)
      (delete-file status-file))))



(defun shell  (control-string &rest arguments)
  "
Formats the COMMAND with the ARGUMENTS and
synchronously execute the result using a Bourne-compatible shell,
with standard output to *SHELL-OUTPUT*, and standard error to *SHELL-ERROR*.
Returns the shell's exit code.
"
  (let ((command (apply (function format) nil control-string arguments)))

    #+abcl
    (ext:run-shell-command command :output *shell-output*)

    #+allegro
    ;; will this fail if command has embedded quotes - it seems to work
    (multiple-value-bind (stdout stderr exit-code)
        (excl.osi:command-output
         (format nil "~a -c \"~a\""
                 #+mswindows "sh" #-mswindows "/bin/sh" command)
         :input nil :whole nil
         #+mswindows :show-window #+mswindows :hide)
      (format *shell-output* "~{~&; ~a~%~}~%" stdout)
      (format *shell-error*  "~{~&; ~a~%~}~%" stderr)
      exit-code)

    #+clisp
    (multiple-value-bind (status output error) (clisp-shell command)
      (write-string output *shell-output*)
      (write-string error  *shell-error*)
      status)

    #+clozure
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output *shell-output*
                                 :wait t)))

    #+(or ecl gcl)
    (flet ((mktemp ()
             #+ecl (SI:MKSTEMP "/tmp/out-")
             #+gcl (format nil "/tmp/out-~A" (random 1000000))))
      (system-shell command
                    #+ecl (si:system command)
                    #+gcl (lisp:system command)
                    (mktemp) (mktemp) (mktemp)))

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :show-cmd nil
     :prefix ""
     :output-stream *shell-output*)

    #+sbcl
    (sb-ext:process-exit-code
     (apply #'sb-ext:run-program
            #+win32 "sh" #-win32 "/bin/sh"
            (list  "-c" command)
            :input nil :output *shell-output*
            #+win32 '(:search t) #-win32 nil))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *shell-output*))

    #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl)
    (not-implemented-here 'shell)))



(defun uname (&rest options)
  "Without OPTIONS, return a keyword naming the system (:LINUX, :DARWIN, etc).
With options, returns the first line output by uname(1)."
  (flet ((first-line (text) (subseq text 0 (position #\newline text))))
    (let ((uname  (with-output-to-string (*shell-output*)
                    (shell "uname ~A" (prepare-options options)))))
      (values (if options
                  (first-line uname)
                  (intern (string-upcase (first-line uname))
                          "KEYWORD"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some emacs style functions.
;;;

(defun find-directories (rootpath)
  "Return a list of recursive subdirectories starting from ROOTPATH
that are accessible by the user."
  (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                             :name nil :type nil :version nil)
                              rootpath nil)))

(defun concat (&rest items) (apply (function concatenate) 'string items))

(defun mapconcat (function sequence separator)
  (etypecase sequence
    (list
     (if sequence
         (let* ((items (mapcar (lambda (item)
                                 (let ((sitem (funcall function item)))
                                   (if (stringp sitem)
                                       sitem
                                       (princ-to-string sitem))))
                               sequence))
                (ssepa (if (stringp separator)
                           separator
                           (princ-to-string separator)))
                (size (+ (reduce (function +) items :key (function length))
                         (* (length ssepa) (1- (length items)))))
                (result (make-array size :element-type 'character))
                (start  0))
           (replace result  (first items) :start1 start)
           (incf start (length (first items)))
           (dolist (item (rest items))
             (replace result ssepa :start1 start) (incf start (length ssepa))
             (replace result item  :start1 start) (incf start (length item)))
           result)
         ""))
    (vector
     (if (plusp (length sequence))
         (let* ((items (map 'vector (lambda (item)
                                      (let ((sitem (funcall function item)))
                                        (if (stringp sitem)
                                            sitem
                                            (princ-to-string sitem))))
                            sequence))
                (ssepa (if (stringp separator)
                           separator
                           (princ-to-string separator)))
                (size (+ (reduce (function +) items :key (function length))
                         (* (length ssepa) (1- (length items)))))
                (result (make-array size :element-type 'character))
                (start  0))
           (replace result (aref items 0) :start1 start) (incf start (length (aref items 0)))
           (loop
              :for i :from 1 :below (length items)
              :do (replace result ssepa :start1 start) (incf start (length ssepa))
              (replace result (aref items i) :start1 start) (incf start (length (aref items i))))
           result)
         ""))))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; The main program, definition of the options
;; ;;;
;; 
;; 
;; (in-package "COMMON-LISP-USER")
;; (load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common
;;                      :defaults *load-pathname*))
;; (use-package "SCRIPT")
;; 
;; ;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))
;; 
;; ;; #-testing-script
;; (ext:exit (main ext:*args*))


;;;; THE END ;;;;
