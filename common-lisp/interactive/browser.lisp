;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               browser.lisp
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
;;;;    2021-06-14 <PJB> Added SELECT-WORKING-DIRECTORY.
;;;;    2017-05-27 <PJB> All commands are functions, not macros anymore.
;;;;                     Renamed DEFCOMMAND -> DEFINE-EXTERNAL-COMMAND.
;;;;    2015-10-10 <PJB> CAT/MORE/LESS can process *STANDARD-INPUT*.
;;;;    2004-11-23 <PJB> Generalized ls formating.
;;;;    2004-09-24 <PJB> Added ls.
;;;;    2004-08-07 <PJB> Added cat, more, and less.
;;;;    2004-06-19 <PJB> Created.
;;;;BUGS
;;;;    The syntax of paths given by users is not specified.
;;;;    Both logical pathnames and physical pathnames are possible and wackily
;;;;    processed...
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2004 - 2021
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
;;;;****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "*LOAD-VERBOSE*"
                                 "*LOAD-PRINT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME"
                                 "PRINT-NOT-READABLE"
                                 "PRINT-NOT-READABLE-OBJECT")
  (:export "MAKE" "MV" "CP" "RM" "DEFINE-EXTERNAL-COMMAND" "*SHELL*" "LESS" "MORE" "CAT" "LS"
           "GREP" "MKDIR" "POPD" "PUSHD" "PWD" "CD" "BROWSE" "*TERMINAL-HEIGHT*"
           "*DIRECTORY-HISTORY*"
           "CHANGE-WORKING-DIRECTORY" "SELECT-WORKING-DIRECTORY" "WORKING-DIRECTORY" "*CHANGE-DIRECTORY-HOOK*"
           "*KEEP-DOT-FILES*")
  (:documentation
   "

This package exports a function to browse the directory hierarchy
and load lisp files, and a few interactive commands:

CD, PWD, PUSHD, POPD, MKDIR,
LS, CAT, MORE, CP, MV, MAKE, GREP.


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2002 - 2021

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER")



;;;---------------------------------------------------------------------
;;; Dates
;;;---------------------------------------------------------------------

(defun now () (get-universal-time))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))


(defvar *today* (now)
  "Used as a reference to determine which short form a date must be formated as.
Client code can rebind it to another universal date or set it to (now).")


(defvar *short-month-names*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))


(defun parse-short-month (short-month-name)
  (let ((pos (position short-month-name *short-month-names*
                       :test (function string-equal))))
    (and pos (1+ pos))))


(defun parse-ls-date (string)
  (let* ((month (parse-short-month (subseq string 0 3)))
         (day (parse-integer string :start 4 :end 6))
         (colon (position #\: string :start 7))
         (year (if colon (current-year) (parse-integer string :start 8)))
         (hour (if colon (parse-integer string :start 7 :end 9) 12))
         (minute (if colon (parse-integer string :start (1+ colon)) 0)))
    (encode-universal-time 0 minute hour day month year 0)))


(defun format-ls-date (stream date colon at &rest arguments)
  (declare (ignore at arguments))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time date)
    (if colon
        (cond
          ((< (- *today* date) (* 24 60 60))
           (format stream  "~2,'0D:~2,'0D:~2,'0D   " ho mi se))
          ((< (- *today* date) (* 6 30 24 60 60))
           (format stream "~2,'0D-~2,'0D ~2,'0D:~2,'0D" mo da ho mi))
          (t
           (format stream "~4,'0D-~2,'0D-~2,'0D " ye mo da)))
        (cond
          ((< (- *today* date) (* 6 30 24 60 60))
           (format stream "~3A ~2D ~2,'0D:~2,'0D"
                   (aref *short-month-names* (1- mo)) da ho mi))
          (t
           (format stream "~3A ~2D ~5D"
                   (aref *short-month-names* (1- mo)) da ye))))))


(defun shorter-date (universal-time)
  (format nil "~/COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER::FORMAT-LS-DATE/"
          universal-time))


;;;---------------------------------------------------------------------



(defvar *shell* nil
  "A function of one string argument executing it as a shell command.")

(defvar *verbose* nil
  "Makes some functions output information on *trace-output*.")

(defun runcommand (name args)
  (if *shell*
      (funcall *shell*
               (funcall
                (if *verbose*
                    (lambda (x) (format *trace-output* "~&~A~%" x) x)
                    (function identity))
                (format nil "~{~A~^ ~}"
                        (mapcar (lambda (item)
                                  (typecase item
                                    (symbol (string-downcase item))
                                    (otherwise item))) (cons name args)))))
      (error "Please assign a shell function to ~S" '*shell*)))

(defmacro define-external-command (name &optional docstring)
  "Define a macro named NAME taking any number of arguments, and
calling the external program of same name thru the shell."
  `(defun ,name (&rest args)
     ,(or docstring (format nil "COMMAND~%Runs the ~A command." name))
     (runcommand ',name args)))

(define-external-command rm)
(define-external-command cp)
(define-external-command mv)
(define-external-command make)
(define-external-command grep)


(defvar *keep-dot-files* nil
  "Whether dot-files should be shown.")

(defun update-default-pathname-default (working-directory)
  (setf *default-pathname-defaults*
        (merge-pathnames working-directory *default-pathname-defaults* nil)))

(defvar *change-directory-hook*
  (list 'update-default-pathname-default)

  "A list of unary functions called with the path of  the new current
working directory.  The default list contains a hook to set the
*DEFAULT-PATHNAME-DEFAULTS*.

A common usage is to set the unix current working directory to the
same directory, so that the *default-pathname-defaults*, the
*working-directory* and the unix current working directory are all
three synchronized.
")

(defvar *directory-stack* nil)
(defvar *directory-history* nil)
(defvar *working-directory* (user-homedir-pathname)
  "The current working directory")

(defun working-directory ()
  "RETURN: The working directory."
  *working-directory*)

(defun check-directories-exist (path)
  "Return: whether all the directories in PATH exist;
           the path to the first directory that doesn't exist."
  (let* ((non-existent
           (find-if-not
            (lambda (dir)

              ;; We cannot use directory to check whether a directory
              ;; exists.  So we try a file pattern, and if not found
              ;; but no error is signaled, assume the directory exists.

              ;; Of course, this doesn't work on some implementations
              ;; such as ccl.

              ;; On ccl, if we try to create a file in an inexistant
              ;; directory, it will create it!!!  But we can use
              ;; probe-file to test directories in ccl…


              (let ((probe-dirpath (make-pathname :name nil :type nil :version nil
                                                  :directory dir
                                                  :defaults path)))
                #+ccl (probe-file probe-dirpath)
                #-ccl (ignore-errors (or (directory (merge-pathnames "PROBE.FILE" probe-dirpath nil))
                                         t))))
            (nreverse
             (loop
               :for dir :on (reverse (pathname-directory path))
               :collect (reverse dir))))))
    (values (not non-existent)
            (merge-pathnames (make-pathname :directory non-existent :defaults path)
                             path nil))))


(defun change-working-directory (path)
  "
DO:     Sets *WORKING-DIRECTORY* to the new PATH, if it exists and is a directory path.
        Runs the hooks on *CHANGE-DIRECTORY-HOOK*.
RETURN: *WORKING-DIRECTORY*
"
  (multiple-value-bind (exists-p dirpath) (check-directories-exist path)
    (if exists-p
        (progn
          ;; (print (list path dirpath (truename dirpath)))
          (setf *working-directory* (truename dirpath))
          (dolist (hook *change-directory-hook*)
            (let ((*working-directory* *working-directory*))
              (funcall hook *working-directory*)))
          (pushnew *working-directory* *directory-history* :test (function equal)))
        (error "nonexistent directory: ~S" dirpath)))
  *working-directory*)

(defun select-working-directory ()
  (when *directory-history*
    (setf *directory-history* (sort *directory-history* (function string<) :key (function namestring)))
    (loop
       (loop :for index :from 1
             :for dir :in *directory-history*
             :do (format *query-io* "~4D) ~A~%" index dir)
             :finally (format *query-io* "~4D) ~A~%" 0 "Cancel."))
       (finish-output *query-io*)
       (let ((selection (read *query-io*)))
         (when (and (integerp selection))
           (cond
             ((zerop selection) (return-from select-working-directory))
             ((plusp selection) (let ((dir (nth (1- selection) *directory-history* )))
                                  (when dir
                                    (change-working-directory dir)
                                    (return-from select-working-directory))))))))))

(defun parent-directory (dirpath)
  (make-pathname :directory (let ((dir (pathname-directory dirpath)))
                              (cons (car dir) (butlast (cdr dir))))
                 :defaults dirpath))


(defun subdirectories (dirpath)
  (directory
   (merge-pathnames (make-pathname :directory '(:relative :wild)) dirpath)))


(defun filter-out-dots (list)
  (delete-if
   (lambda (path)
     (char= (character ".")
            (aref (or (pathname-name path)
                      (car (last (pathname-directory path)))) 0)))
   list))


(defun subdirectories-names (dirpath &key (keep-dot-files *keep-dot-files*))
  (let ((subs (subdirectories dirpath)))
    (unless keep-dot-files  (setf subs (filter-out-dots subs)))
    (mapcar (lambda (path) (car (last (pathname-directory path)))) subs)))


(defun child-directory (dirpath child)
  (merge-pathnames (make-pathname :directory (list :relative child)) dirpath))


(defun files (dirpath &key (type :wild) (keep-dot-files *keep-dot-files*))
  (let ((files (directory
                (make-pathname :name :wild :type type :defaults dirpath))))
    (unless keep-dot-files (setf files (filter-out-dots files)))
    (mapcar (lambda (path) (cons (pathname-name path) path)) files)))


(defparameter *screen-width* 80)

(defun print-list (stream list offset &key (index-width 2))
  (let* ((item-width (reduce (function max) list :key (function length)))
         (max-width (+ index-width 3 item-width))
         (col-count (truncate *screen-width* max-width))
         (row-count (truncate (+ (length list) col-count -1) col-count))
         (table (make-array (list col-count row-count) :initial-element ""))
         (x 0) (y 0))
    (dolist (item list)
      (setf (aref table x y)
            (format nil "~V,D) ~V,A" index-width offset item-width item))
      (incf offset)
      (incf y)
      (if (<= row-count y)
          (setf x (1+ x) y 0)))
    (dotimes (y row-count)
      (dotimes (x (1- col-count))
        (princ (aref table x y) stream)
        (princ " " stream))
      (princ (aref table (1- col-count) y) stream)
      (terpri stream))))


(defun browse ()
  "COMMAND
DO:         Displays the contents of the working directory and
            allows the user to navigate in the directory tree and
            to load files.
"
  (loop
    (let* ((subdirs     (sort (subdirectories-names (working-directory))
                              (function string<)))
           (files       (sort (files (working-directory) :type "lisp")
                              (lambda (a b) (string< (car a) (car b)))))
           (item-count  (+ (length subdirs) (length files)))
           (count-width (if (= 0 item-count) 1 (ceiling (log item-count 10)))))
      (format t "~&")
      (format t "--- current directory ----------------------------~%")
      (format t "~V,A  ~A~%" count-width "" (working-directory))
      (format t "--- parent directory ----------------------------~%")
      (format t "~V,D) ~A~%"
              count-width 0 (parent-directory (working-directory)))
      (when subdirs
        (format t "--- subdirectories -------------------------------~%")
        (print-list t subdirs 1 :index-width count-width))
      (when files
        (format t "--- files ----------------------------------------~%")
        (print-list t (mapcar (function car) files)
                    (1+ (length subdirs)) :index-width count-width))
      (format t "--------------------------------------------------~%")
      (let ((answer
              (block :answer
                (loop
                  (format t "~&Change directory number, ~
                            load file number, or -1 to quit: ")
                  (finish-output)
                  (let ((answer (read t nil nil)))
                    (typecase answer
                      (integer (if (<= -1 answer item-count)
                                   (return-from :answer answer)
                                   (format t "~&Input out of range.~%")))
                      (otherwise (format t "~&Bad input type.~%"))))))))
        (cond
          ((= -1 answer) (return))
          ((= 0 answer)
           (change-working-directory (parent-directory (working-directory))))
          ((<= answer (length subdirs))
           (change-working-directory
            (child-directory (working-directory) (elt subdirs (1- answer)))))
          (t (load (cdr (elt files (- answer (length subdirs) 1)))
              :verbose t)))))))


(defun resolve (path &key (directory nil))
  (setf path (typecase path
               (string    (namestring (pathname path)))
               (pathname  (namestring path))
               (symbol    (namestring (pathname (string-downcase path))))
               (character (string path))
               (number    (format nil "~A" path))
               (list      (cond ((eq :absolute (car path))
                                 (format nil "/~{~A~^/~}" (cdr path)))
                                ((eq :relative (car path))
                                 (format nil "~{~A~^/~}" (cdr path)))
                                (t
                                 (format nil "~{~A~^/~}" path))))
               (otherwise (error "Bad path ~S" path))))
  (merge-pathnames
   (if directory
       (make-pathname
        :directory (cons
                    (if (char= (character "/") (char path 0))
                        :absolute :relative)
                    (substitute :up ".."
                                (delete-if (lambda (item)
                                             (member item '("" ".")
                                                     :test (function string=)))
                                           (split-string path "/"))
                                :test (function string=))))
       path)
   (working-directory)))


(defun split-options (args)
  "
ARGS:   A list of strings containing options.
RETURN: A list of options; a list of arguments
        both in the order found in ARGS, and converted to string.
"
  (do ((options '())
       (arguments '())
       (args args (cdr args)))
      ((null args) (values (nreverse options) (nreverse arguments)))
    (let ((current (typecase (car args)
                     (string    (car args))
                     (keyword   (format nil "-~(~A~)" (car args)))
                     (symbol    (string-downcase (car args)))
                     (character (string (car args)))
                     (list      (car args))
                     (otherwise (format nil "~A" (car args))))))
      (if (and (< 0 (length current))
               (char= (character "-") (char current 0)))
          (push current options)
          (push current arguments)))))


(defun relativize (path default)
  (let ((dp (pathname-directory path))
        (dd (pathname-directory default)))
    (if (and (>= (length dp) (length dd)) (equal (subseq dp 0 (length dd)) dd))
        (make-pathname :directory (cons :relative (subseq dp (length dd)))
                       :defaults path)
        path)))


(defun wilder-path (path)
  (cond
    ((string= "" path)
     (list (make-pathname :name :wild)
           (make-pathname :directory '(:relative :wild))))
    ((intersection '(:wild :wild-inferior) (list* (pathname-name path)
                                                  (pathname-type path)
                                                  (pathname-version path)
                                                  (pathname-host path)
                                                  (pathname-device path)
                                                  (pathname-directory path)))
     (list path))
    (t
     (let* ((dp (pathname-directory path))
            (dk (car dp))
            (dr (if (pathname-name path) (cdr dp) (butlast (cdr dp))))
            (li (or (pathname-name path) (car (last (cdr dp))))))
       ;; (print `(dp ,dp dk ,dk dr ,dr li ,li))
       ;; ""      --> * */
       ;; /a/b/c  --> /a/b/c /a/b/c/ /a/b/c/* /a/b/c/*/
       ;; /a/b/c/ --> /a/b/c /a/b/c/ /a/b/c/* /a/b/c/*/
       ;; /a/b/f  --> /a/b/f /a/b/f/ /a/b/f/*
       (append
        (list
         ;; /a/b/c/*
         (make-pathname
          :directory (delete nil (cons dk (append dr (list li :wild))))
          :name nil :defaults path)
         ;; /a/b/c/*
         (make-pathname
          :directory (delete nil (cons dk (append dr (list li))))
          :name :wild :defaults path))
        (when (stringp li)
          (list
           ;; /a/b/c
           (make-pathname :directory (delete nil (cons dk dr))
                          :name li :defaults path)))
        (when (and (null (pathname-type path))
                   (or (null (pathname-version path))
                       (eq :newest (pathname-version path))))
          (list
           ;; /a/b/c/
           (make-pathname
            :directory (delete nil (cons dk (append dr (list li))))
            :name nil :defaults path))))))))



(defun mkdir (dir &rest other-dirs)
  "COMMAND
DO:         Create the directories.
ARGUMENTS:  A list of paths. If name or type is not nil, then the file name
            is taken as the last item in the directory path.
"
  (dolist (path (cons dir other-dirs))
    (ensure-directories-exist
     (resolve  (if (or (pathname-name path) (pathname-type path))
                   (make-pathname
                    :directory (append (or (pathname-directory path)
                                           '(:relative))
                                       (list (file-namestring path)))
                    :name nil :type nil :defaults path)
                   path)
               :directory t))))


(defun ls (&rest args)
  "COMMAND
DO:         List the files or directories.
OPTIONS:    -L long listing: item kind, size, date, name; otherwise only name.
            -A all entries: list also entries whose name starts with a dot or ends with a tilde.
ARGUMENTS:  A list of paths possibly containing wildcards.
            If none is given, then \"*\" is used.
"
  (setf *today* (get-universal-time))
  (multiple-value-bind (opts args) (split-options args)
    (let ((opt-long nil)
          (opt-all  nil))
      (dolist (opt opts)
        (cond ((or (eq :l opt) (string-equal  "-l" opt)) (setf opt-long t))
              ((or (eq :a opt) (string-equal  "-a" opt)) (setf opt-all  t))
              (t (error "Invalid option ~S" opt))))
      (dolist (entry
               (sort
                (delete-duplicates
                 ;; SBCL RETURNS DIRECTORIES FOR "*" AS WELL AS FOR "*/".
                 (mapcan
                  (lambda (path) (handler-case (directory path) (error () nil)))
                  (mapcar
                   (lambda (path) (resolve path :directory nil))
                   (or (delete-duplicates
                        (mapcan (function wilder-path) args)
                        :key (function namestring)
                        :test (function string=))
                       '("*/" "*" "*.*"))))
                 :key (function namestring)
                 :test (function string=))
                (function string<) :key (function namestring)))
        (when (or  opt-all
                   (let* ((fns  (file-namestring entry))
                          (name (if (string/= "" fns)
                                    fns
                                    (first (last (pathname-directory entry))))))
                     (not (or (prefixp "."   name)
                              #+ccl  (prefixp "\\." name)
                              (suffixp "~"   name)))))
          (if opt-long
              (format t "~1A ~10A ~11A ~A~%"
                      (if (pathname-name entry) "-" "d")
                      (handler-case
                          (with-open-file (file entry :direction :input)
                            (format nil "~10D" (file-length file)))
                        (error () ""))
                      (handler-case (shorter-date (file-write-date entry))
                        (error () ""))
                      (namestring (relativize entry (working-directory))))
              (format t "~A~%"
                      (namestring (relativize entry (working-directory)))))))))
  (values))


(defvar *terminal-height* 50
  "The number of line displayed on the terminal.
Used by functions like MORE.")

(defun more-stream (page *standard-input* *standard-output*)
  (do* ((+eof+ (list '+eof+))
        (lnum  0 (1+ lnum))
        (line (read-line *standard-input* nil +eof+)
              (read-line *standard-input* nil +eof+)))
       ((eq line +eof+))
    (write-line line *standard-output*)
    (when (and page (>= lnum page))
      (setf lnum 0)
      (princ "Type RETURN for next page:" *query-io*)
      (finish-output *query-io*)
      (clear-input *query-io*)
      (read-line *query-io* nil))))


(defun more (&rest args)
  "COMMAND
DO:         concatenate and paginate a list of files.
ARGUMENTS:  If the first argument is :PAGE,
            then the second arguments is
            either an integer giving the page height,
            or NIL indicating that no pagination must be done;
            else the page height is *TERMINAL-HEGIHT*.
            The other arguments are paths of files to be dumped
            on *STANDARD-OUTPUT*; a string-designator for \"-\"
            represents *STANDARD-INPUT*.
            If no path is given, only *STANDARD-INPUT* is processed.
"
  (let (page paths)
    (if (eq :page (first args))
        (setf page (second args)      paths (cddr args))
        (setf page  *terminal-height* paths args))
    (if paths
        (dolist (path paths)
          (if (and (typep path 'string-designator)
                   (string= path "-"))
              (more-stream page *standard-input* *standard-output*)
              (with-open-file (in (resolve path :directory nil)
                                  :direction :input
                                  :if-does-not-exist :error)
                (more-stream page in *standard-output*))))
        (more-stream page *standard-input* *standard-output*)))
  (values))


(defun less (&rest args)
  "COMMAND
SEE:        MORE
"
  (apply (function more) args))


(defun cat  (&rest paths)
  "COMMAND
SEE:        MORE
DO:         Same as more, but force no pagination.
"
  (apply (function more) :page nil paths))


(defun cd (&optional path)
  "COMMAND
DO:         Change the working directory.
ARGUMENTS:  The path of the new working directory.
            If not given, then change to the user home directory.
"
  (change-working-directory
   (if path
       (resolve path  :directory t)
       (user-homedir-pathname))))


(defun pwd   ()
  "COMMAND
DO:         Returns the current working directory.
"
  (working-directory))


(defun popd  ()
  "COMMAND
DO:         Unstack the working directory from the stack.
"
  (if *directory-stack*
      (cons (change-working-directory (pop *directory-stack*)) *directory-stack*)
      (list (working-directory))))


(defun pushd (&optional path)
  "COMMAND
DO:         Push the current working directory onto the stack, and
            change the working directory to the path (or home directory).
SEE;        POPD, CD.
"
  (if path
      (progn
        (push (working-directory) *directory-stack*)
        (cons (cd path) *directory-stack*))
      (let ((top (pop *directory-stack*)))
        (push (working-directory) *directory-stack*)
        (cons (cd top)  *directory-stack*))))


;;;; THE END ;;;;
