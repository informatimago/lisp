;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               browser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to browse the directory hierarchy
;;;;    and load lisp files, and a few interactive commands:
;;;;    CD, PWD, PUSHD, POPD, MKDIR,
;;;;    LS, CAT, MORE, CP, MV, MAKE, GREP.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
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
;;;;****************************************************************************


(IN-PACKAGE "COMMON-LISP-USER") 
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BROWSER"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "MAKE" "MV" "CP" "DEFCOMMAND" "*SHELL*" "LESS" "MORE" "CAT" "LS"
           "MKDIR" "POPD" "PUSHD" "PWD" "CD" "BROWSE" "*TERMINAL-HEIGHT*"
           "CHANGE-WORKING-DIRECTORY" "WORKING-DIRECTORY" "*CHANGE-DIRECTORY-HOOK*"
           "*KEEP-DOT-FILES*")
  (:DOCUMENTATION
   "This package exports a function to browse the directory hierarchy
    and load lisp files.

    Copyright Pascal J. Bourguignon 2002 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BROWSER")



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
  (format nil "~/COM.INFORMATIMAGO.COMMON-LISP.BROWSER::FORMAT-LS-DATE/"
          universal-time))


;;;---------------------------------------------------------------------



(defvar *shell* nil)
(defvar *verbose* nil)

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
  
(defmacro defcommand (name)
  `(defmacro ,name (&rest args)
     (list 'runcommand '',name (list 'quote args))))

(defcommand cp)
(defcommand mv)
(defcommand make)
(defcommand grep)


(DEFVAR *KEEP-DOT-FILES* NIL
  "Whether dot-files should be shown.")

(DEFVAR *CHANGE-DIRECTORY-HOOK*
  (list (lambda (working-directory)
          (setf *default-pathname-defaults*
                (merge-pathnames working-directory
                                 *default-pathname-defaults* nil))))
  "A list of unary functions called with the path of 
   the new current working directory.")

(DEFVAR *WORKING-DIRECTORY* (TRUENAME (USER-HOMEDIR-PATHNAME))
  "The current working directory")

(DEFUN WORKING-DIRECTORY () *WORKING-DIRECTORY*)

(defun check-directories-exist (path)
  (let ((non-existent
         (find-if-not
          (lambda (dir) (directory (merge-pathnames (make-pathname :directory dir)
                                               *default-pathname-defaults* nil)))
          (nreverse
           (loop :for dir :on (reverse (pathname-directory path))
              :collect (reverse dir))))))
    (values (not non-existent)
            (merge-pathnames (make-pathname :directory non-existent)
                             *default-pathname-defaults* nil))))


(DEFUN CHANGE-WORKING-DIRECTORY (PATH)
  (multiple-value-bind (exists-p dirpath) (check-directories-exist path)
    (if exists-p
        (progn
          ;; (print (list path dirpath (truename dirpath)))
          (SETF *WORKING-DIRECTORY* (truename dirpath))
          (DOLIST (HOOK *CHANGE-DIRECTORY-HOOK*)
            (LET ((*WORKING-DIRECTORY* *WORKING-DIRECTORY*))
              (FUNCALL HOOK *WORKING-DIRECTORY*))))
        (error "nonexistent directory: ~S" dirpath)))
  *WORKING-DIRECTORY*)


  
(DEFUN PARENT-DIRECTORY (DIRPATH)
  (MAKE-PATHNAME :DIRECTORY (LET ((DIR (PATHNAME-DIRECTORY DIRPATH)))
                              (CONS (CAR DIR) (BUTLAST (CDR DIR))))
                 :defaults dirpath))


(DEFUN SUBDIRECTORIES (DIRPATH)
  (DIRECTORY
   (merge-pathnames (MAKE-PATHNAME :DIRECTORY '(:RELATIVE :WILD)) DIRPATH)))


(DEFUN FILTER-OUT-DOTS (LIST)
  (DELETE-IF
   (LAMBDA (PATH)
     (CHAR= (CHARACTER ".")
            (AREF (OR (PATHNAME-NAME PATH)
                      (CAR (LAST (PATHNAME-DIRECTORY PATH)))) 0)))
   LIST))


(DEFUN SUBDIRECTORIES-NAMES (DIRPATH &KEY (KEEP-DOT-FILES *KEEP-DOT-FILES*))
  (LET ((SUBS (SUBDIRECTORIES DIRPATH)))
    (UNLESS KEEP-DOT-FILES  (SETF SUBS (FILTER-OUT-DOTS SUBS)))
    (MAPCAR (LAMBDA (PATH) (CAR (LAST (PATHNAME-DIRECTORY PATH)))) SUBS)))


(DEFUN CHILD-DIRECTORY (DIRPATH CHILD)
  (MERGE-PATHNAMES (MAKE-PATHNAME :DIRECTORY (LIST :RELATIVE CHILD)) DIRPATH))


(DEFUN FILES (DIRPATH &KEY (TYPE :WILD) (KEEP-DOT-FILES *KEEP-DOT-FILES*))
  (LET ((FILES (DIRECTORY
                (MAKE-PATHNAME :NAME :WILD :TYPE TYPE :defaults DIRPATH))))
    (UNLESS KEEP-DOT-FILES (SETF FILES (FILTER-OUT-DOTS FILES)))
    (MAPCAR (LAMBDA (PATH) (CONS (PATHNAME-NAME PATH) PATH)) FILES)))


(DEFPARAMETER *SCREEN-WIDTH* 80)

(DEFUN PRINT-LIST (STREAM LIST OFFSET &KEY (INDEX-WIDTH 2))
  (LET* ((ITEM-WIDTH (REDUCE (FUNCTION MAX) LIST :KEY (FUNCTION LENGTH)))
         (MAX-WIDTH (+ INDEX-WIDTH 3 ITEM-WIDTH))
         (COL-COUNT (TRUNCATE *SCREEN-WIDTH* MAX-WIDTH))
         (ROW-COUNT (TRUNCATE (+ (LENGTH LIST) COL-COUNT -1) COL-COUNT))
         (TABLE (MAKE-ARRAY (LIST COL-COUNT ROW-COUNT) :INITIAL-ELEMENT ""))
         (X 0) (Y 0))
    (DOLIST (ITEM LIST)
      (SETF (AREF TABLE X Y)
            (FORMAT NIL "~V,D) ~V,A" INDEX-WIDTH OFFSET ITEM-WIDTH ITEM))
      (INCF OFFSET)
      (INCF Y)
      (IF (<= ROW-COUNT Y)
          (SETF X (1+ X) Y 0)))
    (DOTIMES (Y ROW-COUNT)
      (DOTIMES (X (1- COL-COUNT))
        (PRINC (AREF TABLE X Y) STREAM)
        (PRINC " " STREAM))
      (PRINC (AREF TABLE (1- COL-COUNT) Y) STREAM)
      (TERPRI STREAM))))


(DEFUN BROWSE ()
  "COMMAND
DO:         Displays the contents of the working directory and
            allows the user to navigate in the directory tree and
            to load files.
"
  (LOOP
     (LET* ((SUBDIRS     (SORT (SUBDIRECTORIES-NAMES (WORKING-DIRECTORY))
                               (FUNCTION STRING<)))
            (FILES       (SORT (FILES (WORKING-DIRECTORY) :TYPE "lisp")
                               (LAMBDA (A B) (STRING< (CAR A) (CAR B)))))
            (ITEM-COUNT  (+ (LENGTH SUBDIRS) (LENGTH FILES)))
            (COUNT-WIDTH (if (= 0 item-count) 1 (CEILING (LOG ITEM-COUNT 10)))))
       (FORMAT T "~&")
       (FORMAT T "--- current directory ----------------------------~%")
       (FORMAT T "~V,A  ~A~%" COUNT-WIDTH "" (WORKING-DIRECTORY))
       (FORMAT T "--- parent directory ----------------------------~%")
       (FORMAT T "~V,D) ~A~%"
               COUNT-WIDTH 0 (PARENT-DIRECTORY (WORKING-DIRECTORY)))
       (WHEN SUBDIRS
         (FORMAT T "--- subdirectories -------------------------------~%")
         (PRINT-LIST T SUBDIRS 1 :INDEX-WIDTH COUNT-WIDTH))
       (WHEN FILES
         (FORMAT T "--- files ----------------------------------------~%")
         (PRINT-LIST T (MAPCAR (FUNCTION CAR) FILES)
                     (1+ (LENGTH SUBDIRS)) :INDEX-WIDTH COUNT-WIDTH))
       (FORMAT T "--------------------------------------------------~%")
       (LET ((ANSWER
              (BLOCK :ANSWER
                (LOOP
                   (FORMAT T "~&Change directory number, ~
                            load file number, or -1 to quit: ")
                   (FINISH-OUTPUT)
                   (LET ((ANSWER (READ T NIL NIL)))
                     (TYPECASE ANSWER
                       (INTEGER (IF (<= -1 ANSWER ITEM-COUNT)
                                    (RETURN-FROM :ANSWER ANSWER)
                                    (FORMAT T "~&Input out of range.~%")))
                       (OTHERWISE (FORMAT T "~&Bad input type.~%"))))))))
         (COND
           ((= -1 ANSWER) (RETURN))
           ((= 0 ANSWER)
            (CHANGE-WORKING-DIRECTORY (PARENT-DIRECTORY (WORKING-DIRECTORY))))
           ((<= ANSWER (LENGTH SUBDIRS))
            (CHANGE-WORKING-DIRECTORY 
             (CHILD-DIRECTORY (WORKING-DIRECTORY) (ELT SUBDIRS (1- ANSWER)))))
           (T (LOAD (CDR (ELT FILES (- ANSWER (LENGTH SUBDIRS) 1)))
                    :VERBOSE T)))))))
    

(defun resolve (path &key (directory nil))
  (setf path (typecase path
               (string    path)
               (pathname  (namestring path))
               (symbol    (string-downcase path))
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
ARGUMENTS:  A list of paths possibly containing wildcards.
            If none is given, then \"*\" is used.
"
  (setf *today* (get-universal-time))
  (multiple-value-bind (opts args) (split-options args)
    (let ((opt-long nil))
      (dolist (opt opts)
        (cond ((string-equal  "-l" opt) (setf opt-long t))
              (t (error "Invalid option ~A" opt))))
      (dolist (entry
                (sort
                 (DELETE-DUPLICATES
                  ;; SBCL RETURNS DIRECTORIES FOR "*" AS WELL AS FOR "*/".
                  (mapcan
                   (lambda (path) (handler-case (directory path) (error () nil)))
                   (mapcar
                    (lambda (path) (resolve path :directory nil))
                    (or (delete-duplicates
                         (mapcan (function wilder-path) args)
                         :key (function namestring)
                         :test (function string=))
                        '("*/" "*"))))
                  :KEY (FUNCTION NAMESTRING)
                  :TEST (FUNCTION STRING=))
                 (function string<) :key (function namestring)))
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
                    (namestring (relativize entry (working-directory))))))))
  (values))


(defvar *terminal-height* 50)

(defun more (&rest args)
  "COMMAND
DO:         concatenate and paginate a list of files.
ARGUMENTS:  If the first argument is :PAGE,
            then the second arguments is
            either an integer giving the page height,
            or NIL indicating that no pagination must be done;
            else the page height is *TERMINAL-HEGIHT*.
            The other arguments are paths of files to be dumped
            on *STANDARD-OUTPUT*.
"
  (let* (page paths)
    (if (eq :page (first args)) 
        (setf page (second args)      paths (cddr args))
        (setf page  *terminal-height* paths args))
    (dolist (path paths)
      (with-open-file (in (resolve path  :directory nil)
                          :direction :input :if-does-not-exist :error)
        (do* ((+eof+ (gensym))
              (lnum  0 (1+ lnum))
              (line (read-line in nil +eof+)(read-line in nil +eof+)))
             ((eq line +eof+))
          (princ line)(terpri)
          (when (and page (>= lnum page))
            (setf lnum 0)
            (princ "Type RETURN for next page:")
            (read-line t nil))))))
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
  

(DEFVAR *DIRECTORY-STACK* NIL)


(DEFUN CD (&OPTIONAL PATH)
  "COMMAND
DO:         Change the working directory.
ARGUMENTS:  The path of the new working directory.
            If not given, then change to the user home directory.
"
  (CHANGE-WORKING-DIRECTORY
   (if path
       (resolve path  :directory t)
       (USER-HOMEDIR-PATHNAME))))


(DEFUN PWD   ()
  "COMMAND
DO:         Returns the current working directory.
"
  (WORKING-DIRECTORY))


(DEFUN POPD  ()
  "COMMAND
DO:         Unstack the working directory from the stack.
"
  (IF *DIRECTORY-STACK*
      (CONS (CHANGE-WORKING-DIRECTORY (POP *DIRECTORY-STACK*)) *DIRECTORY-STACK*)
      (LIST (WORKING-DIRECTORY))))


(DEFUN PUSHD (&OPTIONAL PATH)
  "COMMAND
DO:         Push the current working directory onto the stack, and
            change the working directory to the path (or home directory).
SEE;        POPD, CD.
"
  (IF PATH
      (PROGN
        (PUSH (WORKING-DIRECTORY) *DIRECTORY-STACK*)
        (CONS (CD PATH) *DIRECTORY-STACK*))
      (LET ((TOP (POP *DIRECTORY-STACK*)))
        (PUSH (WORKING-DIRECTORY) *DIRECTORY-STACK*)
        (CONS (CD TOP)  *DIRECTORY-STACK*))))


;;;; browser.lisp                     --                     --          ;;;;
