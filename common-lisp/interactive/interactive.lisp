;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               interactive.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package defines various interactive commands.
;;;;    It also re-exports some functions from BROWSER and PACKAGE.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-06-29 <PJB> Added :short option to LSPACK.
;;;;    2006-08-28 <PJB> Extracted from ~/.common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2010
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" 
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER")
  (:documentation "
This package defines various interactive commands intended to be used
at the REPL.  It also re-exports some functions from BROWSER and
PACKAGE.

Copyright Pascal J. Bourguignon 2006 - 2010
This package is provided under the GNU General Public License.
See the source file for details.")
  (:EXPORT "UPTIME" "DATE" "*EDITOR*" "EDIT" "MOZILLA-STRING" "LSCHAR" "LSPACK"
           "DIFF-PACKAGE" "PSWITCH" "SHOW" "MKUPACK" "RESET-CLUSER" "POPP" "PUSHP"
           "COMPARE-PATHNAMES" "PRINT-PATHNAME" "LSSYMBOLS" "REPL" 
           "PRINT-BUG-REPORT-INFO" "MORE" "LESS" "CAT" "LS" "POPD" "PUSHD" "PWD" "CD"
           "BROWSE" "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS" "DEFINE-PACKAGE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
                "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS" "DEFINE-PACKAGE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                "HANDLING-ERRORS"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")



(DEFUN PRINT-BUG-REPORT-INFO ()
  "Prints information for a bug report."
  (FORMAT T "~2%~{~28A ~S~%~}~2%"
          (LIST "LISP-IMPLEMENTATION-TYPE"    (LISP-IMPLEMENTATION-TYPE)
                "LISP-IMPLEMENTATION-VERSION" (LISP-IMPLEMENTATION-VERSION)
                "SOFTWARE-TYPE"               (SOFTWARE-TYPE)
                "SOFTWARE-VERSION"            (SOFTWARE-VERSION)
                "MACHINE-INSTANCE"            (MACHINE-INSTANCE)
                "MACHINE-TYPE"                (MACHINE-TYPE)
                "MACHINE-VERSION"             (MACHINE-VERSION)
                "*FEATURES*"                  *FEATURES*))
  #+clisp (with-open-stream (input (ext:run-program "uname" :arguments '("-a") :output :stream))
            (format t ";;; uname -a~%")
            (loop :for line = (read-line input nil nil) :while line :do (format t "~A~%" line)))
  #+clisp (format t ";;; (EXT:ARGV)~%~S~%" (EXT:ARGV))
  #+clisp (ignore-errors
            (let ((path (make-pathname
                         :type nil
                         :defaults (merge-pathnames
                                    (make-pathname
                                     :directory '(:relative :up :up :up "bin")
                                     :name "clisp"  :type nil :version nil)
                                    (aref (ext:argv) 0) nil))))
              (with-open-stream
                  (input (ext:run-program path :arguments '("--version") :output :stream))
                (format t ";;; ~A --version~%" path)
                (loop :for line = (read-line input nil nil) :while line :do (format t "~A~%" line)))))
  (VALUES))



(defun repl ()
  (catch 'repl
    (do ((+eof+ (gensym))
         (hist 1 (1+ hist)))
        (nil)
      (format t "~%~A[~D]> " (package-name *package*) hist)
      (finish-output)
      (handling-errors
       (setf - (read *standard-input* nil +eof+))
       (when (or (eq - +eof+)
                 (and (listp -)
                      (null (rest -))
                      (member (first -) '(quit  exit continue)
                              :test (function string-equal))))
         (return-from repl))
       (let ((results (multiple-value-list (eval -))))
         (setf +++ ++   ++ +   + -
               /// //   // /   / results
               *** **   ** *   * (first /)))
       (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
       (finish-output)))))


(defun lssymbols (&optional (package *package*))
  (let ((table    (make-hash-table))
        (packages '()))
    (do-symbols (sym package)
      (push sym (gethash (symbol-package sym) table)))
    (maphash (lambda (k v)
               (push k packages)
               (setf (gethash k table) (sort v (function string<))))
             table)
    (dolist (package (sort packages (function string<)
                           :key (function package-name)) (values))
      (format t "~%From package ~A~%" (package-name package))
      (flow-list "   " (gethash package table)))))


(defun print-pathname (p)
  (format t "~&~{~{~@(~9A~) : ~S~&~}~}"
          (mapcar (lambda (name field) (list name (funcall field p)))
                  '(host device directory name type version)
                  '(pathname-host pathname-device pathname-directory
                    pathname-name pathname-type pathname-version)))
  p)


(defun compare-pathnames (p1 p2)
  (flet ((compare (name field)
           (unless (equal (funcall field p1) (funcall field p2))
             (format t "~&~A DIFFERENT: ~A /= ~A~%"
                     name (funcall field p1) (funcall field p2)))))
    (compare 'host      (function pathname-host))
    (compare 'device    (function pathname-device))
    (compare 'directory (function pathname-directory))
    (compare 'name      (function pathname-name))
    (compare 'type      (function pathname-type))
    (compare 'version   (function pathname-version))))



(defvar *package-stack* '())
(defun pushp (&optional package)
  "Push *PACKAGE* on a package stack, 
and makes the package designated by PACKAGE be the new *PACKAGE*.
If PACKAGE is NIL, the rotate *PACKAGE* and the top of the package stack."
  (if package
      (progn
        (push (package-name *package*) *package-stack*)
        (handler-case (setf *package* (find-package package))
          (error () (setf *package* (find-package (pop *package-stack*))))))
      (psetf (first *package-stack*) (package-name *package*)
             *package* (find-package (first *package-stack*))))
  (cons (package-name *package*) *package-stack*))
(defun popp ()
  "Pops the top of the package stack and assign it to *PACKAGE*."
  (if (null *package-stack*)
      (format t "Cannot pop an empty package stack.")
      (setf *package* (find-package (pop *package-stack*))))
  (cons (package-name *package*) *package-stack*))


(defparameter *user-packages* '("COM.INFORMATIMAGO.PJB"))

(defun reset-cluser ()
  "Reset the CL-USER package to a clean state."
  ;; We cannot just delete it because some implementations keep the
  ;; original in a constant to be used with with-standard-io-syntax
  ;; for example...
  (when (find-package "SWANK")
    (error "RESET-CLUSER doesn't work with slime, use a slime command!"))
  (let ((cluser (find-package "COMMON-LISP-USER")))
    (unuse-package (copy-list (package-use-list cluser)) cluser)
    (do-symbols (s cluser) (unintern s cluser))
    (rename-package cluser (string (gensym "CL-USER-")))
    (rename-package cluser "COMMON-LISP-USER" '("CL-USER"))
    (use-package (cons "COMMON-LISP" *user-packages*) cluser)
    cluser))


(defun mkupack (&optional name)
  "Makes a new, temporary, user package like COMMON-LISP-USER."
  (unless (find-package "COM.INFORMATIMAGO.PJB")
    ;; Create a COM.INFORMATIMAGO.PJB package that reexports INTERACTIVE:
    (let ((pjb (make-package
                "COM.INFORMATIMAGO.PJB"
                :use '("CL" "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")))
          (syms (LIST-EXTERNAL-SYMBOLS
                 "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")))
      (import syms pjb)
      (export syms pjb)))
  (setf *package*
        (make-package
         (if name
             (string name)
             (loop
                :for i :from 1 :for p = (format NIL "USER~A" i)
                :while (find-package p) :finally (return p)))
         :use '("COMMON-LISP" "COM.INFORMATIMAGO.PJB"))))


(defmacro show (&body expressions)
  (let ((width (reduce (function max)
                       (mapcar (lambda (expr) (length (format nil "~S" expr)))
                               expressions)
                       :initial-value 0)))
    `(progn
       ,@(mapcar
          (lambda (expr) 
            `(let ((vals  (multiple-value-list ,expr)))
               (format *trace-output* 
                 ,(format nil "~~~DS = ~~{~~S~~^ ; ~~%~:*~VA   ~~}~~%" width "")
                 (quote ,expr) vals)
               (values-list vals)))
          expressions))))


(LET ((ALTERNATE (FIND-PACKAGE "COMMON-LISP-USER")))
  (DEFUN PSWITCH (&OPTIONAL PACKAGE)
    "Switch between two packages."
    (WHEN PACKAGE (SETF ALTERNATE (FIND-PACKAGE PACKAGE)))
    (UNLESS (FBOUNDP (INTERN "PSWITCH" ALTERNATE))
      (IMPORT '(PSWITCH)  ALTERNATE))
    (SETF COMMON-LISP:*PACKAGE*  (IF (EQ ALTERNATE COMMON-LISP:*PACKAGE*)
                                     (FIND-PACKAGE "COMMON-LISP-USER")
                                     ALTERNATE))))


(DEFMETHOD DOCUMENTATION ((PACKAGE T) (DOC-TYPE (EQL 'EXPORTS)))
  (declare (ignore doc-type))
  (FORMAT T "~:{----------------------------------------~%~A~2%~A~2%~}"
          (MAPCAR (LAMBDA (SYM) (LIST SYM (DOCUMENTATION SYM 'FUNCTION)))
                  (DELETE-IF (LAMBDA (SYM) (NULL (DOCUMENTATION SYM 'FUNCTION)))
                             (LIST-EXTERNAL-SYMBOLS PACKAGE))))
  (FORMAT T "Undocumented: ~{~A~^ ~}~%"
          (DELETE-IF (LAMBDA (SYM)  (DOCUMENTATION SYM 'FUNCTION))
                     (LIST-EXTERNAL-SYMBOLS PACKAGE))))


(DEFUN DIFF-PACKAGE (P1 P2)
  "Prints the differences in the exported symbols list of two packages."
  (LET ((*PRINT-READABLY* T))
    (FORMAT T "~2%Symbols exported from ~A not exported from ~A:~%~{  ~S~%~}~%"
            P1 P2
            (SET-DIFFERENCE (LIST-EXTERNAL-SYMBOLS P1) (LIST-EXTERNAL-SYMBOLS P2)
                            :TEST (FUNCTION EQ)))))


(defun string-match-p (pattern string)
  "Matches a string."
  #+(and clisp regexp) (regexp:match pattern string :ignore-case t)
  #-(and clisp regexp) (search       pattern string
                                     :test (function equalp)))


(defun flow-list (TITLE PLIST)
  (WHEN PLIST
    (SETF PLIST (MAPCAR
                 (LAMBDA (NAME) (IF (STRING= "" NAME) "<empty>" NAME))
                 (SORT (MAPCAR (LAMBDA (ITEM)
                                 (ETYPECASE ITEM
                                   (STRING  ITEM)
                                   (SYMBOL  (STRING ITEM))
                                   (PACKAGE (PACKAGE-NAME ITEM))))
                               PLIST)
                       (FUNCTION STRING<))))
    (LET ((OUT (FORMAT NIL "~{~A ~}" PLIST)))
      (IF (< (LENGTH OUT) 60)
          (FORMAT T "   ~14A ~A~%" TITLE OUT)
          (FORMAT T "   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                  TITLE PLIST)))))

(DEFUN LSPACK (&rest arguments)
  "(LSPACK [package [:SHOW-EXPORTS|:EXPORTS|:EXPORT|:T] [:HIDE-USED-BY|:SHORT|:S]]...)
List all the packages, or only the packages matching PACKAGE (a regexp on clisp)
dumping all the exported symbols when :SHOW-EXPORTS is specified,
and not dumping the used-by list when :HIDE-USED-BY is specified.
The keywords are tested with STRING-EQUAL."
  (let ((options '((:show-exports :exports :export :t)
                   (:hide-used-by :short           :s))))
    (flet ((list-package (name options)
             (LET* ((show-exports (not (not (member :show-exports options))))
                    (show-used-by (not (member :hide-used-by options)))
                    (PACKLIST
                     (SORT (cond
                             ((null name)  (COPY-LIST (LIST-ALL-PACKAGES)))
                             ((stringp name)
                              ;; remove-if-not may return the argument!
                              (delete-if-not
                               (lambda (pack)
                                 (some (lambda (pname)
                                         (string-match-p name pname))
                                       (cons (package-name pack)
                                             (package-nicknames pack))))
                               (COPY-LIST (LIST-ALL-PACKAGES))))
                             (t (list (find-package name))))
                           (FUNCTION STRING<) :KEY (FUNCTION PACKAGE-NAME)))
                    #+(or)(NAME-WIDTH
                           (LOOP FOR P IN PACKLIST
                              MAXIMIZE (LENGTH (PACKAGE-NAME P))))
                    (NUMB-WIDTH
                     (LOOP
                        :FOR P :IN PACKLIST
                        :MAXIMIZE (TRUNCATE
                                   (1+ (LOG
                                        (MAX (LENGTH (LIST-EXTERNAL-SYMBOLS P))
                                             (LENGTH (LIST-ALL-SYMBOLS P)) 3)
                                        10))))))
               ;; (print `(,name show-exports ,show-exports show-used-by ,show-used-by))
               (DOLIST (PACKAGE PACKLIST)
                 (FORMAT T "~%~A~%   ~14A ~VD exported, ~VD total.~%"
                         (PACKAGE-NAME PACKAGE)
                         "Symbols:"
                         NUMB-WIDTH (LENGTH (LIST-EXTERNAL-SYMBOLS PACKAGE))
                         NUMB-WIDTH (LENGTH (LIST-ALL-SYMBOLS PACKAGE)))
                 (flow-list "Nicknames:" (PACKAGE-NICKNAMES PACKAGE))
                 (flow-list "Uses:"      (PACKAGE-USE-LIST PACKAGE))
                 (when show-used-by
                   (flow-list "Used by:"   (PACKAGE-USED-BY-LIST PACKAGE)))
                 (WHEN show-exports 
                   (flow-list "Exported:" (LIST-EXTERNAL-SYMBOLS PACKAGE))))
               (VALUES)))
           (eat-options (arguments)
             "
RETURN: a list of options present at the beginning of the arguments list;
        the rest of the argument lists.
        THe options are canonicalized (the first keyword given in the
        OPTIONS lists below is included in the result list).
"
             (loop
                :with result = '()
                :for arg = (first arguments)
                :do (loop
                       :named find-option
                       :for option :in options
                       :when (member arg option :test (function string-equal))
                       :do (progn (pushnew (first option) result)
                                  (pop arguments)
                                  (return-from find-option))
                       :finally (return-from eat-options (values result arguments))))))
      (when (or (null arguments)
                (find (first arguments) options
                      :test (lambda (k o) (member k o :test (function string-equal)))))
        (push "" arguments))
      (loop
         :while arguments
         :do (let ((name (pop arguments)))
               (multiple-value-bind (options rest-arguments) (eat-options arguments)
                 (setf arguments rest-arguments)
                 (list-package name options)))))))


(defun lschar (&key (start 0) (end #x11000) name)
  "Prints all the characters of codes betwen start and end, with their names."
  (if name
      (loop
         :for code :from start :below end
         :when (and (code-char code) ; ccl returns nil on some codes...
                    (string-match-p name (char-name (code-char code))))
         :collect #1=(progn (format t "#x~5,'0X  ~:*~6D  ~C  ~S~%"
                                    code (code-char code)
                                    (char-name (code-char code)))
                            (code-char code)))
      (loop
         :for code :from start :below end
         :when (code-char code) ; ccl returns nil on some codes...
         :collect #1#)))



;;----------------------------------------------------------------------
;; Some personal functions
;; -----------------------

(defun mozilla-string (string)
  "Convert a string as copied from mozilla into unicode characters."
  (map 'string
       (lambda (c) (code-char (parse-integer c :radix 16)))
       (delete "" (split-string string "\\x{} ") :test (function string=))))



;;----------------------------------------------------------------------
;; EDIT --
;; -------

;; editor-name is redefined in config.lisp to be:
;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(DEFUN GET-FIRST-WORD (STRING)
  "
RETURN:     The first word of the string, or the empty string.
"
  (DO ((I 0)
       (J 0)
       (FOUND NIL)
       (DONE NIL))
      (DONE (IF FOUND (SUBSEQ STRING I  J) ""))
    (IF  (<= (LENGTH STRING) I)
         (SETF DONE T FOUND NIL)
         (IF (<= J I)
             (IF (ALPHA-CHAR-P (CHAR STRING I))
                 (SETF J (1+ I))
                 (INCF I))
             (IF (<= (LENGTH STRING) J)
                 (SETF DONE T FOUND T)
                 (IF (ALPHA-CHAR-P (CHAR STRING J))
                     (INCF J)
                     (SETF DONE T FOUND T)))))))


(DEFVAR *EDITOR* (IF (FBOUNDP 'ED)
                     (FUNCTION ED)
                     (LAMBDA (&REST ARGS)
                       (DECLARE (IGNORE ARGS))
                       (ERROR "This implementation doesn't have an ED")))
  "The editor function provided by the implementation.")


;; (defvar *edit-log-path* (make-pathname :name "edit-log" :type "lisp")
;;   "The path to the file where edits of functions are appended.")


(DEFUN EDIT (&OPTIONAL ITEM &KEY (WAIT T WAIT-P))
  "
DO:         Create FILE if it doesn't exist, and
            Calls the editor with the FILE argument.
"
  (SETF ITEM (OR ITEM
                 (MAKE-PATHNAME :DIRECTORY '(:ABSOLUTE "tmp")
                                :NAME "scratch" :TYPE "lisp")))
  (FLET ((DOEDIT (ITEM)
           (COND
             ((NULL *EDITOR*) (WARN "There's no editor (null *editor*)"))
             ((EQ *EDITOR* (FUNCTION ED)) (FUNCALL *EDITOR* ITEM))
             (WAIT-P (HANDLER-CASE (FUNCALL *EDITOR* ITEM :WAIT WAIT)
                       ( #+clisp (OR SIMPLE-KEYWORD-ERROR SIMPLE-PROGRAM-ERROR
                                  SIMPLE-SOURCE-PROGRAM-ERROR)
                         #-clisp error
                         () (FUNCALL *EDITOR* ITEM))))
             (T (FUNCALL *EDITOR* ITEM)))))
    (COND
      ((OR (FUNCTIONP ITEM)
           (AND (OR (PATHNAMEP ITEM) (STRINGP ITEM))
                (PROBE-FILE ITEM)))
       (DOEDIT (if (functionp item) ITEM (truename item))))
      ((SYMBOLP ITEM)
       (IF (SYMBOL-PACKAGE ITEM)
           (LET ((*PACKAGE* (SYMBOL-PACKAGE ITEM)))
             (DOEDIT ITEM))
           (DOEDIT ITEM)))
      (T (LOOP
            (FORMAT *QUERY-IO*
              "File ~S does not exist. Should I create it? " ITEM)
            (FINISH-OUTPUT *QUERY-IO*)
            (LET ((LINE (STRING-UPCASE
                         ;; small optimization to avoid STRING-EQUAL.
                         (GET-FIRST-WORD (LET ((*READ-EVAL* NIL))
                                           (READ-LINE *QUERY-IO* NIL :NO))))))
              (COND
                ((MEMBER LINE '("YES" "Y" "JA" "J" "SI" "S" "OUI" "O" "T")
                         :TEST (FUNCTION STRING=))
                 (let ((file (TRUENAME ITEM)))
                  (CLOSE (OPEN file
                               :DIRECTION :OUTPUT
                               :if-does-not-exist :create))
                  (RETURN-FROM EDIT (DOEDIT file))))
                ((MEMBER LINE '("NO" "N" "NON" "NEIN" "NIL")
                         :TEST (FUNCTION STRING=))
                 (FORMAT *ERROR-OUTPUT* "EDIT OF ~S CANCELED." ITEM)
                 (FINISH-OUTPUT *ERROR-OUTPUT*)
                 (RETURN-FROM EDIT NIL)))))))))



(defun date (&optional (date (get-universal-time)))
  "Prints the date."
  (format t
    "~{~5*~4,'0D-~2:*~2,'0D-~2:*~2,'0D ~2:*~2,'0D:~2:*~2,'0D:~2:*~2,'0D~%~9*~}"
    (multiple-value-list (decode-universal-time date)))
  date)

(defvar *start-time* (get-universal-time)
  "Records the time at which this Common Lisp instance was started.")

(defun uptime ()
  "Prints the uptime of this Common Lisp instance."
  (let ((uptime  (- (get-universal-time) *start-time*)))
    (multiple-value-bind (se mi ho da mo ye) (decode-universal-time uptime 0)
      (decf ye 1900)
      (decf mo 1)
      (decf da 1)
      (format t "~&uptime: ~:[~D year~:*~P, ~;~*~]~
                           ~:[~D month~:*~P, ~;~*~]~
                           ~:[~D day~:*~P, ~;~*~]~
                           ~:[~D hour~:*~P, ~;~*~]~
                           ~:[~D minute~:*~P, ~;~*~]~
                           ~:[~D second~:*~P.~;~*~]~%"
              (zerop ye) ye (zerop mo) mo (zerop da) da
              (zerop ho) ho nil mi nil se))
    uptime)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
