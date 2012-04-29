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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING" 
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER")  
  (:export "UPTIME" "DATE" "*EDITOR*" "EDIT" "MOZILLA-STRING" "LSCHAR" "LSPACK"
           "DIFF-PACKAGE" "PSWITCH" "SHOW" "MKUPACK" "RESET-CLUSER" "POPP" "PUSHP"
           "COMPARE-PATHNAMES" "PRINT-PATHNAME" "LSSYMBOLS" "REPL" 
           "PRINT-BUG-REPORT-INFO" "MORE" "LESS" "CAT" "LS" "POPD" "PUSHD" "PWD" "CD"
           "BROWSE" "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS" "DEFINE-PACKAGE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
                "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS" "DEFINE-PACKAGE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                "HANDLING-ERRORS")
  (:documentation "

This package defines various interactive commands intended to be used
at the REPL.  It also re-exports some functions from BROWSER and
PACKAGE.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2006 - 2012
    
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
    If not, see http://www.gnu.org/licenses/
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")



(defun print-bug-report-info ()
  "Prints information for a bug report."
  (format t "~2%~{~28A ~S~%~}~2%"
          (list "LISP-IMPLEMENTATION-TYPE"    (lisp-implementation-type)
                "LISP-IMPLEMENTATION-VERSION" (lisp-implementation-version)
                "SOFTWARE-TYPE"               (software-type)
                "SOFTWARE-VERSION"            (software-version)
                "MACHINE-INSTANCE"            (machine-instance)
                "MACHINE-TYPE"                (machine-type)
                "MACHINE-VERSION"             (machine-version)
                "*FEATURES*"                  *features*))
  #+clisp (with-open-stream (input (ext:run-program "uname" :arguments '("-a") :output :stream))
            (format t ";;; uname -a~%")
            (loop :for line = (read-line input nil nil) :while line :do (format t "~A~%" line)))
  #+clisp (format t ";;; (EXT:ARGV)~%~S~%" (ext:argv))
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
  (values))



(defun repl ()
  "
DO:        Implements a minimalist CL REPL.
"
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
  "
DO:        Prints a list of the symbols in the PACKAGE (default: *PACKAGE*).
"
  (let ((table    (make-hash-table))
        (packages '()))
    (do-symbols (sym package)
      (push sym (gethash (symbol-package sym) table)))
    (maphash (lambda (k v)
               (push k packages)
               (setf (gethash k table) (sort v (function string<))))
             table)
    (dolist (package (sort packages (function string<)
                           :key (function package-name)))
      (format t "~%From package ~A~%" (package-name package))
      (flow-list "   " (gethash package table)))
    (values)))


(defun print-pathname (p)
  "
DO:        Prints the components of the pathname P one by one.
RETURN:    P.
"
  (format t "~&~{~{~@(~9A~) : ~S~&~}~}"
          (mapcar (lambda (name field) (list name (funcall field p)))
                  '(host device directory name type version)
                  '(pathname-host pathname-device pathname-directory
                    pathname-name pathname-type pathname-version)))
  p)


(defun compare-pathnames (p1 p2)
  "
DO:         Compare the pathnames P1 and P2 component by component,
            reporting their differences on the *standard-output*.
"
  (flet ((compare (name field)
           (unless (equal (funcall field p1) (funcall field p2))
             (format t "~&~A DIFFERENT: ~A /= ~A~%"
                     name (funcall field p1) (funcall field p2)))))
    (compare 'host      (function pathname-host))
    (compare 'device    (function pathname-device))
    (compare 'directory (function pathname-directory))
    (compare 'name      (function pathname-name))
    (compare 'type      (function pathname-type))
    (compare 'version   (function pathname-version))
    (values)))



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
          (syms (list-external-symbols
                 "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")))
      (import syms pjb)
      (export syms pjb)))
  (setf *package*
        (make-package
         (if name
             (string name)
             (loop
                :for i :from 1 :for p = (format nil "USER~A" i)
                :while (find-package p) :finally (return p)))
         :use '("COMMON-LISP" "COM.INFORMATIMAGO.PJB"))))


(defmacro show (&body expressions)
  "
DO:         Prints each expression and their values.
"
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


(let ((alternate (find-package "COMMON-LISP-USER")))
  (defun pswitch (&optional package)
    "Switch between two packages."
    (when package (setf alternate (find-package package)))
    (unless (fboundp (intern "PSWITCH" alternate))
      (import '(pswitch)  alternate))
    (setf common-lisp:*package*  (if (eq alternate common-lisp:*package*)
                                     (find-package "COMMON-LISP-USER")
                                     alternate))))


(defmethod documentation ((package t) (doc-type (eql 'exports)))
  (declare (ignore doc-type))
  (format t "~:{----------------------------------------~%~A~2%~A~2%~}"
          (mapcar (lambda (sym) (list sym (documentation sym 'function)))
                  (delete-if (lambda (sym) (null (documentation sym 'function)))
                             (list-external-symbols package))))
  (format t "Undocumented: ~{~A~^ ~}~%"
          (delete-if (lambda (sym)  (documentation sym 'function))
                     (list-external-symbols package))))


(defun diff-package (p1 p2)
  "Prints the differences in the exported symbols list of two packages."
  (let ((*print-readably* t))
    (format t "~2%Symbols exported from ~A not exported from ~A:~%~{  ~S~%~}~%"
            p1 p2
            (set-difference (list-external-symbols p1) (list-external-symbols p2)
                            :test (function eq)))))


(defun string-match-p (pattern string)
  "Matches a string."
  #+(and clisp regexp) (regexp:match pattern string :ignore-case t)
  #-(and clisp regexp) (search       pattern string
                                     :test (function equalp)))


(defun flow-list (title plist)
  (when plist
    (setf plist (mapcar
                 (lambda (name) (if (string= "" name) "<empty>" name))
                 (sort (mapcar (lambda (item)
                                 (etypecase item
                                   (string  item)
                                   (symbol  (string item))
                                   (package (package-name item))))
                               plist)
                       (function string<))))
    (let ((out (format nil "~{~A ~}" plist)))
      (if (< (length out) 60)
          (format t "   ~14A ~A~%" title out)
          (format t "   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                  title plist)))))

(defun lspack (&rest arguments)
  "(LSPACK [package [:SHOW-EXPORTS|:EXPORTS|:EXPORT|:T] [:HIDE-USED-BY|:SHORT|:S]]...)
List all the packages, or only the packages matching PACKAGE (a regexp on clisp)
dumping all the exported symbols when :SHOW-EXPORTS is specified,
and not dumping the used-by list when :HIDE-USED-BY is specified.
The keywords are tested with STRING-EQUAL."
  (let ((options '((:show-exports :exports :export :t)
                   (:hide-used-by :short           :s))))
    (flet ((list-package (name options)
             (let* ((show-exports (not (not (member :show-exports options))))
                    (show-used-by (not (member :hide-used-by options)))
                    (packlist
                     (sort (cond
                             ((null name)  (copy-list (list-all-packages)))
                             ((stringp name)
                              ;; remove-if-not may return the argument!
                              (delete-if-not
                               (lambda (pack)
                                 (some (lambda (pname)
                                         (string-match-p name pname))
                                       (cons (package-name pack)
                                             (package-nicknames pack))))
                               (copy-list (list-all-packages))))
                             (t (list (find-package name))))
                           (function string<) :key (function package-name)))
                    #+(or)(name-width
                           (loop for p in packlist
                              maximize (length (package-name p))))
                    (numb-width
                     (loop
                        :for p :in packlist
                        :maximize (truncate
                                   (1+ (log
                                        (max (length (list-external-symbols p))
                                             (length (list-all-symbols p)) 3)
                                        10))))))
               ;; (print `(,name show-exports ,show-exports show-used-by ,show-used-by))
               (dolist (package packlist)
                 (format t "~%~A~%   ~14A ~VD exported, ~VD total.~%"
                         (package-name package)
                         "Symbols:"
                         numb-width (length (list-external-symbols package))
                         numb-width (length (list-all-symbols package)))
                 (flow-list "Nicknames:" (package-nicknames package))
                 (flow-list "Uses:"      (package-use-list package))
                 (when show-used-by
                   (flow-list "Used by:"   (package-used-by-list package)))
                 (when show-exports 
                   (flow-list "Exported:" (list-external-symbols package))))
               (values)))
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

(defun get-first-word (string)
  "
RETURN:     The first word of the string, or the empty string.
"
  (do ((i 0)
       (j 0)
       (found nil)
       (done nil))
      (done (if found (subseq string i  j) ""))
    (if  (<= (length string) i)
         (setf done t found nil)
         (if (<= j i)
             (if (alpha-char-p (char string i))
                 (setf j (1+ i))
                 (incf i))
             (if (<= (length string) j)
                 (setf done t found t)
                 (if (alpha-char-p (char string j))
                     (incf j)
                     (setf done t found t)))))))


(defvar *editor* (if (fboundp 'ed)
                     (function ed)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "This implementation doesn't have an ED")))
  "The editor function provided by the implementation.")


;; (defvar *edit-log-path* (make-pathname :name "edit-log" :type "lisp")
;;   "The path to the file where edits of functions are appended.")


(defun edit (&optional item &key (wait t wait-p))
  "
DO:         Create FILE if it doesn't exist, and
            Calls the editor with the FILE argument.
"
  (setf item (or item
                 (make-pathname :directory '(:absolute "tmp")
                                :name "scratch" :type "lisp")))
  (flet ((doedit (item)
           (cond
             ((null *editor*) (warn "There's no editor (null *editor*)"))
             ((eq *editor* (function ed)) (funcall *editor* item))
             (wait-p (handler-case (funcall *editor* item :wait wait)
                       ( #+clisp (or simple-keyword-error simple-program-error
                                  simple-source-program-error)
                         #-clisp error
                         () (funcall *editor* item))))
             (t (funcall *editor* item)))))
    (cond
      ((or (functionp item)
           (and (or (pathnamep item) (stringp item))
                (probe-file item)))
       (doedit (if (functionp item) item (truename item))))
      ((symbolp item)
       (if (symbol-package item)
           (let ((*package* (symbol-package item)))
             (doedit item))
           (doedit item)))
      (t (loop
            (format *query-io*
              "File ~S does not exist. Should I create it? " item)
            (finish-output *query-io*)
            (let ((line (string-upcase
                         ;; small optimization to avoid STRING-EQUAL.
                         (get-first-word (let ((*read-eval* nil))
                                           (read-line *query-io* nil :no))))))
              (cond
                ((member line '("YES" "Y" "JA" "J" "SI" "S" "OUI" "O" "T")
                         :test (function string=))
                 (let ((file (truename item)))
                  (close (open file
                               :direction :output
                               :if-does-not-exist :create))
                  (return-from edit (doedit file))))
                ((member line '("NO" "N" "NON" "NEIN" "NIL")
                         :test (function string=))
                 (format *error-output* "EDIT OF ~S CANCELED." item)
                 (finish-output *error-output*)
                 (return-from edit nil)))))))))



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
