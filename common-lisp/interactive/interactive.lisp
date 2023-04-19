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
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-25 <PJB> Moved print-bug-report-info to com.informatimago.tools.manifest.
;;;;    2010-06-29 <PJB> Added :short option to LSPACK.
;;;;    2006-08-28 <PJB> Extracted from ~/.common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2006 - 2021
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.BROWSER")
  (:export "UPTIME" "DATE" "*EDITOR*" "EDIT" "MOZILLA-STRING" "LSCHAR" "LSPACK"
           "DIFF-PACKAGE" "PSWITCH" "SHOW" "MKUPACK" "RESET-CLUSER" "POPP" "PUSHP"
           "COMPARE-PATHNAMES" "PRINT-PATHNAME" "LSSYMBOLS"
           "REPL" "REPL-EXIT" "REP" "*REPL-PROMPT*"
           "REPL-HISTORY-RESET"
           "REPL-HISTORY-SIZE"
           "REPL-HISTORY-ADD"
           "REPL-HISTORY-REF"
           "REPL-HISTORY-READER-MACRO"
           "MORE" "LESS" "CAT" "LS" "POPD" "PUSHD" "PWD" "CD"
           "BROWSE" "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS"
           "DEFINE-PACKAGE"
           "INITIALIZE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE"
                "LIST-EXTERNAL-SYMBOLS" "LIST-ALL-SYMBOLS" "DEFINE-PACKAGE")
  (:documentation "

This package defines various interactive commands intended to be used
at the REPL.  It also re-exports some functions from BROWSER and
PACKAGE.

License:

    AGPL3

    Copyright Pascal J. Bourguignon 2006 - 2021

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")


(defvar *repl-history*         (make-array 1000 :adjustable t :fill-pointer 0)
  "The history of REPL form is saved there.")

(defvar *repl-enable-debugger* nil
  "BOOLEAN, true to allow INVOKE-DEBUGGER in the error handler.")

(defvar *repl-readtable*       nil
  "Bound to the REPL *readtable*; reinitiaized when history is reset.")


(defun repl-history-reset ()
  (fill *repl-history* nil)
  (setf (fill-pointer *repl-history*) 0))

(defun repl-history-size ()
  (length *repl-history*))

(defun repl-history-add (item)
  (vector-push-extend item *repl-history* (length *repl-history*)))

(defun repl-history-ref (n)
  (unless (zerop n)
    (if (minusp n)
        (when (<= (abs n) (length *repl-history*))
          (values (aref *repl-history* (+ (length *repl-history*) n)) t))
        (when (<= n (length *repl-history*))
          (values (aref *repl-history* (1- n)) t)))))

(defun repl-history-reader-macro (stream ch)
  (declare (ignore ch))
  (let ((history-index (if (equal #\! (peek-char nil stream t nil nil))
                           (progn
                             (read-char stream t nil nil)
                             '!!)
                           (read stream t nil nil))))
    (check-type history-index (or integer (member !!)))
    (multiple-value-bind (expr got-it)
        (repl-history-ref (if (eq '!! history-index)
                              -1
                              history-index))
      (if got-it
          expr
          (error "History reference out of bounds ~A" history-index)))))

(defun repl-exit (&optional result)
  (throw 'repl result))
(defun repl-toplevel (&optional result)
  (throw 'repl-toplevel result))
(defun repl-retry (&optional result)
  (throw 'repl-retry result))

(defun call-handling-repl-errors (where thunk)
  (flet ((report-simple-condition (err)
           (format *error-output* "~&~A:~%~?~&"
                   (class-name (class-of err))
                   (simple-condition-format-control   err)
                   (simple-condition-format-arguments err))
           (finish-output *error-output*)
           nil)
         (report-condition (err)
           (format *error-output* "~&~A:~%~A~%" (class-name (class-of err)) err)
           (finish-output *error-output*)
           nil))
    (handler-bind
        ((simple-error     (lambda (err)
                             (report-simple-condition err)
                             (invoke-debugger err)))
         (error            (lambda (err)
                             (report-condition err)
                             (invoke-debugger err)))
         (simple-condition (lambda (err) (report-simple-condition err)))
         (condition        (lambda (err) (report-condition err))))
      (loop
        :with form := -
        :do (catch 'repl-retry
              (restart-case
                  (return-from call-handling-repl-errors (funcall thunk))
                (retry ()
                  :report (lambda (stream)
                            (handler-bind ((error #'invoke-debugger))
                              (ecase where
                                ((read) (format stream "Retry reading a form"))
                                ((eval) (format stream "Retry evaluating ~S" form)))))
                  (repl-retry))
                (continue ()
                  :report "Return to REPL toplevel"
                  (repl-toplevel))
                (abort ()
                  :report "Exit from the REPL"
                  (repl-exit)))
              (format t "Retry~%") (finish-output))))))

(defmacro handling-repl-errors ((where) &body body)
  "
DO:       Execute the BODY with a handler for CONDITION and
          SIMPLE-CONDITION reporting the conditions.

          If *REPL-ENABLE-DEBUGGER* is true,  then the debugger is
          invoked for ERROR conditions.
"
  (assert (member where '(read eval)))
  `(call-handling-repl-errors ',where (lambda () ,@body)))

(defmacro with-standard-streams (&body body)
  `(let ((*standard-input* *standard-input*)
         (*standard-output* *standard-output*)
         (*trace-output* *trace-output*)
         (*error-output* *error-output*)
         (*terminal-io* *terminal-io*)
         (*debug-io* *debug-io*))
     ,@body))

(defvar *repl-prompt* nil
  "NIL -> prints the default prompt: package[history]>
A string -> prints it as prompt
A function -> calls it to print the prompt. The parameter is the history index, and an optional level.")

(defun %rep (+eof+ hist print-prompt)
  (check-type *repl-prompt* (or null string function)
              "The *repl-prompt* should be either NIL, a string or a function of 1 or 2 arguments.")
  (handling-repl-errors (read)
    (when print-prompt
      (typecase *repl-prompt*
        (null (format t "~%~A[~D]> " (package-name *package*) hist) )
        (string (format t "~%~A" *repl-prompt*))
        (function (funcall *repl-prompt* hist)))
      (finish-output)
      (setf print-prompt t))
    (setf - (read *standard-input* nil +eof+))
    (when (eq - +eof+)
      (return-from %rep))
    (repl-history-add -))
  (handling-repl-errors (eval)
    (let ((results (multiple-value-list (eval -))))
      (setf +++ ++   ++ +   + -
            /// //   // /   / results
            *** **   ** *   * (first /)))
    (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
    (finish-output)
    (incf hist)))

(defun rep (&key (reset-history nil) (enable-debugger nil) (line nil))
  "

DO:         Reads a single expression from LINE if given, concatenated with *STANDARD-INPUT*,
            and evaluates and prints its results.

NOTE:       The caller must catch REPL to let the user call
            (com.informatimago.common-lisp.interactive.interactive:repl-exit)
            to exit the REPL.

RESET-HISTORY:

            Whether the history is reset. If NIL, then the history is
            not reset and the user may refer to previous history
            expressions.

ENABLE-DEBUGGER:

            When true, the debugger is invoked on error, otherwise the
            error is merely reported and the REPL continues.

LINE:       The source of the next sexp; it can be:
            - a STRING (which is read),
            - a STREAM (which is read), or
            - NIL, in which case *STANDARD-INPUT* is read.
            The prompt is printed only when LINE is NIL.
"
  (check-type line (or null string stream))
  (let ((+eof+   (gensym))
        (hist    (if reset-history
                     (progn
                       (repl-history-reset)
                       1)
                     (repl-history-size)))
        (*readtable* (or (and (not reset-history) *repl-readtable*)
                         (progn
                           (setf *repl-readtable* (copy-readtable))
                           (set-macro-character #\! (function repl-history-reader-macro) t *repl-readtable*)
                           *repl-readtable*)))
        (*repl-enable-debugger* enable-debugger))
    (with-standard-streams
        (typecase line
          (string
           (with-input-from-string (line-stream line)
             (with-input-from-string (newline (format nil "~%"))
               (let ((*standard-input* (make-concatenated-stream line-stream newline *standard-input*)))
                 (%rep +eof+ hist nil)))))
          (stream
           (let ((*standard-input* (make-concatenated-stream line *standard-input*)))
             (%rep +eof+ hist nil)))
          (t
           (%rep +eof+ hist t))))))


(defun repl (&key (reset-history t) (enable-debugger nil))
  "

DO:         Implements a CL REPL.  The user may exit the REPL by
            calling:
            (com.informatimago.common-lisp.interactive.interactive:repl-exit).

NOTE:       Keeps a history of the expressions evaluated in
            *REPL-HISTORY*.  One may refer old expressions
            using the ! reader macro:

               !!   previous expression (same as + or !-1).
               !n   expressions number n.
               !-n  previous nth expression.

RESET-HISTORY:

            Whether the history is reset. If NIL, then the history is
            not reset and the user may refer to previous history
            expressions.

ENABLE-DEBUGGER:

            When true, the debugger is invoked on error, otherwise the
            error is merely reported and the REPL continues.

"
  (catch 'repl
    (with-standard-streams
      (let ((+eof+   (gensym))
            (hist    (if reset-history
                         (progn
                           (repl-history-reset)
                           1)
                         (repl-history-size)))
            (*readtable* (copy-readtable))
            (*repl-enable-debugger* enable-debugger))
        (set-macro-character #\! (function repl-history-reader-macro) t)
        (loop
           (catch 'repl-toplevel
             (%rep +eof+ hist t)))))))


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
      (format t "~&Cannot pop an empty package stack.~%")
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


(defun mkupack (&key (name "USER") stepper (use nil usep) (shadow nil shadowp))
  "
DO:         Makes a new, temporary, user package like
            COMMON-LISP-USER, and sets *PACKAGE* to it.

NAME:       the name of the new package. By default, it's assigned the
            name USERn with the first free n.

STEPPER:    whether to use the CL-STEPPER package instead of COMMON-LISP.

USE:        A package use list to use.  When given, STEPPER is ignored.
            By default, it's (\"COMMON-LISP\" \"COM.INFORMATIMAGO.PJB\")
            or (\"COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER\" \"COM.INFORMATIMAGO.PJB\") depending
            on STEPPER.
"
  (let ((cl          "COMMON-LISP")
        (pjb         "COM.INFORMATIMAGO.PJB")
        (cl-stepper  "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER")
        (interactive "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE")
        (name        (loop
                       :for i :from 0
                       :for p = (if name
                                    (string name)
                                    (format nil "~A~A" name i))
                         :then (format nil "~A~A" name i)
                       :while (find-package p)
                       :finally (return p))))
    (unless (find-package pjb)
      ;; Create a COM.INFORMATIMAGO.PJB package that reexports INTERACTIVE:
      (let ((pjb  (make-package pjb :use (list cl interactive)))
            (syms (list-external-symbols interactive)))
        (import syms pjb)
        (export syms pjb)))
    (setf *package* (make-package name :use '()))
    (when shadowp
      (shadow shadow *package*))
    (if usep
        (use-package use *package*)
        (progn
          (use-package (list (if stepper cl-stepper cl)) *package*)
          (shadow '("ED" "APROPOS" "APROPOS-LIST") *package*)
          (use-package pjb *package*)))
    *package*))


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


#-mocl
(defmethod documentation ((package t) (doc-type (eql 'exports)))
  (declare (ignore doc-type))
  (format t "~&~:{----------------------------------------~%~A~2%~A~2%~}"
          (mapcar (lambda (sym) (list sym (documentation sym 'function)))
                  (delete-if (lambda (sym) (null (documentation sym 'function)))
                             (list-external-symbols package))))
  (format t "~&Undocumented: ~{~A~^ ~}~%"
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
          (format t "~&   ~14A ~A~%" title out)
          (format t "~&   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                  title plist)))))

(defun lspack (&rest arguments)
  "(LSPACK [package [:SHOW-EXPORTS|:EXPORTS|:EXPORT|:T] [:HIDE-USED-BY|:SHORT|:S]]...)
List all the packages, or only the packages matching PACKAGE (a regexp on clisp)
dumping all the exported symbols when :SHOW-EXPORTS is specified,
and not dumping the used-by list when :HIDE-USED-BY is specified.
The keywords are tested with STRING-EQUAL."
  (let ((options '((:show-exports :exports :export :t)
                   (:hide-used-by :short           :s)
                   (:very-short                    :ss))))
    (flet ((list-package (name options)
             (let* ((show-exports (not (not (member :show-exports options))))
                    (show-used-by (not (member :hide-used-by options)))
                    (very-short   (intersection '(:very-short :ss) options))
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
               (if very-short
                   (dolist (package packlist)
                     (format t "~&~A~%" (package-name package)))
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
                       (flow-list "Exported:" (list-external-symbols package)))))
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
         :collect #1=(progn (format t "~&#x~5,'0X  ~:*~6D  ~C  ~S~%"
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
  (format t "~%~{~5*~4,'0D-~2:*~2,'0D-~2:*~2,'0D ~2:*~2,'0D:~2:*~2,'0D:~2:*~2,'0D~8*~}~%"
          (multiple-value-list (decode-universal-time date)))
  date)

(defvar *start-time* (get-universal-time)
  "Records the time at which this Common Lisp instance was started.")

(defun initialize ()
  "This function should be called upon launching a lisp image."
  (setf *start-time* (get-universal-time)))

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
