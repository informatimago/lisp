;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               utility.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines a few utilities.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-15 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")



(defun proper-list-p (object)
  (labels ((proper (current slow)
             (cond ((null current)       t)
                   ((atom current)       nil)
                   ((null (cdr current)) t)
                   ((atom (cdr current)) nil)
                   ((eq current slow)    nil)
                   (t                    (proper (cddr current) (cdr slow))))))
    (proper object (cons nil object))))

(defun test-proper-list-p ()
  (assert
   (every 
    (function identity)
    (mapcar (lambda (test) (eq (first test) (proper-list-p (second test))))
            '((nil x)
              (t ())
              (t (a))
              (t (a b))
              (t (a b c))
              (t (a b c d))
              (nil (a . x))
              (nil (a b . x))
              (nil (a b c . x))
              (nil (a b c d . x))
              (nil #1=(a . #1#))
              (nil #2=(a b . #2#))
              (nil #3=(a b c . #3#))
              (nil #4=(a b c d . #4#))
              (nil (1 . #1#))
              (nil (1 2 . #1#))
              (nil (1 2 3 . #1#))
              (nil (1 2 3 4 . #1#))
              (nil (1 . #2#))
              (nil (1 2 . #2#))
              (nil (1 2 3 . #2#))
              (nil (1 2 3 4 . #2#))
              (nil (1 . #3#))
              (nil (1 2 . #3#))
              (nil (1 2 3 . #3#))
              (nil (1 2 3 4 . #3#))
              (nil (1 . #4#))
              (nil (1 2 . #4#))
              (nil (1 2 3 . #4#))
              (nil (1 2 3 4 . #4#)))))))


(defun unsplit-string (string-list &optional (separator " "))
  "
DO:         The inverse than split-string.
            If no separator is provided then a simple space is used.
SEPARATOR:  (OR NULL STRINGP CHARACTERP)
"
  (check-type separator (or string character symbol) "a string designator.")
  (if string-list
      (cl:with-output-to-string (cl:*standard-output*)
        (cl:princ (pop string-list))
        (dolist (item string-list)
          (cl:princ separator) (cl:princ item)))
      ""))



(defun assert-type (datum expected-type)
  "
DO:     Signal a TYPE-ERROR if DATUM is not of the EXPECTED-TYPE.
NOTICE: CHECK-TYPE signals a PROGRAM-ERROR.
"
  (or (typep datum expected-type)
      (error (make-condition 'type-error
                             :datum datum :expected-type expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; regular expressions
;;;


(defun re-compile (re &key extended)
  #+clisp
  (regexp:regexp-compile   re :extended      extended)
  #+(and (not clisp) cl-ppcre)
  (cl-ppcre:create-scanner re :extended-mode extended)
  #-(or clisp cl-ppcre)
  (error "Please implement RE-COMPILE"))

(defun re-exec (re string &key (start 0) (end nil))
  #+clisp
  (mapcar (lambda (match)
            (list (regexp:match-start match)
                  (regexp:match-end   match)
                  match))
          (multiple-value-list (regexp:regexp-exec re string :start start :end (or end (length string)))))
  #+(and (not clisp) cl-ppcre)
  (multiple-value-bind (start end starts ends)
      (cl-ppcre:scan re string :start start :end (or end (length string)))
    (and start end
         (values-list  (cons (list start end)
                             (map 'list (lambda (s e)
                                          (if (or s e)
                                              (list s e)
                                              nil))
                                  starts ends)))))
  #-(or clisp cl-ppcre)
  (error "Please implement RE-EXEC"))

(defun re-match-string (string match)
  #+clisp
  (regexp:match-string string (third match))
  #+(and (not clisp) cl-ppcre)
  (subseq string (first match) (second match))
  #-(or clisp cl-ppcre)
  (error "Please implement RE-MATCH-STRING"))

(defun re-match (regexp string)
  (re-exec (re-compile regexp :extended t) string))


(defun re-quote (re &key extended)
  (assert extended (extended) "re-quote is not implemented yet for non-extended regexps.")
  (cl:with-output-to-string (out)
    (loop
       :for ch :across re
       :do (cond
             ((alphanumericp ch) (princ ch out))
             (t (princ "\\" out) (princ ch out))))))



;;;; THE END ;;;;
