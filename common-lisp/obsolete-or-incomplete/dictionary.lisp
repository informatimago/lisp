;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               dictionary.lisp
;;;;LANGUAGE:           COMMON-LISP/CLOS
;;;;SYSTEM:             COMMON-LISP
;;;;USER-INTERFACE:     None
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;    This is a Dictionary class. "<em>legacy code</em>" Silly implementation
;;;;    using linked lists to store the couples.
;;;;    Should be redone using hash tables.
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-25 <PJB> Removed import from CLOS (it's all in COMMON-LISP!).
;;;;    1995-10-16 <PJB> Creation.
;;;;BUGS
;;;;    Silly implementation using linked lists to store the couples.
;;;;    Should be redone using hash tables.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1995 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DICTIONARY"
  (:use "COMMON-LISP")
  (:export "APPLY-TO-ASSOCIATIONS" "CONTAINS-KEY?" "CONTAINS-ASSOCIATION?"
           "PUT-AT-KEY" "ASSOCIATION-AT-KEY" "VALUE-AT-KEY" "GET-VALUES" "GET-KEYS"
           "DICTIONARY" "DICTIONARY-SUMMARY")
  (:documentation
   "This is a DICTIONARY class.
    Silly implementation using linked lists to store the couples.
    Should be redone using hash tables.

    Copyright Pascal J. Bourguignon 1995 - 1995
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DICTIONARY")




(defun dictionary-summary ()
  (format t "~%")
  ;; egrep -i '^(DEFCLASS|^(DEFMETHOD' dictionary.lisp |sed -e 's/(DEFCLASS \(.*\)/	(FORMAT T "Class \1~%")/' -e 's/(DEFMETHOD\(.*\)/    (FORMAT T "\1~%")/'
  (format t "Class DICTIONARY ()~%")
  (format t " GET-KEYS              ((SELF DICTIONARY))~%")
  (format t " GET-VALUES            ((SELF DICTIONARY))~%")
  (format t " VALUE-AT-KEY           ((SELF DICTIONARY) KEY)~%")
  (format t " ASSOCIATION-AT-KEY     ((SELF DICTIONARY) KEY)~%")
  (format t " PUT-AT-KEY             ((SELF DICTIONARY) KEY VALUE)~%")
  (format t " CONTAINS-ASSOCIATION? ((SELF DICTIONARY) ASSOCIATION)~%")
  (format t " CONTAINS-KEY?         ((SELF DICTIONARY) KEY)~%")
  (format t " APPLY-TO-ASSOCIATIONS  ((SELF DICTIONARY) FUNC)~%"))


;; M-x insert-generic RET
(defgeneric get-keys (a))
(defgeneric get-values (a))
(defgeneric value-at-key (a b))
(defgeneric association-at-key (a b))
(defgeneric put-at-key (a b c))
(defgeneric contains-association? (a b))
(defgeneric contains-key? (a b))
(defgeneric apply-to-associations (a b))


(defclass dictionary ()
  ((associations :accessor associations :initform '())))

(defmethod get-keys              ((self dictionary))
  (mapcar (function car) (associations self)))

(defmethod get-values            ((self dictionary))
  (mapcar (function cdr) (associations self)))

(defun dictionary-private-association-at-key (associations key)
  (cond
    ((null associations)
     associations)
    ((eq (caar associations) key)
     (car associations))
    (t	
     (dictionary-private-association-at-key 
      (cdr associations) key))))

(defmethod value-at-key           ((self dictionary) key)
  (let ((assoc 
         (dictionary-private-association-at-key (associations self) key)))
    (if (null assoc)
        assoc
        (cdr assoc))))

(defmethod association-at-key     ((self dictionary) key)
  (dictionary-private-association-at-key (associations self) key))

(defmethod put-at-key             ((self dictionary) key value)
  (let ((assoc 
         (dictionary-private-association-at-key (associations self) key)))
    (cond
      ((null assoc)
       (setf (associations self) 
             (cons (cons key value) (associations self))))
      (t
       (rplacd assoc value)))))
	
(defmethod contains-association? ((self dictionary) association)
  (member association (associations self)))
	
(defmethod contains-key?         ((self dictionary) key)
  (member key (mapcar 'car (associations self))))

(defmethod apply-to-associations  ((self dictionary) func)
  (declare (type (function (cons) t) func))
  (mapcar func (associations self)))

;;;; dictionary.lisp                  --                     --          ;;;;
