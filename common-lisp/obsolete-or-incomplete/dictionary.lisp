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
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1995 - 1995
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DICTIONARY"
  (:USE "COMMON-LISP")
  (:EXPORT "APPLY-TO-ASSOCIATIONS" "CONTAINS-KEY?" "CONTAINS-ASSOCIATION?"
           "PUT-AT-KEY" "ASSOCIATION-AT-KEY" "VALUE-AT-KEY" "GET-VALUES" "GET-KEYS"
           "DICTIONARY" "DICTIONARY-SUMMARY")
  (:DOCUMENTATION
   "This is a DICTIONARY class.
    Silly implementation using linked lists to store the couples.
    Should be redone using hash tables.

    Copyright Pascal J. Bourguignon 1995 - 1995
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DICTIONARY")




(DEFUN DICTIONARY-SUMMARY ()
  (FORMAT T "~%")
  ;; egrep -i '^(DEFCLASS|^(DEFMETHOD' dictionary.lisp |sed -e 's/(DEFCLASS \(.*\)/	(FORMAT T "Class \1~%")/' -e 's/(DEFMETHOD\(.*\)/    (FORMAT T "\1~%")/'
  (FORMAT T "Class DICTIONARY ()~%")
  (FORMAT T " GET-KEYS              ((SELF DICTIONARY))~%")
  (FORMAT T " GET-VALUES            ((SELF DICTIONARY))~%")
  (FORMAT T " VALUE-AT-KEY           ((SELF DICTIONARY) KEY)~%")
  (FORMAT T " ASSOCIATION-AT-KEY     ((SELF DICTIONARY) KEY)~%")
  (FORMAT T " PUT-AT-KEY             ((SELF DICTIONARY) KEY VALUE)~%")
  (FORMAT T " CONTAINS-ASSOCIATION? ((SELF DICTIONARY) ASSOCIATION)~%")
  (FORMAT T " CONTAINS-KEY?         ((SELF DICTIONARY) KEY)~%")
  (FORMAT T " APPLY-TO-ASSOCIATIONS  ((SELF DICTIONARY) FUNC)~%"))


;; M-x insert-generic RET
(DEFGENERIC GET-KEYS (A))
(DEFGENERIC GET-VALUES (A))
(DEFGENERIC VALUE-AT-KEY (A B))
(DEFGENERIC ASSOCIATION-AT-KEY (A B))
(DEFGENERIC PUT-AT-KEY (A B C))
(DEFGENERIC CONTAINS-ASSOCIATION? (A B))
(DEFGENERIC CONTAINS-KEY? (A B))
(DEFGENERIC APPLY-TO-ASSOCIATIONS (A B))


(DEFCLASS DICTIONARY ()
  ((ASSOCIATIONS :ACCESSOR ASSOCIATIONS :INITFORM '())))

(DEFMETHOD GET-KEYS              ((SELF DICTIONARY))
  (MAPCAR (FUNCTION CAR) (ASSOCIATIONS SELF)))

(DEFMETHOD GET-VALUES            ((SELF DICTIONARY))
  (MAPCAR (FUNCTION CDR) (ASSOCIATIONS SELF)))

(DEFUN DICTIONARY-PRIVATE-ASSOCIATION-AT-KEY (ASSOCIATIONS KEY)
  (COND
    ((NULL ASSOCIATIONS)
     ASSOCIATIONS)
    ((EQ (CAAR ASSOCIATIONS) KEY)
     (CAR ASSOCIATIONS))
    (T	
     (DICTIONARY-PRIVATE-ASSOCIATION-AT-KEY 
      (CDR ASSOCIATIONS) KEY))))

(DEFMETHOD VALUE-AT-KEY           ((SELF DICTIONARY) KEY)
  (LET ((ASSOC 
         (DICTIONARY-PRIVATE-ASSOCIATION-AT-KEY (ASSOCIATIONS SELF) KEY)))
    (IF (NULL ASSOC)
        ASSOC
        (CDR ASSOC))))

(DEFMETHOD ASSOCIATION-AT-KEY     ((SELF DICTIONARY) KEY)
  (DICTIONARY-PRIVATE-ASSOCIATION-AT-KEY (ASSOCIATIONS SELF) KEY))

(DEFMETHOD PUT-AT-KEY             ((SELF DICTIONARY) KEY VALUE)
  (LET ((ASSOC 
         (DICTIONARY-PRIVATE-ASSOCIATION-AT-KEY (ASSOCIATIONS SELF) KEY)))
    (COND
      ((NULL ASSOC)
       (SETF (ASSOCIATIONS SELF) 
             (CONS (CONS KEY VALUE) (ASSOCIATIONS SELF))))
      (T
       (RPLACD ASSOC VALUE)))))
	
(DEFMETHOD CONTAINS-ASSOCIATION? ((SELF DICTIONARY) ASSOCIATION)
  (MEMBER ASSOCIATION (ASSOCIATIONS SELF)))
	
(DEFMETHOD CONTAINS-KEY?         ((SELF DICTIONARY) KEY)
  (MEMBER KEY (MAPCAR 'CAR (ASSOCIATIONS SELF))))

(DEFMETHOD APPLY-TO-ASSOCIATIONS  ((SELF DICTIONARY) FUNC)
  (DECLARE (TYPE (FUNCTION (CONS) T) FUNC))
  (MAPCAR FUNC (ASSOCIATIONS SELF)))

;;;; dictionary.lisp                  --                     --          ;;;;
