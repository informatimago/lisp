;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               scanner.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An abstract scanner class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2004-10-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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

(DECLAIM (DECLARATION ALSO-USE-PACKAGES))
(declaim (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))

(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:EXPORT "GET-TOKEN" "SCANNER-SOURCE" "SCANNER"
           "+SPACES+" "+SPACES-AND-NEWLINES+"
           "+CRLF+" "+LF+" "+CR+" "+NEWLINE+"
           "+C+LF+" "+C+CR+")
  (:DOCUMENTATION
   "An abstract scanner class.

    Copyright Pascal J. Bourguignon 2004 - 2008
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))

(DEFPARAMETER +C+CR+      com.informatimago.common-lisp.cesarum.ecma048:cr)
(DEFPARAMETER +C+LF+      com.informatimago.common-lisp.cesarum.ecma048:lf)
(DEFPARAMETER +NEWLINE+   (NAME-CHAR "NEWLINE"))
(DEFPARAMETER +CR+        (code-char com.informatimago.common-lisp.cesarum.ecma048:cr))
(DEFPARAMETER +LF+        (code-char com.informatimago.common-lisp.cesarum.ecma048:lf))
(DEFPARAMETER +CRLF+      (FORMAT NIL "~C~C" +CR+ +LF+))
(DEFPARAMETER +SPACES+
  (FORMAT NIL " ~C" (code-char com.informatimago.common-lisp.cesarum.ecma048:ht)))
(DEFPARAMETER +SPACES-and-newlines+
  (FORMAT NIL "~A~C~C~C" +spaces+ +CR+ +LF+ +NEWLINE+))



(DEFCLASS SCANNER ()
  ((SOURCE  :TYPE PEEK-STREAM
            :INITARG :SOURCE :ACCESSOR SCANNER-SOURCE))
  (:DOCUMENTATION "An abstract scanner."))


(DEFGENERIC GET-TOKEN (SCANNER))


(DEFMETHOD PRINT-OBJECT ((SELF SCANNER) OUT)
  (print-unreadable-object (self out :type t :identity t)
    (FORMAT OUT " :source ~A" (SCANNER-SOURCE SELF)))
  SELF)


;;;; scanner.lisp                     --                     --          ;;;;
