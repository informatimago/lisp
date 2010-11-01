;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An abstract parser class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-10 <PJB> Created.
;;;;BUGS
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:EXPORT "REPORT-ERROR" "ADVANCE" "PARSER-NEXT-VALUE" "PARSER-NEXT-TOKEN"
           "PARSER-VALUE" "PARSER-TOKEN" "PARSER-SCANNER" "PARSER")
  (:DOCUMENTATION ""))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER")




(DEFCLASS PARSER ()
  ((SCANNER     :ACCESSOR PARSER-SCANNER     :INITFORM NIL :INITARG :SCANNER)
   (TOKEN       :ACCESSOR PARSER-TOKEN       :INITFORM NIL
                :documentation "current token")
   (VALUE       :ACCESSOR PARSER-VALUE       :INITFORM NIL
                :documentation "text of the current token")
   (NEXT-TOKEN  :ACCESSOR PARSER-NEXT-TOKEN  :INITFORM NIL
                :documentation "next token")
   (NEXT-VALUE  :ACCESSOR PARSER-NEXT-VALUE  :INITFORM NIL
                :documentation "text of the next token"))
  (:DOCUMENTATION "A parser."))


(DEFGENERIC ADVANCE (PARSER))
(DEFGENERIC REPORT-ERROR (PARSER MESSAGE &REST ARGUMENTS))


(DEFMETHOD PRINT-OBJECT ((SELF PARSER) OUT)
  (print-unreadable-object (self out :type t :identity t)
    (FORMAT OUT " :scanner ~S :token (~S ~S) :next (~S ~S)"
            (PARSER-SCANNER SELF)
            (PARSER-TOKEN SELF)      (PARSER-VALUE SELF)
            (PARSER-NEXT-TOKEN SELF) (PARSER-NEXT-VALUE SELF)))
  SELF)

          
(DEFMETHOD ADVANCE ((PARSER PARSER))
  (MULTIPLE-VALUE-BIND (TOK VAL) (GET-TOKEN (PARSER-SCANNER PARSER))
    (SETF (PARSER-TOKEN PARSER)      (PARSER-NEXT-TOKEN PARSER)
          (PARSER-VALUE PARSER)      (PARSER-NEXT-VALUE PARSER) 
          (PARSER-NEXT-TOKEN PARSER) TOK
          (PARSER-NEXT-VALUE PARSER) VAL))
  PARSER)


(DEFMETHOD REPORT-ERROR ((PARSER PARSER) MESSAGE &REST ARGUMENTS)
  (ERROR "~A; (~S ~S) (~S ~S)" (APPLY (FUNCTION FORMAT) NIL MESSAGE ARGUMENTS)
         (PARSER-TOKEN PARSER)
         (PARSER-VALUE PARSER)
         (PARSER-NEXT-TOKEN PARSER)
         (PARSER-NEXT-VALUE PARSER)))


;;;; parser.lisp                      --                     --          ;;;;
