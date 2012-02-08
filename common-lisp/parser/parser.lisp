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

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "REPORT-ERROR" "ADVANCE" "PARSER-NEXT-VALUE" "PARSER-NEXT-TOKEN"
           "PARSER-VALUE" "PARSER-TOKEN" "PARSER-SCANNER" "PARSER")
  (:documentation ""))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER")




(defclass parser ()
  ((scanner     :accessor parser-scanner     :initform nil :initarg :scanner)
   (token       :accessor parser-token       :initform nil
                :documentation "current token")
   (value       :accessor parser-value       :initform nil
                :documentation "text of the current token")
   (next-token  :accessor parser-next-token  :initform nil
                :documentation "next token")
   (next-value  :accessor parser-next-value  :initform nil
                :documentation "text of the next token"))
  (:documentation "A parser."))


(defgeneric advance (parser))
(defgeneric report-error (parser message &rest arguments))


(defmethod print-object ((self parser) out)
  (print-unreadable-object (self out :type t :identity t)
    (format out " :scanner ~S :token (~S ~S) :next (~S ~S)"
            (parser-scanner self)
            (parser-token self)      (parser-value self)
            (parser-next-token self) (parser-next-value self)))
  self)

          
(defmethod advance ((parser parser))
  (multiple-value-bind (tok val) (get-token (parser-scanner parser))
    (setf (parser-token parser)      (parser-next-token parser)
          (parser-value parser)      (parser-next-value parser) 
          (parser-next-token parser) tok
          (parser-next-value parser) val))
  parser)


(defmethod report-error ((parser parser) message &rest arguments)
  (error "~A; (~S ~S) (~S ~S)" (apply (function format) nil message arguments)
         (parser-token parser)
         (parser-value parser)
         (parser-next-token parser)
         (parser-next-value parser)))


;;;; THE END ;;;;
