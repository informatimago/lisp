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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "PARSER" "PARSER-SCANNER"
           "PARSER-NEXT-TOKEN" "PARSER-TOKEN"
           "REPORT-ERROR" "ADVANCE")
  (:documentation ""))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER")


(defclass parser ()
  ((scanner     :accessor parser-scanner     :initform nil :initarg :scanner)
   (token       :accessor parser-token       :initform nil
                :documentation "current token")
   (next-token  :accessor parser-next-token  :initform nil
                :documentation "next token"))
  (:documentation "A parser."))

(defgeneric advance (parser))
(defgeneric report-error (parser message &rest arguments))


(defmethod print-object ((self parser) out)
  (print-unreadable-object (self out :type t :identity t)
    (format out " :scanner ~S :token (~S ~S) :next (~S ~S)"
            (parser-scanner self)
            (parser-token self)      (token-text (parser-token parser))
            (parser-next-token self) (token-text (parser-next-token self))))
  self)

          
(defmethod advance ((parser parser))
  (multiple-value-bind (tok val) (scan-next-token (parser-scanner parser))
    (setf (parser-token parser)      (parser-next-token parser)
          (parser-next-token parser) tok))
  parser)


(defmethod report-error ((parser parser) message &rest arguments)
  (error "~A; (~S ~S) (~S ~S)" (apply (function format) nil message arguments)
         (parser-token parser)
         (token-text (parser-token parser))
         (parser-next-token parser)
         (token-text (parser-next-token parser))))


;;;; THE END ;;;;
