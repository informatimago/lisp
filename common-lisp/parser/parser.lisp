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
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER")


(defgeneric parser-scanner (parser)
  (:documentation "The scanner of the parser."))
(defgeneric parser-token (parser)
  (:documentation "The current token of the parser."))
(defgeneric parser-next-token (parser)
  (:documentation "The next-token of the parser."))

(defclass parser ()
  ((scanner     :accessor parser-scanner     :initform nil :initarg :scanner)
   (token       :accessor parser-token       :initform nil
                :documentation "current token")
   (next-token  :accessor parser-next-token  :initform nil
                :documentation "next token"))
  (:documentation "A parser."))


(defmethod print-object ((self parser) out)
  (print-unreadable-object (self out :type t :identity t)
    (format out " :scanner ~S :token (~S ~S) :next (~S ~S)"
            (parser-scanner self)
            (parser-token self)      (token-text (parser-token self))
            (parser-next-token self) (token-text (parser-next-token self))))
  self)


(defgeneric parser-error-parser (error)
  (:documentation "The parser that signaled the error."))
(defgeneric parser-error-token (error)
  (:documentation "The token where the error was detected."))
(defgeneric parser-error-next-token (error)
  (:documentation "The next-token where the error was detected."))
(defgeneric parser-error-format-control (error)
  (:documentation "The error message format control string."))
(defgeneric parser-error-format-arguments (error)
  (:documentation "The error message format control arguments."))

(define-condition parser-error (error)
  ((parser           :initarg :parser           :reader parser-error-parser)
   (token            :initarg :token            :reader parser-error-token)
   (next-token       :initarg :next-token       :reader parser-error-next-token)
   (format-control   :initarg :format-control   :reader parser-error-format-control)
   (format-arguments :initarg :format-arguments :reader parser-error-format-arguments))
  (:documentation "A parser error.")
  (:report (lambda (condition stream)
             (format stream  "~?; (~S ~S) (~S ~S)"
                     (parser-error-format-control condition)
                     (parser-error-format-arguments condition)
                     (parser-error-token condition)
                     (token-text (parser-error-token condition))
                     (parser-error-next-token condition)
                     (token-text (parser-error-next-token condition))))))


(defgeneric advance (parser)
  (:documentation "Shitf next-token into token and scan the next token."))

(defgeneric report-error (parser message &rest arguments)
  (:documentation "Signal a parser-error."))




(defmethod advance ((parser parser))
  (multiple-value-bind (tok val) (scan-next-token (parser-scanner parser))
    (declare (ignore val))
    (setf (parser-token parser)      (parser-next-token parser)
          (parser-next-token parser) tok))
  parser)


(defmethod report-error ((parser parser) message &rest arguments)
  (error 'parser-error
         :parser parser
         :token (parser-token parser)
         :next-token (parser-next-token parser)
         :format-control message
         :format-arguments arguments))


;;;; THE END ;;;;
