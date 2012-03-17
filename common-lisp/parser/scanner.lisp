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
;;;;    2012-02-07 <PJB> Added a TOKEN class.
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2004-10-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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

(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM")
  (:export
   ;; TOKEN:
   "TOKEN" "TOKEN-KIND" "TOKEN-TEXT" "TOKEN-LINE" "TOKEN-COLUMN"
   "*SPACE*"
   ;; SCANNER:
   "SCANNER" "SCANNER-CURRENT-TOKEN" 
   "SCANNER-SOURCE" "SCANNER-LINE" "SCANNER-COLUMN" "SCANNER-STATE"
   "SCANNER-SPACES" "SCANNER-TAB-WIDTH"
   ;; SCANNER-ERROR condition:
   "SCANNER-ERROR" "SCANNER-ERROR-LINE" "SCANNER-ERROR-COLUMN"
   "SCANNER-ERROR-STATE" "SCANNER-ERROR-CURRENT-TOKEN"
   "SCANNER-ERROR-SCANNER"
   "SCANNER-ERROR-FORMAT-CONTROL" "SCANNER-ERROR-FORMAT-ARGUMENTS"
   "SCANNER-ERROR-INVALID-CHARACTER"
   ;; SCANNER methods:
   "SKIP-SPACES" "SCAN-NEXT-TOKEN")
  (:documentation
   "An abstract scanner class.
A method to the SCAN-NEXT-TOKEN generic function needs to be provided.

Copyright Pascal J. Bourguignon 2004 - 2012
This package is provided under the GNU General Public License.
See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")


;;----------------------------------------------------------------------
;; TOKEN
;;----------------------------------------------------------------------

;; We provide a token class, but we use duck-typing, so one could use
;; symbols or strings for tokens too (but losing eg. the line:column
;; feature).

(defgeneric token-kind (token)
  (:documentation "Returns the kind of the token.")
  (:method ((token symbol)) token)
  (:method ((token string)) nil))

(defgeneric token-text (token)
  (:documentation "Returns the literal text the token.")
  (:method ((token symbol)) (symbol-name token))
  (:method ((token string)) token))

(defgeneric token-line (token)
  (:documentation "Returns the line where the token was found.")
  (:method ((token t)) 0))

(defgeneric token-column (token)
  (:documentation "Returns the column of the first character of the token.")
  (:method ((token t)) 0))


(defclass token ()
  ((kind       :initarg :kind
               :accessor token-kind
               :initform nil
               :type symbol)
   (text       :accessor token-text
               :initarg :text
               :initform ""
               :type     string)
   (column     :accessor token-column
               :initarg :column
               :initform 0
               :type (integer 0))
   (line       :accessor token-line
               :initarg :line
               :initform 0
               :type (integer 0))))




;;----------------------------------------------------------------------
;; SCANNER
;;----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-supported-p (name)
     (ignore-errors (read-from-string (format nil "#\\~A" name)))))


(defvar *spaces*
  (let ((spaces '()))
    (dolist (name '("Page" "Linefeed" "Return" "Tab" "Newline" "Space"))
      (let ((ch (char-supported-p name)))
        (when ch (push ch spaces))))
    (coerce spaces 'string)))



;; Note we copy some fields in the condition from the scanner, so that
;; they can change in the scanner object between the condition
;; creation and its handling.

(define-condition scanner-error (simple-error)
  ((line             :initarg :line             :initform 1   :reader scanner-error-line)
   (column           :initarg :column           :initform 0   :reader scanner-error-column)
   (state            :initarg :state            :initform 0   :reader scanner-error-state)
   (current-token    :initarg :current-token    :initform nil :reader scanner-error-current-token)
   (scanner          :initarg :scanner                        :reader scanner-error-scanner)
   (format-control   :initarg :format-control   :initform ""  :reader scanner-error-format-control)
   (format-arguments :initarg :format-arguments :initform '() :reader scanner-error-format-arguments)))


(define-condition scanner-error-invalid-character (scanner-error)
  ((invalid-character :initarg :invalid-character :initform nil :reader scanner-error-invalid-character)))




(defclass scanner ()
  ((source        :initarg :source
                  :accessor scanner-source
                  :documentation "The source can be a PEEK-STREAM, a STREAM, or a STRING.")
   (stream        :type peek-stream
                  :documentation "The source is wrapped into this PEEK-STREAM.")
   (line          :initarg :line
                  :accessor scanner-line
                  :type (integer 0)
                  :initform 0
                  :documentation "The number of the current line.")
   (column        :initarg :column
                  :accessor scanner-column
                  :type (integer 0)
                  :initform 0
                  :documentation "The number of the current column.")
   (state         :initarg :state
                  :accessor scanner-state
                  :initform nil
                  :documentation "The state of the scanner.")
   (spaces        :initarg :spaces
                  :accessor scanner-spaces
                  :type string
                  :initform *spaces*
                  :documentation "A string containing the characters considered space by SKIP-SPACES.")
   (tab-width     :initarg :tab-width
                  :accessor scanner-tab-width
                  :type fixnum
                  :initform 8
                  :documentation "TAB aligns to column number modulo TAB-WIDTH.")
   (current-token :accessor scanner-current-token
                  :initform nil
                  :documentation "The last token read."))
  (:documentation "An abstract scanner."))



(defmethod skip-spaces ((scanner scanner))
  "
DO: Skips over the spaces in the input stream. Updates line and column slots.
RETURN: line; column
"
  (loop
    :with input     = (slot-value scanner 'stream)
    :with tab-width = (scanner-tab-width scanner)
    :with line      = (scanner-line scanner)
    :with column    = (scanner-column scanner)
    :for ch = (nextchar input)
    :while ch
    :do (case ch
          ((#\Newline
            #+#.(cl:if (cl:let ((com.informatimago.common-lisp.parser.scanner::ch
                                 (com.informatimago.common-lisp.parser.scanner::char-supported-p "Linefeed")))
                         (cl:and com.informatimago.common-lisp.parser.scanner::ch
                              (cl:char/= com.informatimago.common-lisp.parser.scanner::ch #\newline))) '(:and) '(:or)) #\linefeed
            #+#.(cl:if (com.informatimago.common-lisp.parser.scanner::char-supported-p "Page")     '(:and) '(:or)) #\page)
           (incf line)
           (getchar input))
          ((#\space)
           (incf column)
           (getchar input))
          ((nil
            #+#.(cl:if (com.informatimago.common-lisp.parser.scanner::char-supported-p "Tab") '(:and) '(:of)) #\tab)
           (setf column (* tab-width (ceiling column tab-width)))
           (getchar input))
          (otherwise (loop-finish)))
    :finally (progn
               (setf (scanner-line   scanner) line
                     (scanner-column scanner) column)
               (return (values line column)))))



(defgeneric scan-next-token (scanner &optional parser-data)
  (:documentation "
DO:           Scans a new token and store it into (scanner-current-token scanner)
PARSER-DATA:  Some parsers give information to the scanner.
RETURN:       (scanner-current-token scanner).
"))


(defmethod initialize-instance :after ((scanner scanner) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (etypecase (scanner-source scanner)
    (peek-stream (setf (slot-value scanner 'stream) (scanner-source scanner)))
    (stream      (setf (slot-value scanner 'stream) (make-instance 'peek-stream :stream (scanner-source scanner))))
    (string      (setf (slot-value scanner 'stream) (make-instance 'peek-stream :stream (make-string-input-stream (scanner-source scanner)))))))


(defmethod print-object ((self scanner) out)
  (print-unreadable-object (self out :type t :identity t)
    (format out "~{~S~^ ~}"
            (list :line          (scanner-line          self)
                  :column        (scanner-column        self)
                  :current-token (scanner-current-token self)
                  :source        (scanner-source        self))))
  self)


;;;; THE END ;;;;
