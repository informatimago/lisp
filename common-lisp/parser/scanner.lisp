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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
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
   "SKIP-SPACES" "SCAN-NEXT-TOKEN"
   ;; PEEK-STREAM methods specialized on SCANNER:
   "NEXTCHAR" "UNGETCHAR" "GETCHAR" "READLINE"
   ;; Internal
   "CHAR-NAME-SUPPORTED-P")
  (:documentation
   "
An abstract scanner class.

A method to the SCAN-NEXT-TOKEN generic function needs to be provided.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2012
    
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
    If not, see http://www.gnu.org/licenses/
"))
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
               :initform 1
               :type (integer 1))
   (line       :accessor token-line
               :initarg :line
               :initform 1
               :type (integer 0)))
  (:documentation "A syntactic element."))


(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :identity t :type t)
    (prin1 (list (token-kind token)
                 (token-text token)
                 (token-column token)
                 (token-line token)) stream))
  token)




;;----------------------------------------------------------------------
;; SCANNER
;;----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-name-supported-p (name)
     (ignore-errors (read-from-string (format nil "#\\~A" name)))))


(defvar *spaces*
  (let ((spaces '()))
    (dolist (name '("Page" "Linefeed" "Return" "Tab" "Newline" "Space"))
      (let ((ch (char-name-supported-p name)))
        (when ch (push ch spaces))))
    (coerce spaces 'string)))



;; Some tools can't deal with #+#.(...) well, so we go thru *feature*
;; instead.

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Semi-standard character names:
  
  (when (let ((ch (char-name-supported-p "Linefeed")))
          (and ch (char/= ch #\Newline)))
    (pushnew :linefeed *features*))

  (when  (char-name-supported-p "Page")
    (pushnew :page *features*))

  (when  (char-name-supported-p "Backspace")
    (pushnew :backspace *features*))

  (when  (char-name-supported-p "Tab")
    (pushnew :tab *features*))

  ;; Non-standard character names:
  
  (when  (char-name-supported-p "Bell")
    (pushnew :bell *features*))

  (when  (char-name-supported-p "Vt")
    (pushnew :vt *features*))
  
  );;eval-when



;; Note we copy some fields in the condition from the scanner, so that
;; they can change in the scanner object between the condition
;; creation and its handling.

(defgeneric scanner-error-line (error)
  (:documentation "The line on which the scanner error was detected."))
(defgeneric scanner-error-column (error)
  (:documentation "The column on which the scanner error was detected."))
(defgeneric scanner-error-state (error)
  (:documentation "The scanner state when error was detected."))
(defgeneric scanner-error-current-token (error)
  (:documentation "The scanner token where error was detected."))
(defgeneric scanner-error-scanner (error)
  (:documentation "The scanner that detected the error."))
(defgeneric scanner-error-format-control (error)
  (:documentation "The error message format control string."))
(defgeneric scanner-error-format-arguments (error)
  (:documentation "The error message format control arguments."))
(defgeneric scanner-error-invalid-character (error)

  (:documentation "The invalid character that made the scanner error."))

(define-condition scanner-error (simple-error)
  ((line             :initarg :line             :initform 1   :reader scanner-error-line
                     :documentation "The number of the line. First line is line number 1.")
   (column           :initarg :column           :initform 1   :reader scanner-error-column
                     :documentation "The number of the column.  First column is column number 1.")
   (state            :initarg :state            :initform 0   :reader scanner-error-state)
   (current-token    :initarg :current-token    :initform nil :reader scanner-error-current-token)
   (scanner          :initarg :scanner                        :reader scanner-error-scanner)
   (format-control   :initarg :format-control   :initform ""  :reader scanner-error-format-control)
   (format-arguments :initarg :format-arguments :initform '() :reader scanner-error-format-arguments))
  (:documentation "A scanner error."))


(define-condition scanner-error-invalid-character (scanner-error)
  ((invalid-character :initarg :invalid-character :initform nil :reader scanner-error-invalid-character))
  (:documentation "An invalid character scanner error."))


(defgeneric scanner-source (scanner)
  (:documentation "The source can be a PEEK-STREAM, a STREAM, or a STRING."))
(defgeneric scanner-line (scanner)
  (:documentation "The number of the current line."))
(defgeneric scanner-column (scanner)
  (:documentation "The number of the current column."))
(defgeneric scanner-state (scanner)
  (:documentation "The state of the scanner."))
(defgeneric scanner-spaces (scanner)
  (:documentation "A string containing the characters considered space by SKIP-SPACES."))
(defgeneric scanner-tab-width (scanner)
  (:documentation "TAB aligns to column number modulo TAB-WIDTH."))
(defgeneric scanner-current-token (scanner)
  (:documentation "The last token read."))


(defclass scanner ()
  ((source        :initarg :source
                  :reader scanner-source
                  :documentation "The source can be a PEEK-STREAM, a STREAM, or a STRING.")
   (stream        :type peek-stream
                  :reader scanner-stream
                  :documentation "The source is wrapped into this PEEK-STREAM.
Subclasses may use scanner-stream to read from the source.")
   (line          :initarg :line
                  :accessor scanner-line
                  :type integer
                  :initform 1
                  :documentation "The number of the current line. First line is line number 1.")
   (column        :initarg :column
                  :accessor scanner-column
                  :type integer
                  :initform 1
                  :documentation "The number of the current column. First column is column number 1.")
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



(defgeneric increment-column-to-next-tab-stop (scanner)
  (:documentation "
DO:             Increments the scanner-column to the next tab-stop.
RETURN:         SCANNER
")
  (:method ((scanner scanner))
    (let ((tab-width (scanner-tab-width scanner)))
      (setf (scanner-column scanner)
            ;; Assuming column starts from 1.
            (1+ (* (ceiling (scanner-column scanner) tab-width)
                   tab-width))
            ;; #+column-base=0 (* tab-width (ceiling (scanner-column scanner) tab-width))
            ))
    scanner))


(defgeneric skip-spaces (scanner)
  (:documentation   "
DO: Skips over the spaces in the input stream. Updates line and column slots.
RETURN: line; column
"))

(defmethod skip-spaces ((scanner scanner))
  "
DO: Skips over the spaces in the input stream. Updates line and column slots.
RETURN: line; column
"
  (loop
    :for ch = (getchar scanner)
    :while ch
    :do (case ch
          ((#\Space
            #\Newline
            #+linefeed #\Linefeed
            #+page     #\Page
            #+tab      #\Tab))
          (otherwise
           (loop-finish)))
    :finally (progn
               (ungetchar scanner ch)
               (return (values (scanner-line   scanner)
                               (scanner-column scanner))))))



(defgeneric scan-next-token (scanner &optional parser-data)
  (:documentation "
DO:           Scans a new token and store it into (scanner-current-token scanner)
PARSER-DATA:  Some parsers give information to the scanner.
RETURN:       (scanner-current-token scanner).
"))


(defmethod (setf scanner-source) (new-source (scanner scanner))
  (setf (slot-value scanner 'stream)
        (etypecase (setf (slot-value scanner 'source) new-source)
          (peek-stream (slot-value scanner 'source))
          (stream      (make-instance 'peek-stream
                         :spaces (scanner-spaces scanner)
                         :stream (slot-value scanner 'source)))
          (string      (make-instance 'peek-stream
                         :spaces (scanner-spaces scanner)
                         :stream (make-string-input-stream
                                  (slot-value scanner 'source))))))
  (slot-value scanner 'source))


(defmethod initialize-instance :after ((scanner scanner) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (scanner-source scanner) (slot-value scanner 'source)))


(defmethod print-object ((self scanner) out)
  (print-unreadable-object (self out :type t :identity t)
    (format out "~{~S~^ ~}"
            (list :line          (scanner-line          self)
                  :column        (scanner-column        self)
                  :current-token (scanner-current-token self)
                  :source        (scanner-source        self))))
  self)



;;; We implement the generic functions of peek-stream to track columns
;;; and lines.


(defmethod nextchar ((scanner scanner) &optional (peek-type nil))
  ;; No impact on line/column.
  (nextchar (scanner-stream scanner) peek-type))


(defmethod getchar ((scanner scanner))
  (let ((ch (getchar (scanner-stream scanner))))
    (case ch
      ((#\Newline)
       (incf (scanner-line scanner))
       (setf (scanner-column scanner) 1))
      #+tab
      ((#\Tab)
       (increment-column-to-next-tab-stop scanner))
      (otherwise
       ;; including #\Return #+linefeed #\Linefeed #+page #\Page
       (incf (scanner-column scanner))))
    ch))

(defmethod ungetchar ((scanner scanner) (ch null))
  ch)

(defmethod ungetchar ((scanner scanner) (ch character))
  (let ((ch (ungetchar (scanner-stream scanner) ch)))
    (case ch
      ((#\Newline)
       ;; We don't know the length of the last line.
       (decf (scanner-line scanner))
       (setf (scanner-column scanner) 0))
      #+tab
      ((#\Tab)
       ;; We don't know how many characters there was in the last tab-width.
       (setf (scanner-column scanner)  (truncate (1- (scanner-column scanner))
                                                 (scanner-tab-width scanner))))
      (otherwise
       ;; including #\Return #+linefeed #\Linefeed #+page #\Page
       (decf (scanner-column scanner))))
    ch))


(defmethod readline ((scanner scanner))
  #-(and)
  (with-output-to-string (out)
    (loop
      :for ch = (getchar scanner)
      :until (find ch #(#\Newline #\Return #+linefeed #\Linefeed #+page #\Page))
      :do (write-char ch out)))
  (prog1 (readline (scanner-stream scanner))
    (incf (scanner-line scanner))
    (setf (scanner-column scanner) 1)))

;;;; THE END ;;;;
