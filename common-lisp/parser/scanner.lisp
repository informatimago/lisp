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
;;;;    2016-01-30 <PJB> SCANNER and TOKEN now inherit from SLOTED-OBJECT.
;;;;    2012-02-07 <PJB> Added a TOKEN class.
;;;;    2005-09-01 <PJB> Made use of iso6429.
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
  (:method ((token string)) token))

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


(defclass token (sloted-object)
  ((kind       :initarg :kind
               :accessor token-kind
               :initform nil
               :type symbol)
   (text       :accessor token-text
               :initarg :text
               :initform ""
               :type     (or null string))
   (column     :accessor token-column
               :initarg :column
               :initform 1
               :type (integer 0)) ; 0 is unknown.
   (line       :accessor token-line
               :initarg :line
               :initform 1
               :type (integer 0))) ; 0 is unknown.
  (:documentation "A syntactic element."))


(defmethod slots-for-print append ((self token))
  (extract-slots self '(kind text column line)))



;;----------------------------------------------------------------------
;; SCANNER
;;----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-name-supported-p (name)
     (ignore-errors (read-from-string (format nil "#\\~A" name)))))


;; Note: the features are defined in .cesarum.characters
(defvar *spaces*  (coerce (remove-duplicates
                           '(#\Space
                             #+has-newline #\Newline
                             #+has-tab #\Tab
                             #+(and has-return (not newline-is-return)) #\return
                             #+(and has-linefeed (not newline-is-linefeed)) #\linefeed
                             #+has-page #\page))
                          'string))


;; Note we copy some fields in the condition from the scanner, so that
;; they can change in the scanner object between the condition
;; creation and its handling.

(defgeneric scanner-error-file (error)
  (:documentation "The file in which the scanner error was detected."))
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
  ((file             :initarg :file             :initform "<stdin>"             :reader scanner-error-file
                     :documentation "The path of the file.")
   (line             :initarg :line             :initform 1                     :reader scanner-error-line
                     :documentation "The number of the line. First line is line number 1.")
   (column           :initarg :column           :initform 1                     :reader scanner-error-column
                     :documentation "The number of the column.  First column is column number 1.")
   (state            :initarg :state            :initform 0                     :reader scanner-error-state)
   (current-token    :initarg :current-token    :initform nil                   :reader scanner-error-current-token)
   (scanner          :initarg :scanner                                          :reader scanner-error-scanner)
   (format-control   :initarg :format-control   :initform ""                    :reader scanner-error-format-control)
   (format-arguments :initarg :format-arguments :initform '()                   :reader scanner-error-format-arguments))
  (:documentation "A scanner error."))


(define-condition scanner-error-invalid-character (scanner-error)
  ((invalid-character :initarg :invalid-character :initform nil :reader scanner-error-invalid-character))
  (:documentation "An invalid character scanner error."))


(defgeneric scanner-source (scanner)
  (:documentation "The source can be a PEEK-STREAM, a STREAM, or a STRING."))
(defgeneric scanner-file (scanner)
  (:documentation "The namestring of the current file being scanned."))
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
(defgeneric scanner-current-text (scanner)
  (:documentation "Text of the current token")
  (:method ((scanner scanner))
    (prin1-to-string (scanner-current-token scanner))))

(defclass scanner (sloted-object)
  ((source               :initarg       :source
                         :reader        scanner-source
                         :documentation "The source can be a PEEK-STREAM, a STREAM, or a STRING.")
   (stream               :type          peek-stream
                         :reader        scanner-stream
                         :documentation "The source is wrapped into this PEEK-STREAM.
Subclasses may use scanner-stream to read from the source.")
   (file                 :initarg       :file
                         :accessor      scanner-file
                         :type          string
                         :initform      "<stdin>"
                         :documentation "The namestring of the current file.")
   (line                 :initarg       :line
                         :accessor      scanner-line
                         :type          integer
                         :initform      0
                         :documentation "The number of the current line. First line is line number 1.")
   (column               :initarg       :column
                         :accessor      scanner-column
                         :type          integer
                         :initform      1
                         :documentation "The number of the current column. First column is column number 1.")
   (state                :initarg       :state
                         :accessor      scanner-state
                         :initform      nil
                         :documentation "The state of the scanner.")
   (spaces               :initarg       :spaces
                         :accessor      scanner-spaces
                         :type          sequence
                         :initform      *spaces*
                         :documentation "A string containing the characters considered space by SKIP-SPACES.")
   (tab-width            :initarg       :tab-width
                         :accessor      scanner-tab-width
                         :type          fixnum
                         :initform      8
                         :documentation "TAB aligns to column number modulo TAB-WIDTH.")
   (current-token        :accessor      scanner-current-token
                         :initform      nil
                         :documentation "The last token read.")
   (token-kind-package   :accessor      scanner-token-kind-package
                         :initform      (load-time-value (find-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"))
                         :initarg       :token-kind-package
                         :documentation "The package where the token-kind symbols are interned in."))
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
")
  (:method ((scanner scanner))
    (loop
      :for ch = (getchar scanner)
      :while (and ch (find ch (scanner-spaces scanner)))
      :finally (when ch
                 (ungetchar scanner ch))
               (return (values (scanner-line   scanner)
                               (scanner-column scanner))))))




(defgeneric scan-next-token (scanner &optional parser-data)
  (:documentation "
DO:           Scans a new token and store it into (scanner-current-token scanner)
PARSER-DATA:  Some parsers give information to the scanner.
RETURN:       (scanner-current-token scanner).
"))


(defgeneric (setf scanner-source) (new-source scanner))
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

(defmethod slots-for-print append ((self scanner))
  (extract-slots self '(line column current-token source)))



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
      #+has-tab
      ((#\Tab)
       (increment-column-to-next-tab-stop scanner))
      (otherwise
       ;; including #\Return #+has-linefeed #\Linefeed #+has-page #\Page
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
      #+has-tab
      ((#\Tab)
       ;; We don't know how many characters there was in the last tab-width.
       (setf (scanner-column scanner)  (truncate (1- (scanner-column scanner))
                                                 (scanner-tab-width scanner))))
      (otherwise
       ;; including #\Return #+has-linefeed #\Linefeed #+has-page #\Page
       (decf (scanner-column scanner))))
    ch))


(defmethod readline ((scanner scanner))
  #-(and)
  (with-output-to-string (out)
    (loop
      :for ch = (getchar scanner)
      :until (find ch #(#\Newline #\Return #+has-linefeed #\Linefeed #+has-page #\Page))
      :do (write-char ch out)))
  (prog1 (readline (scanner-stream scanner))
    (incf (scanner-line scanner))
    (setf (scanner-column scanner) 1)))


(defgeneric make-current-token (scanner)
  (:documentation "Makes an instance of the TOKEN class or a subclass
thereof, filled with the current information in the scanner object.")
  (:method ((scanner scanner))
    (make-instance 'token
                   :line  (scanner-line scanner)
                   :column (scanner-column scanner)
                   :text (scanner-current-text scanner)
                   :kind (etypecase (scanner-current-token scanner)
                           (string (intern (scanner-current-token scanner)
                                           (scanner-token-kind-package scanner)))
                           (symbol (scanner-current-token scanner))))))

;;;; THE END ;;;;
