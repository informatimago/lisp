;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               csv.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package reads and writes CSV files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-05-20 <PJB> Corrected csv-parse-record.
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2004-09-06 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CSV.CSV"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM")
  (:export "LOAD-RECORDS" "WRITE-RECORD")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "UNTIL" "WHILE")
  (:documentation
   "
    This package reads and writes CSV files.

    Copyright Pascal J. Bourguignon 2004 - 2005
   
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
    "))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CSV.CSV")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))


;; http://planet.plt-scheme.org/docs/neil/csv.plt/1/0/doc.txt


;; The CSV File Format
;; 
;;     * Each record is one line   ...but A record separator may consist
;;       of a line feed (ASCII/LF=0x0A), or a carriage return and line
;;       feed pair (ASCII/CRLF=0x0D 0x0A).  ...but: fields may contain
;;       embedded line-breaks (see below) so a record may span more than
;;       one line.
;; 
;;     * Fields are separated with commas.  Example John,Doe,120 any
;;       st.,"Anytown, WW",08123
;; 
;;     * Leading and trailing space-characters adjacent to comma field
;;       separators are ignored.  So   John  ,   Doe  ,... resolves to
;;       "John" and "Doe", etc. Space characters can be spaces, or tabs.
;; 
;;     * Fields with embedded commas must be delimited with double-quote
;;       characters.  In the above example. "Anytown, WW" had to be
;;       delimited in double quotes because it had an embedded comma.
;; 
;;     * Fields that contain double quote characters must be surounded by
;;       double-quotes, and the embedded double-quotes must each be
;;       represented by a pair of consecutive double quotes.  So, John
;;       "Da Man" Doe would convert to "John ""Da Man""",Doe, 120 any
;;       st.,...
;; 
;;     * A field that contains embedded line-breaks must be surounded by
;;       double-quotes
;;       So:
;;         Field 1: Conference room 1  
;;         Field 2:
;;           John,
;;           Please bring the M. Mathers file for review  
;;           -J.L.
;;         Field 3: 10/18/2002
;;         ...
;; 
;;       would convert to:
;;         Conference room 1, "John,  
;;         Please bring the M. Mathers file for review  
;;         -J.L.
;;         ",10/18/2002,...
;; 
;;       Note that this is a single CSV record, even though it takes up
;;       more than one line in the CSV file. This works because the line
;;       breaks are embedded inside the double quotes of the field.
;; 
;;     * Fields with leading or trailing spaces must be delimited with
;;       double-quote characters.  So to preserve the leading and
;;       trailing spaces around the last name above: John ,"   Doe   ",...
;; 
;;           o Usage note: Some applications will insist on helping you
;;             by removing leading and trailing spaces from all fields
;;             regardless of whether the CSV used quotes to preserve
;;             them. They may also insist on removing leading zeros from
;;             all fields regardless of whether you need them. One such
;;             application is Excel. :-(
;; 
;;     * Fields may always be delimited with double quotes.
;;       The delimiters will always be discarded.
;; 
;;           o Implementation note: When importing CSV, do not reach down
;;             a layer and try to use the quotes to impart type
;;             information to fields. Also, when exporting CSV, you may
;;             want to be defensive of apps that improperly try to do
;;             this. Though, to be honest, I have not found any examples
;;             of applications that try to do this. If you have
;;             encountered any apps that attempt to use the quotes to
;;             glean type information from CSV files (like assuming
;;             quoted fields are strings even if they are numeric),
;;             please let me know about it.
;; 
;;     * The first record in a CSV file may be a header record containing
;;       column (field) names There is no mechanism for automatically
;;       discerning if the first record is a header row, so in the
;;       general case, this will have to be provided by an outside
;;       process (such as prompting the user). The header row is encoded
;;       just like any other CSV record in accordance with the rules
;;       above. A header row for the multi-line example above, might be:
;;         Location, Notes, "Start Date", ...


;; field-names
;; 
;; " --> ""
;; " --> \"
;; " --> empty
;; 
;; ~% --> NL
;; ~% --> \n
;; ~% --> empty

(defgeneric newline (scanner))
(defgeneric scan-newline (scanner ch))
(defgeneric get-token (scanner))
(defgeneric advance (parser))
(defgeneric report-error (parser message &rest args))
(defgeneric csv-parse-file (self))
(defgeneric csv-parse-record (self))


(defun escape-field (field)
  (setf field (cond
                ((null field) "")
                ((stringp field) field)
                (t  (format nil "~A" field))))
  (if (position (character "\"") field)
      (do ((result (make-string (+ (length field)
                                   (count (character "\"") field))))
           (i 0 (1+ i))
           (j 0))
          ((>= i (length field)) result)
        (setf (char result j) (char field i))
        (incf j)
        (when (char= (character "\"") (char field i))
          (setf (char result j) (char field i))
          (incf j)))
      field))


(defun write-record (fields &optional (out *standard-output*))
  (let ((*print-pretty* nil))
    (format out "~{\"~A\"~^,~}~%" (mapcar (function escape-field) fields))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; text   ::= { any-char-but-coma-new-line-and-dblquote } .
;; quoted-text ::= { any-char-but-coma-new-line-and-dblquote
;;                   | "\"\"" | "," | CR | LF } .
;; coma
;; new-line (cr, lf, cr-lf)


(defparameter +c+cr+      (code-char com.informatimago.common-lisp.cesarum.ecma048:cr))
(defparameter +c+lf+      (code-char com.informatimago.common-lisp.cesarum.ecma048:lf))
(defparameter +newline+   (format nil "~%"))
(defparameter +crlf+      (format nil "~C~C" +c+cr+ +c+lf+))
(defparameter +cr+        (format nil "~C"   +c+cr+))
(defparameter +lf+        (format nil "~C"   +c+lf+))
(defparameter +spaces+    (format nil " ~C" (code-char com.informatimago.common-lisp.cesarum.ecma048:ht)))



(defclass scanner ()
  ((source  :type peek-stream
            :initarg :source :accessor scanner-source)
   (newline :initform nil :type symbol :accessor scanner-newline)
   (crcnt   :initform 0 :type integer
            :accessor scanner-crcnt)
   (lfcnt   :initform 0 :type integer
            :accessor scanner-lfcnt)
   (crlfcnt :initform 0 :type integer
            :accessor scanner-crlfcnt)
   (field-separator
    :initarg :field-separator
    :initform #\,
    :accessor field-separator)
   (decimal-point
    :initarg :decimal-point
    :initform #\.
    :accessor decimal-point))
  (:documentation "A scanner.
If not set  newline is automatically determined from statistics on newlines
found in the source. At the beginning of the file, or when newlines are
inconsistent, a default newline = LF is used. This imports for \ escapes."))


(defun make-scanner (&key
                     (source *standard-input*) 
                     (field-separator #\,)
                     (decimal-point #\.))
  (make-instance 'scanner
    :source (make-instance 'peek-stream :stream source)
    :field-separator field-separator
    :decimal-point decimal-point))


(defmethod print-object ((self scanner) out)
  (format out "#<~A nl=~S cr=~D lf=~D crlf=~D sep=#\\~C dec=#\\~C source=~S>"
          (class-name (class-of self))
          (scanner-newline self)
          (scanner-crcnt self)
          (scanner-lfcnt self)
          (scanner-crlfcnt self)
          (field-separator self)
          (decimal-point self)
          (scanner-source self))
  self)

(defmethod text-term ((scanner scanner))
  (format nil "~A~A" (field-separator scanner) +crlf+))

(defmethod newline ((scanner scanner))
  "
RETURN:  The newline string determined by the heuristic:
         {crlfcnt,lfcnt} << crcnt ==> +cr+
         {crlfcnt,crcnt} << lfcnt ==> +lf+
         {crcnt,lfcnt} << crlfcnt ==> +crlf+
         otherwise                ==> +lf+
"
  (flet ((<< (a b) (< (* 2 a) b))
         (small (a) (<= a 2)))
    (macrolet
        ((trans (nl knl fnl k1 f1 k2 f2 &key warn)
           `(case  (scanner-newline scanner)
              ((nil)
               (setf (scanner-newline scanner) :lf)
               +lf+)
              ((,knl)
               ,(when warn
                      `(warn "Newline is perhaps ~A. (~A=~D, ~A=~D, ~A=~D).~%"
                             ',knl ,knl (,fnl scanner)
                             ,k1 (,f1 scanner) ,k2 (,f2 scanner)))
               ,nl)
              (otherwise
               (warn "Newline changed from ~A to ~A (~A=~D, ~A=~D, ~A=~D).~%"
                     (scanner-newline scanner) ,knl
                     ,knl (,fnl scanner) ,k1 (,f1 scanner) ,k2 (,f2 scanner))
               (setf (scanner-newline scanner) ,knl)
               ,nl))))
      (cond
        ((and (small (scanner-crcnt scanner))
              (small (scanner-lfcnt scanner))
              (small (scanner-crlfcnt scanner)))  +lf+)
        ((and (<< (scanner-crcnt   scanner) (scanner-lfcnt scanner))
              (<< (scanner-crlfcnt scanner) (scanner-lfcnt scanner)))
         (trans +lf+
                :lf   scanner-lfcnt   :cr scanner-crcnt :crlf scanner-crlfcnt))
        ((and (<< (scanner-lfcnt   scanner) (scanner-crcnt scanner))
              (<< (scanner-crlfcnt scanner) (scanner-crcnt scanner)))
         (trans +cr+
                :cr   scanner-crcnt   :lf scanner-lfcnt :crlf scanner-crlfcnt))
        ((and (<< (scanner-lfcnt scanner) (scanner-crlfcnt scanner))
              (<< (scanner-crcnt scanner) (scanner-crlfcnt scanner)))
         (trans +crlf+
                :crlf scanner-crlfcnt :lf scanner-lfcnt :cr   scanner-crcnt))
        ((and (< (scanner-crcnt   scanner) (scanner-lfcnt scanner))
              (< (scanner-crlfcnt scanner) (scanner-lfcnt scanner)))
         (trans +lf+
                :lf   scanner-lfcnt   :cr scanner-crcnt :crlf scanner-crlfcnt
                :warn t))
        ((and (< (scanner-lfcnt   scanner) (scanner-crcnt scanner))
              (< (scanner-crlfcnt scanner) (scanner-crcnt scanner)))
         (trans +cr+
                :cr   scanner-crcnt   :lf scanner-lfcnt :crlf scanner-crlfcnt
                :warn t))
        ((and (< (scanner-lfcnt scanner) (scanner-crlfcnt scanner))
              (< (scanner-crcnt scanner) (scanner-crlfcnt scanner)))
         (trans +crlf+
                :crlf scanner-crlfcnt :lf scanner-lfcnt :cr   scanner-crcnt
                :warn t))
        (t
         (warn "Newline is completely random! (~A=~D, ~A=~D, ~A=~D).~%"
               :lf (scanner-lfcnt scanner)
               :cr (scanner-crcnt scanner)
               :crlf (scanner-crlfcnt scanner))
         +lf+)))))


(defmethod scan-newline ((scanner scanner) (ch character))
  (cond
    ((char= +c+cr+ ch)
     (if (char= +c+lf+ (nextchar (scanner-source scanner)))
         (progn
           (getchar (scanner-source scanner))
           (incf (scanner-crlfcnt scanner)))
         (incf (scanner-crcnt scanner)))
     t)
    ((char= +c+lf+ ch)
     (incf (scanner-lfcnt scanner))
     t)
    (t
     nil)))
 
       
(defmethod get-token ((scanner scanner))
  "
NOTE:  Multiline values are returned as a list of lines.
BUG:   Line termination should be determined once for the whole file.
       '\' cr lf could mean cr, end of line, or newline
"
  (macrolet
      ((getch   () `(getchar  (scanner-source scanner)))
       (nextch  () `(nextchar (scanner-source scanner)))
       (eat-escape
           (ch value)
         `(if (nextch)
              (progn
                (setf ,ch (getch))
                (if (char= +c+cr+ ,ch)
                    (if (eq +crlf+ (newline scanner))
                        (if (char= +c+lf+ (nextch))
                            (progn
                              (vector-push-extend +c+cr+  ,value)
                              (vector-push-extend (getch) ,value))
                            (vector-push-extend +c+cr+ ,value))
                        (vector-push-extend +c+cr+ ,value))
                    (vector-push-extend ,ch ,value)))
              (error "Found a '\\' at end of file."))))
    (let ((ch (getch)))
      (while (and ch (position ch +spaces+) (setf ch (getch))))
      (cond
        ((null ch) (values :eof nil))
        ((char= ch (field-separator scanner))  (values :coma ch))
        ((char= ch (character "\""))
         ;; quoted-text ::= { any-char-but-coma-new-line-and-dblquote
         ;;                   | "\"\"" | "," | CR | LF } .
         (let ((lines '())
               (value (make-array '(16) :fill-pointer 0 :adjustable t 
                                  :element-type 'character)))
           (do* ((ch (getch)    (or eos (getch)))
                 (eos (null ch) (or eos (null ch))))
                (eos
                 (if lines
                     (progn (push value lines)                        
                            (values :quoted-text (nreverse lines)))
                     (values :quoted-text value)))
             (cond
               ((char= ch (character "\""))
                (if (char= ch (nextch))
                    (progn (vector-push-extend ch value)
                           (getch))
                    (setf eos t)))
               ((char= ch (character "\\"))
                (eat-escape ch value))
               ((scan-newline scanner ch)
                (push value lines)
                (setf value (make-array '(16) :fill-pointer 0 :adjustable t 
                                        :element-type 'character)))
               (t (vector-push-extend ch value))))))
        ((scan-newline scanner ch)
         (values :newline (newline scanner)))
        (t
         ;; text   ::= { any-char-but-coma-new-line-and-dblquote } .
         (let ((value (make-array '(16) :fill-pointer 0 :adjustable t 
                                  :element-type 'character)))
           (until (or (null ch) (position ch (text-term scanner)))
             (if (char= ch (character "\\"))
                 (eat-escape ch value)
                 (vector-push-extend ch value))
             (setf ch (getch)))
           (when ch (ungetchar (scanner-source scanner) ch))
           (values :text (string-trim +spaces+ value))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass parser ()
  ((scanner     :accessor parser-scanner     :initform nil :initarg :scanner)
   (token       :accessor parser-token       :initform nil)
   (value       :accessor parser-value       :initform nil)
   (next-token  :accessor parser-next-token  :initform nil)
   (next-value  :accessor parser-next-value  :initform nil))
  (:documentation "A parser."))


(defmethod print-object ((self parser) out)
  (format out "#<~A :scanner ~S :token (~S ~S) :next (~S ~S)>"
          (class-name (class-of self)) (parser-scanner self)
          (parser-token self)      (parser-value self)
          (parser-next-token self) (parser-next-value self))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csv-parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file   ::= { record newline } .
;; record ::= field { "," field } .
;; field  ::= | data .
;; data   ::= text | "\"" quoted-text "\"" .


(defclass csv-parser (parser)
  ((if-end-of-file-in-last-record
    :initarg :if-end-of-file-in-last-record
    :initform :error
    :type (member :error :ignore)
    :accessor if-end-of-file-in-last-record)))


(defmethod csv-parse-file ((self csv-parser))
  ;; file   ::= { record newline } .
  (while (null (parser-token self)) (advance self))
  (let ((records '()))
    (until (eq :eof (parser-token self))
      (let ((record (csv-parse-record self)))
        (if (eq :newline (parser-token self))
            (advance self)
            (if (eq :eof (parser-token self))
                (ecase (if-end-of-file-in-last-record self)
                  ((:error) (report-error
                             self "Last record ends with end-of-file instead of end-of-line."))
                  ((:ignore) (advance self)))
                (report-error
                 self
                 "INTERNAL: csv-parse-record left a token than end-of-line.")))
        (when record (push record records))))
    (nreverse records)))


(defmethod csv-parse-record ((self csv-parser))
  ;; record ::= field { "," field } .
  ;; field  ::= | data .
  ;; data   ::= text | "\"" quoted-text "\"" .
  (let ((fields '()))
    (loop
       :initially (case (parser-token self)
                    ((:coma)
                     (push nil fields))
                    ((:quoted-text :text)
                     (push (parser-value self) fields)
                     (advance self))
                    (otherwise
                     (report-error self "A record should start with a field or a coma, not ~S ~S."
                                   (parser-token self) (parser-value self))))
       :while (eq  (parser-token self) :coma)
       :do (advance self)
       :do (case (parser-token self)
             ((:coma)
              (push nil fields))
             ((:quoted-text :text)
              (push (parser-value self) fields)
              (advance self))
             (otherwise
              (return-from csv-parse-record (nreverse fields))))
       :finally (if (member (parser-token self) '(:quoted-text :text))
                    (report-error self "Missing a coma between two fields ~S and ~S."
                                  (car fields) (parser-value self))
                    (return-from csv-parse-record (nreverse fields))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading CVS file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-records (path &key
                     (external-format :default)
                     (element-type 'character)
                     (if-end-of-file-in-last-record :error)
                     (field-separator #\,)
                     (decimal-point #\.))
  (with-open-file (input path :direction :input
                         :external-format external-format
                         :element-type element-type)
    (csv-parse-file (make-instance 'csv-parser
                        :scanner (make-scanner :source input
                                               :field-separator field-separator
                                               :decimal-point decimal-point)
                        :if-end-of-file-in-last-record if-end-of-file-in-last-record))))

;;;; THE END ;;;;

