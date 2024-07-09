;;;;**************************************************************************
;;;;FILE:               reader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2024-07-09 <PJB> Corrected || bug. Now 5|| is read as a symbol.
;;;;    2015-07-21 <PJB> Synchronized list-all-macro-characters with the one in com.informatimago.tools.
;;;;    2012-05-14 <PJB> Corrected set-syntax-from-char.
;;;;    2011-04-29 <PJB> Added potential-number-p.
;;;;    2009-08-26 <PJB> Corrected bugs reading "||", "( ;comment )" and "#C(123 456)".
;;;;    2007-03-04 <PJB> Extracted from source.lisp
;;;;BUGS
;;;;
;;;;    When we've reached the end of the stream, if we (read stream nil)
;;;;    it goes on an infinite loop.
;;;;
;;;;    (READ-FROM-STRING "#1=(a b . #1#)") gives an error.
;;;;
;;;;    `(,@x) is read as `(, @x) instead of `(,@ x)
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2006 - 2024
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:shadow "READTABLE"
           "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
           "READ" "READ-PRESERVING-WHITESPACE"
           "READ-DELIMITED-LIST"
           "READ-FROM-STRING"
           "READTABLE-CASE" "READTABLEP"
           "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
           "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
           "SET-SYNTAX-FROM-CHAR"
           "WITH-STANDARD-IO-SYNTAX"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
           "*READ-SUPPRESS*" "*READTABLE*")
  (:export "READTABLE"
           "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
           "READ" "READ-PRESERVING-WHITESPACE"
           "READ-DELIMITED-LIST"
           "READ-FROM-STRING"
           "READTABLE-CASE" "READTABLEP"
           "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
           "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
           "SET-SYNTAX-FROM-CHAR"
           "WITH-STANDARD-IO-SYNTAX"
           "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
           "*READ-SUPPRESS*" "*READTABLE*"
           ;; Extensions:
           "READTABLE-SYNTAX-TABLE" "READTABLE-PARSE-TOKEN"
           "SET-INDIRECT-DISPATCH-MACRO-CHARACTER"
           "SET-INDIRECT-MACRO-CHARACTER"
           "LIST-ALL-MACRO-CHARACTERS"
           "SIMPLE-READER-ERROR" "SIMPLE-END-OF-FILE"
           "MISSING-PACKAGE-ERROR" "SYMBOL-IN-MISSING-PACKAGE-ERROR"
           "MISSING-SYMBOL-ERROR" "SYMBOL-MISSING-IN-PACKAGE-ERROR"
           "UNEXPORTED-SYMBOL-ERROR"
           "INTERN-HERE" "RETURN-UNINTERNED"

           "INVALID-SYMBOL-COMPONENT-LIST"
           "INTERNAL-SYMBOL"
           "MISSING-SYMBOL"
           "MISSING-PACKAGE"
           "SYMBOL-FROM-SPLIT-TOKEN"
           "MAKE-SYMBOL-PARSER-FUNCTION"
           "MAKE-TOKEN-PARSER"

           ;; Utilities:
           "POTENTIAL-NUMBER-P")
  (:documentation
   "
This package implements a standard Common Lisp reader.

We implement a Common Lisp Reader to be able to read lisp
sources.  This is a complete standard compliant lisp reader,
with additionnal hooks (token parser).

A READTABLE-PARSE-TOKEN function takes a TOKEN as argument, and
must return two values:
- A boolean indicating whether the it could parse the token,
- a parsed lisp object it could, or an error message (string) if not.

See also the TOKEN functions, CONSTITUENT-TRAIT, SYNTAX-TABLE and
CHARACTER-DESCRIPTION...


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2006 - 2024

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER")



(define-condition simple-reader-error   (simple-error reader-error)
  ()
  (:documentation "A simple reader error condition."))

(define-condition simple-end-of-file    (simple-error end-of-file)
  ()
  (:documentation "A simple end-of-file condition."))

(define-condition missing-package-error (reader-error)
  ((package-name :initarg :package-name))
  (:documentation "The error condition signaled when trying use an inexistant package."))

(define-condition symbol-in-missing-package-error (missing-package-error)
  ((symbol-name :initarg :symbol-name))
  (:documentation "The error condition signaled when trying to read a symbol in an inexistant package.")
  (:report (lambda (condition stream)
             (format stream "Tried to read a symbol named ~S in an inexistant package named ~S."
                     (slot-value condition 'symbol-name)
                     (slot-value condition 'package-name)))))

(define-condition missing-symbol-error (reader-error)
  ((symbol-name :initarg :symbol-name))
  (:documentation "The error condition signaled when trying to read a symbol not exported from a package."))

(define-condition symbol-missing-in-package-error (missing-symbol-error)
  ((package-name :initarg :package-name))
  (:documentation "The error condition signaled when trying to read a symbol not exported from a package.")
  (:report (lambda (condition stream)
             (format stream "Tried to read an inexistant external symbol named ~S the package ~S."
                     (slot-value condition 'symbol-name)
                     (slot-value condition 'package-name)))))

(define-condition unexported-symbol-error (missing-symbol-error)
  ((package-name :initarg :package-name))
  (:documentation "The error condition signaled when trying to read a symbol not exported from a package.")
  (:report (lambda (condition stream)
             (format stream "Tried to read an unexported symbol named ~S the package ~S."
                     (slot-value condition 'symbol-name)
                     (slot-value condition 'package-name)))))

(defun serror (condition stream control-string &rest arguments)
  (error condition
         :stream stream
         :format-control control-string
         :format-arguments arguments))


;; (LET ((*READTABLE* (COPY-READTABLE NIL)))
;;   (SET-DISPATCH-MACRO-CHARACTER
;;    #\# #\. (LAMBDA (&REST ARGS) ARGS)))
;; ;; (setf (readtable-case *readtable*) :preserve)
;; (let ((*readtable* (copy-readtable)))
;;   ;; Quick and dirty disable : --> read three or four tokens
;;   ;; for pack:sym or pack::sym
;;   (set-macro-character #\: (lambda (stream char) #\:) nil)
;;   (SAFE-TEXT-FILE-TO-STRING-LIST path))
;;
;;
;; (defun unnamed-char-p (ch)
;;   (not (null (regexp:match "^U\\([0-9A-F]\\{4\\}\\|[0-9A-F]\\{8\\}\\)$"
;;                            (char-name ch)))))
;;
;;
;; (defun collect-chars (&key (start 0) (end #x11000) name)
;;   (loop
;;      :with table = (make-hash-table :test (function equalp))
;;      :for code :from start :below end
;;      :for char = (code-char code)
;;      :for name = (char-name char)
;;      :do (unless (unnamed-char-p char)
;;            (dolist (word (regexp:regexp-split "[-_]" name))
;;              (push char (gethash word table nil))))
;;      :finally (return table)))



;;----------------------------------------

(defclass character-description ()
  ((syntax   :reader character-syntax
             :initarg :syntax)
   (traits   :reader character-constituent-traits
             :initarg :traits   :initform nil)
   (macro    :reader character-macro
             :initarg :macro    :initform nil
             :documentation "A macro character function.")
   (dispatch :reader character-dispatch
             :initarg :dispatch :initform nil
             :documentation "A HASH-TABLE character -> dmc function."))
  (:documentation
   "
Description of one character.

In the syntax tables, a single character description instance can be
shared by several characters, but with copy-on-write.
"))

;; macro-character-function
;; dispatch-macro --> map character -> dispatch-macro-character-function


(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Character syntaxes:
  (defconstant +cs-invalid+                         0)
  (defconstant +cs-whitespace+                      1)
  (defconstant +cs-single-escape+                   2)
  (defconstant +cs-multiple-escape+                 3)
  (defconstant +cs-constituent+                     4)
  (defconstant +cs-terminating-macro-character+     5)
  (defconstant +cs-non-terminating-macro-character+ 6)


;;; Constituent traits:
  (defconstant +ct-invalid+                        #b00000000000001)
  (defconstant +ct-alphabetic+                     #b00000000000010)
  (defconstant +ct-digit+                          #b00000000000100)
  (defconstant +ct-alphadigit+                     #b00000000000110)
  (defconstant +ct-package-marker+                 #b00000000001000)
  (defconstant +ct-plus-sign+                      #b00000000010000)
  (defconstant +ct-minus-sign+                     #b00000000100000)
  (defconstant +ct-sign+                           #b00000000110000)
  (defconstant +ct-dot+                            #b00000001000000)
  (defconstant +ct-decimal-point+                  #b00000010000000)
  (defconstant +ct-ratio-marker+                   #b00000100000000)
  (defconstant +ct-float-exponent-marker+          #b00001000000000)
  (defconstant +ct-short-float-exponent-marker+    #b00011000000000)
  (defconstant +ct-single-float-exponent-marker+   #b00101000000000)
  (defconstant +ct-double-float-exponent-marker+   #b01001000000000)
  (defconstant +ct-long-float-exponent-marker+     #b10001000000000)
  (defconstant +ct-max+ +ct-long-float-exponent-marker+)
  ) ;;eval-when


(deftype constituent-trait () `(integer 0 ,(expt 2 (integer-length  +ct-max+))))


(declaim (inline traitp))
(defun traitp (trait traits)
  "Returns whether the TRAIT is in the TRAITS 'set'."
  (plusp (logand trait traits)))


;;; The shared character descriptions:

(defparameter *cd-invalid*                (make-instance 'character-description
                                                         :syntax +cs-invalid+
                                                         :traits +ct-invalid+))
(defparameter *cd-whitespace*             (make-instance 'character-description
                                                         :syntax +cs-whitespace+
                                                         :traits +ct-invalid+))
(defparameter *cd-constituent-invalid*    (make-instance 'character-description
                                                         :syntax +cs-whitespace+
                                                         :traits +ct-invalid+))
(defparameter *cd-constituent-alphabetic* (make-instance 'character-description
                                                         :syntax +cs-constituent+
                                                         :traits +ct-alphabetic+))

;; ----------------------------------------

(defclass syntax-table ()
  (standard-characters
   extended-characters
   constituent
   invalid)
  (:documentation
   "
STANDARD-CHARACTERS is a vector of CHARACTER-DESCRIPTION instances
for the standard character codes below +STANDARD-CHARACTERS-LIMIT+.

EXTENDED-CHARACTERS is NIL, or a HASH-TABLE mapping characters to
CHARACTER-DESCRIPTIONS instances for the extended characters with
codes above +STANDARD-CHARACTERS-LIMIT+.

Extended characters without an entry in EXTENDED-CHARACTERS either
have CONSTITUENT or INVALID CHARACTER-DESCRIPTION, depending on whether
they're GRAPHIC-CHAR-P or not.
"))

(defconstant +standard-characters-limit+ 128)


(defmethod initialize-instance :after ((self syntax-table) &key &allow-other-keys)
  (let ((table        (make-array +standard-characters-limit+
                                  :initial-element *cd-invalid*)))
    (setf (aref table (char-code #\Backspace)) *cd-constituent-invalid*
          (aref table (char-code #\Rubout))    *cd-constituent-invalid*
          (aref table (char-code #\Tab))       *cd-whitespace*
          (aref table (char-code #\Newline))   *cd-whitespace*
          (aref table (char-code #\Linefeed))  *cd-whitespace*
          (aref table (char-code #\Page))      *cd-whitespace*
          (aref table (char-code #\Return))    *cd-whitespace*
          (aref table (char-code #\Space))     *cd-whitespace*)
    (loop
      :for chdesc
        :in '((#.+cs-terminating-macro-character+ "\"'(),;`"
               #.+ct-alphabetic+)
              (#.+cs-non-terminating-macro-character+ "#"
               #.+ct-alphabetic+)
              (#.+cs-single-escape+ "\\"
               #.+ct-alphabetic+)
              (#.+cs-multiple-escape+ "|"
               #.+ct-alphabetic+)
              (#.+cs-constituent+ "!$%&*<=>?@[]^_{}~"
               #.+ct-alphabetic+)
              (#.+cs-constituent+ ":"
               #.+ct-package-marker+)
              (#.+cs-constituent+ "+"
               #.+ct-alphabetic+ #.+ct-plus-sign+)
              (#.+cs-constituent+ "-"
               #.+ct-alphabetic+ #.+ct-minus-sign+)
              (#.+cs-constituent+ "."
               #.+ct-alphabetic+ #.+ct-dot+ #.+ct-decimal-point+)
              (#.+cs-constituent+ "/"
               #.+ct-alphabetic+ #.+ct-ratio-marker+)
              (#.+cs-constituent+ "0123456789"
               #.+ct-alphadigit+)
              (#.+cs-constituent+ "Dd"
               #.+ct-alphadigit+ #.+ct-double-float-exponent-marker+)
              (#.+cs-constituent+ "Ee"
               #.+ct-alphadigit+ #.+ct-float-exponent-marker+)
              (#.+cs-constituent+ "Ff"
               #.+ct-alphadigit+ #.+ct-single-float-exponent-marker+)
              (#.+cs-constituent+ "Ll"
               #.+ct-alphadigit+ #.+ct-long-float-exponent-marker+)
              (#.+cs-constituent+ "Ss"
               #.+ct-alphadigit+ #.+ct-short-float-exponent-marker+)
              (#.+cs-constituent+ "ABCGHIJKMNOPQRTUVWXYZabcghijkmnopqrtuvwxyz"
               #.+ct-alphadigit+))
      :do (loop
            :with desc = (make-instance 'character-description
                                        :syntax (first chdesc)
                                        :traits (if (null (cdddr chdesc))
                                                    (third chdesc)
                                                    (apply (function logior)
                                                           (cddr chdesc))))
            :for ch :across (second chdesc)
            :do (setf (aref table (char-code ch)) desc)))
    (setf (slot-value self 'standard-characters) table
          (slot-value self 'extended-characters) nil))
  self)

(defgeneric copy-syntax-table (syntax-table))
(defgeneric character-description (syntax-table character))

(defmethod copy-syntax-table ((self syntax-table))
  (let ((copy (make-instance 'syntax-table)))
    (setf (slot-value copy 'standard-characters)
          (copy-seq (slot-value self 'standard-characters))
          (slot-value copy 'extended-characters)
          (and (slot-value self 'extended-characters)
               (copy-hash-table (slot-value self 'extended-characters))))
    copy))

(defmethod character-description ((self syntax-table) (ch character))
  (let ((code (char-code ch)))
    (if (< code +standard-characters-limit+)
        (aref (slot-value self 'standard-characters) code)
        (or (and (slot-value self 'extended-characters)
                 (gethash code (slot-value self 'extended-characters)))
            (if (graphic-char-p ch)
                *cd-constituent-alphabetic*
                *cd-invalid*)))))

(defgeneric (setf character-description) (val syntax-table character))
(defmethod (setf character-description) (val (self syntax-table) (ch character))
  (let ((code (char-code ch)))
    (if (< code +standard-characters-limit+)
        (setf (aref (slot-value self 'standard-characters) code) val)
        (progn
          (unless (slot-value self 'extended-characters)
            (setf (slot-value self 'extended-characters) (make-hash-table)))
          (setf  (gethash code (slot-value self 'extended-characters)) val)))))

;;----------------------------------------

(defvar *standard-readtable*         nil
  "Only used by SET-SYNTAX-FROM-CHAR")

(defvar *readtable*                  nil
  "
The value of *READTABLE* is called the current readtable. It controls
the parsing behavior of the Lisp reader, and can also influence the
Lisp printer (e.g., see the  function READTABLE-CASE).

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm>
")

(defvar *read-base*                  10
  "
Controls the interpretation of tokens by READ as being integers or
ratios.

The value of *READ-BASE*, called the current input base, is the radix
in which  integers and ratios are to be read by the Lisp reader. The
parsing of other numeric  types (e.g., floats) is not affected by this
option.

The effect of *READ-BASE* on the reading of any particular rational
number can be locally overridden by explicit use of the #O, #X, #B, or
#nR syntax or by a trailing decimal point.

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_bas.htm>
")

(defvar *read-eval*                  t
  "
If it is true, the #. reader macro has its normal effect. Otherwise,
that reader macro signals an error of type reader-error.

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_eva.htm>
")

(defvar *read-suppress*              nil
  "
This variable is intended primarily to support the operation of the
read-time conditional notations #+ and #-. If it is false, the Lisp
reader operates normally.  If the value of *read-suppress* is true,
read, read-preserving-whitespace,  read-delimited-list, and
read-from-string all return a primary value of nil when they complete
successfully.

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_sup.htm>
")

(defvar *read-default-float-format* 'single-float
  "
Controls the floating-point format that is to be used when reading a
floating-point number that has no exponent marker or that has e or E
for an exponent marker. Other  exponent markers explicitly prescribe
the floating-point format to be used.

The printer uses *read-default-float-format* to guide the choice of
exponent markers when printing floating-point numbers.

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_def.htm>
")

;; extensions
(defvar *input-stream* nil
  "
Bound to the input stream, during token parsing.

Consequences are undefined, if any destructive operations are
attempted on this stream.
")

(declaim (ftype (function (t) t) parse-token))

(defgeneric readtable-parse-token (readtable)
  (:documentation "RETURN: The function used to parse a token that has been read."))
(defgeneric (setf readtable-parse-token) (new-function readtable)
  (:documentation "DO:     Set the function used to parse a token that has been read."))
(defgeneric readtable-syntax-table (readtable)
  (:documentation "RETURN: The syntax-table of the readtable."))
(declaim (function make-token-parser))

(defclass readtable ()
  ((case          :initarg :case
                  :initform :upcase
                  :type (member :upcase :downcase :preserve :invert))
   (syntax-table  :accessor readtable-syntax-table
                  :initarg :syntax-table
                  :initform (make-instance 'syntax-table))
   (parse-token   :accessor readtable-parse-token
                  :initarg :parse-token
                  :initform (make-token-parser)))
  (:documentation
   "
A READTABLE maps characters into syntax types for the Lisp reader; see
Section 2 (Syntax). A readtable also contains associations between
macro characters and their  reader macro functions, and records
information about the case conversion rules to be used by the Lisp
reader when parsing symbols.

Each simple character must be representable in the readtable. It is
implementation-defined whether non-simple characters can have syntax
descriptions in the readtable.

URL: <http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm>
"))



(defun copy-readtable (&optional (from-readtable *readtable*) (to-readtable nil))
  "
DO:     Copy the readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_cp_rdt.htm>
"
  (if (null from-readtable)
      (if (null to-readtable)
          (make-instance 'readtable)
          (progn
            (setf (readtable-case to-readtable) :upcase
                  (readtable-syntax-table to-readtable) (make-instance
                                                         'syntax-table)
                  (readtable-parse-token to-readtable)  (function parse-token))
            to-readtable))
      (if (null to-readtable)
          (make-instance 'readtable
                         :case (readtable-case from-readtable)
                         :syntax-table (copy-syntax-table
                                        (readtable-syntax-table from-readtable))
                         :parse-token (readtable-parse-token from-readtable))
          (progn
            (setf (readtable-case to-readtable) (readtable-case from-readtable)
                  (readtable-syntax-table to-readtable) (copy-syntax-table
                                                         (readtable-syntax-table
                                                          from-readtable))
                  (readtable-parse-token to-readtable)  (readtable-parse-token
                                                         from-readtable))
            to-readtable))))

(defun reader-dispatch-macro-error-undefined (stream ch sub-char)
  (serror 'simple-reader-error stream
          "After #\\~A is #\\~A an undefined dispatch macro character"
          ch sub-char))

(defun reader-dispatch-macro-error-invalid (stream sub-char arg)
  (declare (ignore sub-char arg))
  (serror 'simple-reader-error stream
          "objects printed as # in view of *PRINT-LEVEL* cannot be read back in"))


(defun reader-macro-dispatch-function (stream ch)
  (let* ((arg  (loop
                 :for ch = (read-char stream t nil t)
                 :while (digit-char-p ch)
                 :collect ch :into digits
                 :finally (unread-char ch stream)
                          (return (when digits
                                    (parse-integer (coerce digits 'string))))))
         (sub-char (read-char stream t nil t))
         (cd (character-description (readtable-syntax-table *readtable*) ch))
         (fun (gethash (char-upcase sub-char) (character-dispatch cd))))
    (if fun
        (funcall fun stream  arg sub-char)
        (reader-dispatch-macro-error-undefined stream ch sub-char))))



(defgeneric process-case-function (mode)
  (:method ((mode (eql :preserve))) (declare (ignorable mode)) (function identity))
  (:method ((mode (eql :downcase))) (declare (ignorable mode)) (function char-downcase))
  (:method ((mode (eql :upcase)))   (declare (ignorable mode)) (function char-upcase))
  (:method ((mode (eql :invert)))
    (declare (ignorable mode))
    (lambda (ch)
      (cond ((upper-case-p ch) (char-downcase ch))
            ((lower-case-p ch) (char-upcase   ch))
            (t                                ch)))))


;;; For tokens we need to keep track of the characters and their
;;; traits in parallel:

(declaim (inline make-token  token-text token-traits
                 token-length token-char token-char-traits
                 token-collect-character))


(defstruct token
  (characters
   (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character))
  (constituent-traits
   (make-array 8 :adjustable t :fill-pointer 0 :element-type 'constituent-trait))
  ;; We also need to keep track of the use of || in case of empty escaped substring:
  (has-multiple-escape-p
   nil))

(defun token-text        (token)       (token-characters token))
(defun token-traits      (token)       (token-constituent-traits token))
(defun token-length      (token)       (length (token-characters token)))
(defun token-char        (token index) (aref (token-characters         token) index))
(defun token-char-traits (token index) (aref (token-constituent-traits token) index))
(defun token-collect-character (token character traits)
  (vector-push-extend  character (token-characters         token))
  (vector-push-extend  traits    (token-constituent-traits token)))
(defun token-collect-multiple-escape (token)
  (setf (token-has-multiple-escape-p token) t))

(defun token-delimiter-p (character)
  (let ((cs (character-syntax
             (character-description (readtable-syntax-table *readtable*)
                                    character))))
    (or (= cs +cs-whitespace+) (= cs +cs-terminating-macro-character+))))


(defvar *references* nil "Used to implement #= and ##.")


(defun read-token (input-stream eof-error-p eof-value recursive-p
                   preserve-whitespace-p first-char readtable)
  "
DO:             Implements parts of READ and READ-PRESERVING-WHITESPACE.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
READTABLE:      The readtable to use.

RETURN:         tokenp == t    ; a token.  Or
                tokenp == :EOF ; the eof-value.  Or
                tokenp == NIL  ; a list of values read.

BUG:            The handling of readtable-case :invert is wrong.
"
  (macrolet ((unless-eof (place &body body)
               `(cond
                  (,place      ,@body)
                  (eof-error-p (serror 'simple-end-of-file input-stream
                                       "input stream ~S has reached its end"
                                       input-stream))
                  (t       (return-from read-token (values :eof eof-value)))))
             (error-invalid-character (ch)
               `(serror 'simple-reader-error input-stream
                        "invalid character #\\~A" ,ch)))
    (let ((*references* (if recursive-p
                            *references*
                            (make-hash-table))))
      (prog (x y
             (token (make-token))
             (syntax-table (readtable-syntax-table readtable))
             (procase (process-case-function (readtable-case readtable))))
       :begin
         (setf x (or first-char (read-char input-stream nil nil t))
               first-char nil)
         (unless-eof x
                     (let ((cd (character-description syntax-table x)))
                       (ecase (character-syntax cd)
                         ((#.+cs-invalid+)
                          (error-invalid-character x))
                         ((#.+cs-whitespace+)
                          (go :begin))
                         ((#.+cs-single-escape+)
                          (let ((z (read-char input-stream nil nil t)))
                            (unless-eof z
                                        (token-collect-character token z +ct-alphabetic+)))
                          (go :collect-token))
                         ((#.+cs-multiple-escape+)
                          (go :collect-multiple-escape-token))
                         ((#.+cs-constituent+)
                          (token-collect-character token (funcall procase x)
                                                   (character-constituent-traits cd))
                          (go :collect-token))
                         ((#.+cs-terminating-macro-character+
                           #.+cs-non-terminating-macro-character+)
                          ;; If the macro returns no value, the caller will
                          ;; have to call us again, or not: (#-(and)x)
                          (return-from read-token
                            (values nil (multiple-value-list
                                         (funcall (get-macro-character x readtable)
                                                  input-stream x))))))))
       :collect-token
         (setf y (read-char input-stream nil nil t))
         (if y
             (let ((cd (character-description syntax-table y)))
               (ecase (character-syntax cd)
                 ((#.+cs-invalid+)
                  (error-invalid-character y))
                 ((#.+cs-whitespace+)
                  (when preserve-whitespace-p
                    (unread-char y input-stream))
                  (go :parse-token))
                 ((#.+cs-single-escape+)
                  (let ((z (read-char input-stream nil nil t)))
                    (unless-eof z
                                (token-collect-character token z +ct-alphabetic+)))
                  (go :collect-token))
                 ((#.+cs-multiple-escape+)
                  (go :collect-multiple-escape-token))
                 ((#.+cs-constituent+
                   #.+cs-non-terminating-macro-character+)
                  (token-collect-character token (funcall procase y)
                                           (character-constituent-traits cd))
                  (go :collect-token))
                 ((#.+cs-terminating-macro-character+)
                  (unread-char y input-stream)
                  (go :parse-token))))
             (go :parse-token))
       :collect-multiple-escape-token
         (token-collect-multiple-escape token)
         (setf y (read-char input-stream nil nil t))
         (unless-eof y
                     (let ((cd (character-description syntax-table y)))
                       (ecase (character-syntax cd)
                         ((#.+cs-invalid+)
                          (error-invalid-character y))
                         ((#.+cs-single-escape+)
                          (let ((z (read-char input-stream nil nil t)))
                            (unless-eof z
                                        (token-collect-character token z +ct-alphabetic+)))
                          (go :collect-multiple-escape-token))
                         ((#.+cs-multiple-escape+)
                          (go :collect-token))
                         ((#.+cs-whitespace+
                           #.+cs-constituent+
                           #.+cs-non-terminating-macro-character+
                           #.+cs-terminating-macro-character+)
                          (token-collect-character token y +ct-alphabetic+)
                          (go :collect-multiple-escape-token)))))
       :parse-token
         ;; token can be of zero length...
         (return (values t token))))))




;; numeric-token ::= integer | ratio | float
;; integer  ::= [sign] decimal-digit+ decimal-point
;; integer  ::= [sign] digit+
;; ratio    ::= [sign] {digit}+ slash {digit}+
;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+
;; float    ::= [sign] {decimal-digit}+ exponent
;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
;; exponent ::=  exponent-marker [sign] {digit}+
;;
;; consing-dot   ::= dot
;;
;; symbol        ::= symbol-name
;;                 | package-marker symbol-name
;;                 | package-marker package-marker symbol-name
;;                 | package-name package-marker symbol-name
;;                 | package-name package-marker package-marker symbol-name
;;
;; symbol-name   ::= {alphabetic}+
;; package-name  ::= {alphabetic}+



(defmacro defparser (name arguments &body body)
  "Defines a token parser function, which parses its argument token and returns
three values: a ok flag; a type of value; and a value parsed from the token.
When the ok flag is false, the type indicates whether it's a strong error,
and the value returned is an error message.
A strong error is a lexical error that is not ambiguous.  A weak error is
when the token could still be of another lexical category.
In the body of the parser, there are macrolet defined to REJECT or ACCEPT
the token, and to describe the parsed syntax with ALT, ZERO-OR-MORE,
ONE-OR-MORE and OPT-SIGN."
  (multiple-value-bind (docu decl body) (parse-body :lambda body)
    `(defun ,name ,arguments
       ,@docu
       ,@decl
       (macrolet ((reject (strongp &rest ctrlstring-and-args)
                    `(return-from ,',name
                       (values nil ,strongp
                               ,(when ctrlstring-and-args
                                  `(format nil ,@ctrlstring-and-args)))))
                  (accept (type token)
                    `(return-from ,',name (values t ,type ,token)))
                  (alt (&rest clauses)
                    `(cond ,@clauses))
                  (zero-or-more (test &body body)
                    `(loop :while ,test :do ,@body))
                  (one-or-more  (test &body body)
                    `(progn
                       (if ,test (progn ,@body) (reject nil))
                       (loop :while ,test :do ,@body)))
                  (opt-sign (sign token i)
                    `(alt ((>= ,i (token-length ,token)))
                          ((traitp +ct-plus-sign+  (token-char-traits ,token ,i))
                           (setf ,sign +1 ,i (1+ ,i)))
                          ((traitp +ct-minus-sign+ (token-char-traits ,token ,i))
                           (setf ,sign -1 ,i (1+ ,i))))))
         ,@body))))


(defparser parse-decimal-integer-token (token)
  "integer ::= [sign] decimal-digit+ decimal-point"
  (let ((sign 1)
        (mant 0)
        (i 0))
    (when (token-has-multiple-escape-p token)
      (reject nil))
    (unless (< i (token-length token)) (reject nil))
    (unless (traitp +ct-decimal-point+
                    (token-char-traits token (1- (token-length token))))
      (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf mant (+ (* 10. mant) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (if (and (= (1+ i) (token-length token))
             (traitp +ct-decimal-point+ (token-char-traits token i)))
        (accept 'integer (* sign mant))
        (reject t
                (if (= (1+ i) (token-length token))
                    "Missing decimal point in decimal integer ~S"
                    "Junk after decimal point in decimal integer ~S")
                (token-text token)))))


(defparser parse-integer-token (token)
  "integer ::= [sign] digit+"
  (let ((sign 1)
        (mant 0)
        (i 0))
    (when (token-has-multiple-escape-p token)
      (reject nil))
    (unless (< i (token-length token)) (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf mant (+ (* *read-base* mant)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (if (= i (token-length token))
        (accept 'integer (* sign mant))
        (reject t "Junk after integer ~S" (token-text token)))))

(defun %parse-integer (string &key (start 0) (end nil) (radix 10.) (junk-allowed nil)
                       &aux (end (or end (length string))))
  (loop
    :named parse
    :with sign = (case (aref string start)
                   (#\+ (incf start) +1)
                   (#\- (incf start) -1)
                   (otherwise        +1))
    :with mant = 0
    :for i :from start :below end
    :do (let ((digit (digit-char-p (aref string i) radix)))
          (cond
            (digit         (setf mant (+ (* mant radix) digit)))
            (junk-allowed  (return-from parse (values (* sign mant) i)))
            (t             (error 'parse-error))) ())
    :finally (return-from parse (values (* sign mant) i))))


(defparser parse-ratio-token (token)
  "ratio ::= [sign] {digit}+ slash {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 0)
        (i 0))
    (when (token-has-multiple-escape-p token)
      (reject nil))
    (unless (< i (token-length token)) (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf nume (+ (* *read-base* nume)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (if (traitp +ct-ratio-marker+ (token-char-traits token i))
        (incf i)
        (reject nil))
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i) *read-base*))
                 (setf denu (+ (* *read-base* denu)
                               (digit-char-p (token-char token i) *read-base*))
                       i (1+ i)))
    (cond
      ((< i (token-length token))
       (reject t "Junk after ratio ~S" (token-text token)))
      #|| ((zerop denu) (reject t "Zero denominator ratio ~S" (token-text token))) ||#
      (t
       (accept 'ratio (/ (* sign nume) denu))))))


(defparser parse-float-1-token (token)
  "float ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
exponent ::=  exponent-marker [sign] {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 1)
        (type *read-default-float-format*)
        (esgn 1)
        (expo 0)
        (i 0))
    (when (token-has-multiple-escape-p token)
      (reject nil))
    (opt-sign sign token i)
    (zero-or-more (and (< i (token-length token))
                       (traitp +ct-digit+ (token-char-traits token i))
                       (digit-char-p (token-char token i)))
                  (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                        i (1+ i)))
    (if (and (< i (token-length token))
             (traitp +ct-decimal-point+ (token-char-traits token i)))
        (incf i)
        (reject nil))
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                       denu (* 10. denu)
                       i (1+ i)))
    (when (and (< i (token-length token))
               (traitp +ct-float-exponent-marker+ (token-char-traits token i)))
      (cond
        ((traitp +ct-short-float-exponent-marker+ (token-char-traits token i))
         (setf type 'short-float))
        ((traitp +ct-single-float-exponent-marker+ (token-char-traits token i))
         (setf type 'single-float))
        ((traitp +ct-double-float-exponent-marker+ (token-char-traits token i))
         (setf type 'double-float))
        ((traitp +ct-long-float-exponent-marker+ (token-char-traits token i))
         (setf type 'long-float)))
      (incf i)
      (opt-sign esgn token i)
      (one-or-more (and (< i (token-length token))
                        (traitp +ct-digit+ (token-char-traits token i))
                        (digit-char-p (token-char token i)))
                   (setf expo (+ (* 10. expo) (digit-char-p (token-char token i)))
                         i (1+ i))))
    (if (= i (token-length token))
        (accept type
                (* (coerce (/ (* sign nume) denu) type)
                   (expt (coerce 10. type) (* esgn expo))))
        (reject t "Junk after floating point number ~S" (token-text token)))))


(defparser parse-float-2-token (token)
  "float ::= [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
exponent ::=  exponent-marker [sign] {digit}+"
  (let ((sign 1)
        (nume 0)
        (denu 1)
        (type *read-default-float-format*)
        (esgn 1)
        (expo 0)
        (i 0))
    (when (token-has-multiple-escape-p token)
      (reject nil))
    (opt-sign sign token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (when (and (< i (token-length token))
               (traitp +ct-decimal-point+ (token-char-traits token i)))
      (incf i)
      (one-or-more (and (< i (token-length token))
                        (traitp +ct-digit+ (token-char-traits token i))
                        (digit-char-p (token-char token i)))
                   (setf nume (+ (* 10. nume) (digit-char-p (token-char token i)))
                         denu (* 10. denu)
                         i (1+ i))))
    (unless (and (< i (token-length token))
                 (traitp +ct-float-exponent-marker+ (token-char-traits token i)))
      (reject nil))
    (cond
      ((traitp +ct-short-float-exponent-marker+ (token-char-traits token i))
       (setf type 'short-float))
      ((traitp +ct-single-float-exponent-marker+ (token-char-traits token i))
       (setf type 'single-float))
      ((traitp +ct-double-float-exponent-marker+ (token-char-traits token i))
       (setf type 'double-float))
      ((traitp +ct-long-float-exponent-marker+ (token-char-traits token i))
       (setf type 'long-float)))
    (incf i)
    (opt-sign esgn token i)
    (one-or-more (and (< i (token-length token))
                      (traitp +ct-digit+ (token-char-traits token i))
                      (digit-char-p (token-char token i)))
                 (setf expo (+ (* 10. expo) (digit-char-p (token-char token i)))
                       i (1+ i)))
    (if (= i (token-length token))
        (accept type
                (* (coerce (/ (* sign nume) denu) type)
                   (expt (coerce 10. type) (* esgn expo))))
        (reject t "Junk after floating point number ~S" (token-text token)))))


;; (defparser parse-consing-dot-token (token)
;;   "consing-dot ::= dot"
;;   (if (and (= 1 (token-length token))
;;            (traitp +ct-dot+ (token-char-traits token 0)))
;;       (accept 'consing-dot ".")
;;       (reject nil)))


(defun invalid-symbol-component-list (components)
   "
COMPONENTS:  Parsed symbol components.
DO:          Handles invalid components lists.
"
  (error "Invalid symbol component list ~S" components))

(defun internal-symbol (package sname sym)
  "
We tried to read PNAME:SNAME but the symbol is not exported.

PACKAGE:    the package specified for the symbol.

SNAME:      symbol name.

DO:         Handles the internal symbol error with restarts to export
            it or return it unexported.

NOTE:       We could also find symbols with the same name in other
            packages.

"
  (restart-case (error 'unexported-symbol-error
                       :stream *input-stream*
                       :package-name (package-name package)
                       :symbol-name sname)
    (export (&rest rest)
      :report "Export the unexported symbol."
      (declare (ignore rest))
      (export sym package)
      sym)
    (return-unexported (&rest rest)
      :report "Return the symbol unexported."
      (declare (ignore rest))
      sym)))

(defun missing-symbol (package sname)
  "
We tried to read PNAME:SNAME but no symbol found with this name.

PACKAGE:    the package specified for the symbol.

SNAME:      symbol name.

DO:         Handles the symbol missing error with restarts to intern
            in the package (and possibly export it)
            or return an uninterned symbol.

NOTE:       We could also find symbols with the same name in other
            packages.

"
  (restart-case (error 'symbol-missing-in-package-error
                       :stream *input-stream*
                       :package-name (package-name package)
                       :symbol-name sname)
    (intern-and-export (&rest rest)
      :report "Intern the missing symbol and export it"
      (declare (ignore rest))
      (let ((sym (intern sname package)))
        (export (list sym) package)
        sym))
    (intern (&rest rest)
      :report "Intern the missing symbol without exporting it"
      (declare (ignore rest))
      (intern sname package))
    (return-uninterned (&rest rest)
      :report "Return an uninterned symbol."
      (declare (ignore rest))
      (make-symbol sname))))

(defun missing-package (pname sname)
  "
We tried to read PNAME:SNAME, but there's no package named PNAME.

PNAME:  package name

SNAME:  symbol name

DO:     Handles the missing package error with restarts to intern in
        the current package or return an uninterned symbol.

NOTE:   We could also find other packages with a similar name to
        correct typoes.

"
  (restart-case (error 'symbol-in-missing-package-error
                       :stream *input-stream*
                       :package-name pname
                       :symbol-name sname)
    #|TODO: add a restart to create the missing package.|#
    (intern-here (&rest rest)
      :report "Intern the symbol in the current package instead."
      (declare (ignore rest))
      (intern sname))
    (return-uninterned (&rest rest)
      :report "Return an uninterned symbol."
      (declare (ignore rest))
      (make-symbol sname))))

(defun symbol-from-split-token (components)
  "

COMPONENTS:  a list of strings separated by integers specifying the
             number of colons.

EXAMPLES:    X         (\"X\")
             :Y        (1 \"Y\")
             X:Y       (\"X\" 1 \"Y\")
             X::Y      (\"X\" 2 \"Y\")
             X:::Y     (\"X\" 3 \"Y\")
             X::       (\"X\" 2)
             X:Y:Z     (\"X\" 1 \"Y\" 1 \"Z\")

RETURN:      A symbol designated by the components,
             or signal an error.

NOTE:        This function implements the standard semantics,
             where only one occurence of : or :: is allowed,
             and depending on : or ::, an exported symbol is expected
             or not.

"
  (values
   (case (length components)
     (1
      (if (stringp (first components))
          (intern (first components) *package*)
          (invalid-symbol-component-list components)))
     (2 (case (first components)
          ((1 2)
           (intern (second components)
                   (load-time-value (find-package "KEYWORD"))))
          (otherwise
           (invalid-symbol-component-list components))))
     (3 (destructuring-bind (pname colons sname) components
          (assert (stringp pname) (pname) "Symbol component was expected to be a string.")
          (assert (stringp sname) (sname) "Symbol component was expected to be a string.")
          (let ((package (find-package pname))) ; *** this is the critical call for relative packages.
            (if package
                (case colons
                  (1 (multiple-value-bind (sym where) (find-symbol sname package)
                       (case where
                         ((nil)       (missing-symbol  package sname))
                         ((:internal) (internal-symbol package sname sym))
                         ((:external) sym))))
                  (2 (intern sname package))
                  (otherwise
                   (invalid-symbol-component-list components)))
                (missing-package pname sname)))))
     (otherwise
      (invalid-symbol-component-list components)))))

(defun make-symbol-parser-function (symbol-internalize-function)
  (lambda (token)
    (flet ((package-marker-p (traits) (traitp +ct-package-marker+ traits)))
      (handler-case
          (let ((symbol (funcall symbol-internalize-function
                                 (loop
                                   :with components := '()
                                   :for start := 0 :then after-colons
                                   :for colon := (position-if (function package-marker-p) (token-traits token)
                                                              :start start)
                                   :for after-colons := (when colon
                                                          (position-if-not (function package-marker-p) (token-traits token)
                                                                           :start (1+ colon)))
                                   :while colon
                                   :do (when (plusp colon)
                                         (push (subseq (token-text token) start colon) components))
                                       (push (- after-colons colon) components)
                                   :finally (push (subseq (token-text token) start colon) components)
                                            (return (nreverse components))))))
            (values t 'symbol symbol))
        (error (err)
          (values nil t (princ-to-string err)))))))

(defun make-token-parser (&key
                            (parse-decimal-integer-token (function parse-decimal-integer-token))
                            (parse-integer-token         (function parse-integer-token))
                            (parse-ratio-token           (function parse-ratio-token))
                            (parse-float-1-token         (function parse-float-1-token))
                            (parse-float-2-token         (function parse-float-2-token))
                            (parse-symbol-token          (make-symbol-parser-function (function symbol-from-split-token))))
  (lambda (token)
    "RETURN:  okp ; the parsed lisp object if okp, or an error message if (not okp)"
    (let ((message nil))
      (macrolet
          ((rom (&body body)
             "Result Or Message"
             (if (null body)
                 'nil
                 (let ((vals (gensym)))
                   `(let ((,vals (multiple-value-list ,(car body))))
                      ;; (format *trace-output* "~S --> ~S~%" ',(car body) ,vals)
                      (if (first ,vals)
                          (values-list ,vals)
                          (progn
                            (when (second ,vals)
                              (setf message  (third ,vals)))
                            (rom ,@(cdr body)))))))))
        (multiple-value-bind (ok type object)
            (rom (funcall parse-decimal-integer-token token)
                 (funcall parse-integer-token         token)
                 (funcall parse-ratio-token           token)
                 (funcall parse-float-1-token         token)
                 (funcall parse-float-2-token         token)
                 ;; (parse-consing-dot-token     token)
                 (funcall parse-symbol-token          token))
          (declare (ignorable type))
          ;; (format *trace-output* "ok = ~S ; type = ~S ; object = ~S~%"
          ;;         ok type object)
          (values ok (if ok object message)))))))


(defun all-dots-p (token)
  "
RETURN: Whether the token is all dots, (excluding escaped dots).
"
  (and (not (token-has-multiple-escape-p token))
       (plusp (length (token-text token)))
       (every (lambda (traits) (traitp +ct-dot+ traits)) (token-traits token))))


(defun read-0/1 (input-stream eof-error-p eof-value recursive-p
                 preserve-whitespace-p first-char allowed-all-dots)
  "
DO:             Read zero or one token.  Use the *READTABLE*.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
ALLOWED-ALL-DOTS:
                May be T in which case tokens containing only dots are allowed,
                or a (possibly empty) list of strings containing only dots that are
                explicitely allowed (others rejected). Typically (\".\").

RETURN:         tokenp == t    ; an unparsed (alldots) token.  Or
                tokenp == :EOF ; the eof-value.  Or
                tokenp == NIL  ; a list of values read.
"
  (multiple-value-bind (tokenp token)
      (read-token input-stream eof-error-p eof-value recursive-p
                  preserve-whitespace-p first-char *readtable*)
    (if (eq 't tokenp)
        (cond
          (*read-suppress*
           (values nil (list nil)))
          ((or (eq 't allowed-all-dots)
               (not (all-dots-p token))) ; We got a token, let's parse it.
           (values nil (list
                        (multiple-value-bind (okp object)
                            (let ((*input-stream* input-stream))
                              (funcall (readtable-parse-token *readtable*) token))
                          (if okp
                              object
                              (serror 'simple-reader-error input-stream
                                      "~A" object))))))
          ((member (token-text token) allowed-all-dots :test (function string=))
           (values t token))
          (t
           (serror 'simple-reader-error input-stream
                   "a token consisting only of dots cannot be ~
                   meaningfully read in")))
        (values tokenp token))))




(defun read-1 (input-stream eof-error-p eof-value
               recursive-p preserve-whitespace-p first-char allowed-all-dots)
  "
DO:             Read exactly one token.  Use the *READTABLE*.

INPUT-STREAM:   The stream that is read.
EOF-ERROR-P:    Whether we should signal an END-OF-FILE error upon EOF.
EOF-VALUE:      Unless EOF-ERROR-P, the value to be returned in case of EOF.
RECURSIVE-P:    Whether the read is recursive.
                The *reference* table is reset only when RECURSIVE-P is false.
PRESERVE-WHITESPACE-P:
                Whether a terminating whitespace will be unread or not.
FIRST-CHAR:     NIL or a CHARACTER that is used first, before reading the stream.
                This should be faster than UNREAD-CHAR it, and foremost, it allows
                for two unread character, this FIRST-CHAR plus an actual UNREAD-CHAR one.
ALLOWED-ALL-DOTS:
                May be T in which case tokens containing only dots are allowed,
                or a (possibly empty) list of strings containing only dots that are
                explicitely allowed (others rejected). Typically (\".\").

RETURN:         The token read, or
                when EOF-ERROR-P is false and EOF occurs, EOF-VALUE.
" (loop
    :for (tokenp token) = (multiple-value-list
                           (read-0/1 input-stream eof-error-p eof-value
                                     recursive-p preserve-whitespace-p
                                     first-char allowed-all-dots))
    :until (or (eq :eof tokenp) token)
    :finally (return (if (eq :eof tokenp)
                         token
                         (first token)))))


(defun read (&optional input-stream
               (eof-error-p t) (eof-value nil)
               (recursive-p nil))
  "
RETURN: An object read.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm>
"
  (read-1 input-stream eof-error-p eof-value recursive-p  nil  nil '()))


(defun read-preserving-whitespace (&optional input-stream
                                     (eof-error-p t) (eof-value nil)
                                     (recursive-p nil))
  "
RETURN: An object read.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm>
"
  (read-1 input-stream eof-error-p eof-value recursive-p  t    nil '()))


(defun read-delimited-list (char &optional (input-stream *standard-input*)
                                   (recursive-p nil))
  "
RETURN: A list of objects read.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_del.htm>
"
  (loop
    :with result = '()
    :for peek = (peek-char t input-stream nil input-stream recursive-p)
    :do (cond
          ((eql peek input-stream)
           (serror 'simple-end-of-file input-stream
                   "input stream ~S has reached its end" input-stream))
          ((char= peek char)
           (read-char input-stream nil nil recursive-p)
           (return-from read-delimited-list (nreverse result)))
          (t
           (multiple-value-bind (kind tokens)
               (read-0/1 input-stream t nil recursive-p nil nil '())
             (declare (ignore kind))
             (when tokens
               (push (first tokens) result)))))))


(defun read-from-string (string &optional (eof-error-p t) (eof-value nil)
                         &key (start 0) (end nil) (preserve-whitespace nil))
  "
RETURN: An object read from the string.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm>
"
  (let ((index 0))
    (values
     (with-input-from-string (input string :index index :start start :end end)
       (funcall (if preserve-whitespace
                    (function read-preserving-whitespace)
                    (function read))
                input eof-error-p eof-value))
     index)))


(defun readtable-case (readtable)
  "
RETURN: The case of the readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm>
"
  (slot-value readtable 'case))

(defun (setf readtable-case) (value readtable)
  "
DO:     Set the case of the readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm>
"
  (check-type value (member :upcase :downcase :preserve :invert))
  (setf (slot-value readtable 'case) value))


(defun readtablep (object)
  "
RETURN: Whether the object is a readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_rdta_1.htm>
"
  (typep object 'readtable))


(defun make-dispatch-macro-character
    (char &optional (non-terminating-p nil) (readtable *readtable*))
  "
DO:     Make the character a dispatch macro character in the readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_dis.htm>
"
  (let ((rst  (readtable-syntax-table readtable)))
    (setf (character-description rst char)
          (make-instance 'character-description
                         :syntax (if non-terminating-p
                                     +cs-non-terminating-macro-character+
                                     +cs-terminating-macro-character+)
                         :traits (character-constituent-traits
                                  (character-description rst char))
                         :macro (function reader-macro-dispatch-function)
                         :dispatch (make-hash-table)))))


(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (readtable *readtable*))
  "
RETURN: The dispatch macro character function.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_set__1.htm>
"
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst disp-char)))
    (unless (character-dispatch cd)
      (error "~S is not  a dispatch macro character" disp-char))
    (and (character-dispatch cd)
         (gethash (char-upcase sub-char) (character-dispatch cd)))))


(defun set-dispatch-macro-character (disp-char sub-char new-function
                                     &optional (readtable *readtable*))
  "
DO:     Set the dispatch macro character function.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_set__1.htm>
"
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst disp-char)))
    (unless (character-dispatch cd)
      (error "~S is not  a dispatch macro character" disp-char))
    (setf (gethash (char-upcase sub-char) (character-dispatch cd))
          new-function))
  t)


(defun get-macro-character (char &optional (readtable *readtable*))
  "
RETURN: The macro character function.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_set_ma.htm>
"
  (let* ((rst  (readtable-syntax-table readtable))
         (cd   (character-description rst char)))
    (values (character-macro cd)
            (= (character-syntax cd) +cs-non-terminating-macro-character+))))

(defun set-macro-character (char new-function &optional (non-terminating-p nil)
                                                (readtable *readtable*))
  "
DO:     Set then macro character function.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_set_ma.htm>
"
  (let* ((rst  (readtable-syntax-table readtable)))
    (setf (character-description rst char)
          (make-instance 'character-description
                         :syntax (if non-terminating-p
                                     +cs-non-terminating-macro-character+
                                     +cs-terminating-macro-character+)
                         :traits (character-constituent-traits
                                  (character-description rst char))
                         :macro new-function)))
  t)


(defun set-indirect-dispatch-macro-character (disp-char sub-char function-name
                                              &optional (readtable *readtable*))
  "Like set-dispatch-macro-character, but with an indirect function,
to enable TRACE and redefinitions of the dispatch macro character function."
  (set-dispatch-macro-character
   disp-char sub-char
   #+mocl (lambda (s c a) (funcall function-name s c a))
   #-mocl (compile nil
                   (let ((s (gensym)) (c (gensym)) (a (gensym)))
                     `(lambda (,s ,c ,a) (,function-name ,s ,c ,a))))
   readtable))

(defun set-indirect-macro-character (char function-name
                                     &optional (readtable *readtable*))
  "Like set-macro-character, but with an indirect function,
to enable TRACE and redefinitions of the macro character function."
  (set-macro-character
   char
   #+mocl (lambda (s c) (funcall function-name s c))
   #-mocl (compile nil
                   (let ((s (gensym)) (a (gensym)))
                     `(lambda (,s ,a) (,function-name ,s ,a))))
   readtable))



;; Copied from com.informatimago.common-lisp.cesarum.utility to avoid package use loop.
(defun copy-hash-table (table)
  "
TABLE:  (OR NULL HASH-TABLE)
RETURN: If TABLE is NIL, then NIL,
        else a new HASH-TABLE with the same TEST, SIZE, REHASH-THRESHOLD
        REHASH-SIZE and KEY->VALUE associations than TABLE.
        (Neither the keys nor the values are copied).
"
  (check-type table (or null hash-table))
  (when table
    (let ((copy (make-hash-table
                 :test             (hash-table-test             table)
                 :size             (hash-table-size             table)
                 :rehash-threshold (hash-table-rehash-threshold table)
                 :rehash-size      (hash-table-rehash-size      table))))
      (maphash (lambda (k v) (setf (gethash k copy) v)) table)
      copy)))


(defun set-syntax-from-char (to-char from-char
                             &optional (to-readtable *readtable*)
                               (from-readtable *standard-readtable*))
  "
DO:     Copy the syntax between characters in the readtable.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_set_sy.htm>
"
  (let* ((frst  (readtable-syntax-table from-readtable))
         (trst  (readtable-syntax-table   to-readtable))
         (fcd   (character-description frst from-char))
         (tcd   (character-description trst   to-char)))
    (setf (character-description trst to-char)
          (make-instance 'character-description
                         :syntax   (character-syntax fcd)
                         ;; constituent traits are not copied.
                         :traits   (character-constituent-traits tcd)
                         ;; macros are copied only if from is a macro character.
                         :macro    (or (character-macro fcd) (character-macro tcd))
                         :dispatch (if (character-dispatch fcd)
                                       (copy-hash-table (character-dispatch fcd))
                                       (character-dispatch tcd)))))
  t)


;;;----------------------------------------
;;; STANDARD READER MACRO FUNCTIONS
;;;----------------------------------------

(defun reader-macro-line-comment (stream ch)
  "Standard ; macro reader."
  (declare (ignore ch))
  (read-line stream nil)
  (values))

(defun reader-macro-string (stream delim)
  "Standard \" macro reader."
  (flet ((error-eof ()
           (serror 'simple-end-of-file stream
                   "input stream ~S ends within a string" stream)))
    (loop
      :with rst    = (readtable-syntax-table *readtable*)
      :with string = (make-array 64 :element-type 'character
                                    :adjustable t :fill-pointer 0)
      :for ch      = (read-char stream nil nil t)
      :do (cond
            ((null ch)
             (error-eof))
            ((eql ch delim)
             (return-from reader-macro-string (copy-seq string)))
            ((= (character-syntax (character-description rst ch))
                +cs-single-escape+)
             (let ((next (read-char stream nil nil)))
               (when (null next)
                 (error-eof))
               (vector-push-extend next string)))
            (t (vector-push-extend ch   string))))))


(defun reader-macro-quote (stream ch)
  "Standard ' macro reader."
  (declare (ignore ch))
  `(quote ,(read stream t nil t)))


(defun reader-macro-backquote (stream ch)
  "Standard ` macro reader."
  (declare (ignore ch))
  `(backquote ,(read stream t nil t)))


(defun reader-macro-comma (stream ch)
  "Standard , macro reader."
  (declare (ignore ch))
  `(,(if (char= #\@ (peek-char nil stream t nil t)) 'splice 'unquote)
    ,(read stream t nil t)))


(defun reader-macro-left-parenthesis (stream ch)
  "Standard ( macro reader."
  (declare (ignore ch))
  (loop
    :with result     = (cons nil nil)
    :with last-cons  = result
    :with last-cdr-p = nil
    :for ch = (progn (peek-char t stream nil t) (read-char stream t nil t))
    ;; :do (print `(:result ,result :last-cons ,last-cons
    ;;                      :last-cdr-p ,last-cdr-p :ch ,ch))
    :do (flet ((read-and-nconc (ch)
                 (let ((objects (nth-value 1 (read-0/1 stream t nil t nil ch '()))))
                   (when objects
                     (case last-cdr-p
                       ((nil)     (setf (cdr last-cons) objects
                                        ;; (list (first objects))
                                        last-cons       (cdr last-cons)))
                       ((t)       (setf (cdr last-cons) (first objects)
                                        last-cdr-p      :done))
                       (otherwise (serror 'simple-reader-error stream
                                          "illegal end of dotted list")))))))
          (cond
            ((char= #\) ch)
             #-mocl (loop-finish)
             #+mocl (if (eq last-cdr-p 't)
                        (serror 'simple-reader-error stream
                                "illegal end of dotted list")
                        (return (cdr result))))
            ((char= #\. ch)
             (if (token-delimiter-p (peek-char nil stream t nil t))
                 (if (eq result last-cons)
                     (serror 'simple-reader-error stream
                             "missing an object before the \".\" in a cons cell")
                     (case last-cdr-p
                       ((nil)     (setf last-cdr-p t))
                       ((t)       (serror 'simple-reader-error stream
                                          "token \".\" not allowed here"))
                       (otherwise (serror 'simple-reader-error stream
                                          "illegal end of dotted list"))))
                 (read-and-nconc ch)))
            (t
             (read-and-nconc ch))
            ))
    :finally (if (eq last-cdr-p 't)
                 (serror 'simple-reader-error stream
                         "illegal end of dotted list")
                 (return (cdr result)))))


(defun reader-macro-error-start (stream ch)
  (serror 'simple-reader-error stream
          "an object cannot start with ~C" ch))

;;;----------------------------------------
;;; STANDARD READER DISPATCH MACRO FUNCTIONS
;;;----------------------------------------

(defun reader-dispatch-macro-label-reference   (stream arg sub-char)
  "Standard ## dispatch macro reader."
  (declare (ignore sub-char))
  (unless *read-suppress*
    (when (null arg)
      (serror 'simple-reader-error stream
              "a number must be given between # and #"))
    (multiple-value-bind (object presentp) (gethash arg *references*)
      (if presentp
          object
          (serror 'simple-reader-error stream "undefined label #~D#" arg)))))


(defun reader-dispatch-macro-label-definition  (stream arg sub-char)
  "Standard #= dispatch macro reader."
  (declare (ignore sub-char))
  (unless *read-suppress*
    (when (null arg)
      (serror 'simple-reader-error stream
              "a number must be given between # and ="))
    (multiple-value-bind (object presentp) (gethash arg *references*)
      (if presentp
          (serror 'simple-reader-error stream
                  "label #~D=~S already defined as ~S"
                  (read stream t nil t) arg object)
          (setf (gethash arg *references*) (read stream t nil t))))))


(defun eval-feature (expression stream)
  "Evaluates a feature expression as a BOOLEAN."
  (flet ((illegal-feature ()
           (serror 'simple-reader-error stream "illegal feature ~S" expression))
         (eval-term (term)
           (eval-feature term stream)))
    (cond
      ;; Some implementations accept any atom:
      ((atom    expression) (not (null (member expression *features*))))
      (t (case (first expression)
           ((:not) (if (cddr expression)
                       (illegal-feature)
                       (not (eval-feature (second expression) stream))))
           ((:and) (every (function eval-term) (rest expression)))
           ((:or)  (some  (function eval-term) (rest expression)))
           (t      (illegal-feature)))))))


(defun read-feature (stream affirmativep)
  "Reads a feature expression, and possibly eats one following sexp"
  (let ((expression (let ((*package*  (find-package "KEYWORD"))
                          (*read-suppress* nil))
                      (read stream nil stream t))))
    ;; (print `(:read-feature ,expression))
    (when (eq expression stream)
      (serror 'simple-end-of-file stream
              "EOF in ~S while reading the feature expression" stream))
    (unless (funcall (if affirmativep
                         (function identity)
                         (function not))
                     (eval-feature expression stream))
      ;; (print `(:read-feature ,expression false we eat))
      (let ((*read-suppress* t))
        ;; (print `(:read-feature ,(read stream t nil nil) :eaten))
        (read stream t nil nil)))
    (values)))


(defun reader-dispatch-macro-feature           (stream arg sub-char)
  "Standard #+ dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-feature stream t))


(defun reader-dispatch-macro-not-feature       (stream arg sub-char)
  "Standard #- dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-feature stream nil))


;; (defparameter *rt*
;;   (let ((rt (copy-readtable)))
;;     (set-dispatch-macro-character
;;      #\# #\+ (function reader-dispatch-macro-feature) rt)
;;     (set-dispatch-macro-character
;;      #\# #\- (function reader-dispatch-macro-not-feature) rt)
;;     rt))


(defun reader-dispatch-macro-read-eval         (stream arg sub-char)
  "Standard #. dispatch macro reader."
  (declare (ignore sub-char arg))
  (if *read-eval*
      (eval (read stream t nil t))
      (serror 'simple-reader-error stream
              "*READ-EVAL* = NIL does not allow the evaluation of ~S"
              (read stream t nil t))))


(defun reader-dispatch-macro-uninterned        (stream arg sub-char)
  "Standard #: dispatch macro reader."
  (declare (ignore sub-char arg))
  (multiple-value-bind (tokenp token)
      (read-token stream t nil t nil nil *readtable*)
    (if tokenp
        (make-symbol (token-text token))
        (serror 'simple-reader-error stream
                "token expected after #:"))))


(defun reader-dispatch-macro-unreadable        (stream arg sub-char)
  "Standard #< dispatch macro reader."
  (declare (ignore sub-char arg))
  (serror 'simple-reader-error stream
          "objects printed as #<...> cannot be read back in"))


(defun reader-dispatch-macro-comment           (stream arg sub-char)
  "Standard #| dispatch macro reader."
  (declare (ignore sub-char arg))
  ;; #|...|# is treated as a comment by the reader. It must be balanced
  ;; with respect to other occurrences of #| and |#, but otherwise may
  ;; contain any characters whatsoever.
  (loop
    :with level = 1
    :with state = :normal
    :until (zerop level)
    :do (case state
          ((:normal) (case (read-char stream t nil t)
                       ((#\#)              (setf state :sharp))
                       ((#\|)              (setf state :pipe))))
          ((:sharp)  (case (read-char stream t nil t)
                       ((#\#))
                       ((#\|) (incf level) (setf state :normal))
                       (otherwise          (setf state :normal))))
          ((:pipe)   (case (read-char stream t nil t)
                       ((#\#) (decf level) (setf state :normal))
                       ((#\|))
                       (otherwise          (setf state :normal))))))
  (values))


(defun reader-dispatch-macro-function          (stream arg sub-char)
  "Standard #' dispatch macro reader."
  (declare (ignore sub-char arg))
  `(cl:function ,(read stream t nil t)))


(defun reader-dispatch-macro-vector            (stream arg sub-char)
  "Standard #( dispatch macro reader."
  (declare (ignore sub-char))
  ;; If an unsigned decimal integer appears between the # and (, it
  ;; specifies explicitly the length of the vector. The consequences are
  ;; undefined if the number of objects specified before the closing )
  ;; exceeds the unsigned decimal integer. If the number of  objects
  ;; supplied before the closing ) is less than the unsigned decimal
  ;; integer but greater than zero, the last object is used to fill all
  ;; remaining elements of the  vector. The consequences are undefined if
  ;; the unsigned decimal integer is non-zero and number of objects
  ;; supplied before the closing ) is zero.  In that case, we let the
  ;; implementation initialize the vector.
  ;;
  ;; Thanks to Budden for having signaled a bug in the first version of this function,
  ;; and thanks to Yulian Tarantuk for signaling the "comment before closing parenthesis" bug.
  (flet ((finish-vector (vector i)
           (if arg
               (progn
                 (cond
                   ((>= i arg)
                    ;; vector is longer than the explicitly given length
                    ;; We just eat the remaining stuff.
                    (loop
                      :until (char= #\) (peek-char t stream t nil t))
                      :do (let ((*read-suppress* t))
                            (read-0/1 stream t nil t nil nil '()))
                      :finally (read-char stream nil nil t)))
                   ;; vector is shorter.
                   ((plusp i)
                    ;; If we have at least one element in,
                    ;; we replicate it till the end.
                    (loop
                      :with last-item = (aref vector (1- i))
                      :for j :from i :below arg
                      :do (setf (aref vector j) last-item)))
                   ;; Otherwise we will let it up to the implementation
                   ;; to do its implementation dependent thing.
                   )
                 vector)
               (copy-seq vector))))
    (loop
      :with vector = (if arg
                         (make-array arg)
                         (make-array 1024 :adjustable t :fill-pointer 0))
      :for i :from 0 :while (or (not arg) (< i arg))
      :do (let ((peek (peek-char t stream nil stream t)))
            (cond
              ((eql peek stream)
               (serror 'simple-end-of-file stream
                       "input stream ~S has reached its end" stream))
              ((char= peek #\))
               (read-char stream nil nil t)
               (return-from reader-dispatch-macro-vector (finish-vector vector i)))
              (t
               (multiple-value-bind (kind tokens)
                   (read-0/1 stream t nil t nil nil '())
                 (declare (ignore kind)) ; always nil here.
                 (when tokens
                   (if arg
                       (setf (aref vector i) (first tokens))
                       (vector-push-extend (first tokens) vector)))))))
      :finally (return-from reader-dispatch-macro-vector (finish-vector vector i)))))





(defun reader-dispatch-macro-bit-vector        (stream arg sub-char)
  "Standard #* dispatch macro reader.
URL: <http://www.lispworks.com/documentation/HyperSpec/Body/02_dhd.htm>
"
  (declare (ignore sub-char))
  ;; Syntax: #*<<bits>>
  ;;
  ;; A simple bit vector is constructed containing the indicated bits (0's
  ;; and 1's), where the leftmost bit has index zero and the subsequent
  ;; bits have increasing indices.
  ;;
  ;; Syntax: #<<n>>*<<bits>>
  ;;
  ;; With an argument n, the vector to be created is of length n. If the
  ;; number of bits is less than n but greater than zero, the last bit is
  ;; used to fill all remaining bits of the bit vector.
  ;;
  ;; The notations #* and #0* each denote an empty bit vector.
  ;;
  ;; Regardless of whether the optional numeric argument n is provided, the
  ;; token that follows the asterisk is delimited by a normal token
  ;; delimiter. However, (unless the  value of *read-suppress* is true) an
  ;; error of type reader-error is signaled if that  token is not composed
  ;; entirely of 0's and 1's, or if n was supplied and the token is
  ;; composed of more than n bits, or if n is greater than one, but no bits
  ;; were specified.  Neither a single escape nor a multiple escape is
  ;; permitted in this token.
  (if arg
      (loop
        :with vector = (make-array arg :element-type 'bit :initial-element 0)
        :for i :from 0 :below arg
        :while (let ((ch (peek-char nil stream nil nil t)))
                 (and ch (not (token-delimiter-p ch))))
        :do (setf (aref vector i) (digit-char-p (read-char stream nil nil t)))
        :finally (progn
                   (cond
                     ((>= i arg)
                      (let ((*read-suppress* t))
                        (loop
                          :while (let ((ch (peek-char nil stream nil nil t)))
                                   (and ch (not (token-delimiter-p ch))))
                          :do (read-char stream nil nil t))))
                     ((plusp (aref vector (1- i)))
                      (loop
                        :for j :from i :below arg
                        :do (setf (aref vector j) 1))))
                   (return vector)))
      (loop
        :with vector = (make-array 1024 :adjustable t :fill-pointer 0
                                        :element-type 'bit :initial-element 0)
        :while (let ((ch (peek-char nil stream nil nil t)))
                 (and ch (not (token-delimiter-p ch))))
        ;; TODO: Check the behavior when the character is not a bit.
        :do (vector-push-extend (digit-char-p (read-char stream nil nil t)) vector)
        :finally (return (copy-seq vector)))))


(defun reader-dispatch-macro-char              (stream arg sub-char)
  "Standard #\\ dispatch macro reader."
  (declare (ignore sub-char arg))
  (read-char stream t nil t))


(defun reader-dispatch-macro-array             (stream arg sub-char)
  "Standard #A dispatch macro reader."
  (declare (ignore sub-char))
  (let ((initial-contents (read stream t nil t)))
    (labels ((collect-dimensions (n contents dimensions)
               (if (zerop n)
                   (nreverse dimensions)
                   (collect-dimensions (1- n) (first contents)
                                       (cons (length contents) dimensions)))))
      ;; TODO: we rely on make-array to raise some errors that it may not raise...
      (make-array (collect-dimensions (or arg 1) initial-contents '())
                  :initial-contents initial-contents))))



(defun read-rational-in-base (stream arg sub-char *read-base*)
  "
DO:      Read a rational number in the base specified.
RETURN:  The rational read.
"
  (when arg (serror stream "no number allowed between # and ~A" sub-char))
  (let ((value (read stream t nil t)))
    (if (rationalp value)
        value
        (serror stream
                "token \"~A\" after #~A is not a rational number in base ~D"
                sub-char *read-base*))))

(defun reader-dispatch-macro-binary            (stream arg sub-char)
  "Standard #B dispatch macro reader."
  (read-rational-in-base stream arg sub-char 2.))

(defun reader-dispatch-macro-octal             (stream arg sub-char)
  "Standard #O dispatch macro reader."
  (read-rational-in-base stream arg sub-char 8.))

(defun reader-dispatch-macro-hexadecimal       (stream arg sub-char)
  "Standard #X dispatch macro reader."
  (read-rational-in-base stream arg sub-char 16.))

(defun reader-dispatch-macro-radix             (stream arg sub-char)
  "Standard #R dispatch macro reader."
  (unless arg
    (serror stream "the number base must be given between # and ~A" sub-char))
  (read-rational-in-base stream nil sub-char arg))


;; Copied from com.informatimago.common-lisp.cesarum.list to avoid package use loop.
(defun proper-list-p (object)
  "
RETURN: whether object is a proper list
NOTE:   terminates with any kind of list, dotted, circular, etc.
"
  (labels ((proper (current slow)
             (cond ((null current)       t)
                   ((atom current)       nil)
                   ((null (cdr current)) t)
                   ((atom (cdr current)) nil)
                   ((eq current slow)    nil)
                   (t                    (proper (cddr current) (cdr slow))))))
    (and (listp object) (proper object (cons nil object)))))


(defun reader-dispatch-macro-complex           (stream arg sub-char)
  "Standard #C dispatch macro reader."
  (declare (ignore sub-char arg))
  (let ((c (read stream t nil t)))
    (unless (and (proper-list-p c)
                 (= 2 (length c))
                 (every (function realp) c))
      (serror 'simple-reader-error stream
              "bad syntax for complex number: #C~S" c))
    (complex (first c) (second c))))


(defun reader-dispatch-macro-pathname          (stream arg sub-char)
  "Standard #P dispatch macro reader."
  (declare (ignore sub-char arg))
  (let ((namestring (read stream t nil t)))
    (if *read-suppress*
        nil
        (parse-namestring namestring))))


(defun reader-dispatch-macro-structure         (stream arg sub-char)
  "Standard #S dispatch macro reader."
  (declare (ignore sub-char arg))
  (let* ((data (read stream t nil t))
         (constructor (intern (cl:with-standard-io-syntax (format nil "MAKE-~A" (first data)))))
         (arguments   (loop
                        :with keyword-package = (find-package "KEYWORD")
                        :for (k v) :on (rest data) :by (function cddr)
                        :collect (intern (string k) keyword-package)
                        :collect v)))
    (apply constructor arguments)))


#-sbcl
(defmethod initialize-instance :after ((self readtable) &rest rest &key &allow-other-keys)
  (unless (getf rest :syntax-table)
    (macrolet ((smc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-macro-character
                                  ,(first clause)
                                  (function ,(second clause))
                                  ,(third clause)
                                  self))
                              clauses))))
      (smc
       (#\; reader-macro-line-comment     nil)
       (#\" reader-macro-string           nil)
       (#\' reader-macro-quote            nil)
       (#\` reader-macro-backquote        nil)
       (#\, reader-macro-comma            nil)
       (#\( reader-macro-left-parenthesis nil)
       (#\) reader-macro-error-start      nil)))
    (macrolet ((dmc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-dispatch-macro-character
                                  ,(first  clause)
                                  ,(second clause)
                                  (function ,(third clause))
                                  self))
                              clauses))))
      (make-dispatch-macro-character #\# t self)
      (dmc
       (#\# #\SPACE   reader-dispatch-macro-error-invalid)
       (#\# #\NEWLINE reader-dispatch-macro-error-invalid)
       (#\# #\# reader-dispatch-macro-label-reference)
       (#\# #\' reader-dispatch-macro-function)
       (#\# #\( reader-dispatch-macro-vector)
       (#\# #\* reader-dispatch-macro-bit-vector)
       (#\# #\+ reader-dispatch-macro-feature)
       (#\# #\- reader-dispatch-macro-not-feature)
       (#\# #\. reader-dispatch-macro-read-eval)
       (#\# #\: reader-dispatch-macro-uninterned)
       (#\# #\< reader-dispatch-macro-unreadable)
       (#\# #\= reader-dispatch-macro-label-definition)
       (#\# #\A reader-dispatch-macro-array)
       (#\# #\B reader-dispatch-macro-binary)
       (#\# #\C reader-dispatch-macro-complex)
       (#\# #\O reader-dispatch-macro-octal)
       (#\# #\P reader-dispatch-macro-pathname)
       (#\# #\R reader-dispatch-macro-radix)
       (#\# #\S reader-dispatch-macro-structure)
       (#\# #\X reader-dispatch-macro-hexadecimal)
       (#\# #\\ reader-dispatch-macro-char)
       (#\# #\| reader-dispatch-macro-comment)
       ;; clisp extensions:
       ;; (#\# #\! reader-dispatch-macro-executable)
       ;; (#\# #\" reader-dispatch-macro-clisp-pathname)
       ;; (#\# #\, reader-dispatch-macro-load-eval)
       ;; (#\# #\Y SYSTEM::CLOSURE-READER)
       ))))

;; Working around sbcl bugs is always a pleasure…
#+sbcl
(defun init (self rest)
  (unless (getf rest :syntax-table)
    (macrolet ((smc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-macro-character
                                  ,(first clause)
                                  (function ,(second clause))
                                  ,(third clause)
                                  self))
                              clauses))))
      (smc
       (#\; reader-macro-line-comment     nil)
       (#\" reader-macro-string           nil)
       (#\' reader-macro-quote            nil)
       (#\` reader-macro-backquote        nil)
       (#\, reader-macro-comma            nil)
       (#\( reader-macro-left-parenthesis nil)
       (#\) reader-macro-error-start      nil)))
    (macrolet ((dmc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-dispatch-macro-character
                                  ,(first  clause)
                                  ,(second clause)
                                  (function ,(third clause))
                                  self))
                              clauses))))
      (make-dispatch-macro-character #\# t self)
      (dmc
       (#\# #\SPACE   reader-dispatch-macro-error-invalid)
       (#\# #\NEWLINE reader-dispatch-macro-error-invalid)
       (#\# #\# reader-dispatch-macro-label-reference)
       (#\# #\' reader-dispatch-macro-function)
       (#\# #\( reader-dispatch-macro-vector)
       (#\# #\* reader-dispatch-macro-bit-vector)
       (#\# #\+ reader-dispatch-macro-feature)
       (#\# #\- reader-dispatch-macro-not-feature)
       (#\# #\. reader-dispatch-macro-read-eval)
       (#\# #\: reader-dispatch-macro-uninterned)
       (#\# #\< reader-dispatch-macro-unreadable)
       (#\# #\= reader-dispatch-macro-label-definition)
       (#\# #\A reader-dispatch-macro-array)
       (#\# #\B reader-dispatch-macro-binary)
       (#\# #\C reader-dispatch-macro-complex)
       (#\# #\O reader-dispatch-macro-octal)
       (#\# #\P reader-dispatch-macro-pathname)
       (#\# #\R reader-dispatch-macro-radix)
       (#\# #\S reader-dispatch-macro-structure)
       (#\# #\X reader-dispatch-macro-hexadecimal)
       (#\# #\\ reader-dispatch-macro-char)
       (#\# #\| reader-dispatch-macro-comment)
       ;; clisp extensions:
       ;; (#\# #\! reader-dispatch-macro-executable)
       ;; (#\# #\" reader-dispatch-macro-clisp-pathname)
       ;; (#\# #\, reader-dispatch-macro-load-eval)
       ;; (#\# #\Y SYSTEM::CLOSURE-READER)
       ))))

#+sbcl
(defmethod initialize-instance :after ((self readtable) &rest rest &key &allow-other-keys)
  (init self rest))


(setf *standard-readtable* (copy-readtable nil)
      *readtable*          (copy-readtable nil))


(defun list-all-macro-characters (&optional (*readtable* *readtable*))
  "
RETURN: A list of all the macro and dispatch-macro characters in the readtable.
NOTE:   We have the same function in the com.informatimago.tools.reader-macro
        package, working on cl:readtable
        instead of com.informatimago.common-lisp.lisp-reader.reader:readtable.
"
  (check-type *readtable* readtable)
  (loop
    :with result = '()
    :for code :below char-code-limit
    :for ch = (code-char code)
    :when ch
      :do (multiple-value-bind (mc nt) (get-macro-character ch)
            (when mc
              (if (ignore-errors (progn (get-dispatch-macro-character ch #\a) t))
                  (loop :for code :below char-code-limit
                        :for sub = (code-char code)
                        :when (and sub
                                   (not (and (alpha-char-p sub) (lower-case-p sub)))
                                   (get-dispatch-macro-character ch sub))
                          :do (push (list :dispatch-macro-character nt ch sub
                                          #-(and) (format nil "~C~C" ch sub))
                                    result))
                  (push (list :macro-character nt ch
                              #-(and) (string ch))
                        result))))
    :finally (return (nreverse result))))


(defun potential-number-p (token
                           &optional
                             (*read-base* *read-base*)
                             (ratio-markers "/"))
  "
TOKEN:         A string containing the token to be tested.
*READ-BASE*:   The current radix.
RATIO-MARKER:  / in the standard readtable, but it could be something else...
RETURN:        Whether the TOKEN is a potential number.
"
  ;; sign                 "+-"
  ;; ratio-markers        "/"     (by default)
  ;; decimal-points       "."
  ;; extension-characters "^_"
  ;; number-markers                letters alone

  (and (plusp (length token))

       ;; Letters may be considered to be digits, depending on the
       ;; current input base, but only in tokens containing no decimal
       ;; points.
       (let ((*read-base* (if (find #\. token) 10. *read-base*)))

         (and

          ;; 4. The token does not end with a sign
          (let ((last-ch (aref token (1- (length token)))))
            (not (find last-ch "+-")))

          ;; 3. The token begins with a digit, sign, decimal point or extension character.
          (let ((first-ch (aref token 0)))
            (or (digit-char-p first-ch *read-base*)
                (find first-ch "+-.^_")))

          ;; 2. The token contains at least one digit.
          (find-if (lambda (ch) (digit-char-p ch *read-base*)) token)

          ;; 1. The token consists entirely of digits, signs, ratio
          ;;    markers, decimal points, extension characters, and
          ;;    number markers.
          (loop
            :for prevch = #\0 :then ch
            :for ch :across token
            :always (or (digit-char-p ch *read-base*)
                        (find ch "+-.^_")
                        (find ch ratio-markers)
                        (and (alpha-char-p ch)
                             (not (alpha-char-p prevch)))))))))



(defmacro WITH-STANDARD-IO-SYNTAX (&body body)
  `(let ((*package*                    (find-package "COMMON-LISP-USER"))
         (*print-array*                t)
         (*print-base*                 10)
         (*print-case*                 :upcase)
         (*print-circle*               nil)
         (*print-escape*               t)
         (*print-gensym*               t)
         (*print-length*               nil)
         (*print-level*                nil)
         (*print-lines*                nil)
         (*print-miser-width*          nil)
         (*print-pprint-dispatch*      nil #|implementation dependant|#)
         (*print-pretty*               nil)
         (*print-radix*                nil)
         (*print-readably*             t)
         (*print-right-margin*         nil)
         (*read-base*                  10)
         (*read-default-float-format*  'single-float)
         (*read-eval*                  t)
         (*read-suppress*              nil)
         (*readtable*                  (make-instance 'readtable)))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
