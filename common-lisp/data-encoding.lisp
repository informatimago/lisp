;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               data-encoding.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to encode and decode data 
;;;;    in a byte vector buffer.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-06-18 <PJB> Extracted from palm-dba.lisp. Augmented.
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "COMMON-LISP")
  (:EXPORT "SIZE-OF-ENCTYPE" "ENCTYPE-INSTANCE" "ENCTYPE-WRITE" "ENCTYPE-READ"
           "MAKE-ENCTYPE" "DEF-ENCRECORD" "DEF-ENCTYPE")
  (:DOCUMENTATION
   "This package exports functions to encode and decode data 
    in a byte vector buffer.

    Copyright Pascal J. Bourguignon 2002 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING")



;; Carl Shapiro (cshapiro+spam@panix.com)
;; The right way to manipulate record-oriented data in Common Lisp is by
;; overlaying source data with byte-arrays which are in turn overlaid
;; with displaced arrays of the correct type for the structure member
;; data.  This is the strategy which the Lisp Machine uses deep within
;; the file system internals as well as in other situations where
;; pointer-oriented structures are not an appropriate abstraction for
;; modeling domain data.  If you have control over your implementation's
;; array primitives you can easily clobber the data pointer for an
;; array-header object to reuse the base record and spare yourself some
;; otherwise gratuitous copying.  It's "a veritable bos grunniens of
;; hair", as some would have said, but at the same time, unequivocally
;; effective.


;; Agregate encoding types:
;;
;; (record lisp-type [:size size] (offset name enc-type)...)
;; (array  element-enc-type dimensions)
;;
;;
;; Simple encoding types:
;;
;; (string size [padchar])
;; pascal-string:   (length,characters...)
;; c-string:        characters...,0,garbage
;; modula-2-string: characters...,0,garbage or characters...|end of field
;; cobol-strings:   characters...,padchar...
;;
;; (string size [:green-length [integer-enc-type]
;;               | [padchar] [:terminated [:if-smaller]|:padded [:strip]]])
;;
;; (string size
;;         [:green-length [integer-enc-type]
;;         | [padchar]
;;           [:terminated [:if-smaller]
;;           |:padded [:strip]]] [:encoding encoding] )
;;
;;
;; (number encoding [parameters])
;; encodings:
;;     unsigned                                    -- unsigned binary
;;     two-complement       binary    comp comp-4  -- two-complement signed bin.
;;     one-complement                              -- one-complement signed bin.
;;     binary-coded-decimal packed-decimal comp-3  -- signed BCD
;;     ieee-float-single                   comp-1  -- CHECK IEEE SPECIFIES
;;     ieee-float-double                   comp-2  -- BYTE ORDER!
;;     display                                     -- ASCII
;;
;; unsigned             byte-size big-endian|little-endian
;; two-complement       byte-size big-endian|little-endian
;; one-complement       byte-size big-endian|little-endian
;; binary-coded-decimal byte-size big-endian|little-endian
;; ieee-float-single                     -- no parameter, 32-bit
;; ieee-float-double                     -- no parameter, 64-bit
;; display              "format"         -- picture format usage display
;;
;;     default is big-endian
;;
;; (number display "z(7)9")
;; (number display "-Z(7)9") --> "-       1"
;; (number formated "~10,3F") --> "    -1.000"
;;
;;
;; number-enctype
;;     unsigned-integer-enctype
;;        two-complement-integer-enctype
;;        one-complement-integer-enctype
;;        binary-coded-decimal-integer-enctype
;;     ieee-float-single-number-enctype
;;     ieee-float-double-number-enctype
;;     display-number-enctype


(DEFGENERIC write-value (self buffer offset value))
(DEFGENERIC number-of-digit (self))
(DEFGENERIC maximum-length (self))
(DEFGENERIC to-lisp-type (self))
(DEFGENERIC default-value (self))
(DEFGENERIC size-of-enctype (self))
(DEFGENERIC get-value (self buffer offset))
(DEFGENERIC set-value (self buffer offset record))

;; ------------------------------------------------------------

(defclass enctype ()
  ((name :accessor name :type symbol :initarg :name
         :documentation "The root name of the type."))
  (:documentation "An abstract encoded type."))



;; ------------------------------------------------------------

(defclass number-enctype (enctype)
  ((size
    :accessor size :type fixnum :initarg :size
    :documentation "Number of bytes used by a number in this representation.")
   (modulo
    :accessor modulo :type integer :initarg :modulo
    :documentation "1+the maximum value")
   (endian
    :accessor endian  :initarg :endian
    :documentation "The endian, either :big, :little or whatnot."))
  (:documentation "An abstract number type.")) ;;number-enctype


;; ------------------------------------------------------------

(defclass unsigned-integer-enctype (number-enctype)
  ()
  (:documentation "A binary unsigned integer type."))


(defmethod initialize-instance ((self unsigned-integer-enctype) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (modulo self)  (expt 2 (* 8 (size self))))
  self) ;;initialize-instance


(defmethod print-object ((self unsigned-integer-enctype) (out stream))
  (warn "We don't implement printer control variables in UNSIGNED-INTEGER-ENCTYPE PRINT-OBJECT.~&")
  (format out "#<UNSIGNED-INTEGER ~D-BIT, ~A-ENDIAN>"
          (* 8 (size-of-enctype self)) (endian self))
  self) ;;print-object


(defmethod to-lisp-type    ((self unsigned-integer-enctype))  '(integer 0))
(defmethod default-value   ((self unsigned-integer-enctype))  0)
(defmethod size-of-enctype ((self unsigned-integer-enctype))  (size self))


(defmethod get-value ((self unsigned-integer-enctype) buffer offset)
  (let ((value 0))
    (case (endian self)
      ((:big)    (dotimes (i (size self))
                   (setf value (+ (* 256 value) (aref buffer (+ offset i))))))
      ((:little) (do ((i (1- (size self)) (1- i))) ((< i 0))
                   (setf value (+ (* 256 value) (aref buffer (+ offset i))))))
      (otherwise (error "Unknown endian ~S" (endian self))))
    (values value (+ offset (size self))))) ;;get-value


(defmethod set-value ((self unsigned-integer-enctype) buffer offset value)
  (assert (and (integerp value) (<= 0 value) (< value (modulo self))))
  (write-value self buffer offset value))


(defmethod write-value ((self unsigned-integer-enctype) buffer offset value)
  (case (endian self)
    ((:big)    (do ((i (1- (size self)) (1- i))) ((< i 0))
                 (setf (aref buffer (+ offset i)) (mod value 256)
                       value (truncate value 256))))
    ((:little) (dotimes (i (size self))
                 (setf (aref buffer (+ offset i)) (mod value 256)
                       value (truncate value 256))))
    (otherwise (error "Unknown endian ~S" (endian self))))
  (+ offset (size self))) ;;write-value


;; ------------------------------------------------------------

(defclass two-complement-integer-enctype (unsigned-integer-enctype)
  ()
  (:documentation "A two-complement signed integer type."))


(defmethod print-object ((self two-complement-integer-enctype) (out stream))
  (warn "We don't implement printer control variables in TWO-COMPLEMENT-INTEGER-ENCTYPE PRINT-OBJECT.")
  (format out "#<TWO-COMPLEMENT-INTEGER ~D-BIT, ~A-ENDIAN>"
          (* 8 (size-of-enctype self)) (endian self))
  self) ;;print-object


(defmethod to-lisp-type    ((self two-complement-integer-enctype))  'integer)
(defmethod default-value   ((self two-complement-integer-enctype))  0)
(defmethod size-of-enctype ((self two-complement-integer-enctype))  (size self))



(defmethod get-value ((self two-complement-integer-enctype) buffer offset)
  (multiple-value-bind (uval noffset) (call-next-method self buffer offset)
    (if (< uval (/ (modulo self) 2))
        (values uval noffset)
        (values (- uval (modulo self)) noffset)))) ;;get-value


(defmethod set-value ((self two-complement-integer-enctype) buffer offset value)
  (assert (and (integerp value)
               (<= (- (/ (modulo self) 2)) value (1- (/ (modulo self) 2)))))
  (if (< value 0)
      (write-value self buffer offset (+ value (modulo self)))
      (write-value self buffer offset value))) ;;set-value


;; ------------------------------------------------------------

(defclass one-complement-integer-enctype (unsigned-integer-enctype)
  ()
  (:documentation "A one-complement signed integer type."))


(defmethod print-object ((self one-complement-integer-enctype) (out stream))
  (warn "We don't implement printer control variables in ONE-COMPLEMENT-INTEGER-ENCTYPE PRINT-OBJECT.~&")
  (format out "#<ONE-COMPLEMENT-INTEGER ~D-BIT, ~A-ENDIAN>"
          (* 8 (size-of-enctype self)) (endian self))
  self) ;;print-object


(defmethod to-lisp-type    ((self one-complement-integer-enctype))  'integer)
(defmethod default-value   ((self one-complement-integer-enctype))  0)
(defmethod size-of-enctype ((self one-complement-integer-enctype))  (size self))


(defmethod get-value ((self one-complement-integer-enctype) buffer offset)
  (multiple-value-bind (uval noffset) (call-next-method self buffer offset)
    (if (< uval (/ (modulo self) 2))
        (values uval noffset)
        (values (- uval -1 (modulo self)) noffset)))) ;;get-value


(defmethod set-value ((self one-complement-integer-enctype) buffer offset value)
  (assert (and (integerp value)
               (< (- (/ (modulo self) 2)) value (/ (modulo self) 2))))
  (if (< value 0)
      (write-value self buffer offset (+ value (modulo self) -1))
      (write-value self buffer offset value))) ;;set-value


;; ------------------------------------------------------------

(defclass binary-coded-decimal-integer-enctype (unsigned-integer-enctype)
  ()
  (:documentation "A binary-coded-decimal signed integer type."))


(defmethod initialize-instance ((self binary-coded-decimal-integer-enctype) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (modulo self) (expt 10 (number-of-digit self)))
  self) ;;initialize-instance



(defmethod print-object ((self binary-coded-decimal-integer-enctype) (out stream))
  (warn "We don't implement printer control variables in BINARY-CODED-DECIMAL-INTEGER-ENCTYPE PRINT-OBJECT.~&")
  (format out "#<BINARY-CODED-DECIMAL-INTEGER ~D-DIGITS, ~A-ENDIAN>"
          (number-of-digit self) (endian self))
  self) ;;print-object


(defmethod to-lisp-type    ((self binary-coded-decimal-integer-enctype)) 'integer)
(defmethod default-value   ((self binary-coded-decimal-integer-enctype)) 0)
(defmethod size-of-enctype ((self binary-coded-decimal-integer-enctype)) (size self))

(defmethod number-of-digit ((self binary-coded-decimal-integer-enctype))
  (1- (* 2 (size-of-enctype self))))



(defun integer-to-bcd (value)
  (do ((bcd   (if (< value 0)  #xD      #xC))
       (value (if (< value 0) (- value) value))
       (hex 16 (* 16 hex)))
      ((zerop value) bcd)
    (multiple-value-bind (q r) (truncate value 10)
      (incf bcd (* hex r))
      (setf value q)))) ;;integer-to-bcd


(defun integer-from-bcd (bcd)
  (do ((sign   (if (= #xC (mod bcd 16)) 1 -1))
       (value  0)
       (bcd    (truncate bcd 16))
       (dix 1 (* 10 dix)))
      ((zerop bcd) (* sign value))
    (multiple-value-bind (q r) (truncate bcd 16)
      (incf value (* dix r))
      (setf bcd q)))) ;;integer-from-bcd


(defmethod get-value ((self binary-coded-decimal-integer-enctype) buffer offset)
  (multiple-value-bind (bcd noffset) (call-next-method self buffer offset)
    (values (integer-from-bcd bcd) noffset)))


(defmethod set-value ((self binary-coded-decimal-integer-enctype) buffer offset value)
  (assert (and (integerp value) (< (- (modulo self)) value (modulo self))))
  (write-value self buffer offset (integer-to-bcd value)))


;; ------------------------------------------------------------

;;; (defclass display-number-enctype (number-enctype)
;;;   ()
;;;   (:documentation "A display number type.
;;;     Inspired from COBOL PICTURE xxx USAGE DISPLAY, but in COBOL,
;;;     numbers are stored in EBCDIC (usage DISPLAY), and the PICTURE
;;;     is used when printing (formating) the field.
;;;     Here, we want to ''store'' the number formated as per the PICTURE,
;;;     and to parse it back."));;display-number-enctype


;;; NOT YET: numbers stored as string need charset encoding like strings.
;;;          let's declare these fields as strings and format or parse
;;;          them with normal lisp functions (format, read-from-string).


;; USAGE DISPLAY
;; 
;;     this indicates that the field is stored in an uncompressed,
;;     displayable format. This is actually the default USAGE type and
;;     will be assumed if the field is omitted entirely. A value of 15000
;;     will be stored in a field of this type as x'f1f5f0f0f0' which is
;;     the ebcdic equivalent to 15000. To calculate the length of a
;;     display field count one for each occurrence of A X 9 Z * - + B / ,
;;     . $ and two for each occurrence of G CR DB.

;;
;; The following PICTURE keys specify alphabetic data:      AXG
;; and the following PICTURE keys specify storage variants: TRSV
;; Therefore, they're not relevant to formating numeric data
;; and are rejected..
;;

;; one byte for: A X 9 Z * - + B / , . $ and two for each occurrence of G CR DB.
;;
;; A
;; 
;;     corresponds to a single alphabetic character. The content of this
;;     position within the data field is allowed to be any uppercase of
;;     lowercase alphabetic character or a blank. Numerics and other
;;     symbols are not allowed
;; 
;; X
;; 
;;     corresponds to a single alphanumeric character. Any character from
;;     within the entire ebcdic character set can be contained in this
;;     field.
;; 
;; G
;; 
;;     corresponds to two bytes in the field which are being used to hold
;;     a double byte character. For example in Japan this definition
;;     would be used for fields that hold Kanji characters.
;; 
;; 9
;; 
;;     corresponds to a numeric character. Only the numeric values of
;;     zero through nine can be contained in this character.
;; 
;; E
;; 
;;     indicates that the following digits are the exponential for a
;;     floating point number. For example PIC '9v99999e99'.
;; 
;; S
;; 
;;     used to indicate that a numeric field is signed. The sign is
;;     always contained within the upper half byte of the last character
;;     of a display field or the lower half byte of a packed decimal
;;     field. A value of 'C' (12) representing positive and 'D' (13)
;;     negative. Binary fields represent negative numbers using the twos
;;     complement method.
;; 
;; T
;; 
;;     used to indicate that a display numeric field should only insert
;;     the sign into the upper half of the last byte if the value is
;;     negative.
;; 
;; R
;; 
;;     used to indicate that a display numeric field should only insert
;;     the sign into the upper half of the last byte if the value is
;;     positive.
;; 
;; P
;; 
;;     represents a virtual digit in a number that has no storage
;;     allocated to it. For example PIC '99ppp' can contain the value
;;     15000 as x'f1f5' with the number being assumed to represent
;;     thousands.
;; 
;; V
;; 
;;     used to indicate the position of a virtual decimal point. For
;;     example PIC '99999v99' can contain the value 15000 as
;;     x'f1f5f0f0f0f0f0' with the last two digits being assumed to
;;     represent hundredths.
;; 
;; Z
;; 
;;     corresponds to a leading numeric digit that if zero will be
;;     replaced by blank. Usually used to suppress leading zeros on
;;     numbers being printed.
;; 
;; *
;; 
;;     corresponds to a leading numeric digit that if zero will be
;;     replaced by *. Usually used to suppress leading zeros on numbers
;;     being printed on cheques.
;; 
;; -
;; 
;;     formatting character used with numeric fields. This will display
;;     as a blank if the number is zero or positive and will display as
;;     shown if the number is negative.
;; 
;; +
;; 
;;     formatting character used with numeric fields. This will display
;;     as shown if the number is zero or positive and will display as a -
;;     if the number is negative.
;; 
;; CR
;; 
;;     formatting character used with numeric fields. This will display
;;     as a blank if the number is zero or positive and will display as
;;     shown if the number is negative.
;; 
;; DB
;; 
;;     formatting character used with numeric fields. This will display
;;     as shown if the number is zero or positive and will display as CR
;;     if the number is negative.
;; 
;; B
;; 
;;     corresponds to a character that is always blank. Usually used to
;;     insert a blank into the middle of a field that is about to be
;;     output.
;; 
;; / or , or . or $
;; 
;;     formatting characters used in display fields being output. These
;;     values will display exactly as shown. For example the field PIC
;;     '99,999' containing the value x'f1f5f0f0f0' will print as '15,000'.



;; ------------------------------------------------------------

(defvar +string-enctype-default-encoding+ :standard-ascii
  "The encoding used by default when string enctypes are created
   without specifying an encoding.
   If you want to use another encoding (implementation dependant values),
   you must set DECODE-STRING and ENCODE-STRING to functions that can
   handle it.") ;;+string-enctype-default-encoding+


(defvar +standard-characters+ (format nil "~C~A~A~A"
                                      #\newline
                                      " !\"#$%&'()*+,-./0123456789:;<=>?"
                                      "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                      "`abcdefghijklmnopqrstuvwxyz{|}~")
  "A string containing all the COMMON-LISP standard characters,
  first newline, then from ASCII space to ASCII tilde in order.
  The COMMON-LISP implementation does not necessarily use the ASCII encoding
  for its strings. Our STANDARD-DECODE-STRING and STANDARD-ENCODE-STRING
  functions will take care to convert these characters to and from the
  ASCII encoding.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ASCII-LF+ 10 "Code of ASCII LF")
  (defconstant +ASCII-CR+ 13 "Code of ASCII CR")
  );;eval-when


(defun standard-decode-string (byte-vector encoding &key (start 0) (end nil))
  "
PRE:     (eq encoding :standard-ascii)
DO:      This function decodes a byte vector containing only ASCII codes
         of COMMON-LISP standard characters into a string.
"
  (assert (eq encoding :standard-ascii))
  (setq end (or end (length byte-vector)))
  (assert (and (<= 0 start) (<= start end) (<= end (length byte-vector))))
  (do ((string (make-string (- end start)))
       (i start (1+ i))
       (j 0 (1+ j))
       (code))
      ((>= i end) string)
    (setf code (aref byte-vector i))
    (cond
      ((or (= code +ASCII-LF+) (= code +ASCII-CR+))
       (setf (aref string j) #\newline))
      ((<= 32 code 126)
       (setf (aref string j) (aref +standard-characters+ (- code 31))))
      (t
       (error "Code ~D is not the ASCII code of a COMMON-LISP standard character."
              code)))))


(defun standard-encode-string (string encoding &key (start 0) (end nil))
  "
PRE:     (eq encoding :standard-ascii)
DO:      This function encodes a string containing only COMMON-LISP
         standard characters into an ASCII code byte vector.
"
  (assert (eq encoding :standard-ascii))
  (setq end (or end (length string)))
  (assert (and (<= 0 start) (<= start end) (<= end (length string))))
  (do ((bytes (make-array (list (- end start))
                          :element-type '(unsigned-byte 8)))
       (i start (1+ i))
       (j 0 (1+ j))
       (code))
      ((>= i end) bytes)
    (setf code (position (aref string i) +standard-characters+))
    (cond
      ((null code)
       (error "Character ~C is not a COMMON-LISP standard character."
              (aref string i)))
      ((= 0 code) (setf (aref bytes j) +ASCII-LF+))
      (t          (setf (aref bytes j) (+ 31 code))))) ) ;;standard-encode-string


(defvar decode-string (function standard-decode-string)
  "The function used to decode strings.")


(defvar encode-string (function standard-encode-string)
  "The function used to encode strings.")




;; ------------------------------------------------------------

(defclass string-enctype (enctype)
  ((allocated-size
    :accessor allocated-size :type fixnum :initarg :allocated-size :initform 0
    :documentation "The total number of bytes where the string is stored.")
   (pad-code
    :accessor pad-code :type (unsigned-byte 8) :initarg :pad-code :initform 0
    :documentation "The code used as terminator or to pad the string.")
   (encoding
    :accessor encoding :initarg :encoding
    :initform +string-enctype-default-encoding+
    :documentation
    "The encoding used to convert between strings and byte vectors.
     The :standard encoding use the ASCII code on the COMMON-LISP standard
     characters. The presence of any other character throws an error."))
  (:documentation "")) ;;string-enctype


(defmethod to-lisp-type    ((self string-enctype))  'string)
(defmethod default-value   ((self string-enctype))  "")
(defmethod size-of-enctype ((self string-enctype))  (allocated-size self))


;; ............................................................

(defclass green-length-string-enctype (string-enctype)
  ((green-length
    :accessor green-length :type number-enctype
    :initarg :green-length 
    :documentation "The enctype used to store the green length.
    The PAD-CODE is used to ease processing by external programs."))
  (:documentation "")) ;;green-length-string-enctype


(defmethod print-object ((self green-length-string-enctype) (out stream))
  (warn "We don't implement printer control variables in GREEN-LENGTH-STRING-ENCTYPE PRINT-OBJECT.~&")
  (format out "#<GREEN-LENGTH-STRING ~D WITH GREEN-LENGTH ~S, ENCODED IN ~A>"
          (allocated-size self)  (green-length self) (encoding self))
  self) ;;print-object


(defmethod maximum-length ((self green-length-string-enctype))
  (- (allocated-size self) (size-of-enctype (green-length self))))


(defmethod get-value ((self green-length-string-enctype) buffer offset)
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))
  (multiple-value-bind
        (length bytes-offset) (get-value (green-length self) buffer offset)
    (values (funcall decode-string buffer (encoding self)
                     :start bytes-offset :end (+ bytes-offset length))
            (+ offset (allocated-size self))))) ;;get-value


(defmethod set-value ((self green-length-string-enctype) buffer offset string)
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))

  (let ((bytes (funcall encode-string string (encoding self))))
    (assert (<= (length bytes) (maximum-length self))
            () "String too long for field: encoded bytes=~D>~D=maximum length."
            (length bytes) (maximum-length self))
    (let ((bytes-offset
           (set-value (green-length self) buffer offset (length bytes))))
      (replace buffer bytes
               :start1 bytes-offset)
      (fill buffer (pad-code self)
            :start (+ bytes-offset (length bytes))
            :end   (+ offset (allocated-size self)))))
  (+ offset (allocated-size self))) ;;set-value


;; ............................................................

(defclass terminated-string-enctype (string-enctype)
  ((if-smaller
    :accessor if-smaller :type boolean :initarg :if-smaller :initform nil
    :documentation "When NIL, the maximum length for the string is one less
    the allocated size, and the terminator character is always present."))
  (:documentation "")) ;;terminated-string-enctype


(defmethod print-object ((self terminated-string-enctype) (out stream))
  (warn "We don't implement printer control variables in TERMINATED-STRING-ENCTYPE PRINT-OBJECT.~&")
  (format out
    "#<TERMINATED-STRING ~D TERMINATED WITH ~S~:[~; IF-SMALLER~], ENCODED IN ~A>"
    (allocated-size self) (pad-code self) (if-smaller self) (encoding self))
  self) ;;print-object


(defmethod maximum-length ((self terminated-string-enctype))
  (if (if-smaller self)
      (allocated-size self)
      (1- (allocated-size self))))



(defmethod get-value ((self terminated-string-enctype) buffer offset)
  "
NOTE:   We don't check for if-smaller terminator, but we limit to allocated size.
"
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))
  (values (funcall decode-string buffer (encoding self)
                   :start offset
                   :end  (do ((end offset (1+ end))
                              (limit (+ offset (allocated-size self))))
                             ((or (>= end limit)
                                  (= (pad-code self) (aref buffer end)))
                              end)))
          (+ offset (allocated-size self)))) ;;get-value


(defmethod set-value ((self terminated-string-enctype) buffer offset string)
  "
NOTE:    The handling of IF-SMALLER is hidden in (MAXIMUM-LENGTH SELF).
NOTE:    This is the same implementation as for PADDED-STRING-ENCTYPE.
"
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))
  (let ((bytes (funcall encode-string string (encoding self))))
    (assert (<= (length bytes) (maximum-length self))
            () "String too long for field: encoded bytes=~D>~D=maximum length."
            (length bytes) (maximum-length self))
    (replace buffer bytes :start1 offset)
    (fill buffer (pad-code self)
          :start (+ offset (length bytes))
          :end   (+ offset (allocated-size self))))
  (+ offset (allocated-size self))) ;;set-value


;; ............................................................

(defclass padded-string-enctype (string-enctype)
  ((strip
    :accessor strip :type boolean :initarg :strip      :initform nil
    :documentation "The PAD-CODE must be removed in the lisp string."))
  (:documentation "")) ;;padded-string-enctype

                                                              
(defmethod print-object ((self padded-string-enctype) (out stream))
  (warn "We don't implement printer control variables in PADDED-STRING-ENCTYPE PRINT-OBJECT.~&")
  (format out
    "#<PADDED-STRING ~D PADDED WITH ~S~:[~;, STRIPPED~], ENCODED IN ~A>"
    (allocated-size self) (pad-code self) (strip self) (encoding self))
  self) ;;print-object


(defmethod maximum-length ((self padded-string-enctype))
  (allocated-size self))



(defmethod get-value ((self padded-string-enctype) buffer offset)
  "
"
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))
  (values (funcall decode-string buffer (encoding self)
                   :start offset
                   :end  (if (strip self)
                             (do ((end (+ offset (allocated-size self)) (1- end)))
                                 ((or (<= end offset)
                                      (/= (pad-code self) (aref buffer (1- end))))
                                  end))
                             (+ offset (allocated-size self))))
          (+ offset (allocated-size self)))) ;;get-value


(defmethod set-value ((self padded-string-enctype) buffer offset string)
  "
NOTE:    This is the same implementation as for TERMINATED-STRING-ENCTYPE.
"
  (assert (<= (+ offset (allocated-size self)) (length buffer))
          () "Buffer overflow: field-size=~D > ~D=available size."
          (allocated-size self) (- (length buffer) offset))
  (let ((bytes (funcall encode-string string (encoding self))))
    (assert (<= (length bytes) (maximum-length self))
            () "String too long for field: encoded bytes=~D>~D=maximum length."
            (length bytes) (maximum-length self))
    (replace buffer bytes :start1 offset)
    (fill buffer (pad-code self)
          :start (+ offset (length bytes))
          :end   (+ offset (allocated-size self))))
  (+ offset (allocated-size self))) ;;set-value



;; ------------------------------------------------------------
;; array

(defclass array-enctype (enctype)
  ((dimensions :accessor dimensions :type list :initarg :dimensions
               :initform ())
   (element-type
    :accessor element-type :type enctype :initarg :element-type
    :documentation "The enctype instance of the type of the elements"))
  (:documentation "An array type.")) ;;array-enctype


(defmethod print-object ((self array-enctype) (out stream))
  (warn "We don't implement printer control variables in ARRAY-ENCTYPE PRINT-OBJECT.")
  (format out "#<ARRAY ~S [~S]>" (element-type self) (dimensions self) )
  self)


(defmethod to-lisp-type  ((self array-enctype))
  `(array ,(to-lisp-type (element-type self)) ,(dimensions self)))


(defmethod default-value ((self array-enctype))  
  `(make-array ',(dimensions self)
               :initial-element (default-value (element-type self))))


(defmethod size-of-enctype ((self array-enctype))
  (apply (function *) (size-of-enctype (element-type self)) (dimensions self)))


(defmethod get-value ((self array-enctype) buffer offset)
  (let* ((element-size (size-of-enctype (element-type self)))
         (dimensions (dimensions self))
         (array (make-array
                 dimensions
                 :element-type (to-lisp-type (element-type self)))))
    (when (> (length dimensions) 1)
      (error "Reading multidimensional array not implemented yet."))
    (do ((i 0 (1+ i))
         (o offset (+ o element-size)))
        ((>= i (first dimensions))
         array)
      (setf (aref array i) (get-value (element-type self) buffer o)))
    (values array (+ offset (size-of-enctype self))))) ;;get-value


(defmethod set-value ((self array-enctype) buffer offset array)
  (assert (equal (array-dimensions array) (dimensions self)))
  (do ((i 0 (1+ i))
       (o offset (+ o (size-of-enctype (element-type self)))))
      ((>= i (first (dimensions self)))
       o)
    (set-value (element-type self) buffer o (aref array i)))) ;;set-value


;; ------------------------------------------------------------
;; record


(defclass record-enctype (enctype)
  ((lisp-type
    :accessor lisp-type
    :type symbol
    :initarg :lisp-type
    :documentation
    "The name of the class whose instances store the record fields.
We need to use a class instead of a structure to be able to dynamically
set and retrieve the values of the fields.")
   (size
    :accessor size
    :type (and fixnum (integer 0)) ; fixnum doesn't take any argument.
    :initarg :size
    :initform 0)
   (fields
    :accessor fields
    :type list
    :initarg :fields
    :initform ()
    :documentation "A list of field structures in no particular order."))
  (:documentation "A record type.")) ;;record-enctype


(defstruct field
  (offset 0   :type integer)
  (name   nil :type symbol)
  (type   nil :type (or null enctype)))


(defmethod print-object ((self record-enctype) (out stream))
  (warn "We don't implement printer control variables in RECORD-ENCTYPE PRINT-OBJECT.")
  (format out "~&#<RECORD ~S :SIZE ~D~{~&  ~S~}>" (lisp-type self)
          (size self) (fields self))
  self) ;;print-object


(defmethod to-lisp-type  ((self record-enctype))
  (lisp-type self))


(defmethod default-value ((self record-enctype))  
  `(make-instance (lisp-type self)))


(defmethod size-of-enctype ((self record-enctype))
  (size self))


(defmethod get-value ((self record-enctype) buffer offset)
  (values
   (apply (function make-instance) (lisp-type self)
          (mapcan (lambda (field)
                    (list (conc-symbol (field-name field) :package "KEYWORD")
                          (get-value (field-type field)
                                     buffer (+ offset (field-offset field)))))
                  (fields self)))
   (+ offset (size-of-enctype self)))) ;;get-value


(defmethod set-value ((self record-enctype) buffer offset record)
  (map nil (lambda (field)
             (set-value (field-type field)
                        buffer (+ offset (field-offset field))
                        (slot-value record (field-name field))))
       (fields self))
  (+ offset (size-of-enctype self))) ;;set-value



;; ------------------------------------------------------------------------
;; 
;; Here, we map enctype descriptors (sexps) to clos instances of
;; the enctype classes defined above.
;;

(defparameter *enctype-instances* (make-hash-table :test (function equal))
  "A cache for all the enctypes seen,
   mapping the enctype descriptor to the clos instance representing it.")


(defparameter *enctype-definitions* (make-hash-table :test (function eq))
  "The type definitions")


(defun purge ()
  (setf *enctype-instances* (make-hash-table :test (function equal))
        *enctype-definitions* (make-hash-table :test (function eq))))


(defun defined-enctype (sexp)
  (let ((ad (gethash (car sexp) *enctype-definitions*)))
    (if ad
        (let ((name           (car sexp))
              (effective-args (cdr sexp))
              (formal-args    (car ad))
              (definition     (cdr ad)))
          (unless (= (length effective-args) (length formal-args))
            (error "Number of argument mismatch for type ~A, expected ~D, got ~D."
                   name (length formal-args) (length effective-args)))
          (map nil
               (lambda (value arg)
                 (setf definition (subst value arg definition
                                         :test (function eq))))
               effective-args formal-args)
          definition)
        (error "Unknown enctype ~S." sexp)))) ;;defined-enctype




(defun make-enctype-instance (enctype)
  "
ENCTYPE: A sexp denoting the enctype.
         enctype ::= 
            (record lisp-type [:size size] (offset name enc-type)...)
            (array  element-enc-type dimensions)
            (string size ...)
            (number encoding [parameters])
            (defined-type ...)
RETURN:  An instance of a subclass of enctype representing the enctype.
"
  (assert (and (listp enctype) (car enctype) (symbolp (car enctype))))
  (scase (first enctype)
    ;;   (record :lisp-type lisp-type :size size record-options...
    ;;           :fields ((name type [:offset offset field-options...])...)))
    ((record)
     (make-instance 'record-enctype
       :name :record
       :lisp-type (getf (cdr enctype) :lisp-type)
       :size (let ((size-option (getf (cdr enctype) :size)))
               (if size-option
                   size-option
                   (let ((offset 0))
                     (maximize
                      (lambda (field)
                        (let ((new-offset (getf (cddr field) :offset))
                              (enctype (enctype-instance (second field))))
                          (when new-offset (setf offset new-offset))
                          (incf offset (size-of-enctype enctype))))
                      (getf (cdr enctype) :fields)))))
       :fields (let ((offset 0))
                 (mapcar
                  (lambda (field)
                    (let ((new-offset (getf (cddr field) :offset))
                          (enctype (enctype-instance (second field))))
                      (when new-offset (setf offset new-offset))
                      (prog1 (make-field :offset offset
                                         :name (first field)
                                         :type enctype)
                        (incf offset (size-of-enctype enctype)))))
                  (getf (cdr enctype) :fields)))))
    ((array)
     (make-instance 'array-enctype
       :name :array
       :element-type (enctype-instance (second enctype))
       :dimensions   (third enctype) ))
    ((string)
     (do ((allocated-size (second enctype))
          (args (cddr enctype))
          (green-length nil)(terminated nil)(if-smaller nil)
          (padded nil)(strip nil)(encoding :standard-ascii)(pad-code 0))
         ((null args)
          (when (or (and terminated green-length) (and terminated padded)
                    (and green-length padded))
            (error "Incompatible string enctype arguments in ~S" enctype))
          (cond
            (green-length (make-instance 'green-length-string-enctype
                            :name :string
                            :allocated-size allocated-size
                            :encoding encoding
                            :pad-code pad-code
                            :green-length green-length))
            (terminated   (make-instance 'terminated-string-enctype
                            :name :string
                            :allocated-size allocated-size
                            :encoding encoding
                            :pad-code pad-code
                            :if-smaller if-smaller))
            (t            (make-instance 'padded-string-enctype
                            :name :string
                            :allocated-size allocated-size
                            :encoding encoding
                            :pad-code pad-code
                            :strip strip))))
       (scase (car args)
         ((:green-length)
          (pop args)
          (setf green-length
                (if (or (listp (car args))
                        (and (symbolp (car args)) (not (keywordp (car args)))))
                    (enctype-instance (pop args))
                    (enctype-instance '(number unsigned 8)))))
         ((:terminated) (pop args) (setf terminated t))
         ((:if-smaller) (pop args) (setf if-smaller t))
         ((:padded)     (pop args) (setf padded t))
         ((:strip)      (pop args) (setf strip  t))
         ((:encoding)   (pop args) (setf encoding (pop args)))
         ((null)        (pop args) (setf pad-code 0))
         ((space)
          (pop args)
          (setf pad-code (aref (standard-encode-string " " :standard-ascii) 0)))
         ;; TODO: replace :standard-ascii by the encoding used by the string.
         (otherwise
          (typecase (car args)
            (character
             (setf pad-code (aref (standard-encode-string
                                   (format nil "~C" (pop args))
                                   :standard-ascii) 0)))
            ;; TODO: replace :standard-ascii by the encoding used by the string.
            (string
             (setf pad-code (aref (standard-encode-string
                                   (pop args) :standard-ascii) 0)))
            (integer
             (setf pad-code (pop args))
             (assert (<= 0 pad-code 255)))
            (otherwise
             (error "Invalid argument to string enctype declation: ~S"
                    (car args)))) ))))
    ((number)
     (let ((encoding (second enctype))
           (size     (third enctype))
           (endian   (fourth enctype))
           (class))
       (setf class (scase (second enctype)
                     ((unsigned)
                      'unsigned-integer-enctype)
                     ((two-complement binary comp comp-4)
                      'two-complement-integer-enctype)
                     ((one-complement)
                      'one-complement-integer-enctype)
                     ((binary-coded-decimal packed-decimal comp-3)
                      'binary-coded-decimal-integer-enctype)
                     ;; ((ieee-float-single comp-1) 'ieee-float-single-enctype)
                     ;; ((ieee-float-double comp-2) 'ieee-float-double-enctype)
                     (otherwise (error "Unknown number encoding ~S." 
                                       encoding))))
       (assert (integerp size))
       (setf endian (cdr (assoc (or endian 'big-endian)
                                '((big-endian . :big)
                                  (little-endian . :little))
                                :test (function string=))))
       (assert endian (endian) "Invalid endian ~S" (fourth enctype))
       (make-instance class  :name :number :size size :endian endian)))
    (otherwise (make-enctype-instance (defined-enctype enctype))))
  ) ;;make-enctype-instance



(defun enctype-instance (enctype)
  "
ENCTYPE: A sexp denoting the enctype.
         enctype ::= 
            (record lisp-type [:size size] (offset name enc-type)...)
            (array  element-enc-type dimensions)
            (string size ...)
            (number encoding [parameters])
            (defined-type ...)
            defined-type
RETURN:  An instance of a subclass of enctype representing the enctype.
"
  (or (gethash enctype *enctype-instances*)
      (setf (gethash enctype *enctype-instances*)
            (typecase enctype
              (symbol (make-enctype-instance (defined-enctype (list enctype))))
              (list
               (scase (first enctype)
                 ((string array record number) (make-enctype-instance enctype))
                 (otherwise (make-enctype-instance (defined-enctype enctype)))))
              (otherwise
               (error "Unknown enctype ~S." enctype)))))) ;;enctype-instance



(defun ENCTYPE-READ (encname enctype stream)
  "
DO:      Read from the STREAM a value of type ENCTYPE.
RETURN:  The decoded list value.
"
  (let ((buffer (make-array (list (size-of-enctype enctype))
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (if (= (length buffer) (read-sequence buffer stream))
        (get-value enctype buffer 0)
        (error "Could not read a ~A (~D bytes)." encname (length buffer))))
  ) ;;ENCTYPE-READ


(defun ENCTYPE-WRITE (encname enctype stream value)
  "
DO:      Write to the STREAM a value of type ENCTYPE.
"
  (declare (ignore encname))
  (let ((buffer (make-array (list (size-of-enctype enctype))
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (set-value enctype buffer 0 value)
    (write-sequence buffer stream))) ;;ENCTYPE-WRITE




;; deftype - like API
;; (def-enctype name lambda-list [[declaration* | documentation]] form*)
;; (def-enctype name (arg...)
;;   (record :lisp-type lisp-type :size size record-options...
;;           :fields ((name type [:offset offset field-options...])...)))
;;
;; defstruct - like  API:
;; (def-encrecord name-and-options [documentation] {slot-description}*)



(defun make-enctype (name args definition)
  (setf (gethash name *enctype-definitions*) (cons args definition))
  name)


(defmacro def-enctype (name args definition)
  "
DO:     Defines an enctype template.
"
  ;; TODO: make it more deftype - like.
  ;; TODO: we cannot create an instance because a def-enctype is actually a template.  But not when there's no argument! So we could create the reader and writer sometimes.
  `(make-enctype ',name ',args ',definition)) ;;def-enctype




(defun even-list-p (list)
  (cond
    ((null list) t)
    ((null (cdr list)) nil)
    (t (even-list-p (cddr list))))) ;;even-list-p



(defmacro def-encrecord (name-and-options &rest doc-and-fields)
  "
DO:     Defines an enctype template for a record type,
        a lisp structure with the same name,
        a reader and a writer functions.
"
  (let ((name (if (symbolp name-and-options) 
                  name-and-options
                  (car name-and-options)))
        (options (if (symbolp name-and-options) 
                     nil
                     (cdr name-and-options)))
        (documentation (when (stringp (car doc-and-fields))
                         (car doc-and-fields)))
        (fields (if (stringp (car doc-and-fields))
                    (cdr doc-and-fields)
                    doc-and-fields)))
    (unless (even-list-p options)
      (error "Odd options ~S." options))
    `(progn
       (def-enctype ,name () 
         (record :name ,name ,@options ,@(unless (getf options :lisp-type) 
                                                 (list :lisp-type name))
                 ,@(when documentation
                         (list :documentation documentation))
                 :fields ,fields))
       (defstruct ,name  ,@(when documentation
                                 (list :documentation documentation))
                  ,@(mapcar (lambda (oof)
                              (let ((enctype (enctype-instance (second oof))))
                                `(,(first oof) ,(default-value enctype)
                                   :type ,(to-lisp-type enctype))))
                            fields))
       (let ((enctype (enctype-instance ',name)))
         (setf (name enctype) ',name)
         (defun ,(conc-symbol "READ-" NAME)  (stream)
           (enctype-read  ',name enctype stream))
         (defun ,(conc-symbol "WRITE-" NAME) (value stream)
           (enctype-write ',name enctype stream value))
         ',name)))) ;;def-encrecord

    


;; ----------------------------------------------------------------------
;;
;; http://www.felgall.com/cob1.htm
;;
;;
;; (cobol picture "s99v99"  usage comp-3)
;; One can have a field encoded as a string containing a lisp integer
;; for example.
;; (cobol pic Z(3)9 usage display) or (string 5 #\space) o (string 12 #\null)
;;
;; Should cobol encoding imply EBCDIC?
;; No because we want to benefit from its expressive power with other
;; character encodings too. The character encoding will be specified
;; globally. See: EXT:CONVERT-STRING-TO-BYTES.
;;
;; usage:
;; ------
;;
;; DISPLAY
;; 
;;     this indicates that the field is stored in an uncompressed,
;;     displayable format. This is actually the default USAGE type and
;;     will be assumed if the field is omitted entirely. A value of 15000
;;     will be stored in a field of this type as x'f1f5f0f0f0' which is
;;     the ebcdic equivalent to 15000. To calculate the length of a
;;     display field count one for each occurrence of A X 9 Z * - + B / ,
;;     . $ and two for each occurrence of G CR DB.
;; 
;; INDEX
;; 
;;     A four byte binary field is used to store an index. The value of
;;     an index field should not be directly accessed. An index is
;;     incremented and decremented in multiples of the size of the field
;;     that the index is on.
;; 
;; POINTER
;; 
;;     A four byte binary field is also used to store a pointer.
;; 
;; BINARY or COMP or COMP-4
;; 
;;     These are all equivalent and define a field as being stored in a
;;     binary compressed format. A binary field to hold 1-4 digits will
;;     take up two bytes, one to hold 5-9 digits will take four bytes,
;;     and one to hold 10-18 digits will take eight bytes. A value of
;;     15000 will be stored in a field of this type as x'00003a98' which
;;     is the hexadecimal equivalent of 15000.
;; 
;; PACKED-DECIMAL or COMP-3
;; 
;;     These are equivalent and define the field as having two digits
;;     compressed into each byte (with the last half byte reserved for
;;     the sign). A value of 15000 will be stored in a field of this type
;;     as x'15000C'. To calculate the length of a packed decimal field
;;     add 1 to the number of digits (9s in the pic'999' field) divide by
;;     two and round halves up to the next byte.
;; 
;; COMP-1
;; 
;;     Identifies the field as a single precision floating point number.
;; 
;; COMP-2
;; 
;;     Identifies the field as a double precision floating point number.
;; 
;;
;; picture:
;; --------
;;
;; one byte for: A X 9 Z * - + B / , . $ and two for each occurrence of G CR DB.
;;
;; A
;; 
;;     corresponds to a single alphabetic character. The content of this
;;     position within the data field is allowed to be any uppercase of
;;     lowercase alphabetic character or a blank. Numerics and other
;;     symbols are not allowed
;; 
;; X
;; 
;;     corresponds to a single alphanumeric character. Any character from
;;     within the entire ebcdic character set can be contained in this
;;     field.
;; 
;; G
;; 
;;     corresponds to two bytes in the field which are being used to hold
;;     a double byte character. For example in Japan this definition
;;     would be used for fields that hold Kanji characters.
;; 
;; 9
;; 
;;     corresponds to a numeric character. Only the numeric values of
;;     zero through nine can be contained in this character.
;; 
;; E
;; 
;;     indicates that the following digits are the exponential for a
;;     floating point number. For example PIC '9v99999e99'.
;; 
;; S
;; 
;;     used to indicate that a numeric field is signed. The sign is
;;     always contained within the upper half byte of the last character
;;     of a display field or the lower half byte of a packed decimal
;;     field. A value of 'C' (12) representing positive and 'D' (13)
;;     negative. Binary fields represent negative numbers using the twos
;;     complement method.
;; 
;; T
;; 
;;     used to indicate that a display numeric field should only insert
;;     the sign into the upper half of the last byte if the value is
;;     negative.
;; 
;; R
;; 
;;     used to indicate that a display numeric field should only insert
;;     the sign into the upper half of the last byte if the value is
;;     positive.
;; 
;; P
;; 
;;     represents a virtual digit in a number that has no storage
;;     allocated to it. For example PIC '99ppp' can contain the value
;;     15000 as x'f1f5' with the number being assumed to represent
;;     thousands.
;; 
;; V
;; 
;;     used to indicate the position of a virtual decimal point. For
;;     example PIC '99999v99' can contain the value 15000 as
;;     x'f1f5f0f0f0f0f0' with the last two digits being assumed to
;;     represent hundredths.
;; 
;; Z
;; 
;;     corresponds to a leading numeric digit that if zero will be
;;     replaced by blank. Usually used to suppress leading zeros on
;;     numbers being printed.
;; 
;; *
;; 
;;     corresponds to a leading numeric digit that if zero will be
;;     replaced by *. Usually used to suppress leading zeros on numbers
;;     being printed on cheques.
;; 
;; -
;; 
;;     formatting character used with numeric fields. This will display
;;     as a blank if the number is zero or positive and will display as
;;     shown if the number is negative.
;; 
;; +
;; 
;;     formatting character used with numeric fields. This will display
;;     as shown if the number is zero or positive and will display as a -
;;     if the number is negative.
;; 
;; CR
;; 
;;     formatting character used with numeric fields. This will display
;;     as a blank if the number is zero or positive and will display as
;;     shown if the number is negative.
;; 
;; DB
;; 
;;     formatting character used with numeric fields. This will display
;;     as shown if the number is zero or positive and will display as CR
;;     if the number is negative.
;; 
;; B
;; 
;;     corresponds to a character that is always blank. Usually used to
;;     insert a blank into the middle of a field that is about to be
;;     output.
;; 
;; / or , or . or $
;; 
;;     formatting characters used in display fields being output. These
;;     values will display exactly as shown. For example the field PIC
;;     '99,999' containing the value x'f1f5f0f0f0' will print as '15,000'.
;;
;; ----------------------------------------------------------------------


;;;; data-encoding.lisp               --                     --          ;;;;
