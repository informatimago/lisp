;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rfc3548.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports functions to encode an decode text blocks
;;;;    according to the encoding described in:
;;;;      RFC3548: The Base16, Base32, and Base64 Data Encodings
;;;;    
;;;;    BASE64-ENCODE     (READ-BYTE WRITE-CHAR)
;;;;    BASE64-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
;;;;    FILEBASE64-ENCODE (READ-BYTE WRITE-CHAR)
;;;;    FILEBASE64-DECODE (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
;;;;    BASE32-ENCODE     (READ-BYTE WRITE-CHAR)
;;;;    BASE32-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
;;;;    BASE16-ENCODE     (READ-BYTE WRITE-CHAR)
;;;;    BASE16-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
;;;;    
;;;;    READ-BYTE:  A FUNCTION TAKING NO ARGUMENT AND RETURNING A 
;;;;                BYTE (INTEGER 0 255) OR NIL FOR EOF.  IT MAY BE
;;;;                CALLED SEVERAL TIMES AFTER EOF AND SHOULD KEEP
;;;;                RETURNING NIL.
;;;;    
;;;;    WRITE-BYTE: A FUNCTION TAKING ONE BYTE (INTEGER 0 255) ARGUMENT
;;;;                USED TO COLLECT DECODED BYTES.
;;;;    
;;;;    READ-CHAR:  A FUNCTION TAKING NO ARGUMENT AND RETURNING A
;;;;                CHARACTER OR NIL FOR EOF.  IT MAY BE CALLED
;;;;                SEVERAL TIMES AFTER EOF AND SHOULD KEEP RETURNING 
;;;;                NIL.  ONLY CHARACTERS WHOSE CODE IS BETWEEN 0 AND 
;;;;                255 SHOULD BE RETURNED.
;;;;    
;;;;    WRITE-CHAR: A FUNCTION TAKING ONE CHARACTER ARGUMENT, USED TO
;;;;                COLLECT ENCODED BYTES.
;;;;    
;;;;    IGNORE-INVALID-INPUT:
;;;;                WHEN TRUE, ANY INVALID CHARACTER OR PADDING IS IGNORED
;;;;                AND PROCESSING CONTINUES AS IF IT DID NOT OCCUR.
;;;;                WHEN NIL, SUCH AN OCCURENCE WOULD RAISE AN ERROR.
;;;;    
;;;;    
;;;;    BASE64-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
;;;;    BASE64-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
;;;;    FILEBASE64-ENCODE-BYTES (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
;;;;    FILEBASE64-DECODE-BYTES (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
;;;;    BASE32-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
;;;;    BASE32-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
;;;;    BASE16-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
;;;;    BASE16-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
;;;;    
;;;;    BYTES:      A VECTOR OF (UNSIGNED-BYTE 8).
;;;;    ENCODED:    A STRING.
;;;;    LINE-WIDTH: NIL OR AN INTEGER INDICATING THE LINE WIDTH.
;;;;                THE STRING NEW-LINE WILL BE INSERTED AFTER THAT 
;;;;                MANY CHARACTERS HAVE BEEN WRITTEN ON A GIVEN LINE.
;;;;    NEW-LINE:   A STRING CONTAIING THE NEW-LINE CHARACTER OR CHARACTERS.
;;;;                THE DEFAULT +NEW-LINE+ IS (FORMAT NIL "~%").
;;;;    IGNORE-CRLF:
;;;;                When true, ASCII characters LF and CR are not passed to 
;;;;                the decoding function. When NIL, they're passed, and
;;;;                if invalid input is not ignored, an error would be raised.
;;;;    IGNORE-INVALID-INPUT:
;;;;                Passed to the decoding function. See above.
;;;;    
;;;;    The encoding functions take a vector of bytes 
;;;;    and return an encoded string.
;;;; 
;;;;    The decoding functions take an encoded string 
;;;;    and return a vector of bytes.
;;;;    
;;;;    To encode a string, characters must be converted to bytes, and 
;;;;    to decode a string, bytes must be converted to characters. 
;;;;    This must be done accordingly to the characeter set encoding.
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-13 <PJB> Removed dependency on external data files for tests.
;;;;                     Ran around a bug in ccl-1.5.
;;;;    2004-08-18 <PJB> Added base16, base32 and filebase64.
;;;;    2004-08-17 <PJB> Created (extracted from antispam.lisp).
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.RFC3548.RFC3548"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:export
   "BASE16-DECODE-BYTES" "BASE16-ENCODE-BYTES" "BASE32-DECODE-BYTES"
   "BASE32-ENCODE-BYTES" "FILEBASE64-DECODE-BYTES" "FILEBASE64-ENCODE-BYTES"
   "BASE64-DECODE-BYTES" "BASE64-ENCODE-BYTES" "BASE16-DECODE" "BASE16-ENCODE"
   "BASE32-DECODE" "BASE32-ENCODE" "FILEBASE64-DECODE" "FILEBASE64-ENCODE"
   "BASE64-DECODE" "BASE64-ENCODE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                "BVSTREAM-WRITE-BYTE" "BVSTREAM-READ-BYTE"
                "WITH-INPUT-FROM-BYTE-VECTOR" "WITH-OUTPUT-TO-BYTE-VECTOR")
  (:documentation
   "This packages exports functions to encode an decode text blocks
    according to the encoding described in:
      RFC3548: The Base16, Base32, and Base64 Data Encodings

    Copyright Pascal J. Bourguignon 2004 - 2012
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.RFC3548.RFC3548")




(defun make-decode-table (code &key (case-sensitive t))
  "
CODE:        A string of length (1+ (expt 2 n)).
RETURN:      An array A:[0..255] --> [-1..(expt 2 n)]
             character-code --> encoding-value
"
  (do* ((table (make-array '(256)
                           :element-type 
                           ;; There's a bug in ccl-1.5 :-(
                           #+(and ccl (not ccl-1.6)) t
                           #-(and ccl (not ccl-1.6)) `(integer -1 ,(1- (length code)))
                           :initial-element -1))
        (index 0                (1+ index))
        (char  (aref code index) (aref code index)))
       ((<= (1- (length code)) index) table)
    (if case-sensitive
        (setf (aref table (char-code char)) index)
        (setf (aref table (char-code (char-downcase char))) index
              (aref table (char-code (char-upcase   char))) index))))
        

(defparameter +base64-encode+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  "An array A[0..64] --> character  giving the character used to encode
   the values from 0 to 63, and the padding character in slot 64.")


(defparameter +base64-decode+ (make-decode-table +base64-encode+)
  "An array A:[0..255] |--> [-1..63] 
        character-code ---> encoding-value")


(defparameter +filebase64-encode+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="
  "An array A[0..64] --> character  giving the character used to encode
   the values from 0 to 63, and the padding character in slot 64.")


(defparameter +filebase64-decode+ (make-decode-table +filebase64-encode+)
  "An array A:[0..255] |--> [-1..63] 
        character-code ---> encoding-value")


(defparameter +base32-encode+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567="
  "An array A[0..32] --> character  giving the character used to encode
   the values from 0 to 31, and the padding character in slot 32.")


(defparameter +base32-decode+ (make-decode-table +base32-encode+ :case-sensitive nil)
  "An array A:[0..255] |--> [-1..31] 
        character-code ---> encoding-value")


(defun padding-code (code) 
  "The position of the padding character in code (the last one)."
  (1- (length code)))


;; encode:
;;     base64:   3*8 --> 4*6
;;     base32:   5*8 --> 8*5
;;     base16:   1*8 --> 2*4

    
(defun encode64 (encode read-byte write-char)
  (macrolet ((out (code) `(funcall write-char (aref encode ,code))))
    (do ((i1 (funcall read-byte) (funcall read-byte))
         (i2 (funcall read-byte) (funcall read-byte))
         (i3 (funcall read-byte) (funcall read-byte))
         (padding (padding-code encode)))
        ((null i3)
         (cond
           ((null i1))
           ((null i2)
            (out (ldb (byte 6 2) i1))
            (out (dpb (ldb (byte 2 0) i1) (byte 2 4) 0))
            (out padding)
            (out padding))
           (t
            (out (ldb (byte 6 2) i1))
            (out (dpb (ldb (byte 2 0) i1) (byte 2 4) (ldb (byte 4 4) i2)))
            (out (dpb (ldb (byte 4 0) i2) (byte 4 2) 0))
            (out  padding)))
         (values))
      ;; aaaaaaaa  bbbbbbbb cccccccc
      ;; aaaaaa aabbbb bbbbcc cccccc
      (out (ldb (byte 6 2) i1))
      (out (dpb (ldb (byte 2 0) i1) (byte 2 4) (ldb (byte 4 4) i2)))
      (out (dpb (ldb (byte 4 0) i2) (byte 4 2) (ldb (byte 2 6) i3)))
      (out (ldb (byte 6 0) i3)))))


(defun encode32 (encode read-byte write-char)
  (macrolet ((out (code) `(funcall write-char (aref encode ,code))))
    (do ((i1 (funcall read-byte) (funcall read-byte))
         (i2 (funcall read-byte) (funcall read-byte))
         (i3 (funcall read-byte) (funcall read-byte))
         (i4 (funcall read-byte) (funcall read-byte))
         (i5 (funcall read-byte) (funcall read-byte))
         (padding (padding-code encode)))
        ((null i5)
         (cond
           ((null i1))
           ((null i2)
            (out (ldb (byte 5 3) i1))
            (out (dpb (ldb (byte 3 0) i1) (byte 3 2) 0))
            (out padding)
            (out padding)
            (out padding)
            (out padding)
            (out padding)
            (out padding))
           ((null i3)
            (out (ldb (byte 5 3) i1))
            (out (dpb (ldb (byte 3 0) i1) (byte 3 2) (ldb (byte 2 6) i2)))
            (out (ldb (byte 5 1) i2))
            (out (dpb (ldb (byte 1 0) i2) (byte 1 4) 0))
            (out padding)
            (out padding)
            (out padding)
            (out padding))
           ((null i4)
            (out (ldb (byte 5 3) i1))
            (out (dpb (ldb (byte 3 0) i1) (byte 3 2) (ldb (byte 2 6) i2)))
            (out (ldb (byte 5 1) i2))
            (out (dpb (ldb (byte 1 0) i2) (byte 1 4) (ldb (byte 4 4) i3)))
            (out (dpb (ldb (byte 4 0) i3) (byte 4 1) 0))
            (out padding)
            (out padding)
            (out padding))
           (t
            (out (ldb (byte 5 3) i1))
            (out (dpb (ldb (byte 3 0) i1) (byte 3 2) (ldb (byte 2 6) i2)))
            (out (ldb (byte 5 1) i2))
            (out (dpb (ldb (byte 1 0) i2) (byte 1 4) (ldb (byte 4 4) i3)))
            (out (dpb (ldb (byte 4 0) i3) (byte 4 1) (ldb (byte 1 7) i4)))
            (out (ldb (byte 5 2) i4))
            (out (dpb (ldb (byte 2 0) i4) (byte 2 3) 0))
            (out padding)))
         (values))
      ;; aaaaaaaa  bbbbbbbb  cccccccc dddddddd  eeeeeeee
      ;; aaaaa aaabb bbbbb bcccc ccccd ddddd ddeee eeeee
      (out (ldb (byte 5 3) i1))
      (out (dpb (ldb (byte 3 0) i1) (byte 3 2) (ldb (byte 2 6) i2)))
      (out (ldb (byte 5 1) i2))
      (out (dpb (ldb (byte 1 0) i2) (byte 1 4) (ldb (byte 4 4) i3)))
      (out (dpb (ldb (byte 4 0) i3) (byte 4 1) (ldb (byte 1 7) i4)))
      (out (ldb (byte 5 2) i4))
      (out (dpb (ldb (byte 2 0) i4) (byte 2 3) (ldb (byte 3 5) i5)))
      (out (ldb (byte 5 0) i5)))))

    
;; decode:
;;     base64:   3*8 <-- 4*6
;;     base32:   5*8 <-- 8*5
;;     base16:   1*8 <-- 2*4


(defmacro with-io ((in out) (decode padchar padcode read-char write-byte 
                                    ignore-invalid-input)
                   &body body)
  `(let ((state 0))
     (flet
         ((,in  () (case state
                     ((0) (do* ((ch   (funcall ,read-char)
                                      (funcall ,read-char))
                                (code (when ch (aref ,decode (char-code ch)))
                                      (when ch (aref ,decode (char-code ch)))))
                               ((or (null ch) (char= ,padchar ch) (<= 0 code))
                                (cond ((null ch) (setf state 2) ,padcode)
                                      ((char= ,padchar ch) (setf state 1) ,padcode)
                                      (t code)))
                            (unless ,ignore-invalid-input
                              (error "RFC3548::DECODE got an invalid input ~
                                    character: ~C" ch))))
                     ((1) (do* ((ch   (funcall ,read-char)
                                      (funcall ,read-char)))
                               ((or (null ch) (char= ,padchar ch))
                                (when (null ch) (setf state 2))
                                ,padcode)
                            (unless ,ignore-invalid-input
                              (error "RFC3548::DECODE got an invalid input ~
                                character: ~C, after ,pad character." ch))))
                     ((2) ,padcode)))
          (,out (code) (funcall ,write-byte code)))
       ,@body)))


(defun decode64 (decode padchar padcode read-char write-byte
                 ignore-invalid-input)
  (with-io (in out) 
      (decode padchar padcode read-char write-byte ignore-invalid-input)
    (do ((i1 (in) (in))
         (i2 (in) (in))
         (i3 (in) (in))
         (i4 (in) (in)))
        ((= i4 padcode) 
         (cond 
           ((= i1 padcode))
           ((= i2 padcode) ;; should not occur
            (unless ignore-invalid-input
              (error "DECODE64 got an invalid padcode sequence."))
            (out (dpb (ldb (byte 6 0) i1) (byte 6 2) 0)))
           ((= i3 padcode)
            (out (dpb (ldb (byte 6 0) i1) (byte 6 2) (ldb (byte 2 4) i2))))
           (t
            (out (dpb (ldb (byte 6 0) i1) (byte 6 2) (ldb (byte 2 4) i2)))
            (out (dpb (ldb (byte 4 0) i2) (byte 4 4) (ldb (byte 4 2) i3)))))
         (values))
      ;; aaaaaa aabbbb bbbbcc cccccc
      ;; aaaaaaaa  bbbbbbbb cccccccc
      (out (dpb (ldb (byte 6 0) i1) (byte 6 2) (ldb (byte 2 4) i2)))
      (out (dpb (ldb (byte 4 0) i2) (byte 4 4) (ldb (byte 4 2) i3)))
      (out (dpb (ldb (byte 2 0) i3) (byte 2 6) (ldb (byte 6 0) i4))))))


(defun decode32 (decode padchar padcode read-char write-byte
                 ignore-invalid-input)
  (with-io (in out)
      (decode padchar padcode read-char write-byte ignore-invalid-input)
    (do ((i1 (in) (in))
         (i2 (in) (in))
         (i3 (in) (in))
         (i4 (in) (in))
         (i5 (in) (in))
         (i6 (in) (in))
         (i7 (in) (in))
         (i8 (in) (in)))
        ((= i8 padcode) 
         (cond 
           ((= i1 padcode))
           ((= i3 padcode)
            (out (dpb (ldb (byte 5 0) i1) (byte 5 3) (ldb (byte 3 2) i2))))
           ((= i5 padcode)
            (out (dpb (ldb (byte 5 0) i1) (byte 5 3) (ldb (byte 3 2) i2)))
            (out (dpb (ldb (byte 2 0) i2) (byte 2 6) 
                      (dpb (ldb (byte 5 0) i3) (byte 5 1) (ldb (byte 1 4) i4)))))
           ((= i6 padcode)
            (out (dpb (ldb (byte 5 0) i1) (byte 5 3) (ldb (byte 3 2) i2)))
            (out (dpb (ldb (byte 2 0) i2) (byte 2 6) 
                      (dpb (ldb (byte 5 0) i3) (byte 5 1) (ldb (byte 1 4) i4))))
            (out (dpb (ldb (byte 4 0) i4) (byte 4 4) (ldb (byte 4 1) i5))))
           (t
            (out (dpb (ldb (byte 5 0) i1) (byte 5 3) (ldb (byte 3 2) i2)))
            (out (dpb (ldb (byte 2 0) i2) (byte 2 6) 
                      (dpb (ldb (byte 5 0) i3) (byte 5 1) (ldb (byte 1 4) i4))))
            (out (dpb (ldb (byte 4 0) i4) (byte 4 4) (ldb (byte 4 1) i5)))
            (out (dpb (ldb (byte 1 0) i5) (byte 1 7) 
                      (dpb (ldb (byte 5 0) i6) (byte 5 2) 
                           (ldb (byte 2 3) i7))))))
         (values))
      ;; aaaaa aaabb bbbbb bcccc ccccd ddddd ddeee eeeee
      ;; aaaaaaaa  bbbbbbbb  cccccccc dddddddd  eeeeeeee
      (out (dpb (ldb (byte 5 0) i1) (byte 5 3) (ldb (byte 3 2) i2)))
      (out (dpb (ldb (byte 2 0) i2) (byte 2 6) 
                (dpb (ldb (byte 5 0) i3) (byte 5 1) (ldb (byte 1 4) i4))))
      (out (dpb (ldb (byte 4 0) i4) (byte 4 4) (ldb (byte 4 1) i5)))
      (out (dpb (ldb (byte 1 0) i5) (byte 1 7) 
                (dpb (ldb (byte 5 0) i6) (byte 5 2) (ldb (byte 2 3) i7))))
      (out (dpb (ldb (byte 3 0) i7) (byte 3 5) (ldb (byte 5 0) i8))))))


(defun base64-encode     (read-byte write-char)
  (encode64 +base64-encode+ read-byte write-char))


(defun base64-decode     (read-char write-byte &key (ignore-invalid-input nil))
  (decode64 +base64-decode+ 
            (aref +base64-encode+ (padding-code +base64-encode+))
            (padding-code +base64-encode+)
            read-char write-byte ignore-invalid-input))


(defun filebase64-encode (read-byte write-char)
  (encode64 +filebase64-encode+ read-byte write-char))


(defun filebase64-decode (read-char write-byte &key (ignore-invalid-input nil))
  (decode64 +filebase64-decode+ 
            (aref +filebase64-encode+ (padding-code +filebase64-encode+))
            (padding-code +filebase64-encode+)
            read-char write-byte ignore-invalid-input))


(defun base32-encode     (read-byte write-char)
  (encode32 +base32-encode+ read-byte write-char))


(defun base32-decode     (read-char write-byte &key (ignore-invalid-input nil))
  (decode32 +base32-decode+ 
            (aref +base32-encode+ (padding-code +base32-encode+))
            (padding-code +base32-encode+)
            read-char write-byte ignore-invalid-input))


(defun base16-encode     (read-byte write-char)
  (loop
     :for byte = (funcall read-byte)
     :while byte
     :do (progn
          (funcall write-char 
                   (aref "0123456789ABCDEF" (ldb (byte 4 4) byte)))
          (funcall write-char
                   (aref "0123456789ABCDEF" (ldb (byte 4 0) byte))))))


(defun base16-decode     (read-char write-byte &key (ignore-invalid-input nil))
  (loop 
     :with high = nil
     :for  ch   = (funcall read-char)
     :while ch 
     :do (let ((low (position ch "0123456789ABCDEF"
                             :test (function char-equal))))
          (if low
              (if high
                  (progn (funcall write-byte (dpb high (byte 4 4) low))
                         (setf high nil))
                  (setf high low))
              (unless ignore-invalid-input 
                (error "BASE16-DECODE got an invalid input character: ~C" ch))))
     :finally (when (and high (not ignore-invalid-input))
               (error "BASE16-DECODE got an odd byte number."))))


(defmacro encode-bytes (encode bytes line-width new-line)
  `(with-output-to-string (out)
     (with-input-from-byte-vector (in ,bytes)
       (let ((column 0)) 
         (,encode
          ;; read-byte:
          (lambda () (let ((byte (bvstream-read-byte in)))
                       (if (eq :eof byte) nil byte)))
          ;; write-char
          (if ,line-width
              (lambda (ch) 
                (write-char ch out)
                (incf column)
                (when (<= ,line-width column)
                  (setf column 0)
                  (princ ,new-line out)))
              (lambda (ch)
                (write-char ch out))))
         (when (and ,line-width (/= 0 column))
           (princ ,new-line out))))))


(defmacro decode-bytes (decode encoded ignore-crlf ignore-invalid-input)
  `(with-output-to-byte-vector (out)
     (with-input-from-string (in ,encoded)
       (,decode
        ;; read-char
        (if ,ignore-crlf
            (lambda () (do ((ch (read-char in nil nil)(read-char in nil nil)))
                           ((or (null ch) (not (member (char-code ch) '(10 13)))) 
                            ch)))
            (lambda () (read-char in nil nil)))
        ;; write-byte
        (lambda (byte) (bvstream-write-byte out byte))
        :ignore-invalid-input ,ignore-invalid-input))))


(defparameter +new-line+ (format nil "~%"))


(defun base64-encode-bytes     (bytes   &key line-width (new-line +new-line+))
  (encode-bytes base64-encode bytes line-width new-line))


(defun base64-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
  (decode-bytes base64-decode encoded ignore-crlf ignore-invalid-input))


(defun filebase64-encode-bytes (bytes   &key line-width (new-line +new-line+))
  (encode-bytes filebase64-encode bytes line-width new-line))


(defun filebase64-decode-bytes (encoded &key ignore-crlf ignore-invalid-input)
  (decode-bytes filebase64-decode encoded ignore-crlf ignore-invalid-input))


(defun base32-encode-bytes     (bytes   &key line-width (new-line +new-line+))
  (encode-bytes base32-encode bytes line-width new-line))


(defun base32-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
  (decode-bytes base32-decode encoded ignore-crlf ignore-invalid-input))


(defun base16-encode-bytes     (bytes   &key line-width (new-line +new-line+))
  (encode-bytes base16-encode bytes line-width new-line))


(defun base16-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
  (decode-bytes base16-decode encoded ignore-crlf ignore-invalid-input))


(defun test-encoding (encoding &key line-width ignore-crlf)
  (let (enc dec data encoded decoded)
    (case encoding
      ((:base16)     (setf enc (function base16-encode-bytes)
                           dec (function base16-decode-bytes)))
      ((:base32)     (setf enc (function base32-encode-bytes)
                           dec (function base32-decode-bytes)))
      ((:base64)     (setf enc (function base64-encode-bytes)
                           dec (function base64-decode-bytes)))
      ((:filebase64) (setf enc (function filebase64-encode-bytes)
                           dec (function filebase64-decode-bytes)))
      (:otherwise (error "Unknown encoding ~S~%" encoding)))
    (setf data (map 'vector (function char-code)
                    "
Hao Wang, logicien americain.

L'algorithme en  question  a  été  publié  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  prédicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704
­ machine à lampes, 32 k  mots  de 36 bits, celle­là même qui vit naître
LISP à la même époque. Le programme  a  été écrit en assembleur (Fortran
existait, mais il ne s'était pas encore imposé)  et  l'auteur estime que
\"there is very little in the program that is not straightforward\".

Il observe que les preuves engendrées sont \"essentiellement des arbres\",
et  annonce  que  la  machine  a  démontre 220 théorèmes du  calcul  des
propositions  (tautologies)  en  3  minutes. Il en tire argument pour la
supériorité  d'une  approche  algorithmique  par  rapport à une approche
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et  Simon (à
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat
qui dure encore...

Cet  algorithme  a  été popularisé par J. McCarthy, comme exemple­fanion
d'application  de LISP. Il figure dans le manuel de la première  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est daté  de  Mars
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publié en 1962 par MIT Press, un des maîtres­livres de l'Informatique.
"
                    #-(and)
                    (with-open-file (in "/home/pascal/tmp/misc/wang.accented"
                                        :direction :input
                                        :if-does-not-exist :error)
                      (loop
                         :for ch = (read-char in nil nil)
                         :while ch
                         :collect ch into result
                         :finally (return result)))))
    (dotimes (i 8)
      (setf encoded (funcall enc data :line-width line-width))
      ;; (print encoded)
      (setf decoded (funcall dec encoded :ignore-crlf ignore-crlf))
      (assert (equalp data decoded))
      (setf data (subseq data 0 (1- (length data)))))))


(defun test ()
  (dolist (enc '(:base16 :base32 :base64 :filebase64)) 
    (dolist (line '(nil t))
      (format t "~&TESTING ~A ~:[~;with lines~]" enc line)
      (finish-output)
      (test-encoding enc :line-width (when line 40) :ignore-crlf line)
      (format t "~40TPASSED.~%")
      (finish-output))))
      

(defun test-base16-encode ()
  (base16-encode
   (lambda () (let ((ch (read-char))) (if (char= #\newline ch) nil (char-code ch))))
   (function write-char)))


(defun test-base16-decode ()
  (base16-decode
   (lambda () (let ((ch (read-char))) (if (char= #\newline ch) nil ch)))
   (lambda (byte) (write-char (code-char byte)))))

;;;; THE END ;;;;
