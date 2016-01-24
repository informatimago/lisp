;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rfc3548.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See package docstring.
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
   "

This packages exports functions to encode an decode text blocks
according to the encoding described in:
RFC3548: The Base16, Base32, and Base64 Data Encodings

base64-encode     (read-byte write-char)
base64-decode     (read-char write-byte &key (ignore-invalid-input nil))
filebase64-encode (read-byte write-char)
filebase64-decode (read-char write-byte &key (ignore-invalid-input nil))
base32-encode     (read-byte write-char)
base32-decode     (read-char write-byte &key (ignore-invalid-input nil))
base16-encode     (read-byte write-char)
base16-decode     (read-char write-byte &key (ignore-invalid-input nil))

READ-BYTE:  A function taking no argument and returning a 
            byte (integer 0 255) or nil for eof.  It may be
            called several times after eof and should keep
            returning nil.

WRITE-BYTE: A function taking one byte (integer 0 255) argument
            used to collect decoded bytes.
 
READ-CHAR:  A function taking no argument and returning a
            character or nil for eof.  It may be called
            several times after eof and should keep returning 
            nil.  Only characters whose code is between 0 and 
            255 should be returned.

WRITE-CHAR: A function taking one character argument, used to
            collect encoded bytes.

IGNORE-INVALID-INPUT:
            When true, any invalid character or padding is ignored
            and processing continues as if it did not occur.
            When nil, such an occurence would raise an error.


base64-encode-bytes     (bytes   &key line-width (new-line +new-line+))
base64-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
filebase64-encode-bytes (bytes   &key line-width (new-line +new-line+))
filebase64-decode-bytes (encoded &key ignore-crlf ignore-invalid-input)
base32-encode-bytes     (bytes   &key line-width (new-line +new-line+))
base32-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
base16-encode-bytes     (bytes   &key line-width (new-line +new-line+))
base16-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)

BYTES:      A vector of (unsigned-byte 8).
ENCODED:    A string.
LINE-WIDTH: NIL or an integer indicating the line width.
            the string new-line will be inserted after that 
            many characters have been written on a given line.
NEW-LINE:   A string contaiing the new-line character or characters.
            the default +new-line+ is (format nil \"~%\").
IGNORE-CRLF:
            When true, ASCII characters LF and CR are not passed to 
            the decoding function. When NIL, they're passed, and
            if invalid input is not ignored, an error would be raised.
IGNORE-INVALID-INPUT:
            Passed to the decoding function. See above.


The encoding functions take a vector of bytes 
and return an encoded string.

The decoding functions take an encoded string 
and return a vector of bytes.

To encode a string, characters must be converted to bytes, and 
to decode a string, bytes must be converted to characters. 
This must be done accordingly to the characeter set encoding.



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
    If not, see <http://www.gnu.org/licenses/>

"))
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
  "
DO:         Encode the stream read with the READ-BYTE closure
            in BASE64 text written with WRITE-CHAR closure.

READ-BYTE:  A function taking no argument and returning a 
            byte (integer 0 255) or nil for eof.  It may be
            called several times after eof and should keep
            returning nil.

WRITE-CHAR: A function taking one character argument, used to
            collect encoded bytes.

"
  (encode64 +base64-encode+ read-byte write-char))



(defun base64-decode     (read-char write-byte &key (ignore-invalid-input nil))
  "
DO:         Decode the BASE64 text stream read with the READ-CHAR
            closure into a binary stream written with WRITE-BYTE
            closure.

READ-CHAR:  A function taking no argument and returning a
            character or nil for eof.  It may be called
            several times after eof and should keep returning 
            nil.  Only characters whose code is between 0 and 
            255 should be returned.

WRITE-BYTE: A function taking one byte (integer 0 255) argument
            used to collect decoded bytes.
 
IGNORE-INVALID-INPUT:
            When true, any invalid character or padding is ignored
            and processing continues as if it did not occur.
            When nil, such an occurence would raise an error.
"
  (decode64 +base64-decode+ 
            (aref +base64-encode+ (padding-code +base64-encode+))
            (padding-code +base64-encode+)
            read-char write-byte ignore-invalid-input))


(defun filebase64-encode (read-byte write-char)
  "
DO:         Encode the stream read with the READ-BYTE closure
            in FILEBASE64 text written with WRITE-CHAR closure.

NOTE:       It's the same encoding as BASE64, but the 62nd and 63rd
            characters are - and _ instead of + and /, thus making it
            usable for file names and URLs.

READ-BYTE:  A function taking no argument and returning a 
            byte (integer 0 255) or nil for eof.  It may be
            called several times after eof and should keep
            returning nil.

WRITE-CHAR: A function taking one character argument, used to
            collect encoded bytes.
"
  (encode64 +filebase64-encode+ read-byte write-char))




(defun filebase64-decode (read-char write-byte &key (ignore-invalid-input nil))
  "
DO:         Decode the FILEBASE64 text stream read with the READ-CHAR
            closure into a binary stream written with WRITE-BYTE
            closure.


NOTE:       It's the same encoding as BASE64, but the 62nd and 63rd
            characters are - and _ instead of + and /, thus making it
            usable for file names and URLs.
 
READ-CHAR:  A function taking no argument and returning a
            character or nil for eof.  It may be called
            several times after eof and should keep returning 
            nil.  Only characters whose code is between 0 and 
            255 should be returned.

WRITE-BYTE: A function taking one byte (integer 0 255) argument
            used to collect decoded bytes.

IGNORE-INVALID-INPUT:
            When true, any invalid character or padding is ignored
            and processing continues as if it did not occur.
            When nil, such an occurence would raise an error.
"
  (decode64 +filebase64-decode+ 
            (aref +filebase64-encode+ (padding-code +filebase64-encode+))
            (padding-code +filebase64-encode+)
            read-char write-byte ignore-invalid-input))


(defun base32-encode     (read-byte write-char)
  "
DO:         Encode the stream read with the READ-BYTE closure
            in BASE32 text written with WRITE-CHAR closure.

READ-BYTE:  A function taking no argument and returning a 
            byte (integer 0 255) or nil for eof.  It may be
            called several times after eof and should keep
            returning nil.

WRITE-CHAR: A function taking one character argument, used to
            collect encoded bytes.
"
  (encode32 +base32-encode+ read-byte write-char))


(defun base32-decode     (read-char write-byte &key (ignore-invalid-input nil))
  "
DO:         Decode the BASE32 text stream read with the READ-CHAR
            closure into a binary stream written with WRITE-BYTE
            closure.

READ-CHAR:  A function taking no argument and returning a
            character or nil for eof.  It may be called
            several times after eof and should keep returning 
            nil.  Only characters whose code is between 0 and 
            255 should be returned.

WRITE-BYTE: A function taking one byte (integer 0 255) argument
            used to collect decoded bytes.
 
IGNORE-INVALID-INPUT:
            When true, any invalid character or padding is ignored
            and processing continues as if it did not occur.
            When nil, such an occurence would raise an error.

"
  (decode32 +base32-decode+ 
            (aref +base32-encode+ (padding-code +base32-encode+))
            (padding-code +base32-encode+)
            read-char write-byte ignore-invalid-input))


(defun base16-encode     (read-byte write-char)
  "
DO:         Encode the stream read with the READ-BYTE closure
            in BASE16 text written with WRITE-CHAR closure.

READ-BYTE:  A function taking no argument and returning a 
            byte (integer 0 255) or nil for eof.  It may be
            called several times after eof and should keep
            returning nil.

WRITE-CHAR: A function taking one character argument, used to
            collect encoded bytes.

"
  (loop
    :for byte = (funcall read-byte)
    :while byte
    :do (progn
          (funcall write-char 
                   (aref "0123456789ABCDEF" (ldb (byte 4 4) byte)))
          (funcall write-char
                   (aref "0123456789ABCDEF" (ldb (byte 4 0) byte))))))


(defun base16-decode     (read-char write-byte &key (ignore-invalid-input nil))
  "
DO:         Decode the BASE16 text stream read with the READ-CHAR
            closure into a binary stream written with WRITE-BYTE
            closure.

READ-CHAR:  A function taking no argument and returning a
            character or nil for eof.  It may be called
            several times after eof and should keep returning 
            nil.  Only characters whose code is between 0 and 
            255 should be returned.

WRITE-BYTE: A function taking one byte (integer 0 255) argument
            used to collect decoded bytes.

IGNORE-INVALID-INPUT:
            When true, any invalid character or padding is ignored
            and processing continues as if it did not occur.
            When nil, such an occurence would raise an error.
"
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
  "
DO:         Encode the BYTES in BASE64 text.

RETURN:     An encoded string.

BYTES:      A vector of (unsigned-byte 8).

LINE-WIDTH: NIL or an integer indicating the line width.
            the string new-line will be inserted after that 
            many characters have been written on a given line.

NEW-LINE:   A string contaiing the new-line character or characters.
            the default +new-line+ is (format nil \"~%\").
"  
  (encode-bytes base64-encode bytes line-width new-line))


(defun base64-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
  "
DO:         Decode the BASE64 encoded string ENCODED.

RETURN:     A decoded vector of (unsigned-byte 8).

ENCODED:    A string.

IGNORE-CRLF:
            When true, ASCII characters LF and CR are not passed to 
            the decoding function. When NIL, they're passed, and
            if invalid input is not ignored, an error would be raised.

IGNORE-INVALID-INPUT:
            Passed to the decoding function. See above.
" (decode-bytes base64-decode encoded ignore-crlf ignore-invalid-input))


(defun filebase64-encode-bytes (bytes   &key line-width (new-line +new-line+))
   "
DO:         Encode the BYTES in FILEBASE64 text.

RETURN:     An encoded string.

BYTES:      A vector of (unsigned-byte 8).

LINE-WIDTH: NIL or an integer indicating the line width.
            the string new-line will be inserted after that 
            many characters have been written on a given line.

NEW-LINE:   A string contaiing the new-line character or characters.
            the default +new-line+ is (format nil \"~%\").
"
  (encode-bytes filebase64-encode bytes line-width new-line))


(defun filebase64-decode-bytes (encoded &key ignore-crlf ignore-invalid-input)
   "
DO:         Decode the FILEBASE64 encoded string ENCODED.

RETURN:     A decoded vector of (unsigned-byte 8).

ENCODED:    A string.

IGNORE-CRLF:
            When true, ASCII characters LF and CR are not passed to 
            the decoding function. When NIL, they're passed, and
            if invalid input is not ignored, an error would be raised.

IGNORE-INVALID-INPUT:
            Passed to the decoding function. See above.
"
  (decode-bytes filebase64-decode encoded ignore-crlf ignore-invalid-input))


(defun base32-encode-bytes     (bytes   &key line-width (new-line +new-line+))
    "
DO:         Encode the BYTES in BASE32 text.

RETURN:     An encoded string.

BYTES:      A vector of (unsigned-byte 8).

LINE-WIDTH: NIL or an integer indicating the line width.
            the string new-line will be inserted after that 
            many characters have been written on a given line.

NEW-LINE:   A string contaiing the new-line character or characters.
            the default +new-line+ is (format nil \"~%\").
"
  (encode-bytes base32-encode bytes line-width new-line))


(defun base32-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
   "
DO:         Decode the BASE32 encoded string ENCODED.

RETURN:     A decoded vector of (unsigned-byte 8).

ENCODED:    A string.

IGNORE-CRLF:
            When true, ASCII characters LF and CR are not passed to 
            the decoding function. When NIL, they're passed, and
            if invalid input is not ignored, an error would be raised.

IGNORE-INVALID-INPUT:
            Passed to the decoding function. See above.
"
  (decode-bytes base32-decode encoded ignore-crlf ignore-invalid-input))


(defun base16-encode-bytes     (bytes   &key line-width (new-line +new-line+))
     "
DO:         Encode the BYTES in BASE16 text.

RETURN:     An encoded string.

BYTES:      A vector of (unsigned-byte 8).

LINE-WIDTH: NIL or an integer indicating the line width.
            the string new-line will be inserted after that 
            many characters have been written on a given line.

NEW-LINE:   A string contaiing the new-line character or characters.
            the default +new-line+ is (format nil \"~%\").
"
  (encode-bytes base16-encode bytes line-width new-line))


(defun base16-decode-bytes     (encoded &key ignore-crlf ignore-invalid-input)
   "
DO:         Decode the BASE16 encoded string ENCODED.

RETURN:     A decoded vector of (unsigned-byte 8).

ENCODED:    A string.

IGNORE-CRLF:
            When true, ASCII characters LF and CR are not passed to 
            the decoding function. When NIL, they're passed, and
            if invalid input is not ignored, an error would be raised.

IGNORE-INVALID-INPUT:
            Passed to the decoding function. See above.
"
  (decode-bytes base16-decode encoded ignore-crlf ignore-invalid-input))


;;;; THE END ;;;;
