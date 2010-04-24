;;;; -*- coding:utf-8 -*-
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
;;;;    2004-08-18 <PJB> Added base16, base32 and filebase64.
;;;;    2004-08-17 <PJB> Created (extracted from antispam.lisp).
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RFC3548"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STREAM")
  (:EXPORT
   "BASE16-DECODE-BYTES" "BASE16-ENCODE-BYTES" "BASE32-DECODE-BYTES"
   "BASE32-ENCODE-BYTES" "FILEBASE64-DECODE-BYTES" "FILEBASE64-ENCODE-BYTES"
   "BASE64-DECODE-BYTES" "BASE64-ENCODE-BYTES" "BASE16-DECODE" "BASE16-ENCODE"
   "BASE32-DECODE" "BASE32-ENCODE" "FILEBASE64-DECODE" "FILEBASE64-ENCODE"
   "BASE64-DECODE" "BASE64-ENCODE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
                "BVSTREAM-WRITE-BYTE" "BVSTREAM-READ-BYTE"
                "WITH-INPUT-FROM-BYTE-VECTOR" "WITH-OUTPUT-TO-BYTE-VECTOR")
  (:DOCUMENTATION
   "This packages exports functions to encode an decode text blocks
    according to the encoding described in:
      RFC3548: The Base16, Base32, and Base64 Data Encodings

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RFC3548")




(DEFUN MAKE-DECODE-TABLE (CODE &KEY (CASE-SENSITIVE T))
  "
CODE:        A string of length (1+ (expt 2 n)).
RETURN:      An array A:[0..255] --> [-1..(expt 2 n)]
             character-code --> encoding-value
"
  (DO* ((TABLE (MAKE-ARRAY '(256)
                           :ELEMENT-TYPE `(INTEGER -1 ,(1- (LENGTH CODE)))
                           :INITIAL-ELEMENT -1))
        (INDEX 0                (1+ INDEX))
        (CHAR  (AREF CODE INDEX) (AREF CODE INDEX)))
       ((<= (1- (LENGTH CODE)) INDEX) TABLE)
    (IF CASE-SENSITIVE
        (SETF (AREF TABLE (CHAR-CODE CHAR)) INDEX)
        (SETF (AREF TABLE (CHAR-CODE (CHAR-DOWNCASE CHAR))) INDEX
              (AREF TABLE (CHAR-CODE (CHAR-UPCASE   CHAR))) INDEX)))
  ) ;;MAKE-DECODE-TABLE
        

(DEFPARAMETER +BASE64-ENCODE+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  "An array A[0..64] --> character  giving the character used to encode
   the values from 0 to 63, and the padding character in slot 64.")


(DEFPARAMETER +BASE64-DECODE+ (MAKE-DECODE-TABLE +BASE64-ENCODE+)
  "An array A:[0..255] |--> [-1..63] 
        character-code ---> encoding-value")


(DEFPARAMETER +FILEBASE64-ENCODE+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="
  "An array A[0..64] --> character  giving the character used to encode
   the values from 0 to 63, and the padding character in slot 64.")


(DEFPARAMETER +FILEBASE64-DECODE+ (MAKE-DECODE-TABLE +FILEBASE64-ENCODE+)
  "An array A:[0..255] |--> [-1..63] 
        character-code ---> encoding-value")


(DEFPARAMETER +BASE32-ENCODE+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567="
  "An array A[0..32] --> character  giving the character used to encode
   the values from 0 to 31, and the padding character in slot 32.")


(DEFPARAMETER +BASE32-DECODE+ (MAKE-DECODE-TABLE +BASE32-ENCODE+ :CASE-SENSITIVE NIL)
  "An array A:[0..255] |--> [-1..31] 
        character-code ---> encoding-value")


(DEFUN PADDING-CODE (CODE) 
  "The position of the padding character in code (the last one)."
  (1- (LENGTH CODE)))


;; encode:
;;     base64:   3*8 --> 4*6
;;     base32:   5*8 --> 8*5
;;     base16:   1*8 --> 2*4

    
(DEFUN ENCODE64 (ENCODE READ-BYTE WRITE-CHAR)
  (MACROLET ((OUT (CODE) `(FUNCALL WRITE-CHAR (AREF ENCODE ,CODE))))
    (DO ((I1 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I2 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I3 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (PADDING (PADDING-CODE ENCODE)))
        ((NULL I3)
         (COND
           ((NULL I1))
           ((NULL I2)
            (OUT (LDB (BYTE 6 2) I1))
            (OUT (DPB (LDB (BYTE 2 0) I1) (BYTE 2 4) 0))
            (OUT PADDING)
            (OUT PADDING))
           (T
            (OUT (LDB (BYTE 6 2) I1))
            (OUT (DPB (LDB (BYTE 2 0) I1) (BYTE 2 4) (LDB (BYTE 4 4) I2)))
            (OUT (DPB (LDB (BYTE 4 0) I2) (BYTE 4 2) 0))
            (OUT  PADDING)))
         (VALUES))
      ;; aaaaaaaa  bbbbbbbb cccccccc
      ;; aaaaaa aabbbb bbbbcc cccccc
      (OUT (LDB (BYTE 6 2) I1))
      (OUT (DPB (LDB (BYTE 2 0) I1) (BYTE 2 4) (LDB (BYTE 4 4) I2)))
      (OUT (DPB (LDB (BYTE 4 0) I2) (BYTE 4 2) (LDB (BYTE 2 6) I3)))
      (OUT (LDB (BYTE 6 0) I3))))) ;;ENCODE64


(DEFUN ENCODE32 (ENCODE READ-BYTE WRITE-CHAR)
  (MACROLET ((OUT (CODE) `(FUNCALL WRITE-CHAR (AREF ENCODE ,CODE))))
    (DO ((I1 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I2 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I3 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I4 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (I5 (FUNCALL READ-BYTE) (FUNCALL READ-BYTE))
         (PADDING (PADDING-CODE ENCODE)))
        ((NULL I5)
         (COND
           ((NULL I1))
           ((NULL I2)
            (OUT (LDB (BYTE 5 3) I1))
            (OUT (DPB (LDB (BYTE 3 0) I1) (BYTE 3 2) 0))
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING))
           ((NULL I3)
            (OUT (LDB (BYTE 5 3) I1))
            (OUT (DPB (LDB (BYTE 3 0) I1) (BYTE 3 2) (LDB (BYTE 2 6) I2)))
            (OUT (LDB (BYTE 5 1) I2))
            (OUT (DPB (LDB (BYTE 1 0) I2) (BYTE 1 4) 0))
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING))
           ((NULL I4)
            (OUT (LDB (BYTE 5 3) I1))
            (OUT (DPB (LDB (BYTE 3 0) I1) (BYTE 3 2) (LDB (BYTE 2 6) I2)))
            (OUT (LDB (BYTE 5 1) I2))
            (OUT (DPB (LDB (BYTE 1 0) I2) (BYTE 1 4) (LDB (BYTE 4 4) I3)))
            (OUT (DPB (LDB (BYTE 4 0) I3) (BYTE 4 1) 0))
            (OUT PADDING)
            (OUT PADDING)
            (OUT PADDING))
           (T
            (OUT (LDB (BYTE 5 3) I1))
            (OUT (DPB (LDB (BYTE 3 0) I1) (BYTE 3 2) (LDB (BYTE 2 6) I2)))
            (OUT (LDB (BYTE 5 1) I2))
            (OUT (DPB (LDB (BYTE 1 0) I2) (BYTE 1 4) (LDB (BYTE 4 4) I3)))
            (OUT (DPB (LDB (BYTE 4 0) I3) (BYTE 4 1) (LDB (BYTE 1 7) I4)))
            (OUT (LDB (BYTE 5 2) I4))
            (OUT (DPB (LDB (BYTE 2 0) I4) (BYTE 2 3) 0))
            (OUT PADDING)))
         (VALUES))
      ;; aaaaaaaa  bbbbbbbb  cccccccc dddddddd  eeeeeeee
      ;; aaaaa aaabb bbbbb bcccc ccccd ddddd ddeee eeeee
      (OUT (LDB (BYTE 5 3) I1))
      (OUT (DPB (LDB (BYTE 3 0) I1) (BYTE 3 2) (LDB (BYTE 2 6) I2)))
      (OUT (LDB (BYTE 5 1) I2))
      (OUT (DPB (LDB (BYTE 1 0) I2) (BYTE 1 4) (LDB (BYTE 4 4) I3)))
      (OUT (DPB (LDB (BYTE 4 0) I3) (BYTE 4 1) (LDB (BYTE 1 7) I4)))
      (OUT (LDB (BYTE 5 2) I4))
      (OUT (DPB (LDB (BYTE 2 0) I4) (BYTE 2 3) (LDB (BYTE 3 5) I5)))
      (OUT (LDB (BYTE 5 0) I5))))) ;;ENCODE32

    
;; decode:
;;     base64:   3*8 <-- 4*6
;;     base32:   5*8 <-- 8*5
;;     base16:   1*8 <-- 2*4


(DEFMACRO WITH-IO ((IN OUT) (DECODE PADCHAR PADCODE READ-CHAR WRITE-BYTE 
                                    IGNORE-INVALID-INPUT)
                   &BODY BODY)
  `(LET ((STATE 0))
     (FLET
         ((,IN  () (CASE STATE
                     ((0) (DO* ((CH   (FUNCALL ,READ-CHAR)
                                      (FUNCALL ,READ-CHAR))
                                (CODE (WHEN CH (AREF ,DECODE (CHAR-CODE CH)))
                                      (WHEN CH (AREF ,DECODE (CHAR-CODE CH)))))
                               ((OR (NULL CH) (CHAR= ,PADCHAR CH) (<= 0 CODE))
                                (COND ((NULL CH) (SETF STATE 2) ,PADCODE)
                                      ((CHAR= ,PADCHAR CH) (SETF STATE 1) ,PADCODE)
                                      (T CODE)))
                            (UNLESS ,IGNORE-INVALID-INPUT
                              (ERROR "RFC3548::DECODE got an invalid input ~
                                    character: ~C" CH))))
                     ((1) (DO* ((CH   (FUNCALL ,READ-CHAR)
                                      (FUNCALL ,READ-CHAR)))
                               ((OR (NULL CH) (CHAR= ,PADCHAR CH))
                                (WHEN (NULL CH) (SETF STATE 2))
                                ,PADCODE)
                            (UNLESS ,IGNORE-INVALID-INPUT
                              (ERROR "RFC3548::DECODE got an invalid input ~
                                character: ~C, after ,pad character." CH))))
                     ((2) ,PADCODE)))
          (,OUT (CODE) (FUNCALL ,WRITE-BYTE CODE)))
       ,@BODY))) ;;WITH-IO


(DEFUN DECODE64 (DECODE PADCHAR PADCODE READ-CHAR WRITE-BYTE
                 IGNORE-INVALID-INPUT)
  (WITH-IO (IN OUT) 
      (DECODE PADCHAR PADCODE READ-CHAR WRITE-BYTE IGNORE-INVALID-INPUT)
    (DO ((I1 (IN) (IN))
         (I2 (IN) (IN))
         (I3 (IN) (IN))
         (I4 (IN) (IN)))
        ((= I4 PADCODE) 
         (COND 
           ((= I1 PADCODE))
           ((= I2 PADCODE) ;; should not occur
            (UNLESS IGNORE-INVALID-INPUT
              (ERROR "DECODE64 got an invalid padcode sequence."))
            (OUT (DPB (LDB (BYTE 6 0) I1) (BYTE 6 2) 0)))
           ((= I3 PADCODE)
            (OUT (DPB (LDB (BYTE 6 0) I1) (BYTE 6 2) (LDB (BYTE 2 4) I2))))
           (T
            (OUT (DPB (LDB (BYTE 6 0) I1) (BYTE 6 2) (LDB (BYTE 2 4) I2)))
            (OUT (DPB (LDB (BYTE 4 0) I2) (BYTE 4 4) (LDB (BYTE 4 2) I3)))))
         (VALUES))
      ;; aaaaaa aabbbb bbbbcc cccccc
      ;; aaaaaaaa  bbbbbbbb cccccccc
      (OUT (DPB (LDB (BYTE 6 0) I1) (BYTE 6 2) (LDB (BYTE 2 4) I2)))
      (OUT (DPB (LDB (BYTE 4 0) I2) (BYTE 4 4) (LDB (BYTE 4 2) I3)))
      (OUT (DPB (LDB (BYTE 2 0) I3) (BYTE 2 6) (LDB (BYTE 6 0) I4)))
      ))) ;;DECODE64


(DEFUN DECODE32 (DECODE PADCHAR PADCODE READ-CHAR WRITE-BYTE
                 IGNORE-INVALID-INPUT)
  (WITH-IO (IN OUT)
      (DECODE PADCHAR PADCODE READ-CHAR WRITE-BYTE IGNORE-INVALID-INPUT)
    (DO ((I1 (IN) (IN))
         (I2 (IN) (IN))
         (I3 (IN) (IN))
         (I4 (IN) (IN))
         (I5 (IN) (IN))
         (I6 (IN) (IN))
         (I7 (IN) (IN))
         (I8 (IN) (IN)))
        ((= I8 PADCODE) 
         (COND 
           ((= I1 PADCODE))
           ((= I3 PADCODE)
            (OUT (DPB (LDB (BYTE 5 0) I1) (BYTE 5 3) (LDB (BYTE 3 2) I2))))
           ((= I5 PADCODE)
            (OUT (DPB (LDB (BYTE 5 0) I1) (BYTE 5 3) (LDB (BYTE 3 2) I2)))
            (OUT (DPB (LDB (BYTE 2 0) I2) (BYTE 2 6) 
                      (DPB (LDB (BYTE 5 0) I3) (BYTE 5 1) (LDB (BYTE 1 4) I4)))))
           ((= I6 PADCODE)
            (OUT (DPB (LDB (BYTE 5 0) I1) (BYTE 5 3) (LDB (BYTE 3 2) I2)))
            (OUT (DPB (LDB (BYTE 2 0) I2) (BYTE 2 6) 
                      (DPB (LDB (BYTE 5 0) I3) (BYTE 5 1) (LDB (BYTE 1 4) I4))))
            (OUT (DPB (LDB (BYTE 4 0) I4) (BYTE 4 4) (LDB (BYTE 4 1) I5))))
           (T
            (OUT (DPB (LDB (BYTE 5 0) I1) (BYTE 5 3) (LDB (BYTE 3 2) I2)))
            (OUT (DPB (LDB (BYTE 2 0) I2) (BYTE 2 6) 
                      (DPB (LDB (BYTE 5 0) I3) (BYTE 5 1) (LDB (BYTE 1 4) I4))))
            (OUT (DPB (LDB (BYTE 4 0) I4) (BYTE 4 4) (LDB (BYTE 4 1) I5)))
            (OUT (DPB (LDB (BYTE 1 0) I5) (BYTE 1 7) 
                      (DPB (LDB (BYTE 5 0) I6) (BYTE 5 2) 
                           (LDB (BYTE 2 3) I7))))))
         (VALUES))
      ;; aaaaa aaabb bbbbb bcccc ccccd ddddd ddeee eeeee
      ;; aaaaaaaa  bbbbbbbb  cccccccc dddddddd  eeeeeeee
      (OUT (DPB (LDB (BYTE 5 0) I1) (BYTE 5 3) (LDB (BYTE 3 2) I2)))
      (OUT (DPB (LDB (BYTE 2 0) I2) (BYTE 2 6) 
                (DPB (LDB (BYTE 5 0) I3) (BYTE 5 1) (LDB (BYTE 1 4) I4))))
      (OUT (DPB (LDB (BYTE 4 0) I4) (BYTE 4 4) (LDB (BYTE 4 1) I5)))
      (OUT (DPB (LDB (BYTE 1 0) I5) (BYTE 1 7) 
                (DPB (LDB (BYTE 5 0) I6) (BYTE 5 2) (LDB (BYTE 2 3) I7))))
      (OUT (DPB (LDB (BYTE 3 0) I7) (BYTE 3 5) (LDB (BYTE 5 0) I8)))
      ))) ;;DECODE32


(DEFUN BASE64-ENCODE     (READ-BYTE WRITE-CHAR)
  (ENCODE64 +BASE64-ENCODE+ READ-BYTE WRITE-CHAR))


(DEFUN BASE64-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
  (DECODE64 +BASE64-DECODE+ 
            (AREF +BASE64-ENCODE+ (PADDING-CODE +BASE64-ENCODE+))
            (PADDING-CODE +BASE64-ENCODE+)
            READ-CHAR WRITE-BYTE IGNORE-INVALID-INPUT)) ;;BASE64-DECODE


(DEFUN FILEBASE64-ENCODE (READ-BYTE WRITE-CHAR)
  (ENCODE64 +FILEBASE64-ENCODE+ READ-BYTE WRITE-CHAR))


(DEFUN FILEBASE64-DECODE (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
  (DECODE64 +FILEBASE64-DECODE+ 
            (AREF +FILEBASE64-ENCODE+ (PADDING-CODE +FILEBASE64-ENCODE+))
            (PADDING-CODE +FILEBASE64-ENCODE+)
            READ-CHAR WRITE-BYTE IGNORE-INVALID-INPUT)) ;;FILEBASE64-DECODE


(DEFUN BASE32-ENCODE     (READ-BYTE WRITE-CHAR)
  (ENCODE32 +BASE32-ENCODE+ READ-BYTE WRITE-CHAR))


(DEFUN BASE32-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
  (DECODE32 +BASE32-DECODE+ 
            (AREF +BASE32-ENCODE+ (PADDING-CODE +BASE32-ENCODE+))
            (PADDING-CODE +BASE32-ENCODE+)
            READ-CHAR WRITE-BYTE IGNORE-INVALID-INPUT)) ;;BASE32-DECODE


(DEFUN BASE16-ENCODE     (READ-BYTE WRITE-CHAR)
  (LOOP FOR BYTE = (FUNCALL READ-BYTE)
     WHILE BYTE
     DO (PROGN
          (FUNCALL WRITE-CHAR 
                   (AREF "0123456789ABCDEF" (LDB (BYTE 4 4) BYTE)))
          (FUNCALL WRITE-CHAR
                   (AREF "0123456789ABCDEF" (LDB (BYTE 4 0) BYTE)))))
  ) ;;BASE16-ENCODE


(DEFUN BASE16-DECODE     (READ-CHAR WRITE-BYTE &KEY (IGNORE-INVALID-INPUT NIL))
  (LOOP 
     WITH HIGH = NIL
     FOR  CH   = (FUNCALL READ-CHAR)
     WHILE CH 
     DO (LET ((LOW (POSITION CH "0123456789ABCDEF"
                             :TEST (FUNCTION CHAR-EQUAL))))
          (IF LOW
              (IF HIGH
                  (PROGN (FUNCALL WRITE-BYTE (DPB HIGH (BYTE 4 4) LOW))
                         (SETF HIGH NIL))
                  (SETF HIGH LOW))
              (UNLESS IGNORE-INVALID-INPUT 
                (ERROR "BASE16-DECODE got an invalid input character: ~C" CH))))
     FINALLY (WHEN (AND HIGH (NOT IGNORE-INVALID-INPUT))
               (ERROR "BASE16-DECODE got an odd byte number.")))
  ) ;;BASE16-DECODE


(DEFMACRO ENCODE-BYTES (ENCODE BYTES LINE-WIDTH NEW-LINE)
  `(WITH-OUTPUT-TO-STRING (OUT)
     (WITH-INPUT-FROM-BYTE-VECTOR (IN ,BYTES)
       (LET ((COLUMN 0)) 
         (,ENCODE
          ;; read-byte:
          (LAMBDA () (LET ((BYTE (BVSTREAM-READ-BYTE IN)))
                       (IF (EQ :EOF BYTE) NIL BYTE)))
          ;; write-char
          (IF ,LINE-WIDTH
              (LAMBDA (CH) 
                (WRITE-CHAR CH OUT)
                (INCF COLUMN)
                (WHEN (<= ,LINE-WIDTH COLUMN)
                  (SETF COLUMN 0)
                  (PRINC ,NEW-LINE OUT)))
              (LAMBDA (CH)
                (WRITE-CHAR CH OUT))))
         (WHEN (AND ,LINE-WIDTH (/= 0 COLUMN))
           (PRINC ,NEW-LINE OUT)))))) ;;ENCODE-BYTES


(DEFMACRO DECODE-BYTES (DECODE ENCODED IGNORE-CRLF IGNORE-INVALID-INPUT)
  `(WITH-OUTPUT-TO-BYTE-VECTOR (OUT)
     (WITH-INPUT-FROM-STRING (IN ,ENCODED)
       (,DECODE
        ;; read-char
        (IF ,IGNORE-CRLF
            (LAMBDA () (DO ((CH (READ-CHAR IN NIL NIL)(READ-CHAR IN NIL NIL)))
                           ((OR (NULL CH) (NOT (MEMBER (CHAR-CODE CH) '(10 13)))) 
                            CH)))
            (LAMBDA () (READ-CHAR IN NIL NIL)))
        ;; write-byte
        (LAMBDA (BYTE) (BVSTREAM-WRITE-BYTE OUT BYTE))
        :IGNORE-INVALID-INPUT ,IGNORE-INVALID-INPUT)))) ;;DECODE-BYTES


(DEFPARAMETER +NEW-LINE+ (FORMAT NIL "~%"))


(DEFUN BASE64-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
  (ENCODE-BYTES BASE64-ENCODE BYTES LINE-WIDTH NEW-LINE))


(DEFUN BASE64-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
  (DECODE-BYTES BASE64-DECODE ENCODED IGNORE-CRLF IGNORE-INVALID-INPUT))


(DEFUN FILEBASE64-ENCODE-BYTES (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
  (ENCODE-BYTES FILEBASE64-ENCODE BYTES LINE-WIDTH NEW-LINE))


(DEFUN FILEBASE64-DECODE-BYTES (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
  (DECODE-BYTES FILEBASE64-DECODE ENCODED IGNORE-CRLF IGNORE-INVALID-INPUT))


(DEFUN BASE32-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
  (ENCODE-BYTES BASE32-ENCODE BYTES LINE-WIDTH NEW-LINE))


(DEFUN BASE32-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
  (DECODE-BYTES BASE32-DECODE ENCODED IGNORE-CRLF IGNORE-INVALID-INPUT))


(DEFUN BASE16-ENCODE-BYTES     (BYTES   &KEY LINE-WIDTH (NEW-LINE +NEW-LINE+))
  (ENCODE-BYTES BASE16-ENCODE BYTES LINE-WIDTH NEW-LINE))


(DEFUN BASE16-DECODE-BYTES     (ENCODED &KEY IGNORE-CRLF IGNORE-INVALID-INPUT)
  (DECODE-BYTES BASE16-DECODE ENCODED IGNORE-CRLF IGNORE-INVALID-INPUT))


(DEFUN TEST-ENCODING (ENCODING &KEY LINE-WIDTH IGNORE-CRLF)
  (LET (ENC DEC DATA ENCODED DECODED)
    (CASE ENCODING
      ((:BASE16)     (SETF ENC (FUNCTION BASE16-ENCODE-BYTES)
                           DEC (FUNCTION BASE16-DECODE-BYTES)))
      ((:BASE32)     (SETF ENC (FUNCTION BASE32-ENCODE-BYTES)
                           DEC (FUNCTION BASE32-DECODE-BYTES)))
      ((:BASE64)     (SETF ENC (FUNCTION BASE64-ENCODE-BYTES)
                           DEC (FUNCTION BASE64-DECODE-BYTES)))
      ((:FILEBASE64) (SETF ENC (FUNCTION FILEBASE64-ENCODE-BYTES)
                           DEC (FUNCTION FILEBASE64-DECODE-BYTES)))
      (:OTHERWISE (ERROR "Unknown encoding ~S~%" ENCODING)))
    (SETF DATA (MAP 'VECTOR (FUNCTION CHAR-CODE)
                    (WITH-OPEN-FILE (IN "/home/pascal/tmp/misc/wang.accented"
                                        :DIRECTION :INPUT
                                        :IF-DOES-NOT-EXIST :ERROR)
                      (LOOP FOR CH = (READ-CHAR IN NIL NIL)
                         WHILE CH
                         COLLECT CH INTO RESULT
                         FINALLY (RETURN RESULT)))))
    (DOTIMES (I 8)
      (SETF ENCODED (FUNCALL ENC DATA :LINE-WIDTH LINE-WIDTH))
      ;; (print encoded)
      (SETF DECODED (FUNCALL DEC ENCODED :IGNORE-CRLF IGNORE-CRLF))
      (ASSERT (EQUALP DATA DECODED))
      (SETF DATA (SUBSEQ DATA 0 (1- (LENGTH DATA))))))) ;;TEST-ENCODING


(DEFUN TEST ()
  (DOLIST (ENC '(:BASE16 :BASE32 :BASE64 :FILEBASE64)) 
    (DOLIST (LINE '(NIL T))
      (FORMAT T "~&TESTING ~A ~:[~;with lines~]" ENC LINE)
      (FINISH-OUTPUT)
      (TEST-ENCODING ENC :LINE-WIDTH (WHEN LINE 40) :IGNORE-CRLF LINE)
      (FORMAT T "~40TPASSED.~%")
      (FINISH-OUTPUT)))) ;;TEST
      

(DEFUN TEST-BASE16-ENCODE ()
  (BASE16-ENCODE
   (LAMBDA () (LET ((CH (READ-CHAR))) (IF (CHAR= #\NEWLINE CH) NIL (CHAR-CODE CH))))
   (FUNCTION WRITE-CHAR)))


(DEFUN TEST-BASE16-DECODE ()
  (BASE16-DECODE
   (LAMBDA () (LET ((CH (READ-CHAR))) (IF (CHAR= #\NEWLINE CH) NIL CH)))
   (LAMBDA (BYTE) (WRITE-CHAR (CODE-CHAR BYTE)))))


;; Local Variables:
;; eval: (cl-indent 'with-input-from-byte-vector 1)
;; eval: (cl-indent 'with-output-to-byte-vector  1)
;; eval: (cl-indent 'with-io 2)
;; End:
;;;; rfc3548.lisp                     --                     --          ;;;;
