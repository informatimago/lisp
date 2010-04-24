;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               iban.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This class is an Internationnal Bank Account Number, 
;;;;    according to European standard.
;;;;
;;;;    <a href=http://www.ecbs.org/iban/iban.htm>IBAN Format</a>
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-10 <PJB> Created.
;;;;BUGS
;;;;    The verification of the country code accepts all existing countries
;;;;    as defined by iso-3166.  Some of these country code are not used
;;;;    (GP --> FR for example).  So an incorrect use of GP is not detected.
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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ISO3166"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.IBAN"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "COMMON-LISP")
  (:EXPORT "GET-AND-CHECK-ALPHANUM" "COMPUTE-IBAN-KEY" "CHECK-IBAN-KEY"
           "GET-IBAN" "GET-KEY" "GET-COUNTRY-CODE" "SET-IBAN" "GET-IBAN" "GET-KEY"
           "GET-COUNTRY-CODE" "CHECK-COUNTRY" "BASIC-FORM" "IBAN" "IBAN-ERROR")
  (:DOCUMENTATION ""))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.IBAN")





(DEFINE-CONDITION IBAN-ERROR (ERROR) () (:DOCUMENTATION "An IBAN error."))



(DEFCLASS IBAN ()
  ((BASIC-FORM
    :READER BASIC-FORM :INITFORM "FR00000000000000000000000"
    :INITARG :BASIC-FORM  :TYPE STRING))
  ) ;;IBAN


(DEFGENERIC GET-AND-CHECK-ALPHANUM (SELF STRING &OPTIONAL LENGTH))
(DEFGENERIC CHECK-COUNTRY (SELF))
(DEFGENERIC GET-COUNTRY-CODE (SELF))
(DEFGENERIC GET-KEY (SELF))
(DEFGENERIC GET-IBAN (SELF &KEY WITH-SPACES))
(DEFGENERIC SET-IBAN (SELF IBAN &KEY WITH-KEY))



(DEFMETHOD INITIALIZE-INSTANCE ((SELF IBAN) &REST ARGS)
  (DECLARE (IGNORE ARGS))
  (CALL-NEXT-METHOD)
  (WHEN  (BASIC-FORM  SELF) 
    (SET-IBAN SELF (BASIC-FORM  SELF)))
  SELF) ;;INITIALIZE-INSTANCE



(DEFMETHOD GET-COUNTRY-CODE ((SELF IBAN))
  "
RETURN: The country code in the IBAN.
"
  (SUBSEQ (BASIC-FORM SELF) 0 2)) ;;GET-COUNTRY-CODE


(DEFMETHOD GET-KEY ((SELF IBAN))
  "
RETURN: The computed key of the IBAN.
"
  (SUBSEQ (SLOT-VALUE SELF 'BASIC-FORM) 2 4)) ;;GET-KEY


(DEFMETHOD GET-IBAN ((SELF IBAN) &KEY (WITH-SPACES NIL))
  "
RETURN: The IBAN, with spaces inserted when WITH-SPACES is true, 
        else in basic form.
"
  (IF WITH-SPACES
      (DO ((IBAN (BASIC-FORM SELF))
           (RES '())
           (I 0 (+ I 4)))
          ((>= (+ I 4) (LENGTH IBAN))
           (PROGN (PUSH (SUBSEQ IBAN I) RES)
                  (APPLY (FUNCTION CONCATENATE) 'STRING (NREVERSE RES))))
        (PUSH (SUBSEQ IBAN I (+ I 4)) RES)
        (PUSH " " RES))
      (COPY-SEQ (BASIC-FORM SELF)))) ;;GET-IBAN



;;     We test and convert to upper case letters, because
;;     the RIB and IBAN may contain only the following characters:
;;         0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ 


(DEFPARAMETER +ALPHABET-FROM+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")


(DEFMETHOD GET-AND-CHECK-ALPHANUM ((SELF IBAN) STRING &OPTIONAL LENGTH)
  (WHEN (AND LENGTH (/= LENGTH (LENGTH STRING)))
    (SIGNAL 'IBAN-ERROR
            "For IBAN ~S:~%   Bad length,  expected ~D, got ~D: ~S" 
            SELF LENGTH (LENGTH STRING) STRING))
  (MAP 'STRING (LAMBDA (CH) 
                 (LET ((INDEX (POSITION CH +ALPHABET-FROM+)))
                   (UNLESS INDEX 
                     (SIGNAL 'IBAN-ERROR
                             "For IBAN ~S:~%    Bad character '~C' in ~S, ~
                              should be alphanumeric." SELF CH STRING))
                   (AREF +ALPHABET-FROM+ (IF (< INDEX 36) INDEX (- INDEX 26)))))
       STRING))


(DEFPARAMETER +COUNTRY-CODES+
  (MAPCAR
   (FUNCTION THIRD)
   (COM.INFORMATIMAGO.COMMON-LISP.ISO3166:GET-COUNTRIES :ONLY-EXISTING T))
  "List of 2-letter country codes.") ;;+COUNTRY-CODES+


(DEFMETHOD CHECK-COUNTRY ((SELF IBAN))
  "
DO:     Checks the country code in the basic-form, 
        and raises an error if not valid.
RAISE:  IBAN-ERROR 
RETURN: SELF
"
  (LET ((CC  (SUBSEQ (BASIC-FORM SELF) 0 2)))
    (UNLESS (MEMBER CC +COUNTRY-CODES+ :TEST (FUNCTION STRING-EQUAL))
      (SIGNAL 'IBAN-ERROR "For IBAN ~S:~%   Bad country code: ~S" SELF CC)))
  SELF) ;;CHECK-COUNTRY


(DEFUN CHECK-IBAN-KEY (IBAN)
  ;; IBAN must be in basic format,  all non alphanumeric characters removed.
  ;; 0- move the first four characters of the IBAN to the end.
  ;; 1- convert the letters into numerics.
  ;; 2- apply MOD 97-10 (ISO 7064) : remainder of n by 97 must be 1
  ;; 3- return T when the IBAN key checks.
  (= 1 (MOD
        (LOOP
           FOR CH ACROSS (CONCATENATE 'STRING (SUBSEQ IBAN 4) (SUBSEQ IBAN 0 4))
           WITH N = 0
           DO (SETF N (+ (* (IF (ALPHA-CHAR-P CH) 100 10) N)
                         (PARSE-INTEGER (STRING CH) :RADIX 36 :JUNK-ALLOWED NIL)))
           FINALLY (RETURN N)) 97))) ;;CHECK-IBAN-KEY


(DEFUN COMPUTE-IBAN-KEY (COUNTRY ACCOUNT)
  ;; ACCOUNT must be in basic format, all non alphanumeric characters removed.
  ;; 0- create artificial IBAN with 00 check sum.
  ;; 1- move the first four characters of the IBAN to the end.
  ;; 2- convert the letters into numerics.
  ;; 3- apply MOD 97-10 (ISO 7064): check sum is 98 - n mod 97.
  ;; 4- return the complete IBAN.
  (FORMAT NIL "~2A~2,'0D~A" 
          COUNTRY
          (- 98 (MOD (LOOP
                        FOR CH ACROSS (CONCATENATE 'STRING  ACCOUNT COUNTRY "00")
                        WITH N = 0
                        DO (SETF N (+ (* (IF (ALPHA-CHAR-P CH) 100 10) N)
                                      (PARSE-INTEGER (STRING CH)
                                                     :RADIX 36 
                                                     :JUNK-ALLOWED NIL)))
                        FINALLY (RETURN N)) 97))
          ACCOUNT)) ;;COMPUTE-IBAN-KEY


(DEFMETHOD SET-IBAN ((SELF IBAN) (IBAN STRING) &KEY (WITH-KEY NIL))
  "
DO:     Change the IBAN. If WITH-KEY is true then the IBAN key is checked
        and an error raised if it is not valid, else the IBAN key is
        computed and substituted.
RETURN: SELF
RAISE:  An IBAN-ERROR when with-key and the key in the IBAN is incorrect.
"
  (SETF IBAN (GET-AND-CHECK-ALPHANUM 
              SELF (REMOVE-IF (COMPLEMENT (FUNCTION ALPHANUMERICP)) IBAN)))
  (SETF (SLOT-VALUE SELF 'BASIC-FORM) 
        (IF WITH-KEY
            (IF (CHECK-IBAN-KEY IBAN)
                (SIGNAL 'IBAN-ERROR
                        "For IBAN ~S~%    Invalid key, given=~S, computed=~S."
                        (SUBSEQ IBAN 2 4)
                        (SUBSEQ (COMPUTE-IBAN-KEY (SUBSEQ IBAN 0 2)
                                                  (SUBSEQ IBAN 4)) 2 4))
                IBAN)
            (COMPUTE-IBAN-KEY (SUBSEQ IBAN 0 2) (SUBSEQ IBAN 4))))
  (CHECK-COUNTRY SELF)
  SELF) ;;SET-IBAN


;;;; iban.lisp                        --                     --          ;;;;
