;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               rib.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This class is a French "Relevé d'Identité Banquaire", composed of
;;;;    three codes and a control key value: (banque, branch-code, account-
;;;;    number, check-digits).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2008-05-12 <PJB> Added setters (setf bank-code), etc.
;;;;    2004-08-28 <PJB> Converted from C++.
;;;;    1994-12-28 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1994 - 2004
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RIB"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.IBAN" "COMMON-LISP")
  (:EXPORT "CHECK-DIGITS" "SET-ACCOUNT-NUMBER" "ACCOUNT-NUMBER"
           "SET-BRANCH-CODE" "BRANCH-CODE" "SET-BANK-CODE" "BANK-CODE" "SET-RIB"
           "GET-RIB" "RIB")
  (:DOCUMENTATION
   "This class is a French \"Relevé d'Identité Banquaire\", composed of
    three codes and a control key value: (banque, branch-code, account-number, check-digits).

    Copyright Pascal J. Bourguignon 1994 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RIB")



(DEFINE-CONDITION RIB-ERROR (IBAN-ERROR) () (:DOCUMENTATION "A RIB error"))


(DEFCLASS RIB (IBAN)
  ((bank-code  
    :READER bank-code  :INITFORM "00000"       :INITARG :bank-code  :TYPE (STRING 5))
   (BRANCH-CODE
    :READER BRANCH-CODE :INITFORM "00000"       :INITARG :BRANCH-CODE :TYPE (STRING 5))
   (ACCOUNT-NUMBER
    :READER ACCOUNT-NUMBER  :INITFORM "00000000000" :INITARG :ACCOUNT-NUMBER  :TYPE (STRING 11))
   (CHECK-DIGITS             :INITFORM "00"          :INITARG :CHECK-DIGITS     :TYPE (STRING 2))
   (CHECK-DIGITS-CHANGED :READER CHECK-DIGITS-CHANGED :INITFORM T :TYPE BOOLEAN))
  (:DOCUMENTATION "
INVARIANT:  strlen(banque)=5,
            strlen(branch-code)=5,
            strlen(account-number)=11,
            strlen(check-digits)=2,
            for each attribute in {banque,branch-code,account-number,check-digits},
                foreach i in [0,strlen(attribute)-1],
                    attribute()[i] in {'0',...,'9','A',...,'Z'}.
            check-digits=f(banque,branch-code,account-number).
"))


(DEFGENERIC CHECK-DIGITS (SELF))
(DEFGENERIC GET-RIB (SELF &KEY WITH-SPACES))
(DEFGENERIC SET-bank-code (SELF bank-code))
(DEFGENERIC SET-BRANCH-CODE (SELF BRANCH-CODE))
(DEFGENERIC SET-ACCOUNT-NUMBER (SELF ACCOUNT-NUMBER))
(DEFGENERIC SET-RIB (SELF RIB &KEY WITH-CHECK-DIGITS))
(defgeneric (setf bank-code) (bank-code rib))
(defgeneric (setf branch-code) (branch-code rib))
(defgeneric (setf account-number) (account-number rib))


(DEFPARAMETER +ALPHABET-VALUE+ "012345678912345678912345678923456789")
(DEFPARAMETER +ALPHABET-FROM+  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(DEFUN COMPUTE-CHECK-DIGITS (bank-code BRANCH-CODE ACCOUNT-NUMBER)
  (LET ((K (MOD (PARSE-INTEGER 
                 (MAP 'STRING
                      (LAMBDA (CH) (AREF +ALPHABET-VALUE+ 
                                         (POSITION (char-upcase CH)
                                                   +ALPHABET-FROM+)))
                      (CONCATENATE 'STRING bank-code BRANCH-CODE ACCOUNT-NUMBER "00"))
                 :JUNK-ALLOWED NIL) 97)))
    (FORMAT NIL "~2,'0D" (IF (= 0 K) 0 (- 97 K)))))


(DEFMETHOD INITIALIZE-INSTANCE ((SELF RIB) &REST ARGS)
  (DECLARE (IGNORE ARGS))
  (CALL-NEXT-METHOD)
  (SETF (SLOT-VALUE SELF 'bank-code)  (GET-AND-CHECK-ALPHANUM  5 (bank-code  SELF))
        (SLOT-VALUE SELF 'BRANCH-CODE) (GET-AND-CHECK-ALPHANUM  5 (BRANCH-CODE SELF))
        (SLOT-VALUE SELF 'ACCOUNT-NUMBER)  (GET-AND-CHECK-ALPHANUM 11 (ACCOUNT-NUMBER  SELF))
        (SLOT-VALUE SELF 'CHECK-DIGITS)     (COMPUTE-CHECK-DIGITS (bank-code SELF) (BRANCH-CODE SELF)
                                                                  (ACCOUNT-NUMBER SELF))
        (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) NIL)
  SELF)


(DEFMETHOD CHECK-DIGITS ((SELF RIB))
  (WHEN (CHECK-DIGITS-CHANGED SELF)
    (SETF (SLOT-VALUE SELF 'CHECK-DIGITS)
          (COMPUTE-CHECK-DIGITS  (bank-code SELF) (BRANCH-CODE SELF) (ACCOUNT-NUMBER SELF)))
    (SETF (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) NIL))
  (SLOT-VALUE SELF 'CHECK-DIGITS))


(DEFMETHOD GET-RIB ((SELF RIB) &KEY (WITH-SPACES NIL))
  (FORMAT NIL (IF WITH-SPACES "~5S ~5S ~11S ~2S" "~5S~5S~11S~2S")
          (bank-code SELF) (BRANCH-CODE SELF) (ACCOUNT-NUMBER SELF) (CHECK-DIGITS SELF)))


(DEFMETHOD SET-bank-code ((SELF RIB) (bank-code STRING))
  (SETF (SLOT-VALUE SELF 'bank-code) (GET-AND-CHECK-ALPHANUM 5 bank-code)
        (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) T)
  SELF)


(DEFMETHOD SET-BRANCH-CODE ((SELF RIB) (BRANCH-CODE STRING))
  (SETF (SLOT-VALUE SELF 'BRANCH-CODE) (GET-AND-CHECK-ALPHANUM 5 BRANCH-CODE)
        (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) T)
  SELF)


(DEFMETHOD SET-ACCOUNT-NUMBER ((SELF RIB) (ACCOUNT-NUMBER STRING))
  (SETF (SLOT-VALUE SELF 'ACCOUNT-NUMBER) (GET-AND-CHECK-ALPHANUM 11 ACCOUNT-NUMBER)
        (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) T)
  SELF)


(DEFMETHOD SET-RIB ((SELF RIB) (RIB STRING) &KEY (WITH-CHECK-DIGITS NIL))
  (SETF RIB (GET-AND-CHECK-ALPHANUM (IF WITH-CHECK-DIGITS 23 21) RIB))
  (LET* ((B  (SUBSEQ RIB  0  5))
         (G  (SUBSEQ RIB  5  5))
         (C  (SUBSEQ RIB 10 11))
         (K  (WHEN WITH-CHECK-DIGITS (SUBSEQ RIB 21 2)))
         (CK (COMPUTE-CHECK-DIGITS B G C)))
    (WHEN (AND WITH-CHECK-DIGITS (STRING/= K CK))
      (SIGNAL 'RIB-ERROR "Invalid key, given=~S, computed=~S." K CK))
    (SETF (SLOT-VALUE SELF 'bank-code)  B
          (SLOT-VALUE SELF 'BRANCH-CODE) G
          (SLOT-VALUE SELF 'ACCOUNT-NUMBER)  C
          (SLOT-VALUE SELF 'CHECK-DIGITS)     CK
          (SLOT-VALUE SELF 'CHECK-DIGITS-CHANGED) NIL)
    SELF))

(defmethod (setf bank-code)      (bank-code      (self rib))
  (set-bank-code self bank-code))
(defmethod (setf branch-code)    (branch-code    (self rib))
  (set-branch-code self branch-code))
(defmethod (setf account-number) (account-number (self rib))
  (set-account-number self account-number))


(DEFUN TEST ()
  (EVERY (LAMBDA (TEST)
           (LET ((RIB (MAKE-INSTANCE 'RIB
                        :bank-code (FIRST TEST)
                        :BRANCH-CODE (SECOND TEST)
                        :ACCOUNT-NUMBER (THIRD TEST))))
             (STRING= (FOURTH TEST) (CHECK-DIGITS RIB))))
         '(("10011" "00020" "1202196212N" "93")
           ("10011" "00020" "0335091570T" "41")
           ("11899" "05900" "00014503740" "69")
           ("11899" "05900" "00014503760" "09")
           ("30003" "03083" "00051102516" "52")
           ("30003" "03083" "00021000041" "89")
           ("10278" "05900" "00030094740" "14")
           ("10278" "05900" "00014503740" "02")
           ("10278" "05900" "00014503760" "39"))))


;;;; rib.lisp                         --                     --          ;;;;
