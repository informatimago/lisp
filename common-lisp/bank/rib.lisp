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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1994 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.BANK.RIB"
  (:use  "COMMON-LISP"
         "COM.INFORMATIMAGO.COMMON-LISP.BANK.IBAN")
  (:export "CHECK-DIGITS" "SET-ACCOUNT-NUMBER" "ACCOUNT-NUMBER"
           "SET-BRANCH-CODE" "BRANCH-CODE" "SET-BANK-CODE" "BANK-CODE" "SET-RIB"
           "GET-RIB" "RIB")
  (:documentation
   "This class is a French \"Relevé d'Identité Banquaire\", composed of
    three codes and a control key value: (banque, branch-code, account-number, check-digits).

    Copyright Pascal J. Bourguignon 1994 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.BANK.RIB")



(define-condition rib-error (iban-error) () (:documentation "A RIB error"))


(defclass rib (iban)
  ((bank-code  
    :reader bank-code  :initform "00000"       :initarg :bank-code  :type (string 5))
   (branch-code
    :reader branch-code :initform "00000"       :initarg :branch-code :type (string 5))
   (account-number
    :reader account-number  :initform "00000000000" :initarg :account-number  :type (string 11))
   (check-digits             :initform "00"          :initarg :check-digits     :type (string 2))
   (check-digits-changed :reader check-digits-changed :initform t :type boolean))
  (:documentation "
INVARIANT:  strlen(banque)=5,
            strlen(branch-code)=5,
            strlen(account-number)=11,
            strlen(check-digits)=2,
            for each attribute in {banque,branch-code,account-number,check-digits},
                foreach i in [0,strlen(attribute)-1],
                    attribute()[i] in {'0',...,'9','A',...,'Z'}.
            check-digits=f(banque,branch-code,account-number).
"))


(defgeneric check-digits (self))
(defgeneric get-rib (self &key with-spaces))
(defgeneric set-bank-code (self bank-code))
(defgeneric set-branch-code (self branch-code))
(defgeneric set-account-number (self account-number))
(defgeneric set-rib (self rib &key with-check-digits))
(defgeneric (setf bank-code) (bank-code rib))
(defgeneric (setf branch-code) (branch-code rib))
(defgeneric (setf account-number) (account-number rib))


(defparameter +alphabet-value+ "012345678912345678912345678923456789")
(defparameter +alphabet-from+  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun compute-check-digits (bank-code branch-code account-number)
  (let ((k (mod (parse-integer 
                 (map 'string
                      (lambda (ch) (aref +alphabet-value+ 
                                         (position (char-upcase ch)
                                                   +alphabet-from+)))
                      (concatenate 'string bank-code branch-code account-number "00"))
                 :junk-allowed nil) 97)))
    (format nil "~2,'0D" (if (= 0 k) 0 (- 97 k)))))


(defmethod initialize-instance ((self rib) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (slot-value self 'bank-code)  (get-and-check-alphanum  5 (bank-code  self))
        (slot-value self 'branch-code) (get-and-check-alphanum  5 (branch-code self))
        (slot-value self 'account-number)  (get-and-check-alphanum 11 (account-number  self))
        (slot-value self 'check-digits)     (compute-check-digits (bank-code self) (branch-code self)
                                                                  (account-number self))
        (slot-value self 'check-digits-changed) nil)
  self)


(defmethod check-digits ((self rib))
  (when (check-digits-changed self)
    (setf (slot-value self 'check-digits)
          (compute-check-digits  (bank-code self) (branch-code self) (account-number self)))
    (setf (slot-value self 'check-digits-changed) nil))
  (slot-value self 'check-digits))


(defmethod get-rib ((self rib) &key (with-spaces nil))
  (format nil (if with-spaces "~5S ~5S ~11S ~2S" "~5S~5S~11S~2S")
          (bank-code self) (branch-code self) (account-number self) (check-digits self)))


(defmethod set-bank-code ((self rib) (bank-code string))
  (setf (slot-value self 'bank-code) (get-and-check-alphanum 5 bank-code)
        (slot-value self 'check-digits-changed) t)
  self)


(defmethod set-branch-code ((self rib) (branch-code string))
  (setf (slot-value self 'branch-code) (get-and-check-alphanum 5 branch-code)
        (slot-value self 'check-digits-changed) t)
  self)


(defmethod set-account-number ((self rib) (account-number string))
  (setf (slot-value self 'account-number) (get-and-check-alphanum 11 account-number)
        (slot-value self 'check-digits-changed) t)
  self)


(defmethod set-rib ((self rib) (rib string) &key (with-check-digits nil))
  (setf rib (get-and-check-alphanum (if with-check-digits 23 21) rib))
  (let* ((b  (subseq rib  0  5))
         (g  (subseq rib  5  5))
         (c  (subseq rib 10 11))
         (k  (when with-check-digits (subseq rib 21 2)))
         (ck (compute-check-digits b g c)))
    (when (and with-check-digits (string/= k ck))
      (signal 'rib-error "Invalid key, given=~S, computed=~S." k ck))
    (setf (slot-value self 'bank-code)  b
          (slot-value self 'branch-code) g
          (slot-value self 'account-number)  c
          (slot-value self 'check-digits)     ck
          (slot-value self 'check-digits-changed) nil)
    self))

(defmethod (setf bank-code)      (bank-code      (self rib))
  (set-bank-code self bank-code))
(defmethod (setf branch-code)    (branch-code    (self rib))
  (set-branch-code self branch-code))
(defmethod (setf account-number) (account-number (self rib))
  (set-account-number self account-number))


(defun test ()
  (every (lambda (test)
           (let ((rib (make-instance 'rib
                        :bank-code (first test)
                        :branch-code (second test)
                        :account-number (third test))))
             (string= (fourth test) (check-digits rib))))
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
