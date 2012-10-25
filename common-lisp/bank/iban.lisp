;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               iban.lisp
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
;;;;    2004-10-10 <PJB> Created.
;;;;BUGS
;;;;    The verification of the country code accepts all existing countries
;;;;    as defined by iso-3166.  Some of these country code are not used
;;;;    (GP --> FR for example).  So an incorrect use of GP is not detected.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ISO3166"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.BANK.IBAN"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "GET-AND-CHECK-ALPHANUM" "COMPUTE-IBAN-KEY" "CHECK-IBAN-KEY"
           "GET-IBAN" "GET-KEY" "GET-COUNTRY-CODE" "SET-IBAN" "GET-IBAN" "GET-KEY"
           "GET-COUNTRY-CODE" "CHECK-COUNTRY" "BASIC-FORM" "IBAN" "IBAN-ERROR")
  (:documentation "
This class is an Internationnal Bank Account Number, 
according to the European standard:
IBAN Format: http://www.ecbs.org/iban/iban.htm


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 1994 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see http://www.gnu.org/licenses/
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.BANK.IBAN")






(defgeneric basic-form (iban)
  (:documentation "RETURN: The basic form of the IBAN."))

(defgeneric get-and-check-alphanum (self string &optional length)
  (:documentation "Check that STRING contains only alphanumeric character valid in an IBAN."))

(defgeneric check-country (self)
  (:documentation "
DO:     Checks the country code in the basic-form, 
        and raises an error if not valid.
RAISE:  IBAN-ERROR 
RETURN: SELF
"))

(defgeneric get-country-code (self)
  (:documentation   "
RETURN: The country code in the IBAN.
"))

(defgeneric get-key (self)
  (:documentation   "
RETURN: The computed key of the IBAN.
"))

(defgeneric get-iban (self &key with-spaces)
  (:documentation   "
RETURN: The IBAN as a string, with spaces inserted when WITH-SPACES is
        true, else in basic form.
"))

(defgeneric set-iban (self iban &key with-key)
  (:documentation  "
DO:     Change the IBAN. If WITH-KEY is true then the IBAN key is checked
        and an error raised if it is not valid, else the IBAN key is
        computed and substituted.
RETURN: SELF
SIGNAL: An IBAN-ERROR when with-key and the key in the IBAN is incorrect.
"))



(define-condition iban-error (error)
  ()
  (:documentation "An IBAN error."))


(defclass iban ()
  ((basic-form
    :reader basic-form
    :initform "FR00000000000000000000000"
    :initarg :basic-form
    :type string
    :documentation "The basic form of the IBAN."))
  (:documentation "The Internationnal Bank Account Number class."))


(defmethod initialize-instance ((self iban) &rest args)
  (declare (ignore args))
  (call-next-method)
  (when  (basic-form  self) 
    (set-iban self (basic-form  self)))
  self) ;;INITIALIZE-INSTANCE



(defmethod get-country-code ((self iban))
  "
RETURN: The country code in the IBAN.
"
  (subseq (basic-form self) 0 2))


(defmethod get-key ((self iban))
  "
RETURN: The computed key of the IBAN.
"
  (subseq (slot-value self 'basic-form) 2 4))


(defmethod get-iban ((self iban) &key (with-spaces nil))
  "
RETURN: The IBAN, with spaces inserted when WITH-SPACES is true, 
        else in basic form.
"
  (if with-spaces
      (do ((iban (basic-form self))
           (res '())
           (i 0 (+ i 4)))
          ((>= (+ i 4) (length iban))
           (progn (push (subseq iban i) res)
                  (apply (function concatenate) 'string (nreverse res))))
        (push (subseq iban i (+ i 4)) res)
        (push " " res))
      (copy-seq (basic-form self))))



;;     We test and convert to upper case letters, because
;;     the RIB and IBAN may contain only the following characters:
;;         0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ 


(defparameter +alphabet-from+
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")


(defmethod get-and-check-alphanum ((self iban) string &optional length)
  (when (and length (/= length (length string)))
    (signal 'iban-error
            "For IBAN ~S:~%   Bad length,  expected ~D, got ~D: ~S" 
            self length (length string) string))
  (map 'string (lambda (ch) 
                 (let ((index (position ch +alphabet-from+)))
                   (unless index 
                     (signal 'iban-error
                             "For IBAN ~S:~%    Bad character '~C' in ~S, ~
                              should be alphanumeric." self ch string))
                   (aref +alphabet-from+ (if (< index 36) index (- index 26)))))
       string))


(defparameter +country-codes+
  (mapcar
   (function third)
   (com.informatimago.common-lisp.cesarum.iso3166:get-countries :only-existing t))
  "List of 2-letter country codes.")


(defmethod check-country ((self iban))
  "
DO:     Checks the country code in the basic-form, 
        and raises an error if not valid.
RAISE:  IBAN-ERROR 
RETURN: SELF
"
  (let ((cc  (subseq (basic-form self) 0 2)))
    (unless (member cc +country-codes+ :test (function string-equal))
      (signal 'iban-error "For IBAN ~S:~%   Bad country code: ~S" self cc)))
  self)


(defun check-iban-key (iban)
  "
DO:         Check the IBAN KEY
            The IBAN string must be in basic format,
            all non alphanumeric characters removed.
            0- move the first four characters of the IBAN to the end.
            1- convert the letters into numerics.
            2- apply MOD 97-10 (ISO 7064) : remainder of n by 97 must be 1
            3- return T when the IBAN key checks.
RETURN: Whether the IBAN key checks.
"
  (= 1 (mod
        (loop
           for ch across (concatenate 'string (subseq iban 4) (subseq iban 0 4))
           with n = 0
           do (setf n (+ (* (if (alpha-char-p ch) 100 10) n)
                         (parse-integer (string ch) :radix 36 :junk-allowed nil)))
           finally (return n)) 97)))


(defun compute-iban-key (country account)
  "
DO:         Compute the IBAN key for the given ACCOUNT.
            ACCOUNT must be in basic format, all non alphanumeric characters removed.
            0- create artificial IBAN with 00 check sum.
            1- move the first four characters of the IBAN to the end.
            2- convert the letters into numerics.
            3- apply MOD 97-10 (ISO 7064): check sum is 98 - n mod 97.
            4- return the complete IBAN.
RETURN: The new complete IBANA.
"
  (format nil "~2A~2,'0D~A" 
          country
          (- 98 (mod (loop
                        for ch across (concatenate 'string  account country "00")
                        with n = 0
                        do (setf n (+ (* (if (alpha-char-p ch) 100 10) n)
                                      (parse-integer (string ch)
                                                     :radix 36 
                                                     :junk-allowed nil)))
                        finally (return n)) 97))
          account))


(defmethod set-iban ((self iban) (iban string) &key (with-key nil))
  "
DO:     Change the IBAN. If WITH-KEY is true then the IBAN key is checked
        and an error raised if it is not valid, else the IBAN key is
        computed and substituted.
RETURN: SELF
RAISE:  An IBAN-ERROR when with-key and the key in the IBAN is incorrect.
"
  (setf iban (get-and-check-alphanum 
              self (remove-if (complement (function alphanumericp)) iban)))
  (setf (slot-value self 'basic-form) 
        (if with-key
            (if (check-iban-key iban)
                (signal 'iban-error
                        "For IBAN ~S~%    Invalid key, given=~S, computed=~S."
                        (subseq iban 2 4)
                        (subseq (compute-iban-key (subseq iban 0 2)
                                                  (subseq iban 4)) 2 4))
                iban)
            (compute-iban-key (subseq iban 0 2) (subseq iban 4))))
  (check-country self)
  self)

;;;; THE END ;;;;
