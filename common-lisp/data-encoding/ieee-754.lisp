;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ieee-754.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Export functions to encode/decode IEEE-754 floating point numbers.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from heap.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.IEEE-754"
  (:use "COMMON-LISP")
  (:export
   "FLOAT-32-TO-IEEE-754"
   "IEEE-754-TO-FLOAT-32"
   "FLOAT-64-TO-IEEE-754"
   "IEEE-754-TO-FLOAT-64"
   "GEN-IEEE-ENCODING"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.IEEE-754")


;; [floatx[0,6]  v6 v5 v4 | v3 v2 v1 v0]

(defmacro gen-ieee-encoding (name type exponent-bits mantissa-bits)
  ;; Thanks to ivan4th (~ivan_iv@nat-msk-01.ti.ru) for correcting an off-by-1
  `(progn
     (defun ,(intern (with-standard-io-syntax (format nil "~A-TO-IEEE-754" name))
                     (symbol-package name))  (float)
       "Convert FLOAT to a IEEE-753 representation stored in an integer."
       (if (zerop float)
           0
           (multiple-value-bind (mantissa exponent sign) 
               (integer-decode-float float)
             (dpb (if (minusp sign) 1 0)
                  (byte 1 ,(1- (+ exponent-bits mantissa-bits)))
                  (dpb (+ ,(+ (- (expt 2 (1- exponent-bits)) 2) mantissa-bits)
                          exponent)
                       (byte ,exponent-bits ,(1- mantissa-bits))
                       (ldb (byte ,(1- mantissa-bits) 0) mantissa))))))
     (defun ,(intern (with-standard-io-syntax (format nil "IEEE-754-TO-~A" name))
                     (symbol-package name))  (ieee)
       "Convert the IEEE representatin (stored as an integer) into a floating point number."
       (if (zerop ieee)
           ,(coerce 0 type)
           (let ((aval (scale-float
                        (coerce
                         (dpb 1 (byte 1 ,(1- mantissa-bits))
                              (ldb (byte ,(1- mantissa-bits) 0) ieee))
                         ',type)
                        (- (ldb (byte ,exponent-bits ,(1- mantissa-bits))
                                ieee) 
                           ,(1- (expt 2 (1- exponent-bits)))
                           ,(1- mantissa-bits)))))
             (if (zerop (ldb (byte 1 ,(1- (+ exponent-bits mantissa-bits))) ieee))
                 aval
                 (- aval)))))))


(gen-ieee-encoding float-32 single-float  8 24)
(gen-ieee-encoding float-64 double-float 11 53)

;;;; THE END ;;;;
