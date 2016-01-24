;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ascii-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests ascii.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-26 <PJB> Extracted from ascii.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII.TEST")


(define-test test/ascii ()
  "
DO:     test the ascii package; signal an error if something is wrong.
RETURN: :success
"
  (loop
    :for ch :across *ascii-characters*
    :for code :from sp
    :do (assert-true (= code (ascii-code ch)))
    :do (assert-true (char= ch (code-ascii code))))
  (loop
    :for code :from (ascii-code #\0) :to (ascii-code #\9)
    :for n :from 0
    :do (assert-true (eql n (code-ascii-digit-p code))))
  (assert-true (typep (nth-value 1 (ignore-errors (ascii-string #(65 66 8 67 69)))) 'decoding-error))
  (assert-true (typep (nth-value 1 (ignore-errors (ascii-bytes "En été, il fait chaud."))) 'encoding-error))
  (assert-true (string= "ABCD" (ascii-string #(65 66 67 68))))
  (assert-true (string= "ABCD" (ascii-string #(0 0 65 66 67 68 0 0 0 0) :start 2 :end 6)))
  (assert-true (bytes=  #(65 66 67 68)  (ascii-bytes "ABCD")))
  (assert-true (bytes=  #(65 66 67 68)  (ascii-bytes "00ABCD0000" :start 2 :end 6)))
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\Y (function ascii-dispatch-macro)
                                  *readtable*)
    (set-dispatch-macro-character #\# #\" (function ascii-dispatch-macro)
                                  *readtable*)
    (assert-true (bytes= #(65 66 67 68) (read-from-string "#\"ABCD\"")))
    (assert-true (bytes= #(65 66 67 68) (read-from-string "#Y\"ABCD\""))))
  #| TODO: Added more testing of bytes comparisons.|#)


(define-test test/all ()
  (test/ascii))

;;;; THE END ;;;;
