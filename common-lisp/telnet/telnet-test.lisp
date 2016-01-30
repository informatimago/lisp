;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               telnet-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests telnet.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from telnet.lisp.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.TELNET.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
                "PARSE-MESSAGE" "REMOVE-IAC-IAC" "GET-NEXT-CHUNK"
                "IAC" "NOP" "SB" "SE" "XDISPLOC")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.TELNET.TEST")


(define-test test/parse-message ()
  ;; nothing
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 3 4 5) 1 1))
                      '(nil 0)))
  ;; :bytes
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 3 4 5) 0 5))
                      '(:bytes 5 nil)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac iac 1 2 3) 0 5))
                      '(:bytes 5 t)))
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 3 iac iac) 0 5))
                      '(:bytes 5 t)))
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 iac iac 3) 0 5))
                      '(:bytes 5 t)))
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 3 iac nop) 0 5))
                      '(:bytes 3 nil)))
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 iac iac 3 iac nop) 0 7))
                      '(:bytes 5 t)))
  (assert-true (equal (multiple-value-list (parse-message (vector 1 2 iac iac 3 iac) 0 6))
                      '(:bytes 5 t)))
  ;; iac commands
  (assert-true (equal (multiple-value-list (parse-message (vector iac nop 1 2 3) 0 5))
                      '(:iac 2)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac nop iac iac iac) 0 5))
                      '(:iac 2)))
  ;; iac sb
  (assert-true (equal (multiple-value-list (parse-message (vector iac sb xdisploc
                                                                  75 85 73 80 69 82 58 48 46 48
                                                                  iac se 1 2 3) 0 15))
                      '(:iac-sb 15)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac sb xdisploc
                                                                  75 85 73 80 69 82 58 48 46 48
                                                                  iac se 1 2 3) 0 18))
                      '(:iac-sb 15)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac sb xdisploc
                                                                  75 85 73 80 69 82 58 iac iac 48 46 48
                                                                  iac se 1 2 3) 0 20))
                      '(:iac-sb 17)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac sb xdisploc
                                                                  75 85 73 80 69 82 58 48 46 48
                                                                  iac se 1 2 3) 0 14))
                      '(nil 0)))
  (assert-true (equal (multiple-value-list (parse-message (vector iac sb xdisploc
                                                                  75 85 73 80 69 82 58 48 46 48
                                                                  iac se 1 2 3) 0 13))
                      '(nil 0))))


(define-test test/remove-iac-iac ()
  (let ((buffer (vector 1 2 3 4 5 6 7 8 9)))
    (assert-true (= 5 (remove-iac-iac buffer 2 7)))
    (assert-true (equalp buffer (vector 1 2 3 4 5 6 7 8 9))))
  (let ((buffer (vector iac iac 1 2 3 4 5 6 7 8 9)))
    (assert-true (= 5 (remove-iac-iac buffer 2 7)))
    (assert-true (equalp buffer (vector iac iac 1 2 3 4 5 6 7 8 9)))
    (assert-true (= 6 (remove-iac-iac buffer 0 7)))
    (assert-true (equalp buffer (vector iac 1 2 3 4 5 5 6 7 8 9))))
  (let ((buffer (vector iac iac 1 2 3 iac iac 6 7 8 9)))
    (assert-true (= 4 (remove-iac-iac buffer 2 7)))
    (assert-true (equalp buffer (vector iac iac 1 2 3 iac iac 6 7 8 9)))
    (assert-true (= 5 (remove-iac-iac buffer 0 7)))
    (assert-true (equalp buffer (vector iac 1 2 3 iac iac iac 6 7 8 9)))))


(define-test test/get-next-chunk ()
  (assert (equal
           (let ((buffer (vector 61 62 63 64 13 10 65 66 13 0 67 68 69 70 12 9 71 72 73)))
             (loop
               :with start = 0
               :for res = (multiple-value-list (get-next-chunk buffer start (length buffer)))
               :collect res
               :until (member (first res) '(:done :incomplete))
               :do (setf start (second res))))
           '((:text 8) (:control 10 13) (:text 14) (:control 15 12) (:control 16 9) (:text 19) (:done 19))))
  (assert (equal
           (let ((buffer (vector 61 62 63 64 13 10 65 66 13 0 67 68 69 70 10 9 71 72 73 13)))
             (loop
               :with start = 0
               :for res = (multiple-value-list (get-next-chunk buffer start (length buffer)))
               :collect res
               :until (member (first res) '(:done :incomplete))
               :do (setf start (second res))))
           '((:text 8) (:control 10 13) (:text 14) (:control 15 10) (:control 16 9) (:incomplete 19))))
    (assert (equal
           (let ((buffer (vector 1 2 3 4)))
             (loop
               :with start = 0
               :for res = (multiple-value-list (get-next-chunk buffer start (length buffer)))
               :collect res
               :until (member (first res) '(:done :incomplete))
               :do (setf start (second res))))
           '((:control 1 1) (:control 2 2) (:control 3 3) (:control 4 4) (:done 4)))))


(define-test test/all ()
  (test/parse-message)
  (test/remove-iac-iac)
  (test/get-next-chunk))


;;;; THE END ;;;;


