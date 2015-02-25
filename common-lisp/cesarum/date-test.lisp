;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               date-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests date.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from date.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.UTILITY")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.TEST")


(define-test test/compare-lists-of-numbers ()
  (dolist (test '((()  ()  0)
                  ((1) ()  +1)
                  (()  (1) -1)
                  ((1) (1) 0)
                  ((2) (1) +1)
                  ((1) (2) -1)
                  ((1 1 1)     (1 1 1)   0)
                  ((1 1 1 1)   (1 1 1)   +1)
                  ((1 1 1)     (1 1 1 1) -1)
                  ((2 1 1)     (1 1 1)   +1)
                  ((2 1 1 1)   (1 1 1)   +1)
                  ((2 1 1)     (1 1 1 1) +1)
                  ((0 1 1)     (1 1 1)   -1)
                  ((0 1 1 1)   (1 1 1)   -1)
                  ((0 1 1)     (1 1 1 1) -1)
                  ((1 2 1 1)   (1 1 1)   +1)
                  ((1 2 1 1 1) (1 1 1)   +1)
                  ((1 2 1 1)   (1 1 1 1) +1)
                  ((1 0 1 1)   (1 1 1)   -1)
                  ((1 0 1 1 1) (1 1 1)   -1)
                  ((1 0 1 1)   (1 1 1 1) -1)))
    (assert-true (= (compare-lists-of-numbers (first test) (second test))
                    (third test)))))


(define-test test/hms60 ()
  #||
  (loop
  :for secondes :from 0.0 :below 600.0  :by 13.1
  :do (assert (= secondes
  (multiple-value-call (function hms60-to-secondes)
  (hms60-from-secondes secondes)))
  (secondes)))
  ||#
  (loop
    :for secondes :from 0 :below 4000
    :do (assert-true (= secondes
                        (multiple-value-call (function hms60-to-secondes)
                          (hms60-from-secondes secondes)))
                     (secondes))))


(define-test test/all ()
  (test/compare-lists-of-numbers)
  (test/hms60))


;;;; THE END ;;;;
