;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               array-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Tests for the array functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-07-27 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY.TEST")

(define-test test/positions-of-subsequence ()
  (assert-true (handler-case
                   (positions-of-subsequence "" "abc hello abc world abc how do abc you do abc")
                 (:no-error (&rest results)
                   (declare (ignore results))
                   nil)
                 (error (error)
                   (declare (ignore error))
                   t)))

  (assert-true (equal (positions-of-subsequence "xyz" "abc hello abc world abc how do abc you do abc")
                      '()))
  (assert-true (equal (positions-of-subsequence "abc" "abc")
                      '((0 . 3))))
  (assert-true (equal (positions-of-subsequence "abc" "abcabcabc")
                      '((0 . 3) (3 . 6) (6 . 9))))
  (assert-true (equal (positions-of-subsequence "aaa" "aaaaaaaaaa")
                      '((0 . 3) (3 . 6) (6 . 9))))
  (assert-true (equal (positions-of-subsequence "abc" "abc hello abc world abc how do abc you do abc")
                      '((0 . 3) (10 . 13) (20 . 23) (31 . 34) (42 . 45))))

  (assert-true (equal (positions-of-subsequence "xyz" "abc hello abc world abc how do abc you do abc"
                                                :from-end t)
                      '()))
  (assert-true (equal (positions-of-subsequence "abc" "abc"
                                                :from-end t)
                      '((0 . 3))))
  (assert-true (equal (positions-of-subsequence "abc" "abcabcabc"
                                                :from-end t)
                      '((0 . 3) (3 . 6) (6 . 9))))
  (assert-true (equal (positions-of-subsequence "aaa" "aaaaaaaaaa"
                                                :from-end t)
                      '((1 . 4) (4 . 7) (7 . 10))))
  (assert-true (equal (positions-of-subsequence "abc" "abc hello abc world abc how do abc you do abc"
                                                :from-end t)
                      '((0 . 3) (10 . 13) (20 . 23) (31 . 34) (42 . 45))))

  (assert-true (equal (positions-of-subsequence #((1 ?) (2 ?) (3 ?))
                                                #((1 a) (2 b) (3 c)
                                                  (1 d) (2 e)
                                                  (1 f) (2 g) (3 h)
                                                  (2 i) (3 j))
                                                :key (function car)
                                                :test (function =))
                      '((0 . 3) (5 . 8))))
  (assert-true (equal (positions-of-subsequence #((1 ?) (2 ?) (3 ?))
                                                #((1 a) (2 b) (3 c)
                                                  (1 d) (2 e)
                                                  (1 f) (2 g) (3 h)
                                                  (2 i) (3 j))
                                                :key (function car)
                                                :test (function /=))
                      '((1 . 4) (4 . 7)))))


(define-test test/array-to-list ()
  (assert-true (equal (array-to-lists #0A42)
                      42))
  (assert-true (equal (array-to-lists #(1 2 3))
                      '(1 2 3)))
  (assert-true (equal (array-to-lists "abc")
                      '(#\a #\b #\c)))
  (assert-true (equal (array-to-lists #1A(1 2 3))
                      '(1 2 3)))
  (assert-true (equal (array-to-lists #2A((1 2 3)
                                          (4 5 6)
                                          (7 8 9)))
                      '((1 2 3) (4 5 6) (7 8 9))))
  (assert-true (equal (array-to-lists #())
                      'nil))
  (assert-true (equal (array-to-lists #2A())
                      'nil))
  (assert-true (equal (array-to-lists #2A(()))
                      '(nil)))
  (assert-true (equal (array-to-lists #2A((())))
                      '((nil))))
  (assert-true (equal (array-to-lists #3A(((1 2 3)
                                           (4 5 6)
                                           (7 8 9))
                                          ((a b c)
                                           (d e f)
                                           (g h i))))
                      '(((1 2 3) (4 5 6) (7 8 9)) ((a b c) (d e f) (g h i))))))

(define-test test/all ()
  (test/positions-of-subsequence)
  (test/array-to-list))

;;;; THE END ;;;;
