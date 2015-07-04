;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               list-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test list.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from list.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST.TEST")


(define-test test/list-lengths ()
  (dolist (test
           '( ;; proper lists
             (()  0 0)
             ((a)  1 0)
             ((a b)  2 0)
             ((a b c)  3 0)
             ((a b c d)  4 0)
             ((a b c d e)  5 0)
             ;; dotted lists
             (a  0 nil)
             ((a . b)  1 nil)
             ((a b . c) 2 nil)
             ((a b c . d) 3 nil)
             ((a b c d . e) 4 nil)
             ((a b c d e . f) 5 nil)
             ;; circular lists
             (#1=(a . #1#) 0 1)
             (#2=(a b . #2#) 0 2)
             (#3=(a b c . #3#) 0 3)
             (#4=(a b c d . #4#) 0 4)
             (#5=(a b c d e . #5#) 0 5)
             ((a . #6=(b . #6#)) 1 1)
             ((a . #7=(b c . #7#)) 1 2)
             ((a . #8=(b c d . #8#)) 1 3)
             ((a . #9=(b c d e . #9#)) 1 4)
             ((a b . #10=(c . #10#)) 2 1)
             ((a b . #11=(c d . #11#)) 2 2)
             ((a b . #12=(c d e . #12#)) 2 3)
             ((a b c . #13=(d . #13#)) 3 1)
             ((a b c . #14=(d e . #14#)) 3 2)
             ((a b c d . #15=(e . #15#)) 4 1)
             ((a b c d e . #16=(#16#)) 6 0) ; a proper list! :-)
             )
           :success)
    (destructuring-bind (list . expected) test
      (let ((result  (multiple-value-list (list-lengths list)))
            (*print-circle* t))
        (assert-true (equal expected result)
                     (result)
                     "(list-lengths '~S)~%  returned ~S~%  expected ~S~%"
                     list result expected)))))



(define-test test/list-elements ()
  (dolist (test
           '( ;; proper lists
             (()  ()  0 0)
             ((a)  (a) 1 0)
             ((a b)  (a b) 2 0)
             ((a b c)  (a b c) 3 0)
             ((a b c d)  (a b c d) 4 0)
             ((a b c d e)  (a b c d e) 5 0)
             ;; dotted lists
             (a  (a) 0 nil)
             ((a . b) (a b) 1 nil)
             ((a b . c) (a b c) 2 nil)
             ((a b c . d) (a b c d) 3 nil)
             ((a b c d . e) (a b c d e) 4 nil)
             ((a b c d e . f) (a b c d e f) 5 nil)
             ;; circular lists
             (#1=(a . #1#)  (a) 0 1)
             (#2=(a b . #2#)  (a b) 0 2)
             (#3=(a b c . #3#) (a b c) 0 3)
             (#4=(a b c d . #4#) (a b c d) 0 4)
             (#5=(a b c d e . #5#) (a b c d e) 0 5)
             ((a . #6=(b . #6#)) (a b) 1 1)
             ((a . #7=(b c . #7#)) (a b c) 1 2)
             ((a . #8=(b c d . #8#)) (a b c d) 1 3)
             ((a . #9=(b c d e . #9#)) (a b c d e) 1 4)
             ((a b . #10=(c . #10#)) (a b c) 2 1)
             ((a b . #11=(c d . #11#)) (a b c d) 2 2)
             ((a b . #12=(c d e . #12#)) (a b c d e) 2 3)
             ((a b c . #13=(d . #13#)) (a b c d) 3 1)
             ((a b c . #14=(d e . #14#)) (a b c d e) 3 2)
             ((a b c d . #15=(e . #15#)) (a b c d e) 4 1)
             ((a b c d e . #16=(#16#)) (a b c d e #16#) 6 0) ; a proper list! :-)
             )
           :success)
    (destructuring-bind (list . expected) test
      (let ((result  (multiple-value-list (list-elements list)))
            (*print-circle* t))
        (assert-true (equal expected result)
                (result)
                "(~A '~S)~%  returned ~S~%  expected ~S~%"
                'list-elements list result expected)))))


(define-test test/tree-find ()
  (assert-true (equal 'x (tree-find 'x 'x)))
  (assert-true (equal 'x (tree-find 'x '(x))))
  (assert-true (equal 'x (tree-find 'x '(a b c x d e f))))
  (assert-true (equal 'x (tree-find 'x '(a b c d . x))))
  (assert-true (equal 'x (tree-find 'x '(() (a b c d . x)))))
  (assert-true (equal 'x (tree-find 'x '((a b (a b c d . x) x)))))

  (assert-true (equal 'x (tree-find "x" 'x :test (function string-equal))))
  (assert-true (equal 'x (tree-find "x" '(x) :test (function string-equal))))
  (assert-true (equal 'x (tree-find "x" '(a b c x d e f) :test (function string-equal))))
  (assert-true (equal 'x (tree-find "x" '(a b c d . x) :test (function string-equal))))
  (assert-true (equal 'x (tree-find "x" '(() (a b c d . x)) :test (function string-equal))))
  (assert-true (equal 'x (tree-find "x" '((a b (a b c d . x) |x|)) :test (function string-equal))))

  (assert-true (equal 'x (tree-find "x" 'x :test (function string=) :key (function string-downcase))))
  (assert-true (equal 'x (tree-find "x" '(x) :test (function string=) :key (function string-downcase))))
  (assert-true (equal 'x (tree-find "x" '(a b c x d e f) :test (function string=) :key (function string-downcase))))
  (assert-true (equal 'x (tree-find "x" '(a b c d . x) :test (function string=) :key (function string-downcase))))
  (assert-true (equal 'x (tree-find "x" '(() (a b c d . x)) :test (function string=) :key (function string-downcase))))
  (assert-true (equal 'x (tree-find "x" '((a b (a b c d . x) |x|)) :test (function string=) :key (function string-downcase)))))


(define-test test/iota ()
  (check equalp (iota 5) '(0 1 2 3 4) ())
  (check equalp (iota 5 0 -0.10) '(0 -0.1 -0.2 -0.3 -0.4))
  (check equalp (iota (/ 30 4)) '(0 1 2 3 4 5 6))
  (check equalp (iota (/ 30 4) 0 4) '(0 4 8 12 16 20 24)))

(define-test test/all ()
  (test/list-lengths)
  (test/list-elements)
  (test/tree-find)
  (test/iota))



;;;; THE END ;;;;
