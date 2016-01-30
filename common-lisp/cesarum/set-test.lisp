;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               set-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The tests of set.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-08 <PJB> Extracted from set.lisp
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"
                          "UNION" "INTERSECTION" "MERGE" "INCLUDE")
  (:export "TEST/ALL"
           "TEST/ALL/CLASS"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET.TEST")

;;;-----------------------------------------------------------------------
;;; TESTS
;;;-----------------------------------------------------------------------

(defun test-sets (test-class)
  (list '() '(1) '(1 2 3)
        '#() '#(1) '#(1 2 3)
        (copy test-class '()) (copy test-class '(1)) (copy test-class '(1 2 3))))

(define-test test/all/nil ()
  (loop
    :for seq :in (test-sets 'list-set)
    :do
    (check eql (map-elements nil (function identity) seq) nil)
    (check set-equal (let ((result '()))
                      (map-elements nil (lambda (element) (push element result)) seq)
                      result)
          seq)))

(define-test test/map-elements (test-class)
  (loop
    :for set :in (test-sets test-class)
    :do (loop
          :for class :in (list 'list 'vector test-class)
          :do (check set-equal (map-elements class (function identity) set)
                    (ecase (cardinal set)
                      (0 '())
                      (1 '(1))
                      (3 '(1 2 3)))))))

(define-test test/copy (test-class)
  (loop
    :for (expected type original)
    :in (list (list nil        'nil       '(1 2 3 4))
              (list '(1 2 3 4) 'list      '(1 2 3 4))
              (list '(1 2 3 4) 'vector    '(1 2 3 4))
              (list '(1 2 3 4) test-class '(1 2 3 4)))
    :do
    (check set-equal               (copy type original)  expected (type original))
    (check set-equal (copy 'list   (copy type original)) expected (type original))
    (check set-equal (copy 'vector (copy type original)) expected (type original))))


(define-test test/is-subseq (test-class1 test-class2)
  (flet ((test-set1 (&rest elements)
           (copy test-class1 elements))
         (test-set2 (&rest elements)
           (copy test-class2 elements)))
    (assert-true (is-subset (test-set1)
                            (test-set2)))
    (assert-true (is-subset (test-set1 1)
                            (test-set2 1)))
    (assert-true (is-subset (test-set1 1 2 3)
                            (test-set2 1 2 3)))
    (assert-true (is-subset (test-set1 1 2 3  11 12 13)
                            (test-set2 11 12 13 1 2 3)))
    (assert-true (is-subset (test-set1)
                            (test-set2 1)))
    (assert-true (not (is-subset (test-set1 1)
                                 (test-set2))))
    (assert-true (not (is-subset (test-set1 1)
                                 (test-set2 2))))
    (assert-true (is-subset (test-set1 1 2 3)
                            (test-set2 1 2 3 4)))
    (assert-true (not (is-subset (test-set1 1 2 3 4)
                                 (test-set2 1 2 3))))))


(define-test test/set-equal (test-class)
  (flet ((test-set (&rest elements)
           (copy test-class elements)))
    (assert-true (set-equal (test-set)
                            (test-set)))
    (assert-true (set-equal (test-set 1)
                            (test-set 1)))
    (assert-true (set-equal (test-set 1 2 3)
                            (test-set 1 2 3)))
    (assert-true (set-equal (test-set 1 2 3  11 12 13)
                            (test-set 11 12 13 1 2 3)))
    (assert-true (not (set-equal (test-set)
                                 (test-set 1))))
    (assert-true (not (set-equal (test-set 1)
                                 (test-set))))
    (assert-true (not (set-equal (test-set 1)
                                 (test-set 2))))
    (assert-true (not (set-equal (test-set 1 2 3)
                                 (test-set 1 2 3 4))))
    (assert-true (not (set-equal (test-set 1 2 3 4)
                                 (test-set 1 2 3))))))



(define-test test/union (operator test-class)
  (flet ((test-set (&rest elements)
           (copy test-class elements))) 
    (check set-equal (funcall operator 
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set 1 2 3 7 8 10 11 12))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator 
                             (test-set)
                             (test-set 1 2 3 7 8 10 11 12))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator 
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator 
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set 0 4 5 6 9 10))
          (test-set 0 1 2 3 4 5 6 7 8 9 10 11 12))

    (check set-equal (funcall operator 
                             (test-set 10 11 12)
                             (test-set 1 2 3 7 8))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator 
                             (test-set 1 2 3 7 8)
                             (test-set 10 11 12))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator 
                             (test-set 1 2 3 5 6 7)
                             (test-set 3 4 5 7 8 9  12 13))
          (test-set 1 2 3 4 5 6 7 8 9 12 13))

    (check set-equal (funcall operator 
                             (test-set 1 2 3 5 6 7  12 13)
                             (test-set 3 4 5 7 8 9))
          (test-set 1 2 3 4 5 6 7 8 9 12 13))

    (check set-equal (funcall operator 
                             (test-set 1 2 3  11 12 13)
                             (test-set 3 4 5  13 14 15))
          (test-set 1 2 3 4 5 11 12 13 14 15))
    
    (check set-equal (funcall operator 
                             (test-set 3 4 5  13 14 15)
                             (test-set 1 2 3  11 12 13))
          (test-set 1 2 3 4 5 11 12 13 14 15))))





(define-test test/intersection (operator test-class)
  (flet ((test-set (&rest elements)
           (copy test-class elements)))
    (check set-equal (funcall operator
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set 1 2 3 7 8 10 11 12))
          (test-set 1 2 3 7 8 10 11 12))

    (check set-equal (funcall operator
                             (test-set)
                             (test-set 1 2 3 7 8 10 11 12))
          (test-set))

    (check set-equal (funcall operator
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set))
          (test-set))

    (check set-equal (funcall operator
                             (test-set 1 2 3 7 8 10 11 12)
                             (test-set 0 4 5 6 9 10))
          (test-set 10))

    (check set-equal (funcall operator
                             (test-set 10 11 12)
                             (test-set 1 2 3 7 8))
          (test-set))

    (check set-equal (funcall operator
                             (test-set 1 2 3 7 8)
                             (test-set 10 11 12))
          (test-set))

    (check set-equal (funcall operator
                             (test-set 1 2 3 5 6 7)
                             (test-set 3 4 5 7 8 9  12 13))
          (test-set 3 5 7))

    (check set-equal (funcall operator
                             (test-set 1 2 3 5 6 7  12 13)
                             (test-set 3 4 5 7 8 9))
          (test-set 3 5 7))

    (check set-equal (funcall operator
                             (test-set 1 2 3  11 12 13)
                             (test-set 3 4 5  13 14 15))
          (test-set 3 13))
    
    (check set-equal (funcall operator
                             (test-set 3 4 5  13 14 15)
                             (test-set 1 2 3  11 12 13))
          (test-set 3 13))))



(define-test test/all/sequence (test-class)
  "All the tests working on LIST or VECTOR as sets."
  (test/is-subseq test-class test-class)
  (test/set-equal test-class)
  (test/copy        test-class)
  (test/map-elements test-class))

(define-test test/all/class (test-class)
  "All the tests working on set classes."
  (test/all/sequence test-class)
  (test/is-subseq test-class 'list)
  (test/is-subseq test-class 'vector)
  (test/is-subseq 'list   test-class)
  (test/is-subseq 'vector test-class)
  (test/union (function merge) test-class)
  (test/union (curry (function union) test-class) test-class)
  (test/union (curry (function union) 'vector) test-class)
  (test/intersection (function intersect) test-class)
  (test/intersection (curry (function intersection) test-class) test-class)
  (test/intersection (curry (function intersection) 'vector) test-class))

(define-test test/all ()
  "All the set tests."
  (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET")))
   (test/all/nil)
   (test/all/sequence 'list)
   (test/all/sequence 'vector)
   (test/all/class    'list-set)))


;;;; THE END ;;;;

