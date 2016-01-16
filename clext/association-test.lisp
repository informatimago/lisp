;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               association-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the associations.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-22 <PJB> Extracted from association.lisp.
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

(defpackage "COM.INFORMATIMAGO.CLEXT.ASSOCIATION.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLEXT.ASSOCIATION"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
  (:import-from "COM.INFORMATIMAGO.CLEXT.ASSOCIATION"
                "ADD-NEW-ELEMENT" "MULTIPLICITY")
  (:export "TEST/ALL"
           "TEST/ADD-NEW-ELEMENT" "TEST/MULTIPLICITY"))
(in-package "COM.INFORMATIMAGO.CLEXT.ASSOCIATION.TEST")


(define-test test/add-new-element ()
  (assert-true (equal '(x) (add-new-element nil 'x)))
  (assert-true (equal '(x) (add-new-element nil 'x :test (function equal))))
  (assert-true (equal '(x) (add-new-element nil 'x :lessp (function string<))))
  (assert-true (equal '(x) (add-new-element (list 'x) 'x)))
  (assert-true (equal '(x) (add-new-element (list 'x) 'x :test (function equal))))
  (assert-true (equal '(x) (add-new-element (list 'x) 'x :lessp (function string<))))
  (progn (let* ((list (list 1 2 3))
                (result (add-new-element list 0 :test #'=)))
           (assert-true (equal result '(1 2 3 0)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 0)))
           (assert-true (equal result '(1 2 3 0)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3))
                (result (add-new-element list 4 :lessp #'<)))
           (assert-true (equal result '(1 2 3 4)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3))
                (result (add-new-element list 1 :lessp #'<)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 1)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3))
                (result (add-new-element list 1.0 :test #'=)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 2)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3))
                (result (add-new-element list 2.0 :test #'=)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3)) (result (add-new-element list 3)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 3))
                (result (add-new-element list 3.0 :test #'=)))
           (assert-true (equal result '(1 2 3)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 4))
                (result (add-new-element list 3 :lessp #'<)))
           (assert-true (equal result '(1 2 3 4)))
           (assert-true (eql result list)))
         (let* ((list (list 1 2 4))
                (result (add-new-element list 2 :lessp #'<)))
           (assert-true (equal result '(1 2 4)))
           (assert-true (eql result list)))
         (let* ((list (list 1 3 4))
                (result (add-new-element list 2 :lessp #'<)))
           (assert-true (equal result '(1 2 3 4)))
           (assert-true (eql result list)))
         (let* ((list (list 1 3 4))
                (result (add-new-element list 3 :lessp #'<)))
           (assert-true (equal result '(1 3 4)))
           (assert-true (eql result list))))  
  (let ((list (list 2 3 4)))
    (assert-true (equal '(1 2 3 4) (add-new-element list 1 :lessp (function <)))))
  (let ((list (list 2 3 4)))
    (assert-true (equal '(1 2 3 4) (add-new-element list 1 :lessp (function <))))))


(defun test/multiplicity ()
  (assert-true (equal (mapcar (lambda (test) (multiple-value-list (multiplicity test)))
                              '(0    1    2    3
                                0-1  1-1  0-4  2-4
                                *    0-*  1-*  4-* ; 34-2
                                0..1 1..1 0..4 2..4
                                *    0..* 1..* 4..* ; 34..2
                                ))
                      '((0 0) (1 1) (2 2) (3 3)
                        (0 1) (1 1) (0 4) (2 4)
                        (0 *) (0 *) (1 *) (4 *) ; (34 2)
                        (0 1) (1 1) (0 4) (2 4)
                        (0 *) (0 *) (1 *) (4 *) ; (34 2)
                        ))))


(define-test test/all ()
  (test/add-new-element)
  (test/multiplicity))

;;;; THE END ;;;;


