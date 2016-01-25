;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dictionary-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests dictionary.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from dictionary.lisp.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY.TEST")


(defun test/dictionary (type)
  (let ((d (make-dictionary type
                            :test (function equal)
                            :contents '(a 1
                                        b 2
                                        c 3))))
    (assert-true (= 3 (dictionary-count d)))
    (assert-true (equalp (list (dictionary-get d 'a 0)
                               (dictionary-get d 'b 0)
                               (dictionary-get d 'c 0)
                               (dictionary-get d 'd 0))
                         '(1 2 3 0)))

    (dictionary-set d 'a 11)
    (dictionary-set d 'd 4)
    (dictionary-set d 'c 33)
    (assert-true (= 4 (dictionary-count d)))
    (assert-true (equalp (list (dictionary-get d 'a 0)
                               (dictionary-get d 'b 0)
                               (dictionary-get d 'c 0)
                               (dictionary-get d 'd 0)
                               (dictionary-get d 'e 0))
                         '(11 2 33 4 0)))

    (setf (dictionary-get d 'a 0) 111)
    (setf (dictionary-get d 'd 0) 444)
    (setf (dictionary-get d 'c 0) 333)
    (assert-true (= 4 (dictionary-count d)))
    (assert-true (equalp (list (dictionary-get d 'a 0)
                               (dictionary-get d 'b 0)
                               (dictionary-get d 'c 0)
                               (dictionary-get d 'd 0)
                               (dictionary-get d 'e 0))
                         '(111 2 333 444 0)))

    (dictionary-delete d 'b)
    (assert-true (= 3 (dictionary-count d)))
    (assert-true (equalp (list (dictionary-get d 'a 0)
                               (dictionary-get d 'b 0)
                               (dictionary-get d 'c 0)
                               (dictionary-get d 'd 0)
                               (dictionary-get d 'e 0))
                         '(111 0 333 444 0)))

    (let ((res (dictionary-map (function cons) d)))
      (assert-true (and (subsetp res '((a . 111) (c . 333) (d . 444))
                                 :test (function equalp))
                        (subsetp '((a . 111) (c . 333) (d . 444)) res
                                 :test (function equalp)))))

    (dictionary-map (lambda (key value) (declare (ignore value)) (dictionary-delete d key)) d)
    (assert-true (= 0 (dictionary-count d)))

    (loop
      :for i :from 1 :to 100
      :do (dictionary-set d i (* 1000 i)))
    (assert-true (= 100 (dictionary-count d)))
    (assert-true (loop
                   :for i :from 1 :to 100
                   :always (= (dictionary-get d i 0) (* 1000 i))))
    (loop
      :for i :from 1 :to 96
      :do (dictionary-delete d i))
    (assert-true (= 4 (dictionary-count d)))
    (assert-true (loop
                   :for i :from 1 :to 96
                   :always (null (nth-value 1 (dictionary-get d i 0)))))
    (assert-true (loop
                   :for i :from 97 :to 100
                   :always (= (dictionary-get d i 0) (* 1000 i))))))


(define-test test/all ()
  "Tests all the kinds of dictionary defined in this package."
  (dolist (type '(hash-table p-list a-list adaptating-dictionary))
    (test/dictionary type)))

;;;; THE END ;;;;
