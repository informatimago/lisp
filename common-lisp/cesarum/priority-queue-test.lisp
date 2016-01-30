;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               priority-queue-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests priority-queue.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from priority-queue.lisp.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PRIORITY-QUEUE.TEST")

(define-test test/pq ()
  (let ((p (make-pq)))
    (pq-insert p 4)
    (pq-insert p 8)
    (pq-insert p 2)
    (pq-remove p 4)
    (pq-insert p 16)
    (assert-true (= 2 (pq-first p)))
    (pq-insert p 1)
    (pq-insert p 5)
    (assert-true (= 1 (pq-first p)))
    (assert-true (= 1 (pq-pop p)))
    (assert-true (= 2 (pq-first p)))
    (assert-true (equal (pq-elements p) '(2 5 8 16))))
  (let ((p (make-pq :lessp (function >) :key (function length)))
        (bye "Bye!"))
    (pq-insert p bye)
    (pq-insert p "Au revoir")
    (pq-insert p "Ah")
    (pq-remove p bye)
    (let ((long  "Comment ça va?"))
      (pq-insert p long)
      (assert-true (eql long (pq-first p)))
      (let ((long "Moi ça va, et toi comment ça va?"))
        (pq-insert p long)
        (let ((long "Viens chez moi j'habite chez une copine."))
          (pq-insert p long)
          (assert-true (eql long (pq-first p)))
          (assert-true (eql long (pq-pop p))))
        (assert-true (eql long (pq-first p)))))
    (assert-true (equal '("Moi ça va, et toi comment ça va?" "Comment ça va?" "Au revoir" "Ah")
                        (pq-elements p)))))


(define-test test/all ()
  (test/pq))

;;;; THE END ;;;;
