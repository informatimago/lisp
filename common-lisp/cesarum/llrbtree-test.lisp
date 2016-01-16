;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               llrbtree-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests llrbtree.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from llrbtree.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LLRBTREE.TEST")


(flet ((insert (tree key value) (setf (tree-get key tree) value))
       (finde   (tree key)       (tree-get key tree))
       (erase  (tree key)       (tree-delete key tree))
       (free   (tree)           (tree-clear tree))
       (new-tree ()             (make-tree)))


  (define-test test/tree-creation ()
    (let ((tree (make-tree :lessp (function <))))
      (insert tree 1 "a")
      (insert tree 2 "b")
      (insert tree 12 "c")
      (insert tree 12 "c")
      (insert tree 12 "c")
      (insert tree -1 "d")
      (insert tree -1 "d")
      (insert tree 12 "c")

      (assert-true (equal (finde tree -1) "d") nil "Did not get 'd' for -1")
      (assert-true (equal (finde tree 1) "a") nil "Did not get 'a' for 1")
      (assert-true (equal (finde tree 2) "b") nil "Did not get 'b' for 2")
      (assert-true (equal (finde tree 12) "c") nil "Did not get 'c' for 12")))


  (define-test test/node-erase ()
    (let ((tree (new-tree)))

      (insert tree 4 "a")
      (insert tree 3 "b")
      (insert tree 2 "c")
      (insert tree 1 "d")

      (assert-true (equal (finde tree 1) "d") nil "Did not get 'd' for 1")
      (assert-true (equal (finde tree 2) "c") nil "Did not get 'c' for 2")
      (assert-true (equal (finde tree 3) "b") nil "Did not get 'b' for 3")
      (assert-true (equal (finde tree 4) "a") nil "Did not get 'a' for 4")

      (erase tree 2)
      (assert-true (eq (finde tree 2) nil) nil "Should not have found 2 after it was deleted.")
      (assert-true (equal (finde tree 1) "d") nil "Did not get 'd' for 1")
      (assert-true (equal (finde tree 3) "b") nil "Did not get 'b' for 3")
      (assert-true (equal (finde tree 4) "a") nil "Did not get 'a' for 4")

      (erase tree 4)
      (assert-true (eq (finde tree 4) nil) nil "Should not have found 4 after it was deleted.")
      (assert-true (equal (finde tree 1) "d") nil "Did not get 'd' for 1")
      (assert-true (equal (finde tree 3) "b") nil "Did not get 'b' for 3")

      (erase tree 1)
      (assert-true (eq (finde tree 1) nil) nil "Should not have found 1 after it was deleted.")
      (assert-true (equal (finde tree 3) "b") nil "Did not get 'b' for 3")

      (erase tree 3)
      (assert-true (eq (finde tree 4) nil) nil "Should not have found 4 after it was deleted.")
      (assert-true (eq (finde tree 3) nil) nil "Should not have found 3 after it was deleted.")
      (assert-true (eq (finde tree 2) nil) nil "Should not have found 2 after it was deleted.")
      (assert-true (eq (finde tree 1) nil) nil "Should not have found 1 after it was deleted.")

      (erase tree 3)))

  
  (define-test test/tree-erase ()
    (let ((tree (new-tree)))

      (insert tree 4 "a")
      (insert tree 3 "b")
      (insert tree 2 "c")
      (insert tree 1 "d")

      (assert-true (equal (finde tree 1) "d") nil "Did not get 'd' for 1")
      (assert-true (equal (finde tree 2) "c") nil "Did not get 'c' for 2")
      (assert-true (equal (finde tree 3) "b") nil "Did not get 'b' for 3")
      (assert-true (equal (finde tree 4) "a") nil "Did not get 'a' for 4")

      (free tree)

      (assert-true (eq (finde tree 4) nil) nil "Should not have found 4 after it was deleted.")
      (assert-true (eq (finde tree 3) nil) nil "Should not have found 3 after it was deleted.")
      (assert-true (eq (finde tree 2) nil) nil "Should not have found 2 after it was deleted.")
      (assert-true (eq (finde tree 1) nil) nil "Should not have found 1 after it was deleted.")))


  (define-test test/iterator ()
    (let ((tree (new-tree)))

      (insert tree 4 "a")
      (insert tree 3 "b")
      (insert tree 2 "c")
      (insert tree 1 "d")

      (with-tree-iterator (i tree)
        (assert-true (equal (multiple-value-list (i)) '(t 1 "d"))
                     nil "The first element should have been 'd'")
        (assert-true (equal (multiple-value-list (i)) '(t 2 "c"))
                     nil "Was expecting 'c' next.")
        (assert-true (equal (multiple-value-list (i)) '(t 3 "b"))
                     nil "Was expecting 'b' next yet again.")
        (assert-true (equal (multiple-value-list (i)) '(t 4 "a"))
                     nil "Was expecting 'a' next.")
        (assert-true (not (i)) nil "Was expecting nil this time.")
        (assert-true (not (i)) nil "The next item past the last one should have been nil.")
        (assert-true (not (i)) nil "The next item past the last one should have been nil again."))))

  );;flet


(define-test test/all ()
  (test/tree-creation)
  (test/node-erase)
  (test/tree-erase)
  (test/iterator))

;;;; THE END ;;;;
