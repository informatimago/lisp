;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               avl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;   An implementation of AVL trees (Adelson-Velskii & Landis Balanced Trees).
;;;;
;;;;EXAMPLE
;;;;
;;;;  (let ((tree (avl-empty)))
;;;;    (setf tree (avl-insert tree v1 comparef))
;;;;    (setf tree (avl-insert tree v2 comparef))
;;;;    (setf (avl-insert tree v3 comparef))
;;;;    (setf (avl-insert tree v4 comparef))
;;;;    (assert (eq v3 (avl-value (avl-search tree v3 comparef))))
;;;;    (avl-walk tree :prefix (lambda (v) (print "("))
;;;;                   :infix  (lambda (v) (print v))
;;;;                   :suffix (lambda (v) (print ")")) ))
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-09 <PJB> Created.
;;;;BUGS
;;;;    - Not debugged yet. There's a problem in avl-insert
;;;;      (the test function fails in avl-insert).
;;;;
;;;;    - Removing nodes is not implemented.
;;;;
;;;;    - The data type is not encapsulated. At least we should keep
;;;;      the compare function along with it. And some green functions
;;;;      (avl-count) would be nice too.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.AVL"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "AVL-WALK" "AVL-SEARCH" "AVL-VALUE" "AVL-INSERT" "AVL-EMPTY" "AVL")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
                "MAKE-LIST-OF-RANDOM-NUMBERS")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                "UNTIL" "WHILE")
  (:documentation
   "This package exports an AVL balanced binary tree data type.

    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.AVL")




(defstruct avl
  (left      nil :type (or null avl))
  (right     nil :type (or null avl))
  (unbalance 0   :type integer)
  (value)
  ) ;;AVL


(defun avl-rl (avl)
  "
DO:         Rotate left.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->droite!=NULL)
            a=<p,U,<q,V,W>>
POST:       a=<q,<p,U,V>,W>
"
  (assert (and avl (avl-right avl)))
  (psetf avl             (avl-right avl)
         (avl-right avl) (avl-left avl)
         (avl-left avl)  avl)
  avl) ;;AVL-RL


(defun avl-rr (avl)
  "
DO:         Rotate right.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->gauche!=NULL)
            a=<q,<p,U,V>,W>
POST:       a=<p,U,<q,V,W>>
"
  (assert (and avl (avl-left avl)))
  (psetf avl             (avl-left avl)
         (avl-left avl)  (avl-right avl)
         (avl-right avl)  avl)
  avl) ;;AVL-RR


(defun avl-rlr (avl)
  "
DO:         Rotate left-right.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->gauche!=NULL)&&(a->gauche->droite!=NULL)
            a=<r,<p,T,<q,U,V>>,W>
POST:       a=<q,<p<T,U>,<r,V,W>>
"
  (assert (and avl (avl-left avl) (avl-right (avl-left avl))))
  (setf (avl-left avl) (avl-rl (avl-left avl)))
  (setf avl (avl-rr avl))
  avl) ;;AVL-RLR


(defun avl-rrl (avl)
  "
DO:         Rotate right-left.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->droite!=NULL)&&(a->droite->gauche!=NULL)
            a=<r,T,<p,<q,U,V>,W>>
POST:       a=<q,<r,T,U>,<p,V,W>>
"
  (assert (and avl (avl-right avl) (avl-left (avl-right avl))))
  (setf (avl-right avl) (avl-rr (avl-right avl)))
  (setf avl (avl-rl avl))
  avl) ;;AVL-RRL


(defun avl-empty ()
  "
RETURN: A new empty AVL tree.
"
  nil) ;;AVL-EMPTY


(defun avl-search (avl value compare)
  "
COMPARE:  A function (x y) --> { -1, 0, +1 } indicating whether x<y, x=y or x>y.
RETURN:   The avl node such as (funcall compare value (avl-value avl)) == 0
          or nil of none found.
"
  (while avl
    (case (funcall compare value (avl-value avl))
      ((-1) (setf avl (avl-left avl)))
      ((0)  (return-from avl-search avl))
      ((1)  (setf avl (avl-right avl)))))
  nil) ;;AVL-SEARCH


(defun avl-insert (avl value compare)
  "
COMPARE:  A function (x y) --> { -1, 0, +1 } indicating whether x<y, x=y or x>y.
RETURN:   The modified avl tree where a new node has been added for value.
"
  (let (y a p aa pp)
    (setf y (make-avl :value value))
    (when (null avl) (return-from avl-insert y))
    (setf a  avl
          aa nil
          p  a
          pp nil)
    ;; aa est le pere de a; pp est le pere de p
    (while p
      ;; Descente a la recherche de la feuille, en memorisant le
      ;; dernier noeud pointe par a dont le desequilibrage est +/-1.
      (unless (zerop (avl-unbalance p))
        (setf a p  aa pp))
      (setf pp p)
      (if (<= (funcall compare value (avl-value p)) 0)
          (setf p (avl-left p))
          (setf p (avl-right p))))
    ;; adjonction
    (if (<= (funcall compare value (avl-value pp)) 0)
        (setf (avl-left pp) y)
        (setf (avl-right pp) y))
    ;; modification du desequilibre sur le chemin de a a y
    (setf p a)
    (until (eq p y)
      (if (<= (funcall compare value (avl-value p)) 0)
          (progn (incf (avl-unbalance p))
                 (setf p (avl-left p)))
          (progn (decf (avl-unbalance p))
                 (setf p (avl-right p)))))
    ;; reequilibrage
    (case (avl-unbalance a)
      ((-1 0 +1) (return-from avl-insert avl))
      ((+2)      (case (avl-unbalance (avl-left a))
                   ((+1)
                    (setf a (avl-rr a)
                          (avl-unbalance a) 0
                          (avl-unbalance (avl-right a)) 0))
                   ((-1)
                    (setf a (avl-rlr a))
                    (case (avl-unbalance a)
                      ((+1) (setf (avl-unbalance (avl-left  a)) 0
                                  (avl-unbalance (avl-right a)) -1))
                      ((-1) (setf (avl-unbalance (avl-left  a)) +1
                                  (avl-unbalance (avl-right a)) 0))
                      ((0) ;; case when (eq a y)
                       (setf (avl-unbalance (avl-left  a)) 0
                             (avl-unbalance (avl-right a)) 0)))
                    (setf (avl-unbalance a) 0))))
      ((-2)      (case (avl-unbalance (avl-right a))
                   ((-1)
                    (setf a (avl-rl a)
                          (avl-unbalance a) 0
                          (avl-unbalance (avl-left a)) 0))
                   ((+1)
                    (setf a (avl-rrl a))
                    (case (avl-unbalance a)
                      ((+1) (setf (avl-unbalance (avl-right a)) 0
                                  (avl-unbalance (avl-left  a)) +1))
                      ((-1) (setf (avl-unbalance (avl-right a)) -1
                                  (avl-unbalance (avl-left  a)) 0))
                      ((0) ;; case when (eq a y)
                       (setf (avl-unbalance (avl-right  a)) 0
                             (avl-unbalance (avl-left a)) 0)))
                    (setf (avl-unbalance a) 0)))))
    ;; mise a jour des pointeurs apres une rotation
    (cond
      ((null aa) (setf avl a))
      ((<= (funcall compare (avl-value a) (avl-value aa)) 0)
       (setf (avl-left aa) a))
      (t (setf (avl-right aa) a)))
    avl)) ;;AVL-INSERT


(defun avl-walk (avl &key (prefix (function identity))
                 (infix (function identity))
                 (suffix (function identity)))
  "
DO:  Walks the avl tree applying the functions prefix, infix, and suffix
     on the value of each node.
"
  (when avl
    (funcall prefix (avl-value avl))
    (avl-walk (avl-left avl)  :prefix prefix :infix infix :suffix suffix)
    (funcall infix (avl-value avl))
    (avl-walk (avl-right avl) :prefix prefix :infix infix :suffix suffix)
    (funcall suffix (avl-value avl)))
  ) ;;AVL-WALK



(defun test (size)
  (let* ((data (delete-duplicates (make-list-of-random-numbers size)))
         (hash (make-hash-table :size size))
         (tree (avl-empty)))
    (flet ((reset (hash) (maphash (lambda (k v)
                                    (declare (ignore k))
                                    (setf (second v) 0)) hash))
           (check (hash) (maphash (lambda (k v)
                                    (declare (ignore k))
                                    (assert (= 1 (second v)))) hash))
           (compare (a b) (cond ((< (first a) (first b)) -1)
                                ((> (first a) (first b)) +1)
                                (t 0))))
      ;; -1- fill a tree.
      (dolist (item data)
        (let ((record (list item 0)))
          (setf (gethash (first record) hash) record)
          (setf tree (avl-insert tree record (function compare)))))
      ;; -2- check we have all the data.
      (dolist (item data)
        (let* ((node (avl-search tree (gethash item hash) (function compare)))
               (record (and node (avl-value node))))
          (assert record)
          (assert (eql (first record) (gethash (second record) hash)))))
      ;; -3- walk the tree (three ways) and check we get all the data.
      (dolist (mode '(:prefix :infix :suffix))
        (reset hash)
        (avl-walk tree mode (lambda (record) (incf (third record))))
        (check hash))))) ;;test


;;;; avl.lisp                         --                     --          ;;;;
