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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.AVL"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT "AVL-WALK" "AVL-SEARCH" "AVL-VALUE" "AVL-INSERT" "AVL-EMPTY" "AVL")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.LIST"
                "MAKE-LIST-OF-RANDOM-NUMBERS")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
                "UNTIL" "WHILE")
  (:DOCUMENTATION
   "This package exports an AVL balanced binary tree data type.

    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.AVL")




(DEFSTRUCT AVL
  (LEFT      NIL :TYPE (OR NULL AVL))
  (RIGHT     NIL :TYPE (OR NULL AVL))
  (UNBALANCE 0   :TYPE INTEGER)
  (VALUE)
  ) ;;AVL


(DEFUN AVL-RL (AVL)
  "
DO:         Rotate left.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->droite!=NULL)
            a=<p,U,<q,V,W>>
POST:       a=<q,<p,U,V>,W>
"
  (ASSERT (AND AVL (AVL-RIGHT AVL)))
  (PSETF AVL             (AVL-RIGHT AVL)
         (AVL-RIGHT AVL) (AVL-LEFT AVL)
         (AVL-LEFT AVL)  AVL)
  AVL) ;;AVL-RL
    
    
(DEFUN AVL-RR (AVL)
  "
DO:         Rotate right.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->gauche!=NULL)
            a=<q,<p,U,V>,W>
POST:       a=<p,U,<q,V,W>>
"
  (ASSERT (AND AVL (AVL-LEFT AVL)))
  (PSETF AVL             (AVL-LEFT AVL)
         (AVL-LEFT AVL)  (AVL-RIGHT AVL)
         (AVL-RIGHT AVL)  AVL)
  AVL) ;;AVL-RR


(DEFUN AVL-RLR (AVL)
  "
DO:         Rotate left-right.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->gauche!=NULL)&&(a->gauche->droite!=NULL)
            a=<r,<p,T,<q,U,V>>,W>
POST:       a=<q,<p<T,U>,<r,V,W>>
"
  (ASSERT (AND AVL (AVL-LEFT AVL) (AVL-RIGHT (AVL-LEFT AVL))))
  (SETF (AVL-LEFT AVL) (AVL-RL (AVL-LEFT AVL)))
  (SETF AVL (AVL-RR AVL))
  AVL) ;;AVL-RLR
    
    
(DEFUN AVL-RRL (AVL)
  "
DO:         Rotate right-left.
RETURN:     The rotated tree.
PRE:        (a!=NULL)&&(a->droite!=NULL)&&(a->droite->gauche!=NULL)
            a=<r,T,<p,<q,U,V>,W>>
POST:       a=<q,<r,T,U>,<p,V,W>>
"
  (ASSERT (AND AVL (AVL-RIGHT AVL) (AVL-LEFT (AVL-RIGHT AVL))))
  (SETF (AVL-RIGHT AVL) (AVL-RR (AVL-RIGHT AVL)))
  (SETF AVL (AVL-RL AVL))
  AVL) ;;AVL-RRL


(DEFUN AVL-EMPTY ()
  "
RETURN: A new empty AVL tree.
"
  NIL) ;;AVL-EMPTY
    

(DEFUN AVL-SEARCH (AVL VALUE COMPARE)
  "
COMPARE:  A function (x y) --> { -1, 0, +1 } indicating whether x<y, x=y or x>y.
RETURN:   The avl node such as (funcall compare value (avl-value avl)) == 0
          or nil of none found.
"
  (WHILE AVL
    (CASE (FUNCALL COMPARE VALUE (AVL-VALUE AVL))
      ((-1) (SETF AVL (AVL-LEFT AVL)))
      ((0)  (RETURN-FROM AVL-SEARCH AVL))
      ((1)  (SETF AVL (AVL-RIGHT AVL)))))
  NIL) ;;AVL-SEARCH
    

(DEFUN AVL-INSERT (AVL VALUE COMPARE)
  "
COMPARE:  A function (x y) --> { -1, 0, +1 } indicating whether x<y, x=y or x>y.
RETURN:   The modified avl tree where a new node has been added for value.
"
  (LET (Y A P AA PP)
    (SETF Y (MAKE-AVL :VALUE VALUE))
    (WHEN (NULL AVL) (RETURN-FROM AVL-INSERT Y))
    (SETF A  AVL
          AA NIL
          P  A
          PP NIL)
    ;; aa est le pere de a; pp est le pere de p
    (WHILE P
      ;; Descente a la recherche de la feuille, en memorisant le 
      ;; dernier noeud pointe par a dont le desequilibrage est +/-1.
      (UNLESS (ZEROP (AVL-UNBALANCE P))
        (SETF A P  AA PP))
      (SETF PP P)
      (IF (<= (FUNCALL COMPARE VALUE (AVL-VALUE P)) 0)
          (SETF P (AVL-LEFT P))
          (SETF P (AVL-RIGHT P))))
    ;; adjonction
    (IF (<= (FUNCALL COMPARE VALUE (AVL-VALUE PP)) 0)
        (SETF (AVL-LEFT PP) Y)
        (SETF (AVL-RIGHT PP) Y))
    ;; modification du desequilibre sur le chemin de a a y 
    (SETF P A)
    (UNTIL (EQ P Y)
      (IF (<= (FUNCALL COMPARE VALUE (AVL-VALUE P)) 0)
          (PROGN (INCF (AVL-UNBALANCE P))
                 (SETF P (AVL-LEFT P)))
          (PROGN (DECF (AVL-UNBALANCE P))
                 (SETF P (AVL-RIGHT P)))))
    ;; reequilibrage
    (CASE (AVL-UNBALANCE A)
      ((-1 0 +1) (RETURN-FROM AVL-INSERT AVL))
      ((+2)      (CASE (AVL-UNBALANCE (AVL-LEFT A))
                   ((+1)
                    (SETF A (AVL-RR A)
                          (AVL-UNBALANCE A) 0
                          (AVL-UNBALANCE (AVL-RIGHT A)) 0))
                   ((-1)
                    (SETF A (AVL-RLR A))
                    (CASE (AVL-UNBALANCE A)
                      ((+1) (SETF (AVL-UNBALANCE (AVL-LEFT  A)) 0
                                  (AVL-UNBALANCE (AVL-RIGHT A)) -1))
                      ((-1) (SETF (AVL-UNBALANCE (AVL-LEFT  A)) +1
                                  (AVL-UNBALANCE (AVL-RIGHT A)) 0))
                      ((0) ;; case when (eq a y)
                       (SETF (AVL-UNBALANCE (AVL-LEFT  A)) 0
                             (AVL-UNBALANCE (AVL-RIGHT A)) 0)))
                    (SETF (AVL-UNBALANCE A) 0))))
      ((-2)      (CASE (AVL-UNBALANCE (AVL-RIGHT A))
                   ((-1)
                    (SETF A (AVL-RL A)
                          (AVL-UNBALANCE A) 0
                          (AVL-UNBALANCE (AVL-LEFT A)) 0))
                   ((+1)
                    (SETF A (AVL-RRL A))
                    (CASE (AVL-UNBALANCE A)
                      ((+1) (SETF (AVL-UNBALANCE (AVL-RIGHT A)) 0
                                  (AVL-UNBALANCE (AVL-LEFT  A)) +1))
                      ((-1) (SETF (AVL-UNBALANCE (AVL-RIGHT A)) -1
                                  (AVL-UNBALANCE (AVL-LEFT  A)) 0))
                      ((0) ;; case when (eq a y)
                       (SETF (AVL-UNBALANCE (AVL-RIGHT  A)) 0
                             (AVL-UNBALANCE (AVL-LEFT A)) 0)))
                    (SETF (AVL-UNBALANCE A) 0)))))
    ;; mise a jour des pointeurs apres une rotation
    (COND
      ((NULL AA) (SETF AVL A))
      ((<= (FUNCALL COMPARE (AVL-VALUE A) (AVL-VALUE AA)) 0)
       (SETF (AVL-LEFT AA) A))
      (T (SETF (AVL-RIGHT AA) A)))
    AVL)) ;;AVL-INSERT
    
    
(DEFUN AVL-WALK (AVL &KEY (PREFIX (FUNCTION IDENTITY))
                 (INFIX (FUNCTION IDENTITY))
                 (SUFFIX (FUNCTION IDENTITY)))
  "
DO:  Walks the avl tree applying the functions prefix, infix, and suffix
     on the value of each node.
"
  (WHEN AVL
    (FUNCALL PREFIX (AVL-VALUE AVL))
    (AVL-WALK (AVL-LEFT AVL)  :PREFIX PREFIX :INFIX INFIX :SUFFIX SUFFIX)
    (FUNCALL INFIX (AVL-VALUE AVL))
    (AVL-WALK (AVL-RIGHT AVL) :PREFIX PREFIX :INFIX INFIX :SUFFIX SUFFIX)
    (FUNCALL SUFFIX (AVL-VALUE AVL)))
  ) ;;AVL-WALK



(defun test (size)
  (let* ((data (delete-duplicates (MAKE-LIST-OF-RANDOM-NUMBERS size)))
         (hash (make-hash-table :size size))
         (tree (avl-empty)))
    (flet ((reset (hash) (maphash (lambda (k v)
                                    (declare (ignore k))
                                    (setf (second v) 0)) hash))
           (check (hash) (maphash (lambda (k v)
                                    (declare (ignore k))
                                    (assert (= 1 (second v)))) hash))
           (COMPARE (a b) (cond ((< (first a) (first b)) -1)
                                ((> (first a) (first b)) +1)
                                (t 0))))
      ;; -1- fill a tree.
      (dolist (item data)
        (let ((record (list item 0)))
          (setf (gethash (first record) hash) record)
          (setf tree (avl-insert tree record (FUNCTION COMPARE)))))
      ;; -2- check we have all the data.
      (dolist (item data)
        (let* ((node (avl-search tree (gethash item hash) (FUNCTION COMPARE)))
               (record (and node (avl-value node))))
          (assert record)
          (ASSERT (eql (first record) (gethash (second record) hash)))))
      ;; -3- walk the tree (three ways) and check we get all the data.
      (dolist (mode '(:prefix :infix :suffix))
        (reset hash)
        (avl-walk tree mode (lambda (record) (incf (third record))))
        (check hash))))) ;;test


;;;; avl.lisp                         --                     --          ;;;;
