;;;; -*- coding:utf-8 -*-
;;****************************************************************************
;;FILE:               dll.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    A doubly-linked list.
;;    
;;AUTHORS
;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-04-28 <PJB> Clean-up.
;;    2004-03-01 <PJB> Created.
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal J. Bourguignon 2004 - 2005
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DLL"
  (:USE "COMMON-LISP")
  (:EXPORT "DLL-DELETE" "DLL-INSERT" "DLL-NODE-POSITION" "DLL-NODE-NTH"
           "DLL-NODE-ITEM" "DLL-NODE-PREVIOUS" "DLL-NODE-NEXT" "DLL-NODE" "DLL-LAST"
           "DLL-FIRST" "DLL-POSITION" "DLL-NTH" "DLL-CONTENTS" "DLL-NCONC" "DLL-APPEND"
           "DLL-COPY" "DLL-EQUAL" "DLL-LENGTH" "DLL-EMPTY-P" "DLL-LAST-NODE"
           "DLL-FIRST-NODE" "DLL")
  (:DOCUMENTATION
   "This module exports a double-linked list type.
    This is a structure optimized insertions and deletions in any place,
    each node keeping a pointer to both the previous and the next node.
    The stub keeps a pointer to the head of the list, and the list is
    circularly closed (the tail points to the head).
    
    Copyright Pascal J. Bourguignon 2001 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DLL")





(defstruct (dll (:conc-name %DLL-))
  (first    nil)
  (last     nil))


(defstruct dll-node
  (previous nil)
  (next     nil)
  (item     nil))


(defun dll (&rest list)
  (let ((dlist (make-dll))
        (current nil))
    (map nil (lambda (item) (setf current (dll-append dlist current item))) list)
    dlist))

(defun dll-first-node (dlist) (%dll-first dlist))
(defun dll-last-node  (dlist) (%dll-last dlist))

(defun dll-empty-p (dlist)
  (null (%dll-first dlist)))


(defun dll-length (dlist)
  (do ((len 0 (1+ len))
       (current (%dll-last dlist) (dll-node-previous current)))
      ((null current) len)))


(defun dll-nth (index dlist)
  (do ((i 0 (1+ i))
       (current (%dll-first dlist) (dll-node-next current)))
      ((or (null current) (= i index))
       (when current (dll-node-item current)))))


(defun dll-position (item dlist &key (test (function eql)))
  (do ((i 0 (1+ i))
       (current (%dll-first dlist) (dll-node-next current)))
      ((or (null current) (funcall test (dll-node-item current) item))
       (when current i))))


(defun dll-node-position (item dlist &key (test (function eql)))
  (do ((i 0 (1+ i))
       (current (%dll-first dlist) (dll-node-next current)))
      ((or (null current) (funcall test current item))
       (when current i))))


(defun dll-equal (&rest dlls)
  (or
   (null dlls)
   (null (cdr dlls))
   (and
    (let ((left (first dlls))
          (right (second dlls)))
      (and
       (equal (dll-node-item (%dll-first left))
              (dll-node-item (%dll-first right)))
       (equal (dll-node-item (%dll-last left))
              (dll-node-item (%dll-last right)))
       (do ((lnodes (dll-node-next (%dll-first left))
                    (dll-node-next lnodes))
            (rnodes (dll-node-next (%dll-first right))
                    (dll-node-next rnodes)))
           ((or (eq lnodes (%dll-last left))
                (eq rnodes (%dll-last right))
                (not (equal (dll-node-item lnodes) (dll-node-item rnodes))))
            (and (eq lnodes (%dll-last left))
                 (eq rnodes (%dll-last right)))))
       (dll-equal (cdr dlls)))))))


(defun dll-copy (dlist)
  (do ((new-dll (make-dll))
       (src (%dll-first dlist) (dll-node-next src))
       (dst nil))
      ((null src) new-dll)
    (setf dst (dll-insert new-dll dst (dll-node-item src)))))


(defun dll-append (&rest dlls)
  (apply (function dll-nconc) (mapcar (function dll-copy) dlls)))


(defun dll-nconc (&rest dlls)
  "
PRE:   No dll appears twice in dlls.
DO:    Extract the nodes from all but the first dll,
       and append them to that first dll.
"
  (if (null dlls)
      (make-dll)
      (do ((result  (do ((dll (pop dlls) (pop dlls)))
                        ((%dll-first dll) dll)))
           (dlls dlls (cdr dlls))
           (dll))
          ((null dlls) result)
        (setf dll (car dlls))
        (let ((first (%dll-first dll)))
          (unless (null first)
            (setf (dll-node-previous first) (%dll-last result)
                  (dll-node-next (%dll-last result)) first
                  (%dll-last result) (%dll-last dll)
                  (%dll-first dll) nil
                  (%dll-last dll) nil))))))



(defun dll-contents (dlist)
  "
RETURN:  A new list containing the items of the dll.
"
  (do ((current (%dll-last dlist) (dll-node-previous current))
       (result ()))
      ((null current) result)
    (push (dll-node-item current) result)))


(defun dll-first (dlist)
  (unless (dll-empty-p  dlist)  (dll-node-item (%dll-first dlist))))


(defun dll-last  (dlist)
  (unless (dll-empty-p  dlist)  (dll-node-item (%dll-last  dlist))))


(defun DLL-NODE-NTH (index dlist)
  (do ((i 0 (1+ i))
       (current (%dll-first dlist) (dll-node-next current)))
      ((or (null current) (= i index)) current)))
      




(defun dll-insert (dlist node item)
  "
DO:     Insert a new node after node, or before first position when (null node).
RETURN: The new node.
"
  (let ((new-node nil))
    (cond
      ((dll-empty-p dlist) ;; first item
       (setf new-node (make-dll-node :item item))
       (setf (%dll-first dlist) new-node
             (%dll-last dlist) (%dll-first dlist)))
      ((null node) ;; insert before first
       (setf new-node (make-dll-node :previous nil
                                     :next     (dll-first-node dlist)
                                     :item     item))
       (setf (dll-node-previous (%dll-first dlist)) new-node
             (%dll-first dlist) new-node))
      ((not (dll-node-position node dlist))
       (error "Node not in doubly-linked list."))
      (t
       (setf new-node (make-dll-node :previous node
                                     :next     (dll-node-next node)
                                     :item     item))
       (if (dll-node-next node)
           (setf (dll-node-previous (dll-node-next node)) new-node)
           (setf (%dll-last dlist) new-node))
       (setf (dll-node-next node) new-node)))
    new-node))


(defun dll-extract-node (dlist node)
  (if (eq (dll-first-node dlist) node)
      (setf (%dll-first dlist) (dll-node-next node))
      (setf (dll-node-next (dll-node-previous node)) (dll-node-next node)))
  (if (eq (dll-last-node dlist) node)
      (setf (%dll-last dlist) (dll-node-previous node))
      (setf (dll-node-previous (dll-node-next node)) (dll-node-previous node)))
  dlist)


(defun dll-delete (node dlist)
  (unless (or (null node) (dll-empty-p dlist)
              (not (dll-node-position node dlist))) ;; Note O(N)!
    (dll-extract-node dlist node))
  dlist)


(defun dll-delete-nth (index dlist)
  (dll-extract-node dlist (dll-node-nth index dlist)))

;;;; dll.lisp                         --                     --          ;;;;
