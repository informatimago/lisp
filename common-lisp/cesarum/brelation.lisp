;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               brelation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package implements a relation abstract data type
;;;;    based on an array of bset.
;;;;    It can represent only relations between two positive
;;;;    and bounded integers.
;;;;
;;;;    (Inspired by Modula-2 cocktail-9309/reuse/src/Relations.md).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-02-19 <PJB> Created.
;;;;BUGS
;;;;
;;;;    This is not as lispy as could be (we may want to have brelation
;;;;    of random lisp objects, notably symbols), but it's optimized
;;;;    for small integers.
;;;;
;;;;    There are no checks for mismatching operands
;;;;    (relations with different sizes)
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET"))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION"
  (:use "COMMON-LISP")
  (:export "PROJECT-2" "PROJECT-1" "WRITE-BRELATION" "READ-BRELATION" 
           "FOR-ALL-DO" "EXISTS-1" "EXISTS" "FOR-ALL" "EXTRACT" "SELECT" "CARDINAL"
           "IS-EMPTY" "IS-NOT-EQUAL" "IS-EQUAL" "IS-STRICT-SUBSET" "IS-SUBSET"
           "COMPLEMENT" "SYM-DIFF" "INTERSECTION" "DIFFERENCE" "UNION" "ASSIGN"
           "ASSIGN-ELEMENT" "ASSIGN-EMPTY" "CLOSURE" "GET-CYCLICS" "IS-CYCLIC"
           "HAS-REFLEXIVE" "IS-EQUIVALENCE" "IS-TRANSITIVE" "IS-SYMMETRIC"
           "IS-REFLEXIVE" "IS-TRANSITIVE-1" "IS-REFLEXIVE-1" "IS-RELATED" "IS-ELEMENT"
           "EXCLUDE" "INCLUDE" "MAKE-BRELATION" "BRELATION")
  (:shadow "COMPLEMENT" "INTERSECTION" "UNION")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "VECTOR-INIT" "FOR")
  (:documentation
   "

This package implements a relation abstract data type based on an
array of bset.  It can represent only relations between two positive
and bounded integers.

\(Inspired by Modula-2 cocktail-9309/reuse/src/Relations.md).

See also: COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see http://www.gnu.org/licenses/

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION")



(defstruct (brelation (:constructor %make-brelation))
  "The Binary Relation Class."
  (adjsets (make-array '(0) :element-type 'com.informatimago.common-lisp.cesarum.bset:bset
                       :initial-element (com.informatimago.common-lisp.cesarum.bset:make-bset 0))
           :type (array com.informatimago.common-lisp.cesarum.bset:bset (*)))
  (size-1 0 :type (integer 0))
  (size-2 0 :type (integer 0)))



(defun make-brelation (size-1 size-2)
  "
RETURN: A new BRELATION between sets of sizes SIZE-1 and SIZE-2.
"
  (declare (type (integer 0) size-1 size-2))
  (%make-brelation
   :adjsets (vector-init (make-array (list (1+ size-1))
                                     :element-type 'com.informatimago.common-lisp.cesarum.bset:bset
                                     :initial-element (com.informatimago.common-lisp.cesarum.bset:make-bset 0))
                         (lambda (index)
                           (declare (ignore index))
                           (com.informatimago.common-lisp.cesarum.bset:make-bset size-2)))
   :size-1 size-1
   :size-2 size-2))


(defmacro imply (p q)
  "
RETURN: P ⇒ Q
NOTE:   This short circuits the evaluation of Q if P is false.
"
  `(or (not ,p) ,q))

(defmacro adjref (rel i)
  `(aref (brelation-adjsets ,rel) ,i))

(defmacro related (rel e1 e2)
  `(com.informatimago.common-lisp.cesarum.bset:is-element ,e2 (adjref ,rel ,e1)))


(defun include (rel e1 e2)
  "
DO:     Adds (E1 E2) to the relation REL.
POST:   REL(E1,E2)
"
  (declare (type (integer 0) e1 e2))
  (com.informatimago.common-lisp.cesarum.bset:include (adjref rel e1) e2)
  rel)


(defun exclude (rel e1 e2)
  "
DO:     Remove (E1 E2) from the relation REL.
POST:   ¬ REL(E1,E2)
"
  (declare (type (integer 0) e1 e2))
  (com.informatimago.common-lisp.cesarum.bset:exclude (adjref rel e1) e2)
  rel)


(defun is-element (e1 e2 rel)
  "
RETURN: Whether REL(E1,E2).
"
  (declare (type (integer 0) e1 e2))
  (related rel e1 e2))


(defun is-related (e1 e2 rel)
  "
RETURN: Whether REL(E1,E2).
"
  (declare (type (integer 0) e1 e2))
  (related rel e1 e2))


(defun is-reflexive-1 (e1 rel)
  "
RETURN: Whether REL(E1,E1)
"
  (declare (type (integer 0) e1))
  (related rel e1 e1))


(defun is-symmetric-1 (e1 e2 rel)
    "
RETURN: Whether REL(E1,E2) ∧ REL(E2,E1)
"
  (declare (type (integer 0) e1 e2))
  (imply (related rel e1 e2) (related rel e2 e1)))


(defun is-transitive-1 (e1 e2 e3 rel)
      "
RETURN: Whether (REL(E1,E2) ∧ REL(E2,E3)) ⇒ REL(E1,E3)
NOTE:   Tests the transitivity of the relation REL only on the
        elements E1, E2, and E3.  This doesn't mean the relation REL
        is transitive (but it's a necessary condition).
"
  (declare (type (integer 0) e1 e2 e3))
  (imply (and (related rel e1 e2) (related rel e2 e3)) (related rel e1 e3)))


(defun is-reflexive (rel)
  "
RETURN: Whether the relation REL is reflexive. Ie. ∀i∈[0,SIZE1-1], REL(i,i)
"
  (for (i 0 (brelation-size-1 rel))
       (unless (related rel i i) (return-from is-reflexive nil)))
  t)


(defun is-symmetric (rel)
  "
RETURN: Whether the relation REL is symetric. Ie. ∀(i,j)∈[0,SIZE1-1]², REL(i,j) ⇒ REL(j,i)
"
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:for-all (adjref rel i)
                                                                   (lambda (j) (related rel j i)))
         (return-from is-symmetric nil)))
  t)


(defun is-transitive (rel)
   "
RETURN: Whether the relation REL is transitive. Ie. ∀(i,j,k)∈[0,SIZE1-1]³, REL(i,j) ∧ REL(j,k) ⇒ REL(i,k)
"
   (let ((r (make-brelation (brelation-size-1 rel) (brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (is-equal r rel) ))


(defun is-equivalence (rel)
  "
RETURN: Whether REL is an equivalence relation. Ie. REL is reflexive, symetric and transitive.
"
  (and (is-reflexive rel) (is-symmetric rel) (is-transitive rel)))


(defun has-reflexive (rel)
  "
RETURN: ∃i∈[0,SIZE1-1], REL(i,i)
"
  (for (i 0 (brelation-size-1 rel))
       (when (related rel i i) (return-from has-reflexive t)))
  nil)


(defmacro until (condition &body body) `(do () (,condition) ,@body))


(defun is-cyclic (rel)
  "
RETURN: Whether the relation REL is cyclic.
"
  (let ((with-pred (com.informatimago.common-lisp.cesarum.bset:make-bset (brelation-size-1 rel)))
        (without-pred (com.informatimago.common-lisp.cesarum.bset:make-bset (brelation-size-1 rel)))
        (pred-count (make-array (list (1+ (brelation-size-1 rel)))
                                :element-type '(integer 0)
                                :initial-element 0)))
    (for (i 0 (brelation-size-1 rel))
         (com.informatimago.common-lisp.cesarum.bset:for-all-do (adjref rel i)
                          (lambda (e) (incf (aref pred-count e)))))
    (for (i 0 (brelation-size-1 rel))
         (when (= 0 (aref pred-count i))   (com.informatimago.common-lisp.cesarum.bset:include without-pred i)))
    (com.informatimago.common-lisp.cesarum.bset:complement with-pred)
    (until (com.informatimago.common-lisp.cesarum.bset:is-empty without-pred)
      (let ((i (com.informatimago.common-lisp.cesarum.bset:extract without-pred)))
        (com.informatimago.common-lisp.cesarum.bset:exclude with-pred i)
        (com.informatimago.common-lisp.cesarum.bset:for-all-do (adjref rel i)
                         (lambda (e) (decf (aref pred-count e))
                            (when (= 0 (aref pred-count e))
                              (com.informatimago.common-lisp.cesarum.bset:include without-pred e))))))
    (not (com.informatimago.common-lisp.cesarum.bset:is-empty with-pred))))



(defun get-cyclics (rel bset)
  "
RETURN: The set of elements that are in cycles.
"
  (let ((r (make-brelation (brelation-size-1 rel)(brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (com.informatimago.common-lisp.cesarum.bset:assign-empty bset)
    (for (i 0 (brelation-size-1 rel))
         (when (related r i i) (com.informatimago.common-lisp.cesarum.bset:include bset i))))
  bset)


(defun assign-empty (rel)
  "
POST:   REL is the empty relation.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel i)))
  rel)


(defun assign-element (rel e1 e2)
    "
POST:   REL contains only (E1,E2).
RETURN: REL
"
  (assign-empty rel)
  (include rel e1 e2)
  rel)


(defun assign (rel1 rel2)
  "
POST:   REL1 is a copy of REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel1 i))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel2 i)))
  rel1)


(defun closure (rel)
    "
POST:   REL is the transitive closure of the old REL.
RETURN: REL
"
  (for (j 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty (adjref rel j))
         (for (i 0 (brelation-size-1 rel))
              (when (related rel i j)
                (com.informatimago.common-lisp.cesarum.bset:union (adjref rel i)
                            (adjref rel j))))))
  rel)
           
  
(defun union (rel1 rel2)
     "
POST:   REL1 is the union of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:union (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun difference (rel1 rel2)
  "
POST:   REL1 is the difference of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:difference (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun intersection (rel1 rel2)
    "
POST:   REL1 is the intersection of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:intersection (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun sym-diff (rel1 rel2)
  "
POST:   REL1 is the symetric difference of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:sym-diff (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun complement (rel)
  "
POST:   REL is the complement of old REL.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:complement (adjref rel i)))
  rel)


(defun is-subset (rel1 rel2)
  "
RETURN: Whether REL1 is a subset of REL2.
"
  (for (i 0 (brelation-size-1 rel1))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-subset (adjref rel1 i) (adjref rel2 i))
         (return-from is-subset nil)))
  t)


(defun is-strict-subset (rel1 rel2)
    "
RETURN: Whether REL1 is a strict subset of REL2.
"
  (and (is-subset rel1 rel2) (is-not-equal rel1 rel2)))


(defun is-equal (rel1 rel2)
      "
RETURN: Whether REL1 is equal to REL2.
"
  (for (i 0 (brelation-size-1 rel1))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-equal  (adjref rel1 i) (adjref rel2 i))
         (return-from is-equal nil)))
  t)


(defun is-not-equal (rel1 rel2)
        "
RETURN: Whether REL1 is not equal to REL2.
"
  (not (is-equal rel1 rel2)))


(defun is-empty (rel)
          "
RETURN: Whether REL is empty.
"
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty  (adjref rel i))
         (return-from is-empty  nil)))
  t)


(defun cardinal (rel)
  "
RETURN: The number of couples in the relation REL.
"
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (incf n (com.informatimago.common-lisp.cesarum.bset:cardinal (adjref rel i))))
    n))
    

(defun select (rel)
    "
RETURN: (values i j) such as REL(i,j), or NIL if REL is empty.
"
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty (adjref rel i))
         (return-from select (values i (com.informatimago.common-lisp.cesarum.bset:select (adjref rel i))))))
  nil)


(defun extract (rel)
  "
DO:     Selects a couple in the relation REL, exclude it from REL, and return it.
PRE:    (not (is-empty rel))
POST:   ¬REL(i,j)
RETURN: (values i j) such as old REL(i,j), or NIL if REL is empty.
" 
  (multiple-value-bind (e1 e2) (select rel)
    (when e2
      (exclude rel e1 e2)
      (values e1 e2))))


(defun for-all (rel proc)
  "
DO:     Calls PROC on couples of the relation REL while it returns true.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for all couples.
"
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:for-all (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from for-all nil)))
  t)


(defun exists (rel proc)
   "
DO:     Calls PROC on  couples of the relation REL until it returns true.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for at least one couple.
"
  (for (i 0 (brelation-size-1 rel))
       (when (com.informatimago.common-lisp.cesarum.bset:exists (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from exists t)))
  nil)
  

(defun exists-1 (rel proc)
  "
DO:     Calls PROC on each couples of the relation REL.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for exactly one couple.
"
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (when (com.informatimago.common-lisp.cesarum.bset:exists (adjref rel i) (lambda (e) (funcall proc i e)))
           (incf n)))
    (= n 1)))


(defun for-all-do (rel proc)
  "
DO:     Calls PROC on each couple of the relation REL.
PROC:   A function of two elements.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:for-all-do (adjref rel i) (lambda (e) (funcall proc i e))))
  rel)


(defun read-brelation (stream rel)
  "
DO:     Read a relation from the STREAM.
POST:   REL is the relation read.
RETURN: REL.
NOTE:   The serialization format is that of a list of adjacency lists.
        ((1 (2 3)) (2 (3)) (4)) = ({1 2 3 4} {(1 2) (1 3) (2 3)})
"
  (assign-empty rel)
  (when (peek-char (character "(") stream nil nil)
    (read-char stream)
    (do ()
        ((char= (peek-char t stream nil (character ")")) (character ")"))
         (read-char stream))
      (let ((i (read stream)))
        
        (when (peek-char (character "(") stream nil nil)
          (read-char stream)
          (do ()
              ((char= (peek-char t stream nil (character ")")) (character ")"))
               (read-char stream))
            (include rel i (read stream)))))))
  rel)


(defun write-brelation (stream rel)
  "
DO:     Write the relation REL to the STREAM.
RETURN: REL.
"
  (princ "(" stream)
  (for (i 0 (brelation-size-1 rel))
       (princ i stream)
       (com.informatimago.common-lisp.cesarum.bset:write-bset stream (adjref rel i))
       (terpri stream))
  (princ ")" stream)
  rel)

       
(defun project-1 (rel e1 bset)
  "
POST:   BSET is the set of all elements I that are in relation REL(I,E2).
RETURN: BSET
"
  (assign-empty bset)
  (for (i 0 (brelation-size-1 rel))
       (when (related rel i e1)
         (com.informatimago.common-lisp.cesarum.bset:include bset i)))
  bset)


(defun project-2 (rel e1 bset)
  "
POST:   BSET is the set of all elements E2 that are in relation REL(E1,E2).
RETURN: BSET
"
  (assign bset (adjref rel e1)))


;;;; THE END ;;;;
