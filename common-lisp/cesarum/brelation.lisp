;;;; -*- mode:lisp; coding:utf-8 -*-
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
;;;;    Copyright Pascal J. Bourguignon 2004 - 2015
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET"
                          "COMPLEMENT" "INTERSECTION" "UNION" "SUBSETP")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                "VECTOR-INIT" "FOR" "UNTIL")
  (:export "PROJECT-2" "PROJECT-1" "WRITE-BRELATION" "READ-BRELATION" 
           "FOR-ALL-DO" "EXISTS-1" "EXISTS" "FOR-ALL" "EXTRACT" "SELECT" "CARDINAL"
           "EMPTYP" "IS-NOT-EQUAL" "IS-EQUAL" "IS-STRICT-SUBSET" "IS-SUBSET"
           "COMPLEMENT" "SYM-DIFF" "INTERSECTION" "DIFFERENCE" "UNION" "ASSIGN"
           "ASSIGN-ELEMENT" "ASSIGN-EMPTY" "CLOSURE" "GET-CYCLICS" "IS-CYCLIC"
           "HAS-REFLEXIVE" "IS-EQUIVALENCE" "IS-TRANSITIVE" "IS-SYMMETRIC"
           "IS-REFLEXIVE" "IS-TRANSITIVE-1" "IS-REFLEXIVE-1" "IS-RELATED" "IS-ELEMENT"
           "EXCLUDE" "INCLUDE" "MAKE-BRELATION" "BRELATION")
  (:documentation
   "

This package implements a relation abstract data type based on an
array of bset.  It can represent only relations between two positive
and bounded integers.

\(Inspired by Modula-2 cocktail-9309/reuse/src/Relations.md).

See also: COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2015
    
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
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION")

(defstruct (brelation (:constructor %make-brelation))
  "The Binary Relation Class."
  (adjsets (make-array '(0) :element-type 'bset
                            :initial-element (make-bset 0))
           :type (array bset (*)))
  (size-1 0 :type element)
  (size-2 0 :type element))



(defun make-brelation (size-1 size-2)
  "
RETURN: A new BRELATION between sets of sizes SIZE-1 and SIZE-2.
"
  (check-type size-1 element)
  (check-type size-2 element)
  (%make-brelation
   :adjsets (vector-init (make-array (list (1+ size-1))
                                     :element-type 'bset
                                     :initial-element (make-bset 0))
                         (lambda (index)
                           (declare (ignore index))
                           (make-bset size-2)))
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
  `(is-element ,e2 (adjref ,rel ,e1)))


(deftype arc () 'cons)
(defun arc (e1 e2) (cons e1 e2))
(defun arc-from (arc) (car arc))
(defun arc-to   (arc) (cdr arc))
(defmacro with-arc (((e1 e2) arc) &body body)
  (let ((varc (gensym)))
    `(let ((,varc ,arc))
       (check-type ,varc arc)
       (let ((,e1 (arc-from ,varc))
             (,e2 (arc-to   ,varc)))
         (check-type ,e1 element)
         (check-type ,e2 element)
         ,@body))))

(defmethod include ((rel brelation) arc)
  "
DO:     Adds (E1 E2) to the relation REL.
POST:   REL(E1,E2)
"
  (with-arc ((e1 e2) arc)
    (include (adjref rel e1) e2))
  rel)

(defmethod exclude ((rel brelation) arc)
  "
DO:     Remove (E1 E2) from the relation REL.
POST:   ¬ REL(E1,E2)
"
  (with-arc ((e1 e2) arc)
    (exclude (adjref rel e1) e2))
  rel)

(defmethod is-element (arc (rel brelation))
  "
RETURN: Whether REL(E1,E2).
"
  (with-arc ((e1 e2) arc)
    (related rel e1 e2)))

(defgeneric is-related (e1 e2 rel))
(defmethod is-related (e1 e2 (rel brelation))
  "
RETURN: Whether REL(E1,E2).
"
  (related rel e1 e2))

(defgeneric is-reflexive-1 (e1 rel))
(defmethod is-reflexive-1 (e1 (rel brelation))
  "
RETURN: Whether REL(E1,E1)
"
  (check-type e1 element)
  (related rel e1 e1))

(defgeneric is-symmetric-1 (e1 e2 rel))
(defmethod is-symmetric-1 (e1 e2 (rel brelation))
  "
RETURN: Whether REL(E1,E2) ∧ REL(E2,E1)
"
  (check-type e1 element)
  (check-type e2 element)
  (imply (related rel e1 e2) (related rel e2 e1)))

(defgeneric is-transitive-1 (e1 e2 e3 rel))
(defmethod is-transitive-1 (e1 e2 e3 (rel brelation))
  "
RETURN: Whether (REL(E1,E2) ∧ REL(E2,E3)) ⇒ REL(E1,E3)
NOTE:   Tests the transitivity of the relation REL only on the
        elements E1, E2, and E3.  This doesn't mean the relation REL
        is transitive (but it's a necessary condition).
"
  (check-type e1 element)
  (check-type e2 element)
  (check-type e3 element)
  (imply (and (related rel e1 e2) (related rel e2 e3)) (related rel e1 e3)))


(defgeneric is-reflexive (rel))
(defmethod is-reflexive ((rel brelation))
  "
RETURN: Whether the relation REL is reflexive. Ie. ∀i∈[0,SIZE1-1], REL(i,i)
"
  (for (i 0 (brelation-size-1 rel))
    (unless (related rel i i) (return-from is-reflexive nil)))
  t)

(defgeneric is-symmetric (rel))
(defmethod is-symmetric ((rel brelation))
  "
RETURN: Whether the relation REL is symetric. Ie. ∀(i,j)∈[0,SIZE1-1]², REL(i,j) ⇒ REL(j,i)
"
  (for (i 0 (brelation-size-1 rel))
    (unless (for-all (adjref rel i)
                     (lambda (j) (related rel j i)))
      (return-from is-symmetric nil)))
  t)


(defgeneric is-transitive (rel))
(defmethod is-transitive ((rel brelation))
  "
RETURN: Whether the relation REL is transitive. Ie. ∀(i,j,k)∈[0,SIZE1-1]³, REL(i,j) ∧ REL(j,k) ⇒ REL(i,k)
"
  (let ((r (make-brelation (brelation-size-1 rel) (brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (is-equal r rel) ))


(defgeneric is-equivalence (rel))
(defmethod is-equivalence ((rel brelation))
  "
RETURN: Whether REL is an equivalence relation. Ie. REL is reflexive, symetric and transitive.
"
  (and (is-reflexive rel) (is-symmetric rel) (is-transitive rel)))


(defgeneric has-reflexive (rel))
(defmethod has-reflexive ((rel brelation))
  "
RETURN: ∃i∈[0,SIZE1-1], REL(i,i)
"
  (for (i 0 (brelation-size-1 rel))
    (when (related rel i i) (return-from has-reflexive t)))
  nil)

(defgeneric is-cyclic (rel))
(defmethod is-cyclic ((rel brelation))
  "
RETURN: Whether the relation REL is cyclic.
"
  (let ((with-pred    (make-bset (brelation-size-1 rel)))
        (without-pred (make-bset (brelation-size-1 rel)))
        (pred-count   (make-array (list (1+ (brelation-size-1 rel)))
                                  :element-type 'element
                                  :initial-element 0)))
    (for (i 0 (brelation-size-1 rel))
      (for-all-do (adjref rel i)
                  (lambda (e) (incf (aref pred-count e)))))
    (for (i 0 (brelation-size-1 rel))
      (when (= 0 (aref pred-count i))
        (include without-pred i)))
    (complement with-pred)
    (until (emptyp without-pred)
      (let ((i (extract without-pred)))
        (exclude with-pred i)
        (for-all-do (adjref rel i)
                    (lambda (e) (decf (aref pred-count e))
                      (when (= 0 (aref pred-count e))
                        (include without-pred e))))))
    (not (emptyp with-pred))))

(defgeneric get-cyclics (rel bset))
(defmethod get-cyclics ((rel brelation) (bset bset))
  "
RETURN: The set of elements that are in cycles.
"
  (let ((r (make-brelation (brelation-size-1 rel)(brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (assign-empty bset)
    (for (i 0 (brelation-size-1 rel))
      (when (related r i i) (include bset i))))
  bset)

(defmethod assign-empty ((rel brelation))
  "
POST:   REL is the empty relation.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (assign-empty (adjref rel i)))
  rel)

(defmethod assign-element ((rel brelation) arc)
    "
POST:   REL contains only (E1,E2).
RETURN: REL
"
  (with-arc ((e1 e2) arc)
    (assign-empty rel)
    (include rel (arc e1 e2)))
  rel)

(defmethod assign ((rel1 brelation) (rel2 brelation))
  "
POST:   REL1 is a copy of REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (assign-empty (adjref rel1 i))
       (assign-empty (adjref rel2 i)))
  rel1)

(defgeneric closure (rel))
(defmethod closure ((rel brelation))
  "
POST:   REL is the transitive closure of the old REL.
RETURN: REL
"
  (for (j 0 (brelation-size-1 rel))
    (unless (emptyp (adjref rel j))
      (for (i 0 (brelation-size-1 rel))
        (when (related rel i j)
          (union (adjref rel i)
                 (adjref rel j))))))
  rel)
           
(defmethod union ((rel1 brelation) (rel2 brelation))
     "
POST:   REL1 is the union of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (union (adjref rel1 i) (adjref rel2 i)))
  rel1)

(defmethod difference ((rel1 brelation) (rel2 brelation))
  "
POST:   REL1 is the difference of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (difference (adjref rel1 i) (adjref rel2 i)))
  rel1)

(defmethod intersection ((rel1 brelation) (rel2 brelation))
    "
POST:   REL1 is the intersection of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (intersection (adjref rel1 i) (adjref rel2 i)))
  rel1)

(defmethod sym-diff ((rel1 brelation) (rel2 brelation))
  "
POST:   REL1 is the symetric difference of old REL1 and REL2.
RETURN: REL1
"
  (for (i 0 (brelation-size-1 rel1))
       (sym-diff (adjref rel1 i) (adjref rel2 i)))
  rel1)

(defmethod complement ((rel brelation))
  "
POST:   REL is the complement of old REL.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (complement (adjref rel i)))
  rel)

(defmethod subsetp ((rel1 brelation) (rel2 brelation))
  "
RETURN: Whether REL1 is a subset of REL2.
"
  (for (i 0 (brelation-size-1 rel1))
       (unless (subsetp (adjref rel1 i) (adjref rel2 i))
         (return-from subsetp nil)))
  t)

(defmethod strict-subsetp ((rel1 brelation) (rel2 brelation))
    "
RETURN: Whether REL1 is a strict subset of REL2.
"
  (and (is-subset rel1 rel2) (is-not-equal rel1 rel2)))

(defmethod is-equal ((rel1 brelation) (rel2 brelation))
  "
RETURN: Whether REL1 is equal to REL2.
"
  (for (i 0 (brelation-size-1 rel1))
    (unless (is-equal  (adjref rel1 i) (adjref rel2 i))
      (return-from is-equal nil)))
  t)

(defmethod is-not-equal ((rel1 brelation) (rel2 brelation))
  "
RETURN: Whether REL1 is not equal to REL2.
"
  (not (is-equal rel1 rel2)))


(defmethod emptyp ((rel brelation))
  "
RETURN: Whether REL is empty.
"
  (for (i 0 (brelation-size-1 rel))
    (unless (emptyp (adjref rel i))
      (return-from emptyp nil)))
  t)

(defmethod cardinal ((rel brelation))
  "
RETURN: The number of couples in the relation REL.
"
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (incf n (cardinal (adjref rel i))))
    n))
    
(defmethod select ((rel brelation))
    "
RETURN: (values i j) such as REL(i,j), or NIL if REL is empty.
"
  (for (i 0 (brelation-size-1 rel))
       (unless (emptyp (adjref rel i))
         (return-from select (values i (select (adjref rel i))))))
  nil)

(defmethod extract ((rel brelation))
  "
DO:     Selects a couple in the relation REL, exclude it from REL, and return it.
PRE:    (not (emptyp rel))
POST:   ¬REL(i,j)
RETURN: (values i j) such as old REL(i,j), or NIL if REL is empty.
" 
  (multiple-value-bind (e1 e2) (select rel)
    (when e2
      (exclude rel (arc e1 e2))
      (values e1 e2))))


(defmethod for-all ((rel brelation) proc)
  "
DO:     Calls PROC on couples of the relation REL while it returns true.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for all couples.
"
  (for (i 0 (brelation-size-1 rel))
       (unless (for-all (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from for-all nil)))
  t)

(defmethod exists ((rel brelation) proc)
   "
DO:     Calls PROC on  couples of the relation REL until it returns true.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for at least one couple.
"
  (for (i 0 (brelation-size-1 rel))
       (when (exists (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from exists t)))
  nil)
  

(defmethod exists-1 ((rel brelation) proc)
  "
DO:     Calls PROC on each couples of the relation REL.
PROC:   A predicate of two elements.
RETURN: Whether PROC returned true for exactly one couple.
"
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (when (exists (adjref rel i) (lambda (e) (funcall proc i e)))
           (incf n)))
    (= n 1)))


(defmethod for-all-do ((rel brelation) proc)
  "
DO:     Calls PROC on each couple of the relation REL.
PROC:   A function of two elements.
RETURN: REL
"
  (for (i 0 (brelation-size-1 rel))
       (for-all-do (adjref rel i) (lambda (e) (funcall proc i e))))
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
            (include rel (arc i (read stream))))))))
  rel)


(defun write-brelation (stream rel)
  "
DO:     Write the relation REL to the STREAM.
RETURN: REL.
"
  (princ "(" stream)
  (for (i 0 (brelation-size-1 rel))
       (princ i stream)
       (write-bset stream (adjref rel i))
       (terpri stream))
  (princ ")" stream)
  rel)

(defgeneric project-1 (rel e1 bset))
(defmethod project-1 ((rel brelation) e1 (bset bset))
  "
POST:   BSET is the set of all elements I that are in relation REL(I,E2).
RETURN: BSET
"
  (assign-empty bset)
  (for (i 0 (brelation-size-1 rel))
    (when (related rel i e1)
      (include bset i)))
  bset)

(defgeneric project-2 (rel e1 bset))
(defmethod project-2 ((rel brelation) e1 (bset bset))
  "
POST:   BSET is the set of all elements E2 that are in relation REL(E1,E2).
RETURN: BSET
"
  (assign bset (adjref rel e1)))


;;;; THE END ;;;;
