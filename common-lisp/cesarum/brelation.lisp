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
;;;;    Copyright Pascal J. Bourguignon 2004 - 2004
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
   "This package implements a relation abstract data type
    based on an array of bset.
    It can represent only relations between two positive
    and bounded integers.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BRELATION")



(defstruct (brelation (:constructor %make-brelation))
  (adjsets (make-array '(0) :element-type 'com.informatimago.common-lisp.cesarum.bset:bset
                       :initial-element (com.informatimago.common-lisp.cesarum.bset:make-bset 0))
           :type (array com.informatimago.common-lisp.cesarum.bset:bset (*)))
  (size-1 0 :type (integer 0))
  (size-2 0 :type (integer 0)))



(defun make-brelation (size-1 size-2)
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


(defmacro imply (p q) `(or (not ,p) ,q))
(defmacro adjref (rel i)      `(aref (brelation-adjsets ,rel) ,i))
(defmacro related (rel e1 e2) `(com.informatimago.common-lisp.cesarum.bset:is-element ,e2 (adjref ,rel ,e1)))


(defun include (rel e1 e2)
  (declare (type (integer 0) e1 e2))
  (com.informatimago.common-lisp.cesarum.bset:include (adjref rel e1) e2)
  rel)


(defun exclude (rel e1 e2)
  (declare (type (integer 0) e1 e2))
  (com.informatimago.common-lisp.cesarum.bset:exclude (adjref rel e1) e2)
  rel)


(defun is-element (e1 e2 rel)
  (declare (type (integer 0) e1 e2))
  (related rel e1 e2)
  )


(defun is-related (e1 e2 rel)
  (declare (type (integer 0) e1 e2))
  (related rel e1 e2)
  )


(defun is-reflexive-1 (e1 rel)
  (declare (type (integer 0) e1))
  (related rel e1 e1)
  )


(defun is-symmetric-1 (e1 e2 rel)
  (declare (type (integer 0) e1 e2))
  (imply (related rel e1 e2) (related rel e2 e1))
  )


(defun is-transitive-1 (e1 e2 e3 rel)
  (declare (type (integer 0) e1 e2 e3))
  (imply (and (related rel e1 e2) (related rel e2 e3)) (related rel e1 e3))
  )


(defun is-reflexive (rel)
  (for (i 0 (brelation-size-1 rel))
       (unless (related rel i i) (return-from is-reflexive nil)))
  t)


(defun is-symmetric (rel)
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:for-all (adjref rel i)
                             (lambda (j) (related rel j i)))
         (return-from is-symmetric nil)))
  t)


(defun is-transitive (rel)
  (let ((r (make-brelation (brelation-size-1 rel) (brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (is-equal r rel) ))


(defun is-equivalence (rel)
  (and (is-reflexive rel) (is-symmetric rel) (is-transitive rel))
  )


(defun has-reflexive (rel)
  (for (i 0 (brelation-size-1 rel))
       (when (related rel i i) (return-from has-reflexive t)))
  nil)


(defmacro until (condition &body body) `(do () (,condition) ,@body))


(defun is-cyclic (rel)
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
  (let ((r (make-brelation (brelation-size-1 rel)(brelation-size-2 rel))))
    (assign r rel)
    (closure r)
    (com.informatimago.common-lisp.cesarum.bset:assign-empty bset)
    (for (i 0 (brelation-size-1 rel))
         (when (related r i i) (com.informatimago.common-lisp.cesarum.bset:include bset i))))
  bset)


(defun assign-empty (rel)
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel i)))
  rel)


(defun assign-element (rel e1 e2)
  (assign-empty rel)
  (include rel e1 e2)
  rel)


(defun assign (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel1 i))
       (com.informatimago.common-lisp.cesarum.bset:assign-empty (adjref rel2 i)))
  rel1)


(defun closure (rel)
  (for (j 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty (adjref rel j))
         (for (i 0 (brelation-size-1 rel))
              (when (related rel i j)
                (com.informatimago.common-lisp.cesarum.bset:union (adjref rel i)
                            (adjref rel j))))))
  rel)
           
  
(defun union (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:union (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun difference (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:difference (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun intersection (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:intersection (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun sym-diff (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (com.informatimago.common-lisp.cesarum.bset:sym-diff (adjref rel1 i) (adjref rel2 i)))
  rel1)


(defun complement (rel)
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:complement (adjref rel i)))
  rel)


(defun is-subset (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-subset (adjref rel1 i) (adjref rel2 i))
         (return-from is-subset nil)))
  t)


(defun is-strict-subset (rel1 rel2)
  (and (is-subset rel1 rel2) (is-not-equal rel1 rel2))
  )


(defun is-equal (rel1 rel2)
  (for (i 0 (brelation-size-1 rel1))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-equal  (adjref rel1 i) (adjref rel2 i))
         (return-from is-equal nil)))
  t)


(defun is-not-equal (rel1 rel2)
  (not (is-equal rel1 rel2)))


(defun is-empty (rel)
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty  (adjref rel i))
         (return-from is-empty  nil)))
  t)


(defun cardinal (rel)
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (incf n (com.informatimago.common-lisp.cesarum.bset:cardinal (adjref rel i))))
    n))
    

(defun select (rel)
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:is-empty (adjref rel i))
         (return-from select (values i (com.informatimago.common-lisp.cesarum.bset:select (adjref rel i))))))
  (values 0 0))


(defun extract (rel)
  (multiple-value-bind (e1 e2) (select rel)
    (exclude rel e1 e2)
    (values e1 e2)))


(defun for-all (rel proc)
  (for (i 0 (brelation-size-1 rel))
       (unless (com.informatimago.common-lisp.cesarum.bset:for-all (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from for-all nil)))
  t)


(defun exists (rel proc)
  (for (i 0 (brelation-size-1 rel))
       (when (com.informatimago.common-lisp.cesarum.bset:exists (adjref rel i) (lambda (e) (funcall proc i e)))
         (return-from exists t)))
  nil)
  

(defun exists-1 (rel proc)
  (let ((n 0))
    (for (i 0 (brelation-size-1 rel))
         (when (com.informatimago.common-lisp.cesarum.bset:exists (adjref rel i) (lambda (e) (funcall proc i e)))
           (incf n)))
    (= n 1)))


(defun for-all-do (rel proc)
  (for (i 0 (brelation-size-1 rel))
       (com.informatimago.common-lisp.cesarum.bset:for-all-do (adjref rel i) (lambda (e) (funcall proc i e))))
  rel)


(defun read-brelation (stream rel)
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


(defun write-rel (stream rel)
  (princ "(" stream)
  (for (i 0 (brelation-size-1 rel))
       (princ i stream)
       (com.informatimago.common-lisp.cesarum.bset:write-bset stream (adjref rel i))
       (terpri stream))
  (princ ")" stream)
  rel)

       
(defun project-1 (rel e1 bset)
  (assign-empty bset)
  (for (i 0 (brelation-size-1 rel))
       (when (related rel i e1)
         (com.informatimago.common-lisp.cesarum.bset:include bset i)))
  bset)


(defun project-2 (rel e1 bset)  (assign bset (adjref rel e1)))


;;;; THE END ;;;;
