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
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.BSET"))
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
   "COM.INFORMATIMAGO.COMMON-LISP.BSET" "BSET"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BRELATION"
  (:USE "COMMON-LISP")
  (:EXPORT "PROJECT-2" "PROJECT-1" "WRITE-BRELATION" "READ-BRELATION"
           "FOR-ALL-DO" "EXISTS-1" "EXISTS" "FOR-ALL" "EXTRACT" "SELECT" "CARDINAL"
           "IS-EMPTY" "IS-NOT-EQUAL" "IS-EQUAL" "IS-STRICT-SUBSET" "IS-SUBSET"
           "COMPLEMENT" "SYM-DIFF" "INTERSECTION" "DIFFERENCE" "UNION" "ASSIGN"
           "ASSIGN-ELEMENT" "ASSIGN-EMPTY" "CLOSURE" "GET-CYCLICS" "IS-CYCLIC"
           "HAS-REFLEXIVE" "IS-EQUIVALENCE" "IS-TRANSITIVE" "IS-SYMMETRIC"
           "IS-REFLEXIVE" "IS-TRANSITIVE-1" "IS-REFLEXIVE-1" "IS-RELATED" "IS-ELEMENT"
           "EXCLUDE" "INCLUDE" "MAKE-BRELATION" "BRELATION")
  (:SHADOW "COMPLEMENT" "INTERSECTION" "UNION")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "VECTOR-INIT" "FOR")
  (:DOCUMENTATION
   "This package implements a relation abstract data type
    based on an array of bset.
    It can represent only relations between two positive
    and bounded integers.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BRELATION")



(DEFSTRUCT (BRELATION (:CONSTRUCTOR %MAKE-BRELATION))
  (ADJSETS (MAKE-ARRAY '(0) :ELEMENT-TYPE 'BSET:BSET
                       :initial-element (bset:make-bset 0))
           :TYPE (ARRAY BSET:BSET (*)))
  (SIZE-1 0 :TYPE (INTEGER 0))
  (SIZE-2 0 :TYPE (INTEGER 0)))



(DEFUN MAKE-BRELATION (SIZE-1 SIZE-2)
  (DECLARE (TYPE (INTEGER 0) SIZE-1 SIZE-2))
  (%MAKE-BRELATION
   :ADJSETS (VECTOR-INIT (MAKE-ARRAY (LIST (1+ SIZE-1))
                                     :ELEMENT-TYPE 'BSET:BSET
                                     :initial-element (bset:make-bset 0))
                         (LAMBDA (INDEX)
                           (DECLARE (IGNORE INDEX))
                           (BSET:MAKE-BSET SIZE-2)))
   :SIZE-1 SIZE-1
   :SIZE-2 SIZE-2))


(DEFMACRO IMPLY (P Q) `(OR (NOT ,P) ,Q))
(DEFMACRO ADJREF (REL I)      `(AREF (BRELATION-ADJSETS ,REL) ,I))
(DEFMACRO RELATED (REL E1 E2) `(BSET:IS-ELEMENT ,E2 (ADJREF ,REL ,E1)))


(DEFUN INCLUDE (REL E1 E2)
  (DECLARE (TYPE (INTEGER 0) E1 E2))
  (BSET:INCLUDE (ADJREF REL E1) E2)
  REL)


(DEFUN EXCLUDE (REL E1 E2)
  (DECLARE (TYPE (INTEGER 0) E1 E2))
  (BSET:EXCLUDE (ADJREF REL E1) E2)
  REL)


(DEFUN IS-ELEMENT (E1 E2 REL)
  (DECLARE (TYPE (INTEGER 0) E1 E2))
  (RELATED REL E1 E2)
  )


(DEFUN IS-RELATED (E1 E2 REL)
  (DECLARE (TYPE (INTEGER 0) E1 E2))
  (RELATED REL E1 E2)
  )


(DEFUN IS-REFLEXIVE-1 (E1 REL)
  (DECLARE (TYPE (INTEGER 0) E1))
  (RELATED REL E1 E1)
  )


(DEFUN IS-SYMMETRIC-1 (E1 E2 REL)
  (DECLARE (TYPE (INTEGER 0) E1 E2))
  (IMPLY (RELATED REL E1 E2) (RELATED REL E2 E1))
  )


(DEFUN IS-TRANSITIVE-1 (E1 E2 E3 REL)
  (DECLARE (TYPE (INTEGER 0) E1 E2 E3))
  (IMPLY (AND (RELATED REL E1 E2) (RELATED REL E2 E3)) (RELATED REL E1 E3))
  )


(DEFUN IS-REFLEXIVE (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (UNLESS (RELATED REL I I) (RETURN-FROM IS-REFLEXIVE NIL)))
  T)


(DEFUN IS-SYMMETRIC (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (UNLESS (BSET:FOR-ALL (ADJREF REL I)
                             (LAMBDA (J) (RELATED REL J I)))
         (RETURN-FROM IS-SYMMETRIC NIL)))
  T)


(DEFUN IS-TRANSITIVE (REL)
  (LET ((R (MAKE-BRELATION (BRELATION-SIZE-1 REL) (BRELATION-SIZE-2 REL))))
    (ASSIGN R REL)
    (CLOSURE R)
    (IS-EQUAL R REL) ))


(DEFUN IS-EQUIVALENCE (REL)
  (AND (IS-REFLEXIVE REL) (IS-SYMMETRIC REL) (IS-TRANSITIVE REL))
  )


(DEFUN HAS-REFLEXIVE (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (WHEN (RELATED REL I I) (RETURN-FROM HAS-REFLEXIVE T)))
  NIL)


(DEFMACRO UNTIL (CONDITION &BODY BODY) `(DO () (,CONDITION) ,@BODY))


(DEFUN IS-CYCLIC (REL)
  (LET ((WITH-PRED (BSET:MAKE-BSET (BRELATION-SIZE-1 REL)))
        (WITHOUT-PRED (BSET:MAKE-BSET (BRELATION-SIZE-1 REL)))
        (PRED-COUNT (MAKE-ARRAY (LIST (1+ (BRELATION-SIZE-1 REL)))
                                :ELEMENT-TYPE '(INTEGER 0)
                                :INITIAL-ELEMENT 0)))
    (FOR (I 0 (BRELATION-SIZE-1 REL))
         (BSET:FOR-ALL-DO (ADJREF REL I)
                          (LAMBDA (E) (INCF (AREF PRED-COUNT E)))))
    (FOR (I 0 (BRELATION-SIZE-1 REL))
         (WHEN (= 0 (AREF PRED-COUNT I))   (BSET:INCLUDE WITHOUT-PRED I)))
    (BSET:COMPLEMENT WITH-PRED)
    (UNTIL (BSET:IS-EMPTY WITHOUT-PRED)
      (LET ((I (BSET:EXTRACT WITHOUT-PRED)))
        (BSET:EXCLUDE WITH-PRED I)
        (BSET:FOR-ALL-DO (ADJREF REL I)
                         (LAMBDA (E) (DECF (AREF PRED-COUNT E))
                            (WHEN (= 0 (AREF PRED-COUNT E))
                              (BSET:INCLUDE WITHOUT-PRED E))))))
    (NOT (BSET:IS-EMPTY WITH-PRED))))



(DEFUN GET-CYCLICS (REL BSET)
  (LET ((R (MAKE-BRELATION (BRELATION-SIZE-1 REL)(BRELATION-SIZE-2 REL))))
    (ASSIGN R REL)
    (CLOSURE R)
    (BSET:ASSIGN-EMPTY BSET)
    (FOR (I 0 (BRELATION-SIZE-1 REL))
         (WHEN (RELATED R I I) (BSET:INCLUDE BSET I))))
  BSET)


(DEFUN ASSIGN-EMPTY (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (BSET:ASSIGN-EMPTY (ADJREF REL I)))
  REL)


(DEFUN ASSIGN-ELEMENT (REL E1 E2)
  (ASSIGN-EMPTY REL)
  (INCLUDE REL E1 E2)
  REL)


(DEFUN ASSIGN (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (BSET:ASSIGN-EMPTY (ADJREF REL1 I))
       (BSET:ASSIGN-EMPTY (ADJREF REL2 I)))
  REL1)


(DEFUN CLOSURE (REL)
  (FOR (J 0 (BRELATION-SIZE-1 REL))
       (UNLESS (BSET:IS-EMPTY (ADJREF REL J))
         (FOR (I 0 (BRELATION-SIZE-1 REL))
              (WHEN (RELATED REL I J)
                (BSET:UNION (ADJREF REL I)
                            (ADJREF REL J))))))
  REL)
           
  
(DEFUN UNION (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (BSET:UNION (ADJREF REL1 I) (ADJREF REL2 I)))
  REL1)


(DEFUN DIFFERENCE (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (BSET:DIFFERENCE (ADJREF REL1 I) (ADJREF REL2 I)))
  REL1)


(DEFUN INTERSECTION (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (BSET:INTERSECTION (ADJREF REL1 I) (ADJREF REL2 I)))
  REL1)


(DEFUN SYM-DIFF (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (BSET:SYM-DIFF (ADJREF REL1 I) (ADJREF REL2 I)))
  REL1)


(DEFUN COMPLEMENT (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (BSET:COMPLEMENT (ADJREF REL I)))
  REL)


(DEFUN IS-SUBSET (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (UNLESS (BSET:IS-SUBSET (ADJREF REL1 I) (ADJREF REL2 I))
         (RETURN-FROM IS-SUBSET NIL)))
  T)


(DEFUN IS-STRICT-SUBSET (REL1 REL2)
  (AND (IS-SUBSET REL1 REL2) (IS-NOT-EQUAL REL1 REL2))
  )


(DEFUN IS-EQUAL (REL1 REL2)
  (FOR (I 0 (BRELATION-SIZE-1 REL1))
       (UNLESS (BSET:IS-EQUAL  (ADJREF REL1 I) (ADJREF REL2 I))
         (RETURN-FROM IS-EQUAL NIL)))
  T)


(DEFUN IS-NOT-EQUAL (REL1 REL2)
  (NOT (IS-EQUAL REL1 REL2))
  )


(DEFUN IS-EMPTY (REL)
  
  
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (UNLESS (BSET:IS-EMPTY  (ADJREF REL I))
         (RETURN-FROM IS-EMPTY  NIL)))
  T)


(DEFUN CARDINAL (REL)
  (LET ((N 0))
    (FOR (I 0 (BRELATION-SIZE-1 REL))
         (INCF N (BSET:CARDINAL (ADJREF REL I))))
    N))
    

(DEFUN SELECT (REL)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (UNLESS (BSET:IS-EMPTY (ADJREF REL I))
         (RETURN-FROM SELECT (VALUES I (BSET:SELECT (ADJREF REL I))))))
  (VALUES 0 0))


(DEFUN EXTRACT (REL)
  (MULTIPLE-VALUE-BIND (E1 E2) (SELECT REL)
    (EXCLUDE REL E1 E2)
    (VALUES E1 E2)))


(DEFUN FOR-ALL (REL PROC)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (UNLESS (BSET:FOR-ALL (ADJREF REL I) (LAMBDA (E) (FUNCALL PROC I E)))
         (RETURN-FROM FOR-ALL NIL)))
  T)


(DEFUN EXISTS (REL PROC)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (WHEN (BSET:EXISTS (ADJREF REL I) (LAMBDA (E) (FUNCALL PROC I E)))
         (RETURN-FROM EXISTS T)))
  NIL)
  

(DEFUN EXISTS-1 (REL PROC)
  (LET ((N 0))
    (FOR (I 0 (BRELATION-SIZE-1 REL))
         (WHEN (BSET:EXISTS (ADJREF REL I) (LAMBDA (E) (FUNCALL PROC I E)))
           (INCF N)))
    (= N 1)))


(DEFUN FOR-ALL-DO (REL PROC)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (BSET:FOR-ALL-DO (ADJREF REL I) (LAMBDA (E) (FUNCALL PROC I E))))
  REL)


(DEFUN READ-BRELATION (STREAM REL)
  (ASSIGN-EMPTY REL)
  (WHEN (PEEK-CHAR (CHARACTER "(") STREAM NIL NIL)
    (READ-CHAR STREAM)
    (DO ()
        ((CHAR= (PEEK-CHAR T STREAM NIL (CHARACTER ")")) (CHARACTER ")"))
         (READ-CHAR STREAM))
      (LET ((I (READ STREAM)))
        
        (WHEN (PEEK-CHAR (CHARACTER "(") STREAM NIL NIL)
          (READ-CHAR STREAM)
          (DO ()
              ((CHAR= (PEEK-CHAR T STREAM NIL (CHARACTER ")")) (CHARACTER ")"))
               (READ-CHAR STREAM))
            (INCLUDE REL I (READ STREAM)))))))
  REL)


(DEFUN WRITE-REL (STREAM REL)
  (PRINC "(" STREAM)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (PRINC I STREAM)
       (BSET:WRITE-BSET STREAM (ADJREF REL I))
       (TERPRI STREAM))
  (PRINC ")" STREAM)
  REL)

       
(DEFUN PROJECT-1 (REL E1 BSET)
  (ASSIGN-EMPTY BSET)
  (FOR (I 0 (BRELATION-SIZE-1 REL))
       (WHEN (RELATED REL I E1)
         (BSET:INCLUDE BSET I)))
  BSET)


(DEFUN PROJECT-2 (REL E1 BSET)  (ASSIGN BSET (ADJREF REL E1)))


;;;; THE END ;;;;
