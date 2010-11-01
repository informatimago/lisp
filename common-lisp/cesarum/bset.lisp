;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               bset.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Set of (integer 0 *) implemented with array of bitsets.
;;;;
;;;;    (Inspired by Modula-2 cocktail-9309/reuse/src/Set.md)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-02-18 <PJB> Created.
;;;;BUGS
;;;;    
;;;;    This is not as lispy as could be (we may want to have sets
;;;;    of random lisp objects, notably symbols), but it's optimized
;;;;    for small integers.
;;;;
;;;;    There are no checks for mismatching operands
;;;;    (sets with different sizes)
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET"
  (:USE "COMMON-LISP")
  (:EXPORT "BSET-TO-LIST" "LIST-TO-BSET" "WRITE-BSET" "READ-BSET" "FOR-ALL-DO"
           "ASSIGN-EMPTY" "ASSIGN-ELEMENT" "ASSIGN" "EXISTS-1" "EXISTS" "FOR-ALL"
           "IS-EMPTY" "IS-ELEMENT" "IS-EQUAL" "IS-STRICT-SUBSET" "IS-SUBSET" "EXTRACT"
           "SELECT" "MAXIMUM" "MINIMUM" "SIZE" "CARDINAL" "EXCLUDE" "INCLUDE"
           "COMPLEMENT" "SYM-DIFF" "INTERSECTION" "DIFFERENCE" "UNION" "RESIZE-BSET"
           "COPY-BSET" "MAKE-BSET" "BSET")
  (:SHADOW "COMPLEMENT" "INTERSECTION" "UNION" "SET")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "VECTOR-INIT" "FOR")
  (:DOCUMENTATION
   "Set of (integer 0 *) implemented with array of bitsets.

    Copyright Pascal J. Bourguignon 2004 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFCONSTANT +BIT-PER-BITSET+ 32))

(DEFTYPE BITSET () `(UNSIGNED-BYTE ,+BIT-PER-BITSET+))

(DEFSTRUCT (BSET
             (:CONSTRUCTOR %MAKE-BSET)
             (:COPIER NIL)
             (:PRINT-FUNCTION
              (LAMBDA (BSET STREAM LEVEL)
                (DECLARE (IGNORE LEVEL))
                (FORMAT STREAM "#S(~A :BITSETS #(~{ #16r~16R~} ) :CARDINAL ~A :FIRST-ELEMENT ~A :LAST-ELEMENT ~A)" (TYPE-OF BSET) (MAP 'LIST (FUNCTION IDENTITY) (BSET-BITSETS BSET)) (BSET-CARDINAL BSET) (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET)))))
  (BITSETS (MAKE-ARRAY (LIST 0)
                       :ELEMENT-TYPE 'BITSET
                       :INITIAL-ELEMENT 0
                       :ADJUSTABLE T)
           :TYPE (ARRAY BITSET *))
  ;; max-element == (* +bit-per-bitset+ (array-dimension bitsets 0))
  ;; last-bitset == (1- (array-dimension bitsets 0))
  (CARDINAL      NIL :TYPE (OR NULL (INTEGER 0)))
  (FIRST-ELEMENT 0   :TYPE (INTEGER 0)) ; approximate
  (LAST-ELEMENT  0   :TYPE (INTEGER 0)) ; approximate
  ;; (for all i (==> (< i (bset-first-element bset)) (not (is-element i bset))))
  ;; (for all i (==> (> i (bset-last-element  bset)) (not (is-element i bset))))
  )


(DEFUN COPY-BSET (ORIGINAL)
  (LET ((COPY (%MAKE-BSET)))
    (SETF (BSET-BITSETS       COPY) (BSET-BITSETS       ORIGINAL)
          (BSET-CARDINAL      COPY) (BSET-CARDINAL      ORIGINAL)
          (BSET-FIRST-ELEMENT COPY) (BSET-FIRST-ELEMENT ORIGINAL)
          (BSET-LAST-ELEMENT  COPY) (BSET-LAST-ELEMENT  ORIGINAL))
    COPY))


(DEFMACRO BSREF (BSA I) `(AREF ,BSA ,I))


(PROCLAIM '(INLINE LAST-BITSET))
(DEFUN LAST-BITSET (BITSETS)
  "
RETURN:  The index of the last bitset in the BITSETS array.
"
  (1- (ARRAY-DIMENSION BITSETS 0)))


(PROCLAIM '(INLINE ELEM-TO-BITSET))
(DEFUN ELEM-TO-BITSET (ELEMENT)
  "
RETURN:  The index of the bitset where element is stored.
NOTE:     0 --> 0
         31 --> 0
         32 --> 1
         63 --> 1
         64 --> 2
"
  (TRUNCATE ELEMENT +BIT-PER-BITSET+))


(PROCLAIM '(INLINE ELEM-TO-BIT))
(DEFUN ELEM-TO-BIT (ELEMENT)
  (MOD ELEMENT +BIT-PER-BITSET+))


(PROCLAIM '(INLINE BITSET-TO-ELEM))
(DEFUN BITSET-TO-ELEM (INDEX)
  "
RETURN:  The maximum element + 1 that can be stored in the bitset at INDEX.
NOTE:    0 --> 32
         1 --> 64
         2 --> 96
"
  (* +BIT-PER-BITSET+ (1+ INDEX)))


(DEFUN MAKE-BSET (MAX-SIZE)
  "
PRE:    (<= 0 max-size)
POST:   (<= max-size (size (make-bset max-size)))
RETURN: A new bset allocated to hold at least elements from 0 to max-size.
"
  (DECLARE (TYPE (INTEGER 0) MAX-SIZE))
  (%MAKE-BSET :BITSETS (MAKE-ARRAY (LIST (1+ (ELEM-TO-BITSET MAX-SIZE)))
                                   :ELEMENT-TYPE 'BITSET
                                   :INITIAL-ELEMENT 0
                                   :ADJUSTABLE T)
              :CARDINAL 0))



(DEFUN RESIZE-BSET (BSET MAX-SIZE)
  "
PRE:     (<= 0 max-size)
POST:    (<= max-size (size (resize-bset bset max-size)))
         (if (< max-size (size old-bset))
             (is-equal bset (intersection old-bset
                                         (complement (make-bset max-size))))
             (is-equal bset old-bset))
DO:      Reallocate bset to have it able to hold at least elements
         from 0 to max-size.
RETURN:  bset
"
  (DECLARE (TYPE BSET BSET) (TYPE (INTEGER 0) MAX-SIZE))
  (LET ((OLD-COUNT (ARRAY-DIMENSION (BSET-BITSETS BSET) 0))
        (NEW-COUNT (1+ (ELEM-TO-BITSET MAX-SIZE))))
    (SETF (BSET-BITSETS BSET) (ADJUST-ARRAY (BSET-BITSETS BSET)
                                            (LIST NEW-COUNT)
                                            :ELEMENT-TYPE 'BITSET
                                            :INITIAL-ELEMENT 0))
    (WHEN (< NEW-COUNT OLD-COUNT)
      (SETF (BSET-LAST-ELEMENT BSET) (BITSET-TO-ELEM (1- NEW-COUNT))
            (BSET-CARDINAL BSET) NIL))
    BSET))


       
(DEFUN UNION (SET1 SET2)
  "
DO:      set1 := set1 U ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the union of set1 and set2
         modulo the allocated size of set1.
RETURN:  SET1
"
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I
           (ELEM-TO-BITSET (MIN (SIZE SET1) (BSET-FIRST-ELEMENT SET2)))
           (ELEM-TO-BITSET (MIN (SIZE SET1) (BSET-LAST-ELEMENT  SET2))))
         (WHEN (/= 0 (BSREF BITS2 I))
           (SETF (BSREF BITS1 I) (LOGIOR (BSREF BITS1 I) (BSREF BITS2 I))))))
  (SETF (BSET-CARDINAL SET1) NIL
        (BSET-FIRST-ELEMENT SET1) (MIN (BSET-FIRST-ELEMENT SET1)
                                       (BSET-FIRST-ELEMENT SET2))
        (BSET-LAST-ELEMENT SET1)  (MAX (BSET-LAST-ELEMENT SET1)
                                       (BSET-LAST-ELEMENT SET2)))
  SET1)
         

(DEFUN DIFFERENCE (SET1 SET2)
  "
DO:      set1 := set1 - ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the difference of set1 and set2
         (elements in set1 not in set2)
         modulo the allocated size of set1.
RETURN:  SET1
"
  ;; {x|x in set1 and not x in set2}
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I
           (ELEM-TO-BITSET (BSET-FIRST-ELEMENT SET2))
           (MIN (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET1))
                (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET2))))
         (WHEN (/= 0 (BSREF BITS2 I))
           (SETF (BSREF BITS1 I) (LOGANDC2 (BSREF BITS1 I) (BSREF BITS2 I))))) )
  (SETF (BSET-CARDINAL SET1) NIL)
  SET1)
         

(DEFUN INTERSECTION (SET1 SET2)
  "
DO:      set1 := set1 inter set2 inter
         Accumulate in set1 the intersection of set1 and set2
         (elements in set1 and in set2).
RETURN:  SET1
"
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I
           (ELEM-TO-BITSET (MAX (BSET-FIRST-ELEMENT SET1)
                                (BSET-FIRST-ELEMENT SET2)))
           (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                (BSET-LAST-ELEMENT SET2))))
         (SETF (BSREF BITS1 I) (LOGAND (BSREF BITS1 I) (BSREF BITS2 I)))))
  (SETF (BSET-CARDINAL SET1) NIL
        (BSET-FIRST-ELEMENT SET1) (MAX (BSET-FIRST-ELEMENT SET1)
                                       (BSET-FIRST-ELEMENT SET2))
        (BSET-LAST-ELEMENT SET1)  (MIN (BSET-LAST-ELEMENT SET1)
                                       (BSET-LAST-ELEMENT SET2)))
  SET1)
         


(DEFUN SYM-DIFF (SET1 SET2)
  "
DO:      set1 := set1 delta ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the symetrical difference of set1 and set2
         (elements in set1 not in set2 or in set2 not in bset 1)
         modulo the allocated size of set1.
RETURN:  SET1
"
  ;; {x|(x in set1 and not x in set2) or (x in set2 and not x in set1)}
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I
           (ELEM-TO-BITSET (MAX (BSET-FIRST-ELEMENT SET1)
                                (BSET-FIRST-ELEMENT SET2)))
           (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                (BSET-LAST-ELEMENT SET2))))
         (SETF (BSREF BITS1 I) (LOGXOR (BSREF BITS1 I) (BSREF BITS2 I)))))
  (SETF (BSET-CARDINAL SET1) NIL
        (BSET-FIRST-ELEMENT SET1) (MIN (BSET-FIRST-ELEMENT SET1)
                                       (BSET-FIRST-ELEMENT SET2))
        (BSET-LAST-ELEMENT SET1)  (MAX (BSET-LAST-ELEMENT SET1)
                                       (BSET-LAST-ELEMENT SET2)))
  SET1)


(DEFUN COMPLEMENT (BSET)
  "
DO:      set1 := (complement (make-bset (size set1))) - set1
         Accumulate in set1 the complement of set1
         (elements in not set1)
         modulo the allocated size of set1.
RETURN:  SET1
"
  (LET ((BITS (BSET-BITSETS BSET)))
    (FOR (I 0 (LAST-BITSET BITS))
         (SETF (BSREF BITS I)
               (DPB (LOGNOT (BSREF BITS I)) (BYTE 32 0) 0)))
    (SETF (BSET-CARDINAL BSET) (AND (BSET-CARDINAL BSET)
                                    (- (BITSET-TO-ELEM (LAST-BITSET BITS))
                                       (BSET-CARDINAL BSET)))
          (BSET-FIRST-ELEMENT BSET) 0
          (BSET-LAST-ELEMENT  BSET) (1- (BITSET-TO-ELEM (LAST-BITSET BITS)))))
  BSET)


(DEFUN INCLUDE (BSET ELEMENT)
  "
PRE:    (<= 0 element (size bset))
POST:   (is-element element bset)
RETURN: BSET
"
  (DECLARE (TYPE (INTEGER 0) ELEMENT))
  (LET ((BITS (BSET-BITSETS BSET)))
    (SETF (BSREF BITS (ELEM-TO-BITSET ELEMENT))
          (DPB 1 (BYTE 1 (ELEM-TO-BIT ELEMENT))
               (BSREF BITS (ELEM-TO-BITSET ELEMENT)))))
  (SETF (BSET-CARDINAL BSET) NIL
        (BSET-FIRST-ELEMENT BSET) (MIN ELEMENT (BSET-FIRST-ELEMENT BSET))
        (BSET-LAST-ELEMENT  BSET) (MAX ELEMENT (BSET-LAST-ELEMENT  BSET)))
  BSET)



(DEFUN EXCLUDE (BSET ELEMENT)
  "
PRE:    (<= 0 element (size bset))
POST:   (not (is-element element bset))
RETURN: BSET
"
  (DECLARE (TYPE (INTEGER 0) ELEMENT))
  (LET ((BITS (BSET-BITSETS BSET)))
    (SETF (BSREF BITS (ELEM-TO-BITSET ELEMENT))
          (DPB 0 (BYTE 1 (ELEM-TO-BIT ELEMENT))
               (BSREF BITS (ELEM-TO-BITSET ELEMENT))))
    (SETF (BSET-CARDINAL BSET) NIL)
    (WHEN (AND (= ELEMENT (BSET-FIRST-ELEMENT BSET))
               (< ELEMENT (BITSET-TO-ELEM (LAST-BITSET BITS))))
      (INCF (BSET-FIRST-ELEMENT BSET)))
    (WHEN (AND (= ELEMENT (BSET-LAST-ELEMENT BSET))
               (< 0 ELEMENT))
      (DECF (BSET-LAST-ELEMENT BSET))))
  BSET)


(DEFUN CARDINAL (BSET)
  "
RETURN:  The number of elements in BSET.
"
  (UNLESS (BSET-CARDINAL BSET)
    (LET ((CARDINAL 0))
      (FOR (I (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET))
           (WHEN (IS-ELEMENT I BSET) (INCF CARDINAL)))
      (SETF (BSET-CARDINAL BSET) CARDINAL)))
  (BSET-CARDINAL BSET))


(DEFUN SIZE (BSET)
  "
RETURN:  The maximum element BSET can hold.
"
  (LET ((BITS (BSET-BITSETS BSET)))
    (1- (BITSET-TO-ELEM (LAST-BITSET BITS)))))


(DEFUN MINIMUM (BSET)
  "
PRE:     (not (is-empty bset))
RETURN:  The smallest element of BSET.
"
  (FOR (I (BSET-FIRST-ELEMENT BSET)  (BSET-LAST-ELEMENT BSET))
       (WHEN (IS-ELEMENT I BSET)
         (SETF (BSET-FIRST-ELEMENT BSET) I)
         (RETURN-FROM MINIMUM I)))
  0)


(DEFUN MAXIMUM (BSET)
  "
PRE:     (not (is-empty bset))
RETURN:  The greatest element of BSET.
"
  (FOR (I (BSET-LAST-ELEMENT BSET)  (BSET-FIRST-ELEMENT BSET))
       (WHEN (IS-ELEMENT I BSET)
         (SETF (BSET-LAST-ELEMENT BSET) I)
         (RETURN-FROM MAXIMUM I)))
  0)



(DEFUN SELECT (BSET)
  "
PRE:      (not (is-empty bset))
RETURN:   An element of BSET.
WARNING:  May return always the same element if it's not removed from the BSET.
"
  (MINIMUM BSET))


(DEFUN EXTRACT (BSET)
  "
PRE:      (not (is-empty bset))
POST:     (not (is-element (extract bset) bset))
DO:       Select an element from the BSET and removes it from the BSET.
RETURN:   An element that was in BSET.
"
  (LET ((I (MINIMUM BSET))) (EXCLUDE BSET I) I))


(DEFUN IS-SUBSET (SET1 SET2)
  "
RETURN:  Whether  SET1 is a subset of SET2.
"
  ;; x in set1 ==> x in set2
  ;; 1 1  0
  ;; 1 0  1
  ;; 0 1  0
  ;; 0 0  0
  ;; set2|~set1 : logandc2
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I (ELEM-TO-BITSET (BSET-FIRST-ELEMENT SET1))
            (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                 (BSET-LAST-ELEMENT SET2))))
         (COND
           ((= 0 (BSREF BITS1 I)))
           ((= 0 (BSREF BITS2 I))
            (RETURN-FROM IS-SUBSET NIL))
           ((/= 0 (LOGANDC2 (BSREF BITS1 I) (BSREF BITS2 I)))
            (RETURN-FROM IS-SUBSET NIL)))
         (WHEN (> (BSET-LAST-ELEMENT SET1) (BSET-LAST-ELEMENT SET2))
           (FOR (I (1+ (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET1)))
                   (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET2)))
                (WHEN (/= 0 (BSREF BITS1 I))
                  (RETURN-FROM IS-SUBSET NIL))))))
  T)


(DEFUN IS-STRICT-SUBSET (SET1 SET2)
  "
RETURN:  Whether SET1 is a strict subset of SET2.
"
  (AND (IS-SUBSET SET1 SET2) (NOT (IS-EQUAL SET1 SET2))))


(DEFUN IS-EQUAL (SET1 SET2)
  "
RETURN:  Whether SET1 and SET2 contain the same elements.
  "
  (OR (EQ SET1 SET2)
      (LET ((BITS1 (BSET-BITSETS SET1))
            (BITS2 (BSET-BITSETS SET2)))
        (FOR (I
               (ELEM-TO-BITSET (MIN (BSET-FIRST-ELEMENT SET1)
                                    (BSET-FIRST-ELEMENT SET2)))
               (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                    (BSET-LAST-ELEMENT SET2))))
             (UNLESS (= (BSREF BITS1 I) (BSREF BITS2 I))
               (RETURN-FROM IS-EQUAL NIL)))
        (WHEN (> (ELEM-TO-BITSET (SIZE SET1))
                 (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET1))
                 (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET2)))
          (FOR (I
                 (1+ (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                          (BSET-LAST-ELEMENT SET2))))
                 (ELEM-TO-BITSET (SIZE SET1)))
               (WHEN (/= 0 (BSREF BITS1 I))
                 (RETURN-FROM IS-EQUAL NIL))))
        (WHEN (> (ELEM-TO-BITSET (SIZE SET2))
                 (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET2))
                 (ELEM-TO-BITSET (BSET-LAST-ELEMENT SET1)))
          (FOR (I
                 (1+ (ELEM-TO-BITSET (MIN (BSET-LAST-ELEMENT SET1)
                                          (BSET-LAST-ELEMENT SET2))))
                 (ELEM-TO-BITSET (SIZE SET2)))
               (WHEN (/= 0 (BSREF BITS2 I))
                 (RETURN-FROM IS-EQUAL NIL))))
        T)))


(DEFUN IS-NOT-EQUAL (SET1 SET2)
  "
RETURN:  (not (is-equal set1 set2))
"
  (NOT (IS-EQUAL SET1 SET2)))


(DEFUN IS-ELEMENT (ELEMENT BSET)
  "
RETURN:  Whether element is in BSET.
"
  (DECLARE (TYPE (INTEGER 0) ELEMENT))
  (LET ((BITS (BSET-BITSETS BSET)))
    (AND (< ELEMENT (BITSET-TO-ELEM (LAST-BITSET BITS)))
         (/= 0 (LOGAND (BSREF BITS (ELEM-TO-BITSET ELEMENT)) 
                       (ASH 1 (ELEM-TO-BIT ELEMENT)))))))


(DEFUN IS-EMPTY (BSET)
  "
RETURN: (= 0 (cardinal bset))
"
  (OR (AND (BSET-CARDINAL BSET) (= 0 (BSET-CARDINAL BSET)))
      (LET ((BITS (BSET-BITSETS BSET)))
        (FOR (I 0 (LAST-BITSET BITS))
             (WHEN (/= 0 (BSREF BITS I)) (RETURN-FROM IS-EMPTY NIL)))
        (SETF (BSET-CARDINAL BSET) 0)
        T)))


(DEFUN FOR-ALL (BSET PROC)
  "
DO:     Call function PROC for each element in the BSET until PROC returns NIL.
RETURN: Whether no call to PROC returned NIL.
"
  (FOR (I (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET))
       (WHEN (AND (IS-ELEMENT I BSET) (NOT (FUNCALL PROC I)))
         (RETURN-FROM FOR-ALL NIL)))
  T)


(DEFUN EXISTS (BSET PROC)
  "
DO:      Call function PROC for each element in the BSET
         until PROC returns non nil.
RETURN:  Whether PROC returned non nil.
"
  (FOR (I (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET))
       (WHEN (AND (IS-ELEMENT I BSET) (FUNCALL PROC I))
         (RETURN-FROM EXISTS T)))
  NIL)


(DEFUN EXISTS-1 (BSET PROC)
  "
DO:       Call function PROC on all elements in the BSET.
RETURN:   Whether PROC returned non nil for exactly one element.
"
  (LET ((N 0))
    (FOR (I (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET))
         (WHEN (AND (IS-ELEMENT I BSET) (FUNCALL PROC I))
           (INCF N)))
    (= N 1)))


(DEFUN ASSIGN (SET1 SET2)
  "
DO:      Accumulate in set1 the elements of set2 that are less than (size set1).
POST:    (is-equal set1 (intersection (complement (make-bset (size set1)))set2))
RETURN:  SET1
"
  (LET ((BITS1 (BSET-BITSETS SET1))
        (BITS2 (BSET-BITSETS SET2)))
    (FOR (I 0 (MIN (LAST-BITSET BITS1) (LAST-BITSET BITS2)))
         (SETF (BSREF BITS1 I) (BSREF BITS2 I)))
    (WHEN (< (MIN (LAST-BITSET BITS1) (LAST-BITSET BITS2)) (LAST-BITSET BITS1))
      (FOR (I (1+ (MIN (LAST-BITSET BITS1) (LAST-BITSET BITS2)))
              (LAST-BITSET BITS1))
           (SETF (BSREF BITS1 I) 0)))
    (SETF (BSET-CARDINAL SET1) (BSET-CARDINAL SET2)
          (BSET-FIRST-ELEMENT SET1) (MIN (BSET-FIRST-ELEMENT SET2)
                                         (BITSET-TO-ELEM (LAST-BITSET BITS1)))
          (BSET-LAST-ELEMENT  SET1) (MIN (BSET-LAST-ELEMENT  SET2)
                                         (1- (BITSET-TO-ELEM (LAST-BITSET BITS1))))))
  SET1)


(DEFUN ASSIGN-ELEMENT (BSET ELEMENT)
  "
DO:     Empties BSET and include element.
PRE:    (<= 0 element (size bset))
POST:   (and (exists bset (lambda (x) (= x element)))
             (for-all bset (lambda (x) (= x element))))
RETURN:  BSET
"
  (DECLARE (TYPE (INTEGER 0) ELEMENT))
  (ASSIGN-EMPTY BSET)
  (INCLUDE BSET ELEMENT)
  (SETF (BSET-CARDINAL BSET) 1
        (BSET-FIRST-ELEMENT BSET) ELEMENT
        (BSET-LAST-ELEMENT  BSET) ELEMENT)
  BSET)


(DEFUN ASSIGN-EMPTY (BSET)
  "
POST:    (is-empty bset)
RETURN:  BSET.
"
  (LET ((BITS (BSET-BITSETS BSET)))
    (FOR (I 0 (LAST-BITSET BITS))   (SETF (BSREF BITS I) 0))
    (SETF (BSET-CARDINAL BSET) 0
          (BSET-FIRST-ELEMENT BSET) 0
          (BSET-LAST-ELEMENT  BSET) 0))
  BSET)


(DEFUN FOR-ALL-DO (BSET PROC)
  "
DO:      Call PROC on all elements in BSET.
RETURN:  BSET.
"
  (FOR (I (BSET-FIRST-ELEMENT BSET) (BSET-LAST-ELEMENT BSET))
       (WHEN (IS-ELEMENT I BSET)
         (FUNCALL PROC I)))
  BSET)

 
(DEFUN BSET-TO-LIST (BSET)
  "
RETURN:  A list of all elements of BSET, sorted in increasing order.
"
  (LET ((ELEMENTS '()))
    (FOR (I (BSET-LAST-ELEMENT BSET) (BSET-FIRST-ELEMENT BSET))
         (WHEN (IS-ELEMENT I BSET)
           (PUSH I ELEMENTS)))
    ELEMENTS))


(DEFUN LIST-TO-BSET (LIST)
  "
PRE:     LIST is a list of positive integer.
RETURN:  A new bset containing all the elements in the list.
"
  (LET ((BSET (MAKE-BSET (APPLY (FUNCTION MAX) LIST))))
    (DOLIST (ELEMENT LIST)
      (INCLUDE BSET ELEMENT))
    BSET))


(DEFUN READ-BSET (STREAM BSET)
  "
DO:      Accumulate in BSET the elements read from the stream.
RETURN:  BSET.
"
  (LET ((CARDINAL 0))
    (ASSIGN-EMPTY BSET)
    (WHEN (PEEK-CHAR (CHARACTER "(") STREAM NIL NIL)
      (READ-CHAR STREAM)
      (DO ()
          ((CHAR= (PEEK-CHAR T STREAM NIL (CHARACTER ")")) (CHARACTER ")")))
        (INCLUDE BSET (READ STREAM))
        (FORMAT T "~S~%" BSET)
        (INCF CARDINAL)))
    (SETF (BSET-CARDINAL BSET) CARDINAL)
    (READ-CHAR STREAM))
  BSET)


(DEFUN WRITE-BSET (STREAM BSET)
  "
DO:     Writes to the stream the elements in BSET.
RETURN: BSET.
"
  (PRINC "(" STREAM)
  (FOR-ALL-DO BSET (LAMBDA (ELEMENT) (PRINC ELEMENT STREAM) (PRINC " " STREAM)))
  (PRINC ")" STREAM)
  BSET)


;;;; THE END ;;;;
