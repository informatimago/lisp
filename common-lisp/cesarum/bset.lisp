;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               bset.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
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


(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET"
  (:use "COMMON-LISP")
  (:shadow "COMPLEMENT" "INTERSECTION" "UNION" "SET" "SUBSETP")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "VECTOR-INIT" "FOR")
  (:export "BSET-TO-LIST" "LIST-TO-BSET" "WRITE-BSET" "READ-BSET" "FOR-ALL-DO"
           "ASSIGN-EMPTY" "ASSIGN-ELEMENT" "ASSIGN" "EXISTS-1" "EXISTS" "FOR-ALL"
           "IS-EMPTY" "IS-ELEMENT" "IS-EQUAL" "IS-STRICT-SUBSET" "IS-SUBSET" "EXTRACT"
           "SELECT" "MAXIMUM" "MINIMUM" "SIZE" "CARDINAL" "EXCLUDE" "INCLUDE"
           "COMPLEMENT" "SYM-DIFF" "INTERSECTION" "DIFFERENCE" "UNION" "RESIZE-BSET"
           "COPY-BSET" "MAKE-BSET" "BSET"
           "SUBSETP" "STRICT-SUBSETP" )
  (:documentation
   "

This package implements sets of (integer 0 *) as arrays of bitsets.

\(Inspired by Modula-2 cocktail-9309/reuse/src/Set.md)


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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.BSET")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +bit-per-bitset+ 32))

(deftype bitset () `(unsigned-byte ,+bit-per-bitset+))

(defstruct (bset
             (:constructor %make-bset)
             (:copier nil)
             (:print-function
              (lambda (bset stream level)
                (declare (ignore level))
                (format stream "#S(~A :BITSETS #(~{ #16r~16R~} ) :CARDINAL ~A :FIRST-ELEMENT ~A :LAST-ELEMENT ~A)" (type-of bset) (map 'list (function identity) (bset-bitsets bset)) (bset-cardinal bset) (bset-first-element bset) (bset-last-element bset)))))
  "A set of small integers, implemented as a vector of words."
  (bitsets (make-array (list 0)
                       :element-type 'bitset
                       :initial-element 0
                       :adjustable t)
           :type (array bitset *))
  ;; max-element == (* +bit-per-bitset+ (array-dimension bitsets 0))
  ;; last-bitset == (1- (array-dimension bitsets 0))
  (cardinal      nil :type (or null (integer 0)))
  (first-element 0   :type (integer 0)) ; approximate
  (last-element  0   :type (integer 0)) ; approximate
  ;; (for all i (==> (< i (bset-first-element bset)) (not (is-element i bset))))
  ;; (for all i (==> (> i (bset-last-element  bset)) (not (is-element i bset))))
  )


(defun copy-bset (original)
  "
RETURN: A new copy of the ORIGINAL bset.
"
  (let ((copy (%make-bset)))
    (setf (bset-bitsets       copy) (bset-bitsets       original)
          (bset-cardinal      copy) (bset-cardinal      original)
          (bset-first-element copy) (bset-first-element original)
          (bset-last-element  copy) (bset-last-element  original))
    copy))


(defmacro bsref (bsa i) `(aref ,bsa ,i))


(declaim (inline last-bitset))
(defun last-bitset (bitsets)
  "
RETURN:  The index of the last bitset in the BITSETS array.
"
  (1- (array-dimension bitsets 0)))


(declaim (inline elem-to-bitset))
(defun elem-to-bitset (element)
  "
RETURN:  The index of the bitset where element is stored.
NOTE:     0 --> 0
         31 --> 0
         32 --> 1
         63 --> 1
         64 --> 2
"
  (truncate element +bit-per-bitset+))


(declaim (inline elem-to-bit))
(defun elem-to-bit (element)
  (mod element +bit-per-bitset+))


(declaim (inline bitset-to-elem))
(defun bitset-to-elem (index)
  "
RETURN:  The maximum element + 1 that can be stored in the bitset at INDEX.
NOTE:    0 --> 32
         1 --> 64
         2 --> 96
"
  (* +bit-per-bitset+ (1+ index)))


(defun make-bset (max-size)
  "
PRE:    (<= 0 max-size)
POST:   (<= max-size (size (make-bset max-size)))
RETURN: A new bset allocated to hold at least elements from 0 to max-size.
"
  (declare (type (integer 0) max-size))
  (%make-bset :bitsets (make-array (list (1+ (elem-to-bitset max-size)))
                                   :element-type 'bitset
                                   :initial-element 0
                                   :adjustable t)
              :cardinal 0))



(defun resize-bset (bset max-size)
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
  (declare (type bset bset) (type (integer 0) max-size))
  (let ((old-count (array-dimension (bset-bitsets bset) 0))
        (new-count (1+ (elem-to-bitset max-size))))
    (setf (bset-bitsets bset) (adjust-array (bset-bitsets bset)
                                            (list new-count)
                                            :element-type 'bitset
                                            :initial-element 0))
    (when (< new-count old-count)
      (setf (bset-last-element bset) (bitset-to-elem (1- new-count))
            (bset-cardinal bset) nil))
    bset))


(defgeneric union (s1 s2))       
(defmethod union ((set1 bset) (set2 bset))
  "
DO:      set1 := set1 U ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the union of set1 and set2
         modulo the allocated size of set1.
RETURN:  SET1
"
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i
           (elem-to-bitset (min (size set1) (bset-first-element set2)))
           (elem-to-bitset (min (size set1) (bset-last-element  set2))))
         (when (/= 0 (bsref bits2 i))
           (setf (bsref bits1 i) (logior (bsref bits1 i) (bsref bits2 i))))))
  (setf (bset-cardinal set1) nil
        (bset-first-element set1) (min (bset-first-element set1)
                                       (bset-first-element set2))
        (bset-last-element set1)  (max (bset-last-element set1)
                                       (bset-last-element set2)))
  set1)
         
(defgeneric difference (s1 s2))
(defmethod difference ((set1 bset) (set2 bset))
  "
DO:      set1 := set1 - ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the difference of set1 and set2
         (elements in set1 not in set2)
         modulo the allocated size of set1.
RETURN:  SET1
"
  ;; {x|x in set1 and not x in set2}
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i
           (elem-to-bitset (bset-first-element set2))
           (min (elem-to-bitset (bset-last-element set1))
                (elem-to-bitset (bset-last-element set2))))
         (when (/= 0 (bsref bits2 i))
           (setf (bsref bits1 i) (logandc2 (bsref bits1 i) (bsref bits2 i))))) )
  (setf (bset-cardinal set1) nil)
  set1)
         
(defgeneric intersection (s1 s2))
(defmethod intersection ((set1 bset) (set2 bset))
  "
DO:      set1 := set1 inter set2 inter
         Accumulate in set1 the intersection of set1 and set2
         (elements in set1 and in set2).
RETURN:  SET1
"
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i
           (elem-to-bitset (max (bset-first-element set1)
                                (bset-first-element set2)))
           (elem-to-bitset (min (bset-last-element set1)
                                (bset-last-element set2))))
         (setf (bsref bits1 i) (logand (bsref bits1 i) (bsref bits2 i)))))
  (setf (bset-cardinal set1) nil
        (bset-first-element set1) (max (bset-first-element set1)
                                       (bset-first-element set2))
        (bset-last-element set1)  (min (bset-last-element set1)
                                       (bset-last-element set2)))
  set1)
         
(defgeneric sym-diff (s1 s2))
(defmethod sym-diff ((set1 bset) (set2 bset))
  "
DO:      set1 := set1 delta ( set2 inter (complement (make-bset (size set1))) )
         Accumulate in set1 the symetrical difference of set1 and set2
         (elements in set1 not in set2 or in set2 not in bset 1)
         modulo the allocated size of set1.
RETURN:  SET1
"
  ;; {x|(x in set1 and not x in set2) or (x in set2 and not x in set1)}
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i
           (elem-to-bitset (max (bset-first-element set1)
                                (bset-first-element set2)))
           (elem-to-bitset (min (bset-last-element set1)
                                (bset-last-element set2))))
         (setf (bsref bits1 i) (logxor (bsref bits1 i) (bsref bits2 i)))))
  (setf (bset-cardinal set1) nil
        (bset-first-element set1) (min (bset-first-element set1)
                                       (bset-first-element set2))
        (bset-last-element set1)  (max (bset-last-element set1)
                                       (bset-last-element set2)))
  set1)

(defgeneric complement (s))
(defmethod complement ((bset bset))
  "
DO:      set1 := (complement (make-bset (size set1))) - set1
         Accumulate in set1 the complement of set1
         (elements in not set1)
         modulo the allocated size of set1.
RETURN:  SET1
"
  (let ((bits (bset-bitsets bset)))
    (for (i 0 (last-bitset bits))
         (setf (bsref bits i)
               (dpb (lognot (bsref bits i)) (byte 32 0) 0)))
    (setf (bset-cardinal bset) (and (bset-cardinal bset)
                                    (- (bitset-to-elem (last-bitset bits))
                                       (bset-cardinal bset)))
          (bset-first-element bset) 0
          (bset-last-element  bset) (1- (bitset-to-elem (last-bitset bits)))))
  bset)

(defgeneric include (set element))
(defmethod include ((bset bset) element)
  "
PRE:    (<= 0 element (size bset))
POST:   (is-element element bset)
RETURN: BSET
"
  (declare (type (integer 0) element))
  (let ((bits (bset-bitsets bset)))
    (setf (bsref bits (elem-to-bitset element))
          (dpb 1 (byte 1 (elem-to-bit element))
               (bsref bits (elem-to-bitset element)))))
  (setf (bset-cardinal bset) nil
        (bset-first-element bset) (min element (bset-first-element bset))
        (bset-last-element  bset) (max element (bset-last-element  bset)))
  bset)


(defgeneric exclude (set element))
(defmethod exclude ((bset bset) element)
  "
PRE:    (<= 0 element (size bset))
POST:   (not (is-element element bset))
RETURN: BSET
"
  (declare (type (integer 0) element))
  (let ((bits (bset-bitsets bset)))
    (setf (bsref bits (elem-to-bitset element))
          (dpb 0 (byte 1 (elem-to-bit element))
               (bsref bits (elem-to-bitset element))))
    (setf (bset-cardinal bset) nil)
    (when (and (= element (bset-first-element bset))
               (< element (bitset-to-elem (last-bitset bits))))
      (incf (bset-first-element bset)))
    (when (and (= element (bset-last-element bset))
               (< 0 element))
      (decf (bset-last-element bset))))
  bset)

(defgeneric cardinal (set))
(defmethod cardinal ((bset bset))
  "
RETURN:  The number of elements in BSET.
"
  (unless (bset-cardinal bset)
    (let ((cardinal 0))
      (for (i (bset-first-element bset) (bset-last-element bset))
           (when (is-element i bset) (incf cardinal)))
      (setf (bset-cardinal bset) cardinal)))
  (bset-cardinal bset))


(defgeneric size (bset))
(defmethod size ((bset bset))
  "
RETURN:  The maximum element BSET can hold.
"
  (let ((bits (bset-bitsets bset)))
    (1- (bitset-to-elem (last-bitset bits)))))


(defgeneric minimum (bset))
(defmethod minimum ((bset bset))
  "
PRE:     (not (is-empty bset))
RETURN:  The smallest element of BSET.
"
  (for (i (bset-first-element bset)  (bset-last-element bset))
    (when (is-element i bset)
      (setf (bset-first-element bset) i)
      (return-from minimum i)))
  0)


(defgeneric maximum (bset))
(defmethod maximum ((bset bset))
  "
PRE:     (not (is-empty bset))
RETURN:  The greatest element of BSET.
"
  (for (i (bset-last-element bset)  (bset-first-element bset))
    (when (is-element i bset)
      (setf (bset-last-element bset) i)
      (return-from maximum i)))
  0)



(defgeneric select (bset))
(defmethod select ((bset bset))
  "
PRE:      (not (is-empty bset))
RETURN:   An element of BSET.
WARNING:  May return always the same element if it's not removed from the BSET.
"
  (minimum bset))


(defgeneric extract (bset))
(defmethod extract ((bset bset))
  "
PRE:      (not (is-empty bset))
POST:     (not (is-element (extract bset) bset))
DO:       Select an element from the BSET and removes it from the BSET.
RETURN:   An element that was in BSET.
"
  (let ((i (minimum bset))) (exclude bset i) i))


(defgeneric is-subset (set1 set2)
  (:method (set1 set2)
    (subsetp set1 set2)))

(defgeneric subsetp (set1 set2))
(defmethod subsetp ((set1 bset) (set2 bset))
  "
RETURN:  Whether  SET1 is a subset of SET2.
"
  ;; x in set1 ==> x in set2
  ;; 1 1  0
  ;; 1 0  1
  ;; 0 1  0
  ;; 0 0  0
  ;; set2|~set1 : logandc2
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i (elem-to-bitset (bset-first-element set1))
           (elem-to-bitset (min (bset-last-element set1)
                                (bset-last-element set2))))
      (cond
        ((= 0 (bsref bits1 i)))
        ((= 0 (bsref bits2 i))
         (return-from subsetp nil))
        ((/= 0 (logandc2 (bsref bits1 i) (bsref bits2 i)))
         (return-from subsetp nil)))
      (when (> (bset-last-element set1) (bset-last-element set2))
        (for (i (1+ (elem-to-bitset (bset-last-element set1)))
               (elem-to-bitset (bset-last-element set2)))
          (when (/= 0 (bsref bits1 i))
            (return-from subsetp nil))))))
  t)

(defgeneric is-strict-subset (set1 set2)
  (:method (set1 set2)
    (strict-subsetp set1 set2)))

(defgeneric strict-subsetp (set1 set2))
(defmethod strict-subsetp ((set1 bset) (set2 bset))
  "
RETURN:  Whether SET1 is a strict subset of SET2.
"
  (and (subsetp set1 set2) (not (is-equal set1 set2))))


(defgeneric is-equal (set1 set2))
(defmethod is-equal ((set1 bset) (set2 bset))
  "
RETURN:  Whether SET1 and SET2 contain the same elements.
  "
  (or (eq set1 set2)
      (let ((bits1 (bset-bitsets set1))
            (bits2 (bset-bitsets set2)))
        (for (i
                 (elem-to-bitset (min (bset-first-element set1)
                                      (bset-first-element set2)))
               (elem-to-bitset (min (bset-last-element set1)
                                    (bset-last-element set2))))
          (unless (= (bsref bits1 i) (bsref bits2 i))
            (return-from is-equal nil)))
        (when (> (elem-to-bitset (size set1))
                 (elem-to-bitset (bset-last-element set1))
                 (elem-to-bitset (bset-last-element set2)))
          (for (i
                   (1+ (elem-to-bitset (min (bset-last-element set1)
                                            (bset-last-element set2))))
                 (elem-to-bitset (size set1)))
            (when (/= 0 (bsref bits1 i))
              (return-from is-equal nil))))
        (when (> (elem-to-bitset (size set2))
                 (elem-to-bitset (bset-last-element set2))
                 (elem-to-bitset (bset-last-element set1)))
          (for (i
                   (1+ (elem-to-bitset (min (bset-last-element set1)
                                            (bset-last-element set2))))
                 (elem-to-bitset (size set2)))
            (when (/= 0 (bsref bits2 i))
              (return-from is-equal nil))))
        t)))


(defgeneric is-not-equal (set1 set2))
(defmethod is-not-equal ((set1 bset) (set2 bset))
  "
RETURN:  (not (is-equal set1 set2))
"
  (not (is-equal set1 set2)))


(defgeneric is-element (element bset))
(defmethod is-element (element (bset bset))
  "
RETURN:  Whether element is in BSET.
"
  (declare (type (integer 0) element))
  (let ((bits (bset-bitsets bset)))
    (and (< element (bitset-to-elem (last-bitset bits)))
         (/= 0 (logand (bsref bits (elem-to-bitset element)) 
                       (ash 1 (elem-to-bit element)))))))

(defgeneric is-empty (set)
  (:method (set) (emptyp set)))

(defgeneric emptyp (set))
(defmethod emptyp ((bset bset))
  "
RETURN: (= 0 (cardinal bset))
"
  (or (and (bset-cardinal bset) (= 0 (bset-cardinal bset)))
      (let ((bits (bset-bitsets bset)))
        (for (i 0 (last-bitset bits))
          (when (/= 0 (bsref bits i)) (return-from is-empty nil)))
        (setf (bset-cardinal bset) 0)
        t)))


(defgeneric for-all (bset proc))
(defmethod for-all ((bset bset) proc)
  "
DO:     Call function PROC for each element in the BSET until PROC returns NIL.
RETURN: Whether no call to PROC returned NIL.
"
  (for (i (bset-first-element bset) (bset-last-element bset))
    (when (and (is-element i bset) (not (funcall proc i)))
      (return-from for-all nil)))
  t)


(defgeneric exists (bset proc))
(defmethod exists ((bset bset) proc)
  "
DO:      Call function PROC for each element in the BSET
         until PROC returns non nil.
RETURN:  Whether PROC returned non nil.
"
  (for (i (bset-first-element bset) (bset-last-element bset))
    (when (and (is-element i bset) (funcall proc i))
      (return-from exists t)))
  nil)


(defgeneric exists-1 (bset proc))
(defmethod exists-1 ((bset bset) proc)
  "
DO:       Call function PROC on all elements in the BSET.
RETURN:   Whether PROC returned non nil for exactly one element.
"
  (let ((n 0))
    (for (i (bset-first-element bset) (bset-last-element bset))
      (when (and (is-element i bset) (funcall proc i))
        (incf n)))
    (= n 1)))


(defgeneric assign (set1 set2))
(defmethod assign ((set1 bset) (set2 bset))
  "
DO:      Accumulate in set1 the elements of set2 that are less than (size set1).
POST:    (is-equal set1 (intersection (complement (make-bset (size set1)))set2))
RETURN:  SET1
"
  (let ((bits1 (bset-bitsets set1))
        (bits2 (bset-bitsets set2)))
    (for (i 0 (min (last-bitset bits1) (last-bitset bits2)))
      (setf (bsref bits1 i) (bsref bits2 i)))
    (when (< (min (last-bitset bits1) (last-bitset bits2)) (last-bitset bits1))
      (for (i (1+ (min (last-bitset bits1) (last-bitset bits2)))
             (last-bitset bits1))
        (setf (bsref bits1 i) 0)))
    (setf (bset-cardinal set1) (bset-cardinal set2)
          (bset-first-element set1) (min (bset-first-element set2)
                                         (bitset-to-elem (last-bitset bits1)))
          (bset-last-element  set1) (min (bset-last-element  set2)
                                         (1- (bitset-to-elem (last-bitset bits1))))))
  set1)


(defgeneric assign-element (bset element))
(defmethod assign-element ((bset bset) element)
  "
DO:     Empties BSET and include element.
PRE:    (<= 0 element (size bset))
POST:   (and (exists bset (lambda (x) (= x element)))
             (for-all bset (lambda (x) (= x element))))
RETURN:  BSET
"
  (declare (type (integer 0) element))
  (assign-empty bset)
  (include bset element)
  (setf (bset-cardinal bset) 1
        (bset-first-element bset) element
        (bset-last-element  bset) element)
  bset)


(defgeneric assign-empty (bset))
(defmethod assign-empty ((bset bset))
  "
POST:    (is-empty bset)
RETURN:  BSET.
"
  (let ((bits (bset-bitsets bset)))
    (for (i 0 (last-bitset bits))   (setf (bsref bits i) 0))
    (setf (bset-cardinal bset) 0
          (bset-first-element bset) 0
          (bset-last-element  bset) 0))
  bset)


(defgeneric for-all-do (bset proc))
(defmethod for-all-do ((bset bset) proc)
  "
DO:      Call PROC on all elements in BSET.
RETURN:  BSET.
"
  (for (i (bset-first-element bset) (bset-last-element bset))
    (when (is-element i bset)
      (funcall proc i)))
  bset)

 
(defgeneric bset-to-list (bset))
(defmethod bset-to-list ((bset bset))
  "
RETURN:  A list of all elements of BSET, sorted in increasing order.
"
  (let ((elements '()))
    (for (i (bset-last-element bset) (bset-first-element bset))
      (when (is-element i bset)
        (push i elements)))
    elements))


(defun list-to-bset (list)
  "
PRE:     LIST is a list of positive integer.
RETURN:  A new bset containing all the elements in the list.
"
  (let ((bset (make-bset (apply (function max) list))))
    (dolist (element list)
      (include bset element))
    bset))


(defun read-bset (stream bset)
  "
DO:      Accumulate in BSET the elements read from the stream.
RETURN:  BSET.
"
  (let ((cardinal 0))
    (assign-empty bset)
    (when (peek-char (character "(") stream nil nil)
      (read-char stream)
      (do ()
          ((char= (peek-char t stream nil (character ")")) (character ")")))
        (include bset (read stream))
        (format t "~S~%" bset)
        (incf cardinal)))
    (setf (bset-cardinal bset) cardinal)
    (read-char stream))
  bset)


(defun write-bset (stream bset)
  "
DO:     Writes to the stream the elements in BSET.
RETURN: BSET.
"
  (princ "(" stream)
  (for-all-do bset (lambda (element) (princ element stream) (princ " " stream)))
  (princ ")" stream)
  bset)


;;;; THE END ;;;;
