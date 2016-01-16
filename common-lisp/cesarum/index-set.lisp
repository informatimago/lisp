;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               index-set.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a set of indexes, represented as a list of ranges.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-05-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"
                          "INCLUDE"  "MERGE" "INTERSECTION" "UNION")
  (:export
   "CONTAINS" "CARDINAL" "EMPTYP" "MINIMUM" "MAXIMUM"
   "MAKE-COLLECTOR" "MAP-ELEMENTS" "THEREIS" "THEREIS1" "ALWAYS"
   "SET-EQUAL" "IS-SUBSET" "IS-STRICT-SUBSET" "INTENSION" "COPY"
   "UNION" "INTERSECTION" "DIFFERENCE" "SYMETRIC-DIFFERENCE" "INCLUDE"
   "EXCLUDE" "ASSIGN-EMPTY" "ASSIGN-SINGLETON" "ASSIGN" "MERGE"
   "INTERSECT" "SUBTRACT")

  (:export
   "INDEX-SET" "MAP-RANGES"
   
   "MAKE-RANGE" "COPY-RANGE" "EQUAL-RANGE"
   "RANGE" "RANGE-EMPTYP" "RANGE-COUNT"
   "RANGE-START" "RANGE-END" "RANGE-FIRST" "RANGE-LAST")
  
  (:documentation
   "

This package implements sets of INTEGER as a sequence of ranges.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2013 - 2013
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET")



;;;=====================================================================
;;; RANGE CLASS

(defclass range ()
  ((start :initarg :start :initform 0 :type integer :writer (setf range-start)
          :documentation "First element in the range.")
   (end   :initarg :end   :initform 0 :type integer :writer (setf range-end)
          :documentation "First element beyond the range.")))

(defmethod print-object ((range range) stream)
  (print-unreadable-object (range stream :type t)
    ;; (format stream "~{~S~^ ~}" (list :start (slot-value range 'start)
    ;;                                  :end   (slot-value range 'end)))
    (if (range-emptyp range)
        (princ "empty" stream)
        (format stream "~A-~A" (range-first range) (range-last range))))
  range)

(defun exactly-one (&rest parameters)
  (= 1 (count nil parameters :key (function not))))

(defun make-range (&key start end first last count)
  (assert (or (and (exactly-one start first)
                   (exactly-one end last count))
              (and (exactly-one start first count)
                   (exactly-one end last))))
  (make-instance 'range
      :start (cond (start start)
                   (first first)
                   (end (- end count))
                   (t   (- last count -1)))
      :end   (cond (end end)
                   (last (1+ last))
                   (count (+ start count)))))

(defgeneric range-emptyp (range)
  (:method ((range range))
    (<= (slot-value range 'end) (slot-value range 'start))))

(defgeneric range-count (range)
  (:method ((range range))
    (max 0 (- (range-end range) (range-start range)))))

(defgeneric range-start (range)
  (:method ((range range))
    (unless (range-emptyp range)
      (slot-value range 'start))))

(defgeneric range-end (range)
  (:method ((range range))
    (unless (range-emptyp range)
      (slot-value range 'end))))

(defgeneric range-first (range)
  (:method ((range range))
    (unless (range-emptyp range)
      (slot-value range 'start))))

(defgeneric range-last (range)
  (:method ((range range))
    (unless (range-emptyp range)
      (1- (slot-value range 'end)))))

(defgeneric copy-range (range)
  (:method ((range range))
    (make-instance 'range
        :start (slot-value range 'start)
        :end (slot-value range 'end))))

(defgeneric equal-range (r1 r2)
  (:method ((r1 range) (r2 range))
    (or (and (range-emptyp r1) (range-emptyp r2))
        (and (= (range-start r1) (range-start r2))
             (= (range-end   r1) (range-end   r2))))))


;;;=====================================================================
;;; INDEX-SET CLASS

(defclass index-set ()
  ((ranges :initform #() :type vector :initarg ranges)))
;; Invariants:
;; no empty range
;; ∀i∈[0 .. (1- (length ranges))[, (< (range-end (aref ranges i)) (range-start (aref ranges (1+ i))))

(defmethod print-object ((set index-set) stream)
  (print-unreadable-object (set  stream :identity t :type t)
    (format stream "~{~S~^ ~}" (coerce (slot-value set 'ranges) 'list)))
  set)

(defun index-set (&rest elements)
  (assign (make-instance 'index-set) elements))

(defgeneric check-invariant (object))
(defmethod check-invariant ((set index-set))
  (assert (slot-boundp set 'ranges))
  (let ((ranges (slot-value set 'ranges)))
    (check-type ranges vector)
    (notany (function range-emptyp) ranges)
    (when (<= 2 (length ranges))
      (assert
       (loop
         :for i :below (1- (length ranges))
         :always (< (range-end (aref ranges i)) (range-start (aref ranges (1+ i)))))))))


(defgeneric map-ranges (result-type mapper index-set)
  (:method (result-type mapper (set index-set))
    (collecting-result (collect result-type)
      (loop
        :for range :across (slot-value set 'ranges)
        :do (collect (funcall mapper range))))))




(defmethod emptyp              ((set index-set))
  (vector-emptyp (slot-value set 'ranges)))

(defmethod cardinal              ((set index-set))
  (reduce (function +) (slot-value set 'ranges) :key (function range-count)))

(defmethod minimum               ((set index-set))
  (unless (emptyp set)
    (range-start (aref (slot-value set 'ranges) 0))))

(defmethod maximum               ((set index-set))
  (unless (emptyp set)
    (range-last (vector-last (slot-value set 'ranges)))))


(defgeneric range-of-element (set element)
  (:method ((set index-set) element)
    (check-type element integer)
    (dichotomy-search (slot-value set 'ranges)
                      element
                      (lambda (element range)
                        (cond
                          ((< element (range-start range)) -1)
                          ((< element (range-end range))    0)
                          (t                               +1))))))

(defmethod contains              ((set index-set) element)
  (declare (ignore element))
  nil)

(defmethod contains              ((set index-set) (element integer))
  (values (range-of-element set element)))


(defmethod make-collector        ((result-type (eql 'index-set)))
  (declare (ignorable result-type))
  (lambda (&optional set (element nil add-element-p))
    (if add-element-p
        (include set element)
        (make-instance 'index-set))))


(defmethod map-elements           (result-type mapper (set index-set))
  (collecting-result (collect result-type)
    (loop
      :for range :across (slot-value set 'ranges)
      :do (loop
            :for element :from (range-start range) :below (range-end range)
            :do (collect (funcall mapper element))))))


(defmethod set-equal              ((set1 index-set) (set2 index-set))
  (and (= (length (slot-value set1 'ranges)) (length (slot-value set2 'ranges)))
       (loop
         :for r1 :across (slot-value set1 'ranges)
         :for r2 :across (slot-value set2 'ranges)
         :always (equal-range r1 r2))))


(defmethod is-subset             ((set1 index-set) (set2 index-set))
  (loop
    :for range :across (slot-value set1 'ranges)
    :always (multiple-value-bind (f1 i1) (range-of-element set2 (range-start range))
              (multiple-value-bind (f2 i2) (range-of-element set2 (range-last range))
                (and f1 f2 (= i1 i2))))))

(defmethod is-strict-subset      ((set1 index-set) (set2 index-set))
  (and (< (cardinal set1) (cardinal set2))
       (is-subset set1 set2)))

;;-----------------------------------------------------------------------
;; Algorithms

(defun complement-ranges (ranges start end)
  (assert (or (vector-emptyp ranges)
              (and (<= start (range-start (vector-first ranges)))
                   (<= (range-end (vector-last ranges)) end))))
  (cond
    ((vector-emptyp ranges)
     (vector (make-range :start start :end end)))
    (t
     (loop
       :with len = (length ranges)
       :with result = (make-array (1+ len) :fill-pointer 0 :adjustable t)
       :for r :across ranges
       :do (progn
             (unless (= start (range-start r))
               (vector-push-extend (make-range :start start :end (range-start r)) result (length result)))
             (setf start (range-end r)))
       :finally (progn
                  (unless (= start end)
                    (vector-push-extend (make-range :start start :end end) result (length result)))
                  (return result))))))


(defun merge-ranges (a b)
  (cond
    ((vector-emptyp b) a)
    ((vector-emptyp a) b)
    (t
     (loop
       :with lena = (length a)
       :with lenb = (length b)
       :with result = (make-array (+ lena lenb) :fill-pointer 0 :adjustable t)
       :with a-is-smallest =  (< (range-start (aref a 0))
                                 (range-start (aref b 0)))
       :with current = (copy-range (aref (if a-is-smallest a b) 0))
       :with i = (if a-is-smallest 1 0)
       :with j = (if a-is-smallest 0 1)
       :do (progn
             (loop
               :with merge-a 
               :while (or (setf merge-a (and (< i lena)
                                             (<= (range-start (aref a i)) (range-end current))))
                          (and (< j lenb)
                               (<= (range-start (aref b j)) (range-end current))))
               :do (if merge-a
                       (progn
                         (setf (range-end current) (range-end (aref a i)))
                         (incf i))
                       (progn
                         (setf (range-end current) (range-end (aref b j)))
                         (incf j))))
             (vector-push-extend current result (length result))
             (if (and (< i lena) (< j lenb))
                 (if (< (range-start (aref a i)) (range-start (aref b j)))
                     (progn
                       (setf current (copy-range (aref a i)))
                       (incf i))
                     (progn
                       (setf current (copy-range (aref b j)))
                       (incf j)))
                 (progn
                   (loop
                     :while (< i lena)
                     :do (progn (vector-push-extend (copy-range (aref a i)) result (length result))
                                (incf i)))
                   (loop
                     :while (< j lenb)
                     :do (progn (vector-push-extend (copy-range (aref b j)) result (length result))
                                (incf j)))
                   (return result))))))))


(defun intersect-ranges (a b)
  (cond
    ((vector-emptyp a) a)
    ((vector-emptyp b) b)
    (t
     (loop
       :with lena = (length a)
       :with lenb = (length b)
       :with result = (make-array 4 :fill-pointer 0 :adjustable t)
       :with i = 0 :with current-a = (aref a i)
       :with j = 0 :with current-b = (aref b j)
       :do (progn
             
             (loop
               :while (and (< i lena)
                           (<= (range-end current-a) (range-start current-b)))
               :do (progn
                     (incf i)
                     (setf current-a (when (< i lena) (aref a i)))))
             (unless current-a (return result))

             (loop
               :while (and (< j lenb)
                           (<= (range-end current-b) (range-start current-a)))
               :do (progn
                     (incf j)
                     (setf current-b (when (< j lenb) (aref b j)))))
             (unless current-b (return result))

             (unless (or (<= (range-end current-a) (range-start current-b))
                         (<= (range-end current-b) (range-start current-a)))
               (vector-push-extend (make-range :start (max (range-start current-a)
                                                           (range-start current-b))
                                               :end   (min (range-end current-a)
                                                           (range-end current-b)))
                                   result (length result))
               (cond
                 ((= (range-end current-a) (range-end current-b))
                  (incf i)
                  (if (< i lena)
                      (setf current-a (aref a i))
                      (return result)) 
                  (incf j)
                  (if (< j lenb)
                      (setf current-b (aref b j))
                      (return result)))
                 ((< (range-end current-a) (range-end current-b))
                  (incf i)
                  (if (< i lena)
                      (setf current-a (aref a i))
                      (return result)))
                 (t
                  (incf j)
                  (if (< j lenb)
                      (setf current-b (aref b j))
                      (return result))))))))))


(defun difference-ranges (r1 r2)
  (if (or (vector-emptyp r1)
          (vector-emptyp r2))
      r1
      (let* ((start (min (range-start r1) (range-start r2)))
             (end   (max (range-end   r1) (range-end   r2))))
        (intersect-ranges r1 (complement-ranges r2 start end)))))


(defun symetric-difference-ranges (r1 r2)
  (cond
    ((vector-emptyp r1) r2)
    ((vector-emptyp r2) r1)
    (t
     (let* ((start (min (range-start r1) (range-start r2)))
            (end   (max (range-end   r1) (range-end   r2))))
       (intersect-ranges (merge-ranges r1 r2) (complement-ranges (intersect-ranges r1 r2) start end))))))


(defun collect-ranges (result-type ranges)
  (collecting-result (collect result-type)
    (loop
      :for range :across ranges
      :do (loop
            :for element :from (range-start range) :below (range-end range)
            :do (collect element)))))


(defun equal-ranges (a b)
  (and (vectorp a)
       (vectorp b)
       (= (length a) (length b))
       (every (function equal-range) a b)))


;;----------------------------------------------------------------------
;; Functional

(defmethod union                 ((result-type (eql 'index-set)) (set1 index-set) (set2 index-set))
  (make-instance 'index-set 'ranges (merge-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod union                 (result-type (set1 index-set) (set2 index-set))
  (collect-ranges result-type (merge-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod intersection          ((result-type (eql 'index-set)) (set1 index-set) (set2 index-set))
  (make-instance 'index-set 'ranges (intersect-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod intersection          (result-type (set1 index-set) (set2 index-set))
  (collect-ranges result-type (intersect-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod difference            ((result-type (eql 'index-set)) (set1 index-set) (set2 index-set))
  (make-instance 'index-set 'ranges (difference-ranges  (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod difference            (result-type (set1 index-set) (set2 index-set))
  (collect-ranges result-type (difference-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod symetric-difference   ((result-type (eql 'index-set)) (set1 index-set) set2)
  (make-instance 'index-set 'ranges (symetric-difference-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

(defmethod symetric-difference   (result-type (set1 index-set) set2)
  (collect-ranges result-type (symetric-difference-ranges (slot-value set1 'ranges) (slot-value set2 'ranges))))

;;----------------------------------------------------------------------
;; Mutation

(defmethod include               ((destination-set index-set) (range range))
  (unless (range-emptyp range)
    (merge destination-set (make-instance 'index-set 'ranges (vector range))))
  destination-set)

(defmethod include               ((destination-set index-set) (element integer))
  (multiple-value-bind (found index order) (range-of-element destination-set element)
    (unless found
      (let ((ranges (slot-value destination-set 'ranges)))
        (flet ((check-fusion (index)
                 (when (= (range-end (aref ranges index))
                          (range-start (aref ranges (1+ index))))
                   (setf (range-end (aref ranges index)) (range-end (aref ranges (1+ index)))
                         (slot-value destination-set 'ranges)
                         (replace-subseq '() ranges index (1+ index))))))
          (cond
            ((vector-emptyp ranges)
             (setf (slot-value destination-set 'ranges)
                   (vector (make-range :start element :count 1))))
            ((minusp order)
             (if (= (1+ element) (range-start (vector-first ranges)))
                 (decf (range-start (vector-first ranges)))
                 (setf (slot-value destination-set 'ranges)
                       (replace-subseq (list (make-range :start element :count 1))
                                       ranges 0 0))))
            ((< (1+ (maximum destination-set)) element)
             (setf (slot-value destination-set 'ranges)
                   (replace-subseq (list (make-range :start element :count 1))
                                   ranges (length ranges) (length ranges))))
            ((< (maximum destination-set) element)
             (incf (range-end (vector-last ranges))))
            ((= (1+ element) (range-start (aref ranges (1+ index))))
             (decf (range-start (aref ranges (1+ index))))
             (check-fusion index))
            ((= (range-end (aref ranges index)) element)
             (incf (range-end (aref ranges index)))
             (check-fusion index))
            (t
             (setf (slot-value destination-set 'ranges)
                   (replace-subseq (list (make-range :start element :count 1))
                                   ranges index index))))))))
  destination-set)


(defmethod exclude               ((destination-set index-set) (range range))
  (unless (range-emptyp range)
    (subtract destination-set (make-instance 'index-set 'ranges (vector range))))
  destination-set)

(defmethod exclude               ((destination-set index-set) (element integer))
  (multiple-value-bind (found index) (range-of-element destination-set element)
    (when found
      (let ((ranges (slot-value destination-set 'ranges)))
        (flet ((check-empty (index)
                 (when (range-emptyp (aref ranges index))
                   (setf (slot-value destination-set 'ranges)
                         (replace-subseq '() ranges index (1+ index))))))
          (cond
            ((= element (range-start (aref ranges index)))
             (incf (range-start (aref ranges index)))
             (check-empty index))
            ((= (range-last (aref ranges index)) element)
             (decf (range-end (aref ranges index)))
             (check-empty index))
            (t
             (let ((new-range (make-range :start (1+ element)
                                          :end (range-end (aref ranges index)))))
               (setf (range-end (aref ranges index)) element
                     (slot-value destination-set 'ranges)
                     (replace-subseq (list new-range) ranges (1+ index) (1+ index))))))))))
  destination-set)


(defmethod assign-empty          ((destination-set index-set))
  (setf (slot-value destination-set 'ranges) #())
  destination-set)


(defmethod assign-singleton      ((destination-set index-set) element)
  (setf (slot-value destination-set 'ranges)
        (vector (make-range :start element :count 1)))
  destination-set)


(defmethod assign                ((destination-set index-set) (source-set index-set))
  (setf (slot-value destination-set 'ranges)
        (map 'vector (function copy-range) (slot-value source-set 'ranges)))
  destination-set)


(defmethod merge                 ((destination-set index-set) (source-set index-set))
  (let ((merged-ranges (merge-ranges (slot-value destination-set 'ranges)
                                     (slot-value source-set 'ranges))))
    (setf (slot-value destination-set 'ranges)
          (if (eq merged-ranges (slot-value source-set 'ranges))
              (map-into (make-array (length merged-ranges)
                                    :fill-pointer (length merged-ranges)
                                    :adjustable t)
                        (function copy-range) merged-ranges)
              merged-ranges)))
  destination-set)


(defmethod intersect             ((destination-set index-set) (source-set index-set))
  (let ((intersected-ranges (intersect-ranges (slot-value destination-set 'ranges)
                                              (slot-value source-set 'ranges))))
    (setf (slot-value destination-set 'ranges)
          (if (eq intersected-ranges (slot-value source-set 'ranges))
              (map-into (make-array (length intersected-ranges)
                                    :fill-pointer (length intersected-ranges)
                                    :adjustable t)
                        (function copy-range) intersected-ranges)
              intersected-ranges)))
  destination-set)


(defmethod subtract              ((destination-set index-set) (source-set index-set))
  (setf (slot-value destination-set 'ranges)
        (difference-ranges (slot-value destination-set 'ranges)
                           (slot-value source-set 'ranges)))
  destination-set)


;; (copy 'index-set '(1 2 3 4))
;; (map-elements 'list 'identity  (copy 'index-set '(1 2 3 4)))
;; (map-elements 'vector 'identity  (copy 'index-set '(1 2 3 4)))
;; (copy 'vectorx (copy 'index-set '(1 2 3 4)))

;;;; THE END ;;;;
