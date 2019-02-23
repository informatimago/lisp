;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sequence.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This module exports sequence functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-24 <PJB> Added REPLACE-SUBSEQ.
;;;;    2012-02-19 <PJB> Extracted from list.lisp and some other code.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
  (:use "COMMON-LISP")
  (:export "HASHED-SET-REMOVE-DUPLICATES"
           "HASHED-REMOVE-DUPLICATES" "HASHED-DELETE-DUPLICATES"
           "DUPLICATES"
           "REPLACE-SUBSEQ"
           "DELETEF"
           "GROUP-BY"
           "PARSE-SEQUENCE-TYPE"
           "CONCATENATE-SEQUENCES"
           "PREFIXP"
           "SUFFIXP"
           "MAPCONCAT")
  (:documentation
   "

This package exports sequence processing functions.


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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE")


(defun item-delete (sequence item &rest keys &key from-end test test-not start end count key)
  (declare (ignore from-end test test-not start end count key))
  (apply (function delete) item sequence keys))

(define-modify-macro deletef (item &rest keys)
  item-delete "Delete the item from the sequence in PLACE.")


(defun duplicates (sequence &key (test 'eql) (key 'identity))
  "
RETURN: A sequence of items appearing in SEQUENCE in duplicate.
        There are no duplicates in the result, a single representant
        is included.
"
  (remove-duplicates
   (if (or (member test '(eq eql equal equalp))
           (member test (list (function eq) (function eql) (function equal) (function equalp))))
       (let ((table (make-hash-table :test test)))
         (map nil (lambda (item) (incf (gethash (funcall key item) table 0)))
              sequence)
         (remove 1 sequence :key (lambda (item) (gethash (funcall key item) table 0))
                 :test '=))
       (let ((table '()))
         (map nil (lambda (item) (let ((entry (assoc item table :test test :key key)))
                                   (if entry
                                       (incf (cdr entry))
                                       (push (cons item 1) table))))
              sequence)
         (remove 1 sequence :key (lambda (item) (or (cdr (assoc item table :test test :key key)) 0))
                 :test '=)))
   :test test :key key))





(defun hashed-set-remove-duplicates (sequence &key (test (function eql))
                                     (key (function identity)))
  "
DO:       Remove duplicates from the SEQUENCE, using a hash-table.
RETURN:   A list of unique elements from the SEQUENCE.
SEQUENCE: A sequence.
TEST:     A comparison function.  Default: EQL.
KEY:      A key function. Default: IDENTITY.
"
  (let ((table (make-hash-table :test test :size (length sequence)))
        (result '()))
    (map nil (lambda (item) (setf (gethash (funcall key item) table) item)) sequence)
    (maphash (lambda (key value) (declare (ignore key)) (push value result)) table)
    result))


(defun hashed-remove-duplicates (sequence &key (test (function eql))
                                 test-not
                                 (start 0) (end (length sequence))
                                 (key (function identity))
                                 (from-end nil))
  "Like REMOVE-DUPLICATES but implemented using a HASH-TABLE."
  (when test-not
    (warn ":TEST-NOT is deprecated.")
    (setf test (complement test-not)))
  (let ((table (make-hash-table :test test :size (- end start))))
    (map nil (if from-end
                 (lambda (item)
                   (let ((item-key (funcall key item)))
                     (multiple-value-bind (val pre) (gethash item-key table)
                       (declare (ignore val))
                       (unless pre (setf (gethash item-key table) item)))))
                 (lambda (item) (setf (gethash (funcall key item) table) item)))
         (if (or (/= start 0) (/= end (length sequence)))
             (subseq sequence start end) sequence))
    (if (eq (type-of sequence) 'cons)
        (let ((result '()))
          (maphash (lambda (key value) (declare (ignore key)) (push value result))
                   table)
          (if (or (/= start 0) (/= end (length sequence)))
              (nconc (subseq sequence 0 start) result (subseq sequence end))
              result))
        (if (or (/= start 0) (/= end (length sequence)))
            (let ((result (make-sequence (type-of sequence)
                                         (+ start (hash-table-count table)
                                            (- (length sequence) end))))
                  (i start))
              (replace result sequence :end2 start)
              (maphash (lambda (key value) (declare (ignore key))
                               (setf (aref result i) value) (incf i)) table)
              (replace result sequence :start2 end :start1 i)
              result)
            (let ((result (make-sequence (type-of sequence)
                                         (hash-table-count table)))
                  (i 0))
              (maphash (lambda (key value) (declare (ignore key))
                               (setf (aref result i) value) (incf i)) table)
              result)))))


(defun hashed-delete-duplicates (sequence &key (test (function eql))
                                 test-not
                                 (start 0) (end (length sequence))
                                 (key (function identity))
                                 (from-end nil))
  "Like DELETE-DUPLICATES but implemented using a HASH-TABLE."
  (hashed-remove-duplicates
   sequence :test test :test-not test-not :start start :end end
   :key key :from-end from-end))



(defgeneric replace-subseq (insert sequence start &optional end)

  ;; Note: we could add a filler parameter and not adjust the size
  ;;       when reduced, but this would make a different behavior than
  ;;       when increasing the size.  Would still be nice for some
  ;;       applications.  Perhaps also a truncatep parameter?

  ;; BUG:  we should use the fill-pointer before trying to adjust.

  (:documentation "

DO:             Destructively (if possible) replace the (subseq
                sequence start end) with the elements from INSERT.  If
                START and END don't specify a strict subseq of
                SEQUENCE then an error is signaled.

INSERT:         A sequence.

SEQUENCE:       A sequence.   If it is a list or an adjustable vector,
                or if the subseq is of same length as INSERT then it
                will be destructively modified and returned.
                Otherwise a fresh adjustable vector will be returned.

START:          Bounding index designator of SEQUENCE.

END:            Bounding index designator of SEQUENCE. The default for
                end is NIL, which means (LENGTH SEQUENCE).

RETURN:         Either the modified SEQUENCE, or a fresh sequence of
                the same type with the specified subseq replaced by
                elements from INSERT.

")

  (:method (insert (sequence null) start &optional end)
    (let ((end (or end 0)))
      (assert (<= 0 start end 0))
      (map 'list (function identity) insert)))

  (:method (insert (sequence list) start &optional end)
    (let* ((length (length sequence))
          (end    (or end length))
          (excise (- end start))
          (incise (length insert)))
      (assert (<= 0 start end length))
      (cond
        ((= incise excise)
         (replace sequence insert :start1 start))
        ((< incise excise)
         ;; reducing the length of sequence
         (let* ((guarded (cons nil sequence))
                (previous (nthcdr start guarded)))
           (map nil (lambda (item) (setf previous (cdr previous)
                                         (car previous) item))
                insert)
           (loop :repeat (- excise incise) :do (pop (cdr previous)))
           (cdr guarded)))
        (t
         ;; increasing the length of sequence
         (let* ((guarded  (cons nil sequence))
                (previous (nthcdr start guarded)))
           (loop :repeat (- incise excise) :do (push nil (cdr previous)))
           (replace (cdr previous) insert)
           (cdr guarded))))))

  (:method (insert (sequence vector) start &optional end)
    (let* ((length (length sequence))
           (end    (or end length))
           (excise (- end start))
           (incise (length insert)))
      (assert (<= 0 start end length))
      (cond
        ((= incise excise)
         (replace sequence insert :start1 start))
        ((not (adjustable-array-p sequence))
         (let* ((new-length (- (+ length incise) excise))
                (new-sequence (make-array new-length
                                          :element-type (array-element-type sequence)
                                          :adjustable t
                                          :fill-pointer new-length)))
           (replace new-sequence sequence :end2 start)
           (replace new-sequence insert :start1 start)
           (replace new-sequence sequence :start1 (+ start incise) :start2 end)))
        ((< incise excise)
         ;; reducing the length of sequence
         (replace sequence insert :start1 start)
         (replace sequence sequence :start1 (+ start incise) :start2 end)
         (if (array-has-fill-pointer-p sequence)
             (decf (fill-pointer sequence) excise)
             ;; adjust-array cannot add a fill-pointer to an array,
             ;; even adjustable, that doesn't have one already.
             (adjust-array sequence (- (+ length incise) excise)))
         sequence)
        (t
         ;; increasing the length of sequence
         (cond
           ((< (array-dimension sequence 0) (- (+ length incise) excise))
            (adjust-array sequence (- (+ length incise) excise)
                          :fill-pointer (not (not (array-has-fill-pointer-p sequence)))))
           ((array-has-fill-pointer-p sequence)
            (incf (fill-pointer sequence) (- incise excise))))
         (replace sequence sequence :start1 (+ end (- incise excise)) :start2 end)
         (replace sequence insert :start1 start))))))


(defgeneric group-by (sequence n)
  (:documentation "Returns a list of subsequences of SEQUENCE of length N,
whose concatenation is equal to SEQUENCE.")
  (:method ((sequence vector) n)
    (check-type n (integer 1))
    (loop
      :with length := (length sequence)
      :for i :from 0 :by n
      :while (< i length)
      :collect (subseq sequence i (min length (+ i n)))))
  (:method ((sequence list) n)
    (check-type n (integer 1))
    (loop
      :for sub  := sequence :then rest
      :for rest := (nthcdr n sub)
      :while sub
      :collect (ldiff sub rest))))


(defun parse-sequence-type (type)
  "Parses the type which is expected to be a sequence subtype
RETURN:  the base type (list or vector), the element-type and the length (or nil).
"
  (cond
    ((eq type 'list)   (values 'list   t nil))
    ((eq type 'vector) (values 'vector t nil))
    ((atom type)       (error "Unrecognized sequence type ~S" type))
    ((eq (first type) 'vector)
     ;; vector [{element-type | *} [{size | *}]
     (destructuring-bind (vector &optional element-type size) type
       (declare (ignore vector))
       (values 'vector
               (if (member element-type '(nil *))
                   t
                   element-type)
               (if (eq size '*)
                   nil
                   size))))
    ((eq (first type) 'array)
     ;; array [{element-type | *} [dimension-spec]]
     ;; dimension-spec::= rank | * | ({dimension | *}*)
     (destructuring-bind (array &optional element-type dimension-spec) type
       (declare (ignore array))
       (when (member dimension-spec '(nil * (*)) :test (function equal))
         (error "Not a sequence subtype: ~S" type))
       (values 'array
               (if (member element-type '(nil *))
                   t
                   element-type)
               (cond
                 ((eql 1 dimension-spec)  nil)
                 ((and (listp dimension-spec)
                       (= 1 (length dimension-spec))
                       (integerp (first dimension-spec)))
                  (first dimension-spec))
                 (t (error "Not a sequence subtype: ~S" type))))))
    (t                 (error "Unrecognized sequence type ~S" type))))


(defun concatenate-sequences (result-type sequence-of-sequences &key (adjustable nil) (fill-pointer nil))
  "
RESULT-TYPE:     Indicates the type of resulting sequence.
                 If LIST, then ADJUSTABLE and FILL-POINTER are ignored.

SEQUENCE-OF-SEQUENCES:
                 EACH element may be either a string-designator,
                 or a list containing a string-designator, and a start and end position
                 denoting a substring.

ADJUSTABLE:      Whether the result must be adjustable.

FILL-POINTER:    The result fill pointer.

RETURN:          A vector containing all the elements of the vectors
                 in sequence-of-vectors, in order.
"
  (cond
    ((eq result-type 'list)
     (let* ((result (cons nil nil))
            (tail result))
       (map nil (lambda (seq)
                  (map nil (lambda (item)
                             (setf (cdr tail) (cons item nil)
                                   tail (cdr tail)))
                    seq))
         sequence-of-sequences)
       (cdr result)))
    (t
     (multiple-value-bind (base-type element-type size) (parse-sequence-type result-type)
       (declare (ignore base-type))
       (let* ((lengths    (map 'list (function length) sequence-of-sequences))
              (total-size (reduce (function +) lengths))
              (result     (make-array (if (and size (< total-size size))
                                          size
                                          total-size)
                                      :element-type element-type
                                      :adjustable adjustable
                                      :fill-pointer (if fill-pointer t nil)))
              (start      0))
         (map nil (lambda (seq length)
                    (replace result seq :start1 start)
                    (incf start length))
           sequence-of-sequences lengths)
         (when (integerp fill-pointer)
           (setf (fill-pointer result) fill-pointer))
         result)))))


(defgeneric prefixp (prefix sequence &key start end test)
  (:documentation   "
PREFIX:  A sequence designator.
STRING:  A sequence designator.
START:   The start of the subsequence of SEQUENCE to consider. Default: 0.
END:     The end   of the subsequence of SEQUENCE to consider. Default: NIL.
TEST:    A function to compare the elements of the SEQUENCE.
RETURN:  Whether PREFIX is a prefix of the (subseq SEQUENCE START END).
"))



(defgeneric suffixp (suffix string &key start end test)
  (:documentation   "
SUFFIX:  A sequence designator.
STRING:  A sequence designator.
START:   The start of the subsequence of SEQUENCE to consider. Default: 0.
END:     The end   of the subsequence of SEQUENCE to consider. Default: NIL.
TEST:    A function to compare the elements of the SEQUENCE.
RETURN:  Whether SUFFIX is a suffix of the (subseq SEQUENCE START END).
"))



(defmethod prefixp ((prefix sequence) (sequence sequence) &key (start 0) (end nil) (test (function eql)))
  (let ((mis (mismatch  prefix sequence :start2 start :end2 end :test test)))
    (or (null mis) (<= (length prefix) mis))))


(defmethod suffixp ((suffix sequence) (sequence sequence) &key (start 0) (end nil) (test (function eql)))
  (zerop (or (mismatch  suffix  sequence :start2 start :end2 end :test test
                                         :from-end t)
             0)) )


(defun mapconcat (function sequence separator)
  (etypecase sequence
    (list
     (if sequence
         (let* ((items (mapcar (lambda (item)
                                 (let ((sitem (funcall function item)))
                                   (if (stringp sitem)
                                       sitem
                                       (princ-to-string sitem))))
                               sequence))
                (ssepa (if (stringp separator)
                           separator
                           (princ-to-string separator)))
                (size (+ (reduce (function +) items :key (function length))
                         (* (length ssepa) (1- (length items)))))
                (result (make-array size :element-type 'character))
                (start  0))
           (replace result  (first items) :start1 start)
           (incf start (length (first items)))
           (dolist (item (rest items))
             (replace result ssepa :start1 start) (incf start (length ssepa))
             (replace result item  :start1 start) (incf start (length item)))
           result)
         ""))
    (vector
     (if (plusp (length sequence))
         (let* ((items (map 'vector (lambda (item)
                                      (let ((sitem (funcall function item)))
                                        (if (stringp sitem)
                                            sitem
                                            (princ-to-string sitem))))
                            sequence))
                (ssepa (if (stringp separator)
                           separator
                           (princ-to-string separator)))
                (size (+ (reduce (function +) items :key (function length))
                         (* (length ssepa) (1- (length items)))))
                (result (make-array size :element-type 'character))
                (start  0))
           (replace result (aref items 0) :start1 start) (incf start (length (aref items 0)))
           (loop
              :for i :from 1 :below (length items)
              :do (replace result ssepa :start1 start) (incf start (length ssepa))
              (replace result (aref items i) :start1 start) (incf start (length (aref items i))))
           result)
         ""))))


;;;; THE END ;;;;
