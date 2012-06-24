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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
  (:use "COMMON-LISP")
  (:export "HASHED-SET-REMOVE-DUPLICATES"
           "HASHED-REMOVE-DUPLICATES" "HASHED-DELETE-DUPLICATES"
           "DUPLICATES"
           "REPLACE-SUBSEQ")
  (:documentation
   "

This package exports sequence processing functions.
    

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE")


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
                end is NIL, whicg means (LENGTH SEQUENCE).

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



(defun test/replace-subseq ()
  (let ((*standard-output* (make-broadcast-stream)))
    (let ((str (make-array 10 :adjustable t :element-type 'character :initial-contents "abcdefghij")))
      (print str)
      (assert (string= "abc123ghij"   (print (replace-subseq "123" str 3 6))))
      (assert (string= "abcABCij"     (print (replace-subseq "ABC" str 3 8))))
      (assert (string= "abc12345ij"   (print (replace-subseq "12345" str 3 6))))
      (assert (string= "78912345ij"   (print (replace-subseq "789" str 0 3))))
      (assert (string= "7891234501"   (print (replace-subseq "01" str 8))))
      (assert (string= "78912301"     (print (replace-subseq "" str 6 8))))
      (assert (string= "7891230123"   (print (replace-subseq "23" str (length str)))))
      (assert (string= "567891230123" (print (replace-subseq "56" str 0 0))))
      (assert (string= "123"          (print (replace-subseq "123" str 0))))
      (assert (string= "hello"        (print (replace-subseq "hello" "" 0))))
      (assert (string= ""             (print (replace-subseq "" "hello" 0)))))
    (let ((str (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
      (print str)
      (assert (equal '(a b c 1 2 3 g h i j)     (print (replace-subseq '(1 2 3) str 3 6))))
      (assert (equal '(a b c A B C i j)         (print (replace-subseq '(a b c) str 3 8))))
      (assert (equal '(a b c 1 2 3 4 5 i j)     (print (replace-subseq '(1 2 3 4 5) str 3 6))))
      (assert (equal '(7 8 9 1 2 3 4 5 i j)     (print (replace-subseq #(7 8 9) str 0 3))))
      (assert (equal '(7 8 9 1 2 3 4 5 0 1)     (print (replace-subseq #(0 1) str 8))))
      (assert (equal '(7 8 9 1 2 3 0 1)         (print (replace-subseq "" str 6 8))))
      (assert (equal '(7 8 9 1 2 3 0 1 2 3)     (print (replace-subseq #(2 3) str (length str)))))
      (assert (equal '(5 6 7 8 9 1 2 3 0 1 2 3) (print (replace-subseq '(5 6) str 0 0))))
      (assert (equal '(1 2 3)                   (print (replace-subseq '(1 2 3) str 0))))
      (assert (equal (coerce "hello" 'list)     (print (replace-subseq "hello" '() 0))))
      (assert (equal '()                        (print (replace-subseq "" '(1 2 3) 0))))))
  (assert (nth-value 1 (ignore-errors (replace-subseq "abc" "def" -1 2))))
  (assert (nth-value 1 (ignore-errors (replace-subseq "abc" "def" 1 4))))
  (assert (nth-value 1 (ignore-errors (replace-subseq "abc" "def" 2 1))))
  (assert (nth-value 1 (ignore-errors (replace-subseq "abc" "def" -2 4))))
  :success)


(test/replace-subseq)

;;;; THE END ;;;;
