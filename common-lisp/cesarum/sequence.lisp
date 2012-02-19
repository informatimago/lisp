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
;;;;    2012-02-19 <PJB> Extracted from list.lisp and some other code.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
  (:use "COMMON-LISP")
  (:export "HASHED-SET-REMOVE-DUPLICATES"
           "HASHED-REMOVE-DUPLICATES" "HASHED-DELETE-DUPLICATES"
           "DUPLICATES")
  (:documentation
   "This package exports sequence processing functions.
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    This package is provided under the GNU General Public License.
    See the source file for details."))
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


;;;; THE END ;;;;
