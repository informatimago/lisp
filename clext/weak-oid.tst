;;;; -*- mode:lisp -*-
;;;;**************************************************************************
;;;;FILE:               weak-oid.tst
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Added these tests I'm specifically interested in for closer-weak.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-01 <PJB> Changed license from GPL3 to AGPL3.
;;;;    2006-06-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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

(progn
  (defparameter *chunk-size* 100)
  (defparameter *big-oid* 1000000000000)
  (defparameter *oid* 0)
  (defclass object ()
    ((oid :accessor oid :initarg :oid :initform (incf *oid*))))
  (defun weak-oid-fill-initially (table)
    (loop
       :repeat *chunk-size*
       :for o = (make-instance 'object)
       :collect (setf (gethash (oid o) table) o)))
  nil)
nil


(let ((ht (make-hash-table :test (function eql) :weak :value))
      (o (make-instance 'object :oid 4)))
  (setf (gethash 4 ht) o)
  (gc)
  (setf (gethash 4 ht) o)
  (hash-table-count ht))
1

(let ((ht (make-hash-table :test (function eql) :weak :value)))
  (let ((o (make-instance 'object :oid 4)))
    (setf (gethash 4 ht) o))
  (gc)
  (let ((o (make-instance 'object :oid 4)))
    (setf (gethash 4 ht) o)
    (hash-table-count ht)))
1


;;; Test that weak hash-tables work for OID->object maps.
(let ((ht))
  (setf ht (make-hash-table :test (function eql) :weak :value))
  (flet ((check (table keep)
           ;; (c2weak::dump-wht table)
           (maphash (lambda (k v) 
                      (assert (= k (oid v)))
                      #+clisp (assert (member v keep))
                      ;; others don't break the weak pointers early...
                      ) table)
           (mapcar (lambda (v) (assert (and (gethash (oid v) table)
                                       (eq v  (gethash (oid v) table))))) keep)
           (assert
            (#+clisp = 
             #-clisp <= ;; others don't break the weak pointers early...
             (length keep) (hash-table-count ht)))))
    (let ((keep (weak-oid-fill-initially ht))
          (one-in-two (let ((bit t))
                        (lambda (x)
                          (declare (ignore x))
                          (setf bit (not bit))))))
      (check ht keep)
      (gc)
      (check ht keep)
      (setf keep (delete-if one-in-two keep))
      (gc)
      (check ht keep)
      (let ((*oid* *big-oid*))
        (setf keep (nconc keep (weak-oid-fill-initially ht))))
      (check ht keep)
      (gc)
      (check ht keep)
      (setf keep (delete-if one-in-two keep))
      (gc)
      (check ht keep))
    (gc)
    (check ht '())
    nil))
nil


