;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dictionary.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a DICTIONARY API over HASH-TABLE, P-LIST, A-LIST
;;;;    and an ADAPTATIVE-DICTIONARY class that automatically switch
;;;;    between HASH-TABLE and A-LIST depending on the number of entries.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-08-16 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY"
  (:use "COMMON-LISP")
  (:export "DICTIONARY" 
           "MAKE-DICTIONARY" "A-LIST" "P-LIST" "ADAPTATING-DICTIONARY" ; "HASH-TABLE"
           "DICTIONARY-SET" "DICTIONARY-GET" "DICTIONARY-DELETE"
           "DICTIONARY-MAP" "DICTIONARY-COUNT"
           ;; low-level:
           "DICTIONARY-CLASS" "DICTIONARY-TEST" "DICTIONARY-DATA"
           "ADAPTATING-DICTIONARY-LIMIT"
           "TEST" "TEST-DICTIONARY")
  (:documentation "
    GPL

    Copyright Pascal J. Bourguignon 2010 - 2010

    Implements a DICTIONARY API over HASH-TABLE, P-LIST, A-LIST
    and an ADAPTATIVE-DICTIONARY class that automatically switch
    between HASH-TABLE and A-LIST depending on the number of entries.
"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY")


(defclass dictionary-class ()
  ((test :initarg :test
         :initform (function eql)
         :reader dictionary-test)))

(deftype dictionary () '(or hash-table dictionary-class))

(defgeneric make-dictionary   (type &key test size contents)
  (:documentation "
TYPE:     Name of a class implementing the dictionary protocol.
TEST:     Restricted to EQL, EQUAL or EQUALP when type is HASH-TABLE.
CONTENTS: A p-list containing the initial key value pairs.
"))

(defgeneric dictionary-set    (dictionary key value))
(defgeneric dictionary-get    (dictionary key &optional default))
(defgeneric dictionary-delete (dictionary key))
(defgeneric dictionary-map    (fun dictionary))
(defgeneric dictionary-count  (dictionary))

(defsetf dictionary-get (dictionary key &optional default) (new-value)
  (declare (ignore default))
  `(dictionary-set ,dictionary ,key ,new-value))


;; hash-table

(defmethod make-dictionary ((type (eql 'hash-table)) &key (test (function eql)) (size 8) (contents '()))
  (declare (ignore type))
  (let ((dictionary (make-hash-table :test test :size size)))
    (loop :for (key value) :on contents :by (function cddr) :do
       (dictionary-set dictionary key value))
    dictionary))

(defmethod dictionary-test   ((dictionary hash-table))
  (hash-table-test dictionary))

(defmethod dictionary-set    ((dictionary hash-table) key value)
  (setf (gethash key dictionary) value))

(defmethod dictionary-get    ((dictionary hash-table) key &optional default)
  (gethash key dictionary default))

(defmethod dictionary-delete ((dictionary hash-table) key)
  (remhash key dictionary))

(defmethod dictionary-map    (fun (dictionary hash-table))
  (let ((results '()))
    (maphash (lambda (key value) (push (funcall fun key value) results)) dictionary)
    (nreverse results)))

(defmethod dictionary-count  ((dictionary hash-table))
  (hash-table-count dictionary))



(defclass a-list (dictionary-class)
  ((data :initarg :data
         :initform '()
         :accessor dictionary-data)))


(defmethod make-dictionary ((type (eql 'a-list)) &key (test (function eql)) (size 8) (contents '()))
  (declare (ignore type size))
  (let ((dictionary (make-instance 'a-list :test test)))
    (loop :for (key value) :on contents :by (function cddr) :do
       (dictionary-set dictionary key value))
    dictionary))

(defmethod dictionary-set    ((dictionary a-list) key value)
  (let ((pair (assoc key (dictionary-data dictionary)
                     :test (dictionary-test dictionary))))
    (if pair
        (setf (cdr pair) value)
        (setf (dictionary-data dictionary) (acons key value (dictionary-data dictionary))))
    value))

(defmethod dictionary-get    ((dictionary a-list) key &optional default)
  (let ((pair (assoc key (dictionary-data dictionary)
                     :test (dictionary-test dictionary))))
    (if pair
        (values (cdr pair) t)
        (values default nil))))

(defmethod dictionary-delete ((dictionary a-list) key)
  (let ((pair (assoc key (dictionary-data dictionary)
                     :test (dictionary-test dictionary))))
    (setf (dictionary-data dictionary) (delete key (dictionary-data dictionary)
                                               :test (dictionary-test dictionary)
                                               :key (function car)))
    pair))

(defmethod dictionary-map    (fun (dictionary a-list))
  (mapcar (lambda (pair) (funcall fun (car pair) (cdr pair)))
          (dictionary-data dictionary)))

(defmethod dictionary-count  ((dictionary a-list))
  (length (dictionary-data dictionary)))





(defclass p-list (dictionary-class)
  ((data :initarg :data
         :initform '()
         :accessor dictionary-data)))

;; Note: these are not lisp p-list, which are restricted to symbol keys and therefore eql test.

(defmethod make-dictionary ((type (eql 'p-list)) &key (test (function eql)) (size 8) (contents '()))
  (declare (ignore type size))
   (make-instance 'p-list :test test :data (copy-list contents)))

(defmethod dictionary-set    ((dictionary p-list) key value)
  (loop
     :with test = (dictionary-test dictionary)
     :for cell :on (dictionary-data dictionary) :by (function cddr)
     :when (funcall test (first cell) key)
     :do (return-from dictionary-set (setf (second cell) value))
     :finally (setf (dictionary-data dictionary) (list* key value  (dictionary-data dictionary)))
              (return-from dictionary-set value)))

(defmethod dictionary-get    ((dictionary p-list) key &optional default)
    (loop
     :with test = (dictionary-test dictionary)
     :for cell :on (dictionary-data dictionary) :by (function cddr)
     :when (funcall test (first cell) key)
     :do (return-from dictionary-get (values (second cell) t))
     :finally (return-from dictionary-get (values default nil))))

(defmethod dictionary-delete ((dictionary p-list) key)
  (let ((test  (dictionary-test dictionary))
        (data  (dictionary-data dictionary)))
    (when data
      (when (funcall test (first data) key)
        (setf (dictionary-data dictionary) (cddr (dictionary-data dictionary)))
        (return-from dictionary-delete (cons (first data) (second data))))
      (loop
         :for cell :on  data :by (function cddr)
         :for k = (third cell)
         :while (cddr data)
         :when (funcall test key k)
         :do (let ((v (fourth cell)))
               (setf (cddr cell) (cddddr cell))
               (return-from dictionary-delete (cons k v))))
      nil)))

(defmethod dictionary-map    (fun (dictionary p-list))
  (loop
     :for (key value) :on (dictionary-data dictionary) :by (function cddr)
     :collect (funcall fun key value)))

(defmethod dictionary-count  ((dictionary p-list))
  (truncate (length (dictionary-data dictionary)) 2))




(defclass adaptating-dictionary (dictionary-class)
  ((dictionary :initarg :dictionary)
   (limit      :initarg :limit
               :initform 10
               :type (integer 0)
               :accessor adaptating-dictionary-limit)))


(defmethod adaptating-dictionary-adapt ((dictionary adaptating-dictionary))
  (flet ((copy-dictionary (dictionary type)
           (make-dictionary type
                            :test (dictionary-test dictionary)
                            :size (dictionary-count dictionary)
                            :contents (let ((contents '()))
                                        (dictionary-map (lambda (key value)
                                                          (push value contents)
                                                          (push key contents))
                                                        dictionary)
                                        contents))))
    (with-slots (dictionary limit)  dictionary
      (cond
        ((and (typep dictionary 'hash-table)
              (< (dictionary-count dictionary) limit))
         (setf dictionary (copy-dictionary dictionary 'a-list)))
        ((and (not (typep dictionary 'hash-table))
              (<= limit (dictionary-count dictionary)))
         (setf dictionary (copy-dictionary dictionary 'hash-table)))))))

(defmethod make-dictionary ((type (eql 'adaptating-dictionary)) &key (test (function eql)) (size 8) (contents '()))
  (declare (ignore type))
  ;; TODO: determine the limit automatically.
  (let ((limit 10))
    (make-instance 'adaptating-dictionary
        :limit limit
        :dictionary (make-dictionary (if (< size limit)
                                         'a-list
                                         'hash-table)
                                     :test test :size size :contents contents))))

(defmethod dictionary-set    ((dictionary adaptating-dictionary) key value)
  (prog1 (with-slots (dictionary) dictionary
           (dictionary-set dictionary key value))
    (adaptating-dictionary-adapt dictionary)))

(defmethod dictionary-get    ((dictionary adaptating-dictionary) key &optional default)
  (multiple-value-prog1 (with-slots (dictionary) dictionary
                          (dictionary-get dictionary key default))
    (adaptating-dictionary-adapt dictionary)))

(defmethod dictionary-delete ((dictionary adaptating-dictionary) key)
  (prog1 (with-slots (dictionary) dictionary
           (dictionary-delete dictionary key))
    (adaptating-dictionary-adapt dictionary)))

(defmethod dictionary-map    (fun (dictionary adaptating-dictionary))
  (with-slots (dictionary) dictionary
    (dictionary-map fun dictionary)))

(defmethod dictionary-count  ((dictionary adaptating-dictionary))
  (with-slots (dictionary) dictionary
    (dictionary-count dictionary)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-dictionary (type)
  (let ((d (make-dictionary type
                            :test (function equal)
                            :contents '(a 1
                                        b 2
                                        c 3))))
    (assert (= 3 (dictionary-count d)))
    (assert (equalp (list (dictionary-get d 'a 0)
                          (dictionary-get d 'b 0)
                          (dictionary-get d 'c 0)
                          (dictionary-get d 'd 0))
                    '(1 2 3 0)))

    (dictionary-set d 'a 11)
    (dictionary-set d 'd 4)
    (dictionary-set d 'c 33)
    (assert (= 4 (dictionary-count d)))
    (assert (equalp (list (dictionary-get d 'a 0)
                          (dictionary-get d 'b 0)
                          (dictionary-get d 'c 0)
                          (dictionary-get d 'd 0)
                          (dictionary-get d 'e 0))
                    '(11 2 33 4 0)))

    (setf (dictionary-get d 'a 0) 111)
    (setf (dictionary-get d 'd 0) 444)
    (setf (dictionary-get d 'c 0) 333)
    (assert (= 4 (dictionary-count d)))
    (assert (equalp (list (dictionary-get d 'a 0)
                          (dictionary-get d 'b 0)
                          (dictionary-get d 'c 0)
                          (dictionary-get d 'd 0)
                          (dictionary-get d 'e 0))
                    '(111 2 333 444 0)))

    (dictionary-delete d 'b)
    (assert (= 3 (dictionary-count d)))
    (assert (equalp (list (dictionary-get d 'a 0)
                          (dictionary-get d 'b 0)
                          (dictionary-get d 'c 0)
                          (dictionary-get d 'd 0)
                          (dictionary-get d 'e 0))
                    '(111 0 333 444 0)))

    (let ((res (dictionary-map (function cons) d)))
      (assert (and (subsetp res '((a . 111) (c . 333) (d . 444))
                            :test (function equalp))
                   (subsetp '((a . 111) (c . 333) (d . 444)) res
                            :test (function equalp)))))

    (dictionary-map (lambda (key value) (declare (ignore value)) (dictionary-delete d key)) d)
    (assert (= 0 (dictionary-count d)))

    (loop
       :for i :from 1 :to 100
       :do (dictionary-set d i (* 1000 i)))
    (assert (= 100 (dictionary-count d)))
    (assert (loop
               :for i :from 1 :to 100
               :always (= (dictionary-get d i 0) (* 1000 i))))
    (loop
       :for i :from 1 :to 96
       :do (dictionary-delete d i))
    (assert (= 4 (dictionary-count d)))
    (assert (loop
               :for i :from 1 :to 96
               :always (null (nth-value 1 (dictionary-get d i 0)))))
    (assert (loop
               :for i :from 97 :to 100
               :always (= (dictionary-get d i 0) (* 1000 i))))
    
    :success))


(defun test ()
  "Tests all the kinds of dictionary defined in this package."
  (print (mapcar (function test-dictionary)
                 '(hash-table p-list a-list adaptating-dictionary))))


;;;; THE END ;;;;
