;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               memory.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports a memory abstract class 
;;;;    and a concrete subclass implemented as a lisp array of unsigned bytes.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-12-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY"
  (:use "COMMON-LISP")
  (:export "MEMORY-EPILOG" "MEMORY-PROLOG" "DUMP" "VALID-ADDRESS-P"
           "WITH-MEMORY" "POKE-UINT64" "POKE-UINT32" "POKE-UINT16" "POKE-UINT8"
           "PEEK-UINT64" "PEEK-UINT32" "PEEK-UINT16" "PEEK-UINT8" "SIZE" "BASE"
           "MEMORY-VECTOR-64" "MEMORY")
  (:documentation
   "

This packages exports a memory abstract class  and a concrete subclass
implemented as a lisp array of unsigned bytes.


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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MEMORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric base (memory)
  (:documentation "Minimum value for an address in the given memory."))
(defgeneric size (memory)
  (:documentation "Number of bytes this memory holds."))

(defclass memory ()
  ((base :reader base :initarg :base :type (integer 0)
         :documentation "Minimum value for an address.")
   (size :reader size :initarg :size :type (integer 0)
         :documentation "Number of bytes this memory holds."))
  (:documentation "An abstract memory."))


(defmethod initialize-instance :before ((self memory) &key (base 0) (size nil))
  (assert (typep size '(integer 1))
          (size)
          "SIZE must be specified as a positive integer, not ~S" size)
  (assert (typep base '(integer 0))
          (base)
          "BASE must be specified as a positive integer, not ~S" base)
  self)


(defgeneric peek-uint8  (memory address)
  (:documentation "RETURN: The 8-bit byte at the given ADDRESS of the MEMORY."))
(defgeneric peek-uint16 (memory address)
  (:documentation "RETURN: The 16-bit byte at the given ADDRESS of the MEMORY."))
(defgeneric peek-uint32 (memory address)
  (:documentation "RETURN: The 32-bit byte at the given ADDRESS of the MEMORY."))
(defgeneric peek-uint64 (memory address)
  (:documentation "RETURN: The 64-bit byte at the given ADDRESS of the MEMORY."))
(defgeneric poke-uint8  (memory address value)
  (:documentation "DO: Store the 8-bit VALUE into the given ADDRESS of the MEMORY."))
(defgeneric poke-uint16 (memory address value)
  (:documentation "DO: Store the 16-bit VALUE into the given ADDRESS of the MEMORY."))
(defgeneric poke-uint32 (memory address value)
  (:documentation "DO: Store the 32-bit VALUE into the given ADDRESS of the MEMORY."))
(defgeneric poke-uint64 (memory address value)
  (:documentation "DO: Store the 64-bit VALUE into the given ADDRESS of the MEMORY."))
(defgeneric valid-address-p (memory address)
  (:documentation "RETURN: Whether ADDRESS is a valid address of the MEMORY."))
(defgeneric memory-prolog (memory)
  (:documentation "DO:  Prepare access to the memory (eg. acquire any needed lock)."))
(defgeneric memory-epilog (memory)
  (:documentation "DO:  Finalize access to the memory (eg. relinquish any lock)."))
(defgeneric dump (memory address length &key byte-size stream margin)
  (:documentation "Print on the STREAM the contents of the MEMORY from
the ADDRESS for LENGTH bytes of bit size BYTE-SIZE."))


(defmacro with-memory (memory &body body)
  "
Protects access to the memory, giving the memory object a chance to
set signal handler, or to acquire locks, and then release them.
"
  (let ((vmemory (gensym)))
    `(let ((,vmemory ,memory))
       (memory-prolog ,vmemory)
       (unwind-protect (progn ,@body)
         (memory-epilog ,vmemory)))))


(defmethod valid-address-p ((self memory) address)
  (<= (base self) address (+ (base self) (size self) -1)))


(defmethod dump ((self memory) address length 
                 &key (byte-size 1) (stream *standard-output*) (margin ""))
  (let ((peek (case byte-size
                ((1) (function peek-uint8))
                ((2) (function peek-uint16))
                ((4) (function peek-uint32))
                ((8) (function peek-uint64))
                (otherwise (error "BYTE-SIZE must be either 1, 2, 4 or 8.")))))
    (do ((address address (+ byte-size address))
         (i       0       (+ byte-size i)))
        ((>= i length) (format stream "~&") (values))
      (when (zerop (mod i 16))
        (format stream "~&~A~8,'0X: " margin address))
      (format stream "~V,'0X " (* 2 byte-size) (funcall peek self address)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MEMORY-VECTOR-64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass memory-vector-64 (memory)
  ((bytes :accessor bytes :type (vector (unsigned-byte 64))))
  (:documentation "A 64-bit memory."))


(defmethod initialize-instance :after ((self memory-vector-64)
                                       &key (base 0) (size nil))
  (assert (and (typep size '(integer 8)) (zerop (mod size 8)))
          (size)
          "SIZE must congruent to 0 modulo 8 and must be greater than 8")
  (assert (zerop (mod base 8))
          (base)
          "BASE must be congruent to 0 modulo 8, not ~S" base)
  (setf (bytes self) (make-array (list (truncate size 8))
                                 :element-type '(unsigned-byte 64)
                                 :initial-element 0))
  self)
                                 

(defmethod memory-prolog ((self memory-vector-64)) (declare (ignorable self)) (values))
(defmethod memory-epilog ((self memory-vector-64)) (declare (ignorable self)) (values))


(defmethod peek-uint8  ((self memory-vector-64) address)
  (decf address (base self))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (ldb (byte 8 (* low 8)) (aref (bytes self) high))))


(defmethod peek-uint16 ((self memory-vector-64) address)
  (decf address (base self))
  (unless (zerop (mod address 2))
    (error "Misaligned 16-bit address ~8,'0X" address))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (ldb (byte 16 (* low 4)) (aref (bytes self) high))))


(defmethod peek-uint32 ((self memory-vector-64) address)
  (decf address (base self))
  (unless (zerop (mod address 4))
    (error "Misaligned 32-bit address ~8,'0X" address))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (ldb (byte 32 (* low 2)) (aref (bytes self) high))))


(defmethod peek-uint64 ((self memory-vector-64) address)
  (decf address (base self))
  (unless (zerop (mod address 8))
    (error "Misaligned 64-bit address ~8,'0X" address))
  (aref (bytes self) (truncate address 8)))


(defmethod poke-uint8  ((self memory-vector-64) address value)
  (assert (typep value '(integer 0 #.(1- (expt 2 8)))))
  (decf address (base self))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (setf (aref (bytes self) high)
          (dpb value (byte 8 (* low 8))
               (aref (bytes self) high)))))


(defmethod poke-uint16 ((self memory-vector-64) address value)
  (assert (typep value '(integer 0 #.(1- (expt 2 16)))))
  (decf address (base self))
  (unless (zerop (mod address 2))
    (error "Misaligned 16-bit address ~8,'0X" address))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (setf (aref (bytes self) high)
          (dpb value (byte 16 (* low 4)) 
               (aref (bytes self) high)))))


(defmethod poke-uint32 ((self memory-vector-64) address value)
  (assert (typep value '(integer 0 #.(1- (expt 2 32)))))
  (decf address (base self))
  (unless (zerop (mod address 4))
    (error "Misaligned 32-bit address ~8,'0X" address))
  (let ((high (truncate address 8))
        (low  (mod address 8)))
    (setf (aref (bytes self) high)
          (dpb value (byte 32 (* low 2)) 
               (aref (bytes self) high)))))


(defmethod poke-uint64 ((self memory-vector-64) address value)
  (assert (typep value '(integer 0 #.(1- (expt 2 64)))))
  (decf address (base self))
  (unless (zerop (mod address 8))
    (error "Misaligned 64-bit address ~8,'0X" address))
  (setf (aref (bytes self) (truncate address 8)) value))


;;;; memory.lisp                      --                     --          ;;;;
