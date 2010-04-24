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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.MEMORY"
  (:USE "COMMON-LISP")
  (:EXPORT "MEMORY-EPILOG" "MEMORY-PROLOG" "DUMP" "VALID-ADDRESS-P"
           "WITH-MEMORY" "POKE-UINT64" "POKE-UINT32" "POKE-UINT16" "POKE-UINT8"
           "PEEK-UINT64" "PEEK-UINT32" "PEEK-UINT16" "PEEK-UINT8" "SIZE" "BASE"
           "MEMORY-VECTOR-64" "MEMORY")
  (:DOCUMENTATION
   "
      This packages exports a memory abstract class 
      and a concrete subclass implemented as a lisp array of unsigned bytes.

      Copyright Pascal J. Bourguignon 2004 - 2004
      
      This program is free software  you can redistribute it and/or
      modify it under the terms of the GNU General Public License
      as published by the Free Software Foundation  either version
      2 of the License, or (at your option) any later version.
      "))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.MEMORY")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MEMORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(defgeneric peek-uint8  (memory address))
(defgeneric peek-uint16 (memory address))
(defgeneric peek-uint32 (memory address))
(defgeneric peek-uint64 (memory address))
(defgeneric poke-uint8  (memory address value))
(defgeneric poke-uint16 (memory address value))
(defgeneric poke-uint32 (memory address value))
(defgeneric poke-uint64 (memory address value))
(defgeneric valid-address-p (memory address))
(defgeneric memory-prolog (memory))
(defgeneric memory-epilog (memory))
(defgeneric dump (memory address length &key byte-size stream margin))


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
                                 

(defmethod memory-prolog ((self memory-vector-64)))
(defmethod memory-epilog ((self memory-vector-64)))


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
