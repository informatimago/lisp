;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               p127n2.lisp
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
;;;;    2012-02-04 <PJB> Converted from C++ code.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 1994 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.P127N2"
  (:use "COMMON-LISP")
  (:export "POLY" "POLY-PRIN1-TO-STRING" "POLY-FROM-BYTES" "POLY-TO-BYTES"
           "REMOVE-BIT7"
           "INSERT-BIT7"
           "ADD32"
           "MULTIPLY32"
           "DIVIDE32"
           "REMAINDER32"
           "EVEN-PARITY" "ODD-PARITY")
  (:documentation "

This module implements routines to compute modulo-2 polynomials
in P127[N/2]. (Ensemble de polynômes de degré inférieur ou égal à 127
dans l'ensemble quotient N/2 (ensemble des classes d'équivalences
modulo 2 dans ℕ)).


License:

    AGPL3

    Copyright Pascal J. Bourguignon 1994 - 2012

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.P127N2")


(deftype poly ()
  "A polynom of order 127.  We represent them as vectors of four 32-bit words."
  '(vector (unsigned-byte 32) 4))

;;      CARD32* bb parameters are pointers to an array of 4 CARD32:
;;
;;              CARD32  b[4];
;;
;;      b[0] contains the most significant coefficients of the polynomial,
;;      b[0]&(1<<31) gives the most significant coefficient of the polynomial.
;;      b[3] contains the least significant coefficients of the polynomial.
;;      b[3]&1 gives the least significant coefficient of the polynomial.
;;      With a big-endian architecture, this layout allow a direct
;;      interpretation of the 16-byte ECP blocks. Bytes would be re-ordered
;;      in a little-endian architecture.




(defun poly-from-bytes (bytes)
  "
BYTES:   A vector of at least 16 octets, in big endian order.
         Only the 16 first octets are used.
RETURN:  The poly stored in the bytes.
"
  (check-type bytes vector)
  (loop :for i :below 16 :do (check-type (aref bytes i) (unsigned-byte 8)))
  (let ((poly (make-array 4 :element-type '(unsigned-byte 32) :initial-element 0))
        (j -1))
    (dotimes (i 4 poly)
      (setf (aref poly i)
            (dpb (aref bytes (incf j)) (byte 8 24)
                 (dpb (aref bytes (incf j)) (byte 8 16)
                      (dpb (aref bytes (incf j)) (byte 8 8)
                           (dpb (aref bytes (incf j)) (byte 8 0) 0))))))))

(assert (equalp (poly-from-bytes #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
                #(#x01020304 #x05060708 #x090a0b0c #x0d0e0f10)))



(defun poly-to-bytes (poly)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
RETURN:  A vector of 16 octets in big endian order encoding the polynom.
"
  (check-type poly poly)
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
        (i -1))
    (dotimes (j 4 bytes)
      (setf (aref bytes (incf i)) (ldb (byte 8 24) (aref poly j))
            (aref bytes (incf i)) (ldb (byte 8 16) (aref poly j))
            (aref bytes (incf i)) (ldb (byte 8  8) (aref poly j))
            (aref bytes (incf i)) (ldb (byte 8  0) (aref poly j))))))

(assert (equalp (poly-to-bytes (poly-from-bytes #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
                #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))


(defun poly-prin1-to-string (poly)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
RETURN:  A string containing a human readable representation of the polynom POLY.
         (it is of the form \"x^M + … + x^m\").
"
  (with-output-to-string (*standard-output*)
    (let ((e (1- (* 4 32)))
          (first t))
      (loop
         :for i :below 4
         :do (let ((bit (ash 1 31))
                   (b (aref poly i)))
               (if (zerop b)
                   (decf e 32)
                   (loop :while (plusp bit) :do
                      (when (plusp (logand bit b))
                        (format t "~:[ + ~;~]x^~A" first e)
                        (setf first nil))
                      (setf bit (ash bit -1))
                      (decf e))))))))

(assert (equalp (poly-prin1-to-string  (poly-from-bytes #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
                "x^120 + x^113 + x^105 + x^104 + x^98 + x^90 + x^88 + x^82 + x^81 + x^74 + x^73 + x^72 + x^67 + x^59 + x^56 + x^51 + x^49 + x^43 + x^41 + x^40 + x^35 + x^34 + x^27 + x^26 + x^24 + x^19 + x^18 + x^17 + x^11 + x^10 + x^9 + x^8 + x^4"))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parity (byte predicate)
    (let* ((byte (logand #x7f byte))
           (sum  (loop
                   :for i :below 7
                   :sum (ldb (byte 1 i) byte))))
      (+ byte (if (funcall predicate sum)
                  #x00
                  #x80))))
  (defun parity-vector (predicate)
    (loop
      :with bytes := (make-array 128 :element-type '(unsigned-byte 8))
      :for b :below 128
      :for p := (parity b predicate)
      :do (setf (aref bytes b) p)
      :finally (return bytes))))

(defparameter *even-parity* (parity-vector (function evenp))
  "A vector with all legal even-parity bytes, in 7-bit order.")

(defparameter *odd-parity*  (parity-vector (function oddp))
  "A vector with all legal odd-parity bytes, in 7-bit order.")

(declaim (inline even-parity odd-parity))

(defun even-parity (byte)
  "
RETURN: The BYTE with the parity bit set to even parity.
"
  (aref *even-parity* (logand byte #x7f)))

(defun odd-parity  (byte)
    "
RETURN: The BYTE with the parity bit set to odd parity.
"
    (aref *odd-parity* (logand byte #x7f)))


(defun remove-bit7 (poly)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
DO:      Modifies POLY, removing the 7th bit.
RETURN:  POLY
"
  (check-type poly poly)
  (let (bit0 bit1 bit2 bit3)
    (setf bit0 (aref poly 0)
          (aref poly 0) (logand (ash (aref poly 0) -1) #xffffffff)
          bit1 (aref poly 1)
          (aref poly 1) (logand (logior (ash bit1 -1) (ash (logand bit0 1) 31)) #xffffffff)
          bit2 (aref poly 2)
          (aref poly 2) (logand (logior (ash bit2 -1) (ash (logand bit1 1) 31)) #xffffffff)
          bit3 (aref poly 3)
          (aref poly 3) (logior (logand (logior (ash bit3 -1) (ash bit2 31)) #xffffff80)
                                (logand bit3 #x7f)))
    poly))


(defun insert-bit7 (poly)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
DO:      Inserts into the POLY a 7th bit that is the even parity of the lowest 7 bits.
RETURN:  POLY
"
  (check-type poly poly)
  (let (bit1 bit2 bit3)
    (setf bit3 (aref poly 3)
          (aref poly 3) (logand (logior (logand (ash bit3 1) #xffffff00)
                                        (even-parity bit3))  #xffffffff)
          bit2 (aref poly 2)
          (aref poly 2) (logand (logior (ash bit2 1)          (ash bit3 -31)) #xffffffff)
          bit1 (aref poly 1)
          (aref poly 1) (logand (logior (ash bit1 1)          (ash bit2 -31)) #xffffffff)
          (aref poly 0) (logand (logior (ash (aref poly 0) 1) (ash bit1 -31)) #xffffffff))
    poly))


(defun add32 (poly gg)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
GG:      A polynom of degree 32 modulo N/2 (type (unsigned-byte 32)).
DO:      Adds the polynom GG to POLY.
RETURN:  The modified POLY.
"
  (check-type poly poly)
  (check-type gg (unsigned-byte 32))
  (setf (aref poly 3) (logxor (aref poly 3) gg))
  poly)


(defun multiply32 (poly gg)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
GG:      A polynom of degree 32 modulo N/2 (type (unsigned-byte 32)).
DO:      Multiplies the polynom POLY by GG.
RETURN:  The modified POLY.
"
  (check-type poly poly)
  (check-type gg (unsigned-byte 32))
  (let ((tt0 (aref poly 0))
        (tt1 (aref poly 1))
        (tt2 (aref poly 2))
        (tt3 (aref poly 3))
        (p0 0)
        (p1 0)
        (p2 0)
        (p3 0))
    (loop
       :while (plusp gg)
       :do (when (plusp (logand gg 1))
             (setf p0 (logxor p0 tt0)
                   p1 (logxor p1 tt1)
                   p2 (logxor p2 tt2)
                   p3 (logxor p3 tt3))
             (setf gg (ash gg -1)
                   tt0 (logand (logior (ash tt0 1) (ash tt1 -31)) #xffffffff)
                   tt1 (logand (logior (ash tt1 1) (ash tt2 -31)) #xffffffff)
                   tt2 (logand (logior (ash tt2 1) (ash tt3 -31)) #xffffffff)
                   tt3 (logand (ash tt3 1) #xffffffff))))
    (setf (aref poly 0) p0
          (aref poly 1) p1
          (aref poly 2) p2
          (aref poly 3) p3)
    poly))


(defun %division (poly gg)
  (check-type poly poly)
  (check-type gg (unsigned-byte 32))
  (check-type gg (integer 1))
  (let ((tt0 (aref poly 0))
        (tt1 (aref poly 1))
        (tt2 (aref poly 2))
        (tt3 (aref poly 3))
        (p0 0)
        (p1 0)
        (p2 0)
        (p3 0)
        (n 0)
        (hh1 #x80000000))
    (loop :while (zerop (logand hh1 gg)) :do (setf hh1 (ash hh1 -1)) (incf n))
    (setf gg (logand (ash gg n) #xffffffff))
    (let ((gg0 gg)
          (gg1 0)
          (hh0 #x80000000))
      (loop :while (plusp gg0) :do
         (when (plusp (logand hh0 tt0))
           (setf tt0 (logxor tt0 gg0)
                 tt1 (logxor tt1 gg1)
                 p0  (logxor p0  hh0)))
         (setf gg1 (logand (logior (ash gg1 -1) (ash gg0 31)) #xffffffff)
               gg0 (ash gg0 -1)
               hh0 (ash hh0 -1))))
    (let ((gg0 gg)
          (gg1 0)
          (hh0 #x80000000))
      (loop :while (plusp gg0) :do
         (when (plusp (logand hh0 tt1))
           (setf tt1 (logxor tt1 gg0)
                 tt2 (logxor tt2 gg1)
                 p1  (logxor p1  hh0)))
         (setf gg1 (logand (logior (ash gg1 -1) (ash gg0 31)) #xffffffff)
               gg0 (ash gg0 -1)
               hh0 (ash hh0 -1))))
    (let ((gg0 gg)
          (gg1 0)
          (hh0 #x80000000))
      (loop :while (plusp gg0) :do
         (when (plusp (logand hh0 tt2))
           (setf tt2 (logxor tt2 gg0)
                 tt3 (logxor tt3 gg1)
                 p2  (logxor p2  hh0)))
         (setf gg1 (logand (logior (ash gg1 -1) (ash gg0 31)) #xffffffff)
               gg0 (ash gg0 -1)
               hh0 (ash hh0 -1))))
    (let ((gg0 gg)
          (hh0 #x80000000))
      (loop :while (<= hh1 hh0) :do
         (when (plusp (logand hh0 tt3))
           (setf tt3 (logxor tt3 gg0)
                 p3  (logxor p3  hh0)))
         (setf gg0 (ash gg0 -1)
               hh0 (ash hh0 -1))))
    (values tt3 (1+ n) p0 p1 p2 p3)))


(defun divide32 (poly gg)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
GG:      A polynom of degree 32 modulo N/2 (type (unsigned-byte 32)).
DO:      Divides the polynom POLY by GG.
RETURN:  POLY; the remainder (unsigned-byte 32)..
"
  (multiple-value-bind (tt3 n p0 p1 p2 p3) (%division poly gg)
    ;; Renormalize the quotient
    (setf (aref poly 3) (logand (logior (ash p3 (- n 32)) (ash p2 n)) #xffffffff)
          (aref poly 2) (logand (logior (ash p2 (- n 32)) (ash p1 n)) #xffffffff)
          (aref poly 1) (logand (logior (ash p1 (- n 32)) (ash p0 n)) #xffffffff)
          (aref poly 0) (logand (ash p0 (- n 32)) #xffffffff))
    (values poly tt3)))


(defun remainder32 (poly gg)
  "
POLY:    A polynom of degree 127 modulo N/2 (type POLY).
GG:      A polynom of degree 32 modulo N/2 (type (unsigned-byte 32)).
RETURN:  the remainder (unsigned-byte 32) of POLY divided by GG.
NOTE:    Doesn't modify POLY.
"
  (values (%division poly gg)))


;;;; THE END ;;;;
