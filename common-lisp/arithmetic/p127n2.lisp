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
;;;;    Copyright Pascal J. Bourguignon 1994 - 2012
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
    If not, see http://www.gnu.org/licenses/
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



(defvar *even-parity*
  (make-array 128 :element-type '(unsigned-byte 8)
              :initial-contents '(
                                  #x00 #x81 #x82 #x03 #x84 #x05 #x06 #x87 
                                  #x88 #x09 #x0a #x8b #x0c #x8d #x8e #x0f 
                                  #x90 #x11 #x12 #x93 #x14 #x95 #x96 #x17 
                                  #x18 #x99 #x9a #x1b #x9c #x1d #x1e #x9f 
                                  #xa0 #x21 #x22 #xa3 #x24 #xa5 #xa6 #x27 
                                  #x28 #xa9 #xaa #x2b #xac #x2d #x2e #xaf 
                                  #x30 #xb1 #xb2 #x33 #xb4 #x35 #x36 #xb7 
                                  #xb8 #x39 #x3a #xbb #x3c #xbd #xbe #x3f 
                                  #xc0 #x41 #x42 #xc3 #x44 #xc5 #xc6 #x47 
                                  #x48 #xc9 #xca #x4b #xcc #x4d #x4e #xcf 
                                  #x50 #xd1 #xd2 #x53 #xd4 #x55 #x56 #xd7 
                                  #xd8 #x59 #x5a #xdb #x5c #xdd #xde #x5f 
                                  #x60 #xe1 #xe2 #x63 #xe4 #x65 #x66 #xe7 
                                  #xe8 #x69 #x6a #xeb #x6c #xed #xee #x6f 
                                  #xf0 #x71 #x72 #xf3 #x74 #xf5 #xf6 #x77 
                                  #x78 #xf9 #xfa #x7b #xfc #x7d #x7e #xff
                                  ))
  "A vector with all legal even-parity bytes, in 7-bit order.")


(defvar *odd-parity*
  (make-array 128 :element-type '(unsigned-byte 8)
              :initial-contents '(
                                  #x80 #x01 #x02 #x83 #x04 #x85 #x86 #x07 
                                  #x08 #x89 #x8a #x0b #x8c #x0d #x0e #x8f 
                                  #x10 #x91 #x92 #x13 #x94 #x15 #x16 #x97 
                                  #x98 #x19 #x1a #x9b #x1c #x9d #x9e #x1f 
                                  #x20 #xa1 #xa2 #x23 #xa4 #x25 #x26 #xa7 
                                  #xa8 #x29 #x2a #xab #x2c #xad #xae #x2f 
                                  #xb0 #x31 #x32 #xb3 #x34 #xb5 #xb6 #x37 
                                  #x38 #xb9 #xba #x3b #xbc #x3d #x3e #xbf 
                                  #x40 #xc1 #xc2 #x43 #xc4 #x45 #x46 #xc7 
                                  #xc8 #x49 #x4a #xcb #x4c #xcd #xce #x4f 
                                  #xd0 #x51 #x52 #xd3 #x54 #xd5 #xd6 #x57 
                                  #x58 #xd9 #xda #x5b #xdc #x5d #x5e #xdf 
                                  #xe0 #x61 #x62 #xe3 #x64 #xe5 #xe6 #x67 
                                  #x68 #xe9 #xea #x6b #xec #x6d #x6e #xef 
                                  #x70 #xf1 #xf2 #x73 #xf4 #x75 #x76 #xf7 
                                  #xf8 #x79 #x7a #xfb #x7c #xfd #xfe #x7f
                                  ))
  "A vector with all legal odd-parity bytes, in 7-bit order.")


(defun even-parity (byte)
  "
RETURN: The BYTE with the parity bit set to even parity.
"
  (aref *even-parity* (logand byte #x7f)))


(defun odd-parity  (byte)
    "
RETURN: The BYTE with the parity bit set to odd parity.

"
    (aref *odd-parity*  (logand byte #x7f)))

(declaim (inline even-parity odd-parity))


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
