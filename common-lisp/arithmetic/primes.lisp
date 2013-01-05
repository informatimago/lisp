;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               primes.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;    
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-18 <PJB> Corrected the limit on COMPUTE-PRIMES-TO.
;;;;    2003-12-04 <PJB> Added a FACTORIZE function that returns a SEXP.
;;;;    2003-12-03 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "STR-DECODE" "STR-ENCODE" "PRINT-FACTORIZATION" "FACTORIZE"
           "COMPUTE-PRIMES-TO")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WHILE")
  (:documentation
   "

Compute primes and factorize numbers.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES")


(defun compute-primes-to (n)
  "
DO:     Compute an Eratostene sieve to find all prime numbers up to N.
RETURN: An array of prime numbers.
"
  (cond
    ((< n 2) #())
    ((= n 2) #(2))
    ((= n 3) #(2 3))
    (t
     (let (bits-max bits bit (prime-count 2) (cur-prime 3) primes)
       ;; (SETQ N (+ N (IF (ODDP N) 3 2)))
       (setq n (- n (if (oddp n) 3 2))) ; base of bits array is 3.
       (setq bits-max (/ n 2))
       ;; set the bitset to full bits;
       (setq bits (make-array (list bits-max)
                              :initial-element 1 :element-type 'bit))
       (while (< cur-prime n)
         (setq bit (+ cur-prime (/ (- cur-prime 3) 2)))
         (while (< bit bits-max)
           (setf (aref bits bit) 0)
           (incf bit cur-prime))
         (setq bit (1+ (/ (- cur-prime 3) 2)))
         ;; search next prime
         (setq bit (position 1 bits :start bit))
         (if bit
             (setq cur-prime (+ bit bit 3)
                   prime-count (1+ prime-count))
             (setq cur-prime n)))
       ;; gather the primes.
       (setq primes (make-array (list prime-count) :element-type 'integer))
       (let ((curnum 0))
         (setf (aref primes curnum) 2)
         (incf curnum)
         (setf (aref primes curnum) 3)
         (incf curnum)
         (setq cur-prime 3)
         (setq bit 0)
         (setq bit (position 1 bits :start (1+ bit)))
         (while bit
           (setq cur-prime (+ bit bit 3))
           (setf (aref primes curnum) cur-prime)
           (incf curnum)
           (setq bit (position 1 bits :start (1+ bit)))))
       primes))))


(defun factorize (n &optional (primes nil))
  "
N:        an INTEGER
PRIMES:   a VECTOR of prime factors sorted in increasing order.
RETURN:   a SEXP of the form: (* uncomensurate-factor
                                 [ prime | (EXPT prime exponent) ]… [ -1 ] )
"
  (setf primes (or primes (compute-primes-to (1+ (isqrt n)))))
  (let ((factors '())
        (prime-idx 0) )
    (unless (integerp n)
      (error "I can only decompose integer values."))
    (when (< n 0)
      (push -1 factors)
      (setq n (- n)))
    (while (and (< prime-idx (length primes)) (< 1 n))
      (let ((prime (elt primes prime-idx))
            (expo 0))
        (multiple-value-bind (q r) (truncate n prime)
          (while (= 0 r)
            (incf expo)
            (setq n q)
            (multiple-value-setq (q r) (truncate n prime))))
        (when (< 0 expo)
          (push (if (= 1 expo) prime (list 'expt prime expo)) factors)  ))
      (incf prime-idx))
    (when (< 1 n)
      (push n factors))
    (cons '* factors)))


(defun factorize-vector (n primes)
  "
N:        an INTEGER
PRIMES:   a VECTOR of prime factors sorted in increasing order.
RETURN:   a VECTOR of length (1+ (LENGTH PRIMES)), with the uncommensurate
          factor in the slot 0, and the exponents of the primes in the
          following slots. (PRIMES could have a 1 in the first slot!)
"
  (let ((last-prime (1- (length primes)))
        (exponents (make-array (list (1+ (length primes)))
                               :initial-element 0 :element-type 'integer))
        (cur-exponent 0)
        (cur-prime -1)
        prime)
    (if (< n 0)
        (setf (aref exponents 0) -1
              n (- n))
        (setf (aref exponents 0) 1))
    (setq prime (aref primes (incf cur-prime)))
    (while (and (< cur-prime last-prime) (< 1 n))
      (let ((expo 0))
        (multiple-value-bind (q r) (truncate n prime)
          (while (= 0 r)
            (incf expo)
            (setq n q)
            (multiple-value-setq (q r) (truncate n prime))))
        (setf (aref exponents (incf cur-exponent)) expo)
        (setq prime (aref primes (incf cur-prime)))))
    (setf (aref exponents 0) (* (aref exponents 0) n))
    exponents))

;; (defparameter *primes* (compute-primes-to 1000))
;; (factorize -4004 *primes*)


(defun print-factorization (exponents primes)
  "
EXPONENTS:  A sequence: ( uncommensurate-factor  exponents… )
PRIMES:     A sequence: ( prime-factors… )
PRE:        (= (LENGTH EXPONENTS) (1+ (LENGTH PRIMES)))
DO:         Prints on *STANDARD-OUTPUT* an expression of the number.
"
  (map nil (lambda (p e) (unless (zerop e) (format t "~12D ^ ~D *~%" p e)))
       primes (subseq exponents 1))
  (format t "~12A   ~D~%" "" (elt exponents 0)))


(defun str-encode (str primes)
  "
RETURN:  An integer encoding the string STR factorized with the PRIMES.
"
  (apply (function *)
         (map 'list (lambda (ch prime) (expt prime (char-code ch))) str primes)))


(defun str-decode (num primes)
  "
RETURN:  A string decoding the integer NUM factorized with the PRIMES.
"
  (with-output-to-string (*standard-output*)
    (map nil (lambda (p e)
               (declare (ignore p))
               (unless (zerop e) (format t "~C" (code-char e))))
         primes (subseq (factorize-vector num primes) 1))))


;;;; THE END ;;;;
