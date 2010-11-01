;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               primes.lisp
;;;;LANGUAGE:           common-lisp
;;;;SYSTEM:             common-lisp
;;;;USER-INTERFACE:     common-lisp
;;;;DESCRIPTION
;;;;    
;;;;    Compute primes and factorize numbers.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-18 <PJB> Corrected the limit on COMPUTE-PRIMES-TO.
;;;;    2003-12-04 <PJB> Added a FACTORIZE function that returns a SEXP.
;;;;    2003-12-03 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:EXPORT "STR-DECODE" "STR-ENCODE" "PRINT-FACTORIZATION" "FACTORIZE"
           "COMPUTE-PRIMES-TO")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WHILE")
  (:DOCUMENTATION
   "Compute primes and factorize numbers.

    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES")


(DEFUN COMPUTE-PRIMES-TO (N)
  "
DO:     Compute an Eratostene cribble to find all prime numbers up to N.
RETURN: An array of prime numbers.
"
  (cond
    ((< n 2) #())
    ((= n 2) #(2))
    ((= n 3) #(2 3))
    (t
     (LET (BITS-MAX BITS BIT (PRIME-COUNT 2) (CUR-PRIME 3) PRIMES)
       ;; (SETQ N (+ N (IF (ODDP N) 3 2)))
       (SETQ N (- N (IF (ODDP N) 3 2))) ; base of bits array is 3.
       (SETQ BITS-MAX (/ N 2))
       ;; set the bitset to full bits;
       (SETQ BITS (MAKE-ARRAY (LIST BITS-MAX)
                              :INITIAL-ELEMENT 1 :ELEMENT-TYPE 'BIT))
       (WHILE (< CUR-PRIME N)
         (SETQ BIT (+ CUR-PRIME (/ (- CUR-PRIME 3) 2)))
         (WHILE (< BIT BITS-MAX)
           (SETF (AREF BITS BIT) 0)
           (INCF BIT CUR-PRIME))
         (SETQ BIT (1+ (/ (- CUR-PRIME 3) 2)))
         ;; search next prime
         (SETQ BIT (POSITION 1 BITS :START BIT))
         (IF BIT
             (SETQ CUR-PRIME (+ BIT BIT 3)
                   PRIME-COUNT (1+ PRIME-COUNT))
             (SETQ CUR-PRIME N)))
       ;; gather the primes.
       (SETQ PRIMES (MAKE-ARRAY (LIST PRIME-COUNT) :ELEMENT-TYPE 'INTEGER))
       (LET ((CURNUM 0))
         (SETF (AREF PRIMES CURNUM) 2)
         (INCF CURNUM)
         (SETF (AREF PRIMES CURNUM) 3)
         (INCF CURNUM)
         (SETQ CUR-PRIME 3)
         (SETQ BIT 0)
         (SETQ BIT (POSITION 1 BITS :START (1+ BIT)))
         (WHILE BIT
           (SETQ CUR-PRIME (+ BIT BIT 3))
           (SETF (AREF PRIMES CURNUM) CUR-PRIME)
           (INCF CURNUM)
           (SETQ BIT (POSITION 1 BITS :START (1+ BIT)))))
       PRIMES))))


(DEFUN FACTORIZE (N &optional (PRIMES nil))
  "
N:        an INTEGER
PRIMES:   a VECTOR of prime factors sorted in increasing order.
RETURN:   a SEXP of the form: (* uncomensurate-factor
                                 [ prime | (EXPT prime exponent) ]... [ -1 ] )
"
  (setf primes (or primes (compute-primes-to (truncate (1+ (isqrt n))))))
  (LET ((FACTORS '())
        (PRIME-IDX 0) )
    (UNLESS (INTEGERP N)
      (ERROR "I can only decompose integer values."))
    (WHEN (< N 0)
      (PUSH -1 FACTORS)
      (SETQ N (- N)))
    (WHILE (AND (< PRIME-IDX (LENGTH PRIMES)) (< 1 N))
      (LET ((PRIME (elt PRIMES PRIME-IDX))
            (EXPO 0))
        (MULTIPLE-VALUE-BIND (Q R) (TRUNCATE N PRIME)
          (WHILE (= 0 R)
            (INCF EXPO)
            (SETQ N Q)
            (MULTIPLE-VALUE-SETQ (Q R) (TRUNCATE N PRIME))))
        (WHEN (< 0 EXPO)
          (PUSH (IF (= 1 EXPO) PRIME (LIST 'EXPT PRIME EXPO)) FACTORS)  ))
      (INCF PRIME-IDX))
    (WHEN (< 1 N)
      (PUSH N FACTORS))
    (CONS '* FACTORS)))


(DEFUN FACTORIZE-VECTOR (N PRIMES)
  "
N:        an INTEGER
PRIMES:   a VECTOR of prime factors sorted in increasing order.
RETURN:   a VECTOR of length (1+ (LENGTH PRIMES)), with the uncommensurate
          factor in the slot 0, and the exponents of the primes in the
          following slots. (PRIMES could have a 1 in the first slot!)
"
  (LET ((LAST-PRIME (1- (LENGTH PRIMES)))
        (EXPONENTS (MAKE-ARRAY (LIST (1+ (LENGTH PRIMES)))
                               :INITIAL-ELEMENT 0 :ELEMENT-TYPE 'INTEGER))
        (CUR-EXPONENT 0)
        (CUR-PRIME -1)
        PRIME)
    (IF (< N 0)
        (SETF (AREF EXPONENTS 0) -1
              N (- N))
        (SETF (AREF EXPONENTS 0) 1))
    (SETQ PRIME (AREF PRIMES (INCF CUR-PRIME)))
    (WHILE (AND (< CUR-PRIME LAST-PRIME) (< 1 N))
      (LET ((EXPO 0))
        (MULTIPLE-VALUE-BIND (Q R) (TRUNCATE N PRIME)
          (WHILE (= 0 R)
            (INCF EXPO)
            (SETQ N Q)
            (MULTIPLE-VALUE-SETQ (Q R) (TRUNCATE N PRIME))))
        (SETF (AREF EXPONENTS (INCF CUR-EXPONENT)) EXPO)
        (SETQ PRIME (AREF PRIMES (INCF CUR-PRIME)))))
    (SETF (AREF EXPONENTS 0) (* (AREF EXPONENTS 0) N))
    EXPONENTS))

;; (defparameter *primes* (compute-primes-to 1000))
;; (factorize -4004 *primes*)


(DEFUN PRINT-FACTORIZATION (EXPONENTS PRIMES)
  "
EXPONENTS:  [ uncommensurate-factor  exponents... ]
PRIMES:     [ prime-factors ... ]
PRE:        (= (LENGTH EXPONENTS) (1+ (LENGTH PRIMES)))
DO:         Prints on *STANDARD-OUTPUT* an expression of the number.
"
  (MAP NIL (LAMBDA (P E) (UNLESS (ZEROP E) (FORMAT T "~12D ^ ~D *~%" P E)))
       PRIMES (SUBSEQ EXPONENTS 1))
  (FORMAT T "~12A   ~D~%" "" (AREF EXPONENTS 0)))


(DEFUN STR-ENCODE (STR PRIMES)
  "
RETURN:  An integer encoding the string STR factorized with the PRIMES.
"
  (APPLY (FUNCTION *)
         (MAP 'LIST (LAMBDA (CH PRIME) (EXPT PRIME (CHAR-CODE CH))) STR PRIMES)))


(DEFUN STR-DECODE (NUM PRIMES)
  "
RETURN:  A string decoding the integer NUM factorized with the PRIMES.
"
  (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
    (MAP NIL (LAMBDA (P E)
               (DECLARE (IGNORE P))
               (UNLESS (ZEROP E) (FORMAT T "~C" (CODE-CHAR E))))
         PRIMES (SUBSEQ (FACTORIZE-VECTOR NUM PRIMES) 1))))


;;;; THE END ;;;;
