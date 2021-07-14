;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               algebra.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This packages exports some simple algebra functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-06-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.ALGEBRA"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.PRIMES")
  (:export
   ;; Some arithmetic:
   "SQUARE" "SQUARE-ROOT"
   "CUBE"   "CUBE-ROOT"
   "DIVIDES"
   ;; Some Algebra:
   "EQUA2"
   "CARDANO"
   "SOLVE-POLYNOMIAL-EQUATION")
  (:documentation "Some simple algebra functions.

   ;; 4x+1=0
   (solve-polynomial-equation #(1 4))
   --> (-1/4)

   ;; 2x²+4x+1=0
   (solve-polynomial-equation #(1 4 2))
   --> (-0.29289323 -1.7071068) ;
       ((/ (+ -4 (* 2 (sqrt 2))) 4)
        (/ (- -4 (* 2 (sqrt 2))) 4))

   ;; 3x³+2x²+4x+1=0
   (solve-polynomial-equation #(1 4 2 3))
   --> (#C(0.70890605 0.56626135)
        #C(-0.19738963 -0.043011278)
        #C(-1.1781831 -0.5232501))

   ;; x³-10x²+31x-30=0
   (solve-polynomial-equation #(-30 31 -10 1))
   --> (#C(5.0 0.0)
        #C(2.0 0.0)
        #C(3.0 -0.0))

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ARITHMETIC.ALGEBRA")

(defun ratiop (x) (typep x 'ratio))

(defun square (x) (* x x))
(defun cube   (x) (* x x x))

(defun divides (d n)
  (zerop (mod n d)))

(defun square-root (n)
  (if (zerop n)
      0
      (typecase n
        (ratio
         `(/ ,(square-root (numerator   n))
             ,(square-root (denominator n))))
        (integer
         (let* ((factors (factorize (abs n)))
                (root    (loop
                           with primes = 1
                           with squares = 1
                           for term in (rest factors)
                           do (cond
                                ((atom term)
                                 (setf primes (* primes term)))
                                ((evenp (third term))
                                 (setf squares (* squares (expt (second term) (/ (third term) 2)))))
                                (t
                                 (setf primes (* primes (second term))
                                       squares (* squares (expt (second term) (/ (- (third term) 1) 2))))))
                           finally (return (if (= 1 squares)
                                               (if (= 1 primes)
                                                   1
                                                   `(sqrt ,primes))
                                               (if (= 1 primes)
                                                   squares
                                                   `(* ,squares
                                                       (sqrt ,primes))))))))
           (if (minusp n)
               (if (eql 1 root)
                   'i
                   `(* i ,root))
               root)))
        (number (sqrt n))
        (t      `(sqrt n)))))

(defun cube-root (n)
  (typecase n
    (integer (let* ((factors (factorize (abs n)))
                    (root    (loop
                               with primes = 1
                               with cubes = 1
                               for term in (rest factors)
                               do (cond
                                    ((atom term)
                                     (setf primes (* primes term)))
                                    ((divides 3 (third term))
                                     (setf cubes (* cubes (expt (second term) (/ (third term) 3)))))
                                    (t
                                     (multiple-value-bind (q r) (truncate (third term) 3)
                                       (setf primes (* primes (expt (second term) r))
                                             cubes  (* cubes  (expt (second term) q))))))
                               finally (return (if (= 1 cubes)
                                                   (if (minusp n)
                                                       `(expt ,(- primes) 1/3)
                                                       `(expt ,primes 1/3))
                                                   `(* ,(if (minusp n)
                                                            (- cubes)
                                                            cubes)
                                                       (expt ,primes 1/3)))))))
               root))
    (number (expt n 1/3))
    (t      `(expt ,n 1/3))))


(defun equa2 (a b c)
  (let* ((delta               (- (* b b) (* 4 a c)))
         (symbolic-sqrt-delta (square-root delta)))
    (values (list (/ (+ (- b) (sqrt delta)) 2 a )
                  (/ (- (- b) (sqrt delta)) 2 a ))
            (list `(/ (+ ,(- b) ,symbolic-sqrt-delta) ,(* 2 a))
                  `(/ (- ,(- b) ,symbolic-sqrt-delta) ,(* 2 a))))))

(defun cardano (a b c d)
  "Solve a 3rd degree equation."
  (let* ((Q   (/ (- (* 3 a c) (* b b))
                 9 a a))
         (R   (/ (- (* 9 a b c) (* 27 a a d) (* 2 b b b))
                 54 a a a))
         (vqr (sqrt (+ (* q q q) (* r r))))
         (S   (expt (+ r vqr) 1/3))
         (U   (expt (- r vqr) 1/3))
         (x1  (- (+ s u) (/ b 3 a)))
         (x2  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3)  2) (- s u))))
         (x3  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3) -2) (- s u)))))
    (list x1 x2 x3)))

(defun symbolic-cardano (a b c d)
  "Solve a 3rd degree equation."
  (let* ((Q   (/ (- (* 3 a c) (* b b))
                 9 a a))
         (R   (/ (- (* 9 a b c) (* 27 a a d) (* 2 b b b))
                 54 a a a))
         (vqr (sqrt (+ (* q q q) (* r r))))
         (s-vqr (if (ratiop vqr)
                    vqr
                    (square-root (+ (* q q q) (* r r)))))
         (S   (expt (+ r vqr) 1/3))
         (U   (expt (- r vqr) 1/3))
         (s-S (if (ratiop S)
                  S
                  (if (and (ratiop r) (ratiop s-vqr))
                      (cube-root (+ r S))
                      `(expt (+ ,r ,s-vqr) 1/3))))
         (s-U (if (ratiop U)
                  U
                  (if (and (ratiop r) (ratiop s-vqr))
                      (cube-root (- r U))
                      `(expt (- ,r ,s-vqr) 1/3))))
         (x1  (- (+ s u) (/ b 3 a)))
         (x2  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3)  2) (- s u))))
         (x3  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3) -2) (- s u))))

         (s-x1  (if (and (ratiop s) (ratiop u))
                    x1
                    `(- (+ ,s-s ,s-u) ,(/ b 3 a))))
         (s-x2  (if (and (ratiop s) (ratiop u))
                    x2
                    `(+ (/ (+ ,s-s ,s-u) -2)
                        ,(/ b -3 a)
                        (* i (/ (sqrt 3) 2)
                           (- ,s-s ,s-u)))))
         (s-x3  (if (and (ratiop s) (ratiop u))
                    x3
                    `(+ (/ (+ ,s-s ,s-u) -2)
                        ,(/ b -3 a)
                        (* i (/ (sqrt 3) -2)
                           (- ,s-s ,s-u))))))
    (values (list x1 x2 x3)
            (list s-x1 s-x2 s-x3))))


(defun solve-polynomial-equation (coefficients)
  "
Solve a polynomial equation with the given coefficients.
         p
    0 =  Σ C_i x^i
        i=0
Currently supported only orders from 0 to 3.
"
  (let ((p (position 0 coefficients :from-end t :test-not '=)))
    (case p
      ((nil)                            ; 0 = 0
       'number)
      ((0)                              ; 0 = C₀ with C₀≠0
       '())
      ((1)                              ; 0 = C₁ x + C₀
       (list (- (/ (elt coefficients 0)
                   (elt coefficients 1)))))
      ((2)                              ; 0 = C₂ x² + C₁ x + C₀
       (equa2 (elt coefficients 2)
              (elt coefficients 1) (elt coefficients 0)))
      ((3)                             ; 0 = C₃ x³ + C₂ x² + C₁ x + C₀
       (cardano (elt coefficients 3) (elt coefficients 2)
                (elt coefficients 1) (elt coefficients 0)))
      (otherwise
       (error "Equations of degree greater than 3 are not supported (yet).")))))


;;;; THE END ;;;;
