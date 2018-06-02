;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mersenne.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements CL RANDOM API with a Mersenne's twister
;;;;    Pseudo Random Number Generator.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-01-30 <PJB> Translated from scheme created 2006-10-23.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.MERSENNE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "RANDOM-STATE" "RANDOM-STATE-P" "MAKE-RANDOM-STATE"
           "*RANDOM-STATE*" "RANDOM")
  (:export "RANDOM-STATE" "RANDOM-STATE-P" "MAKE-RANDOM-STATE"
           "*RANDOM-STATE*" "RANDOM")
  (:export "SRAND" "RAND")
  (:documentation "
Implements the Mersenne's twister Pseudo Random Number Generator.
(Not cryptographically secure).
AGPL3
Copyright Pascal J. Bourguignon 2016 - 2016
"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.MERSENNE")


;; The coefficients for MT19937 are:
;;
;;     (w, n, m, r) = (32, 624, 397, 31)
;;     a = 9908B0DF16
;;     (u, d) = (11, FFFFFFFF16)
;;     (s, b) = (7, 9D2C568016)
;;     (t, c) = (15, EFC6000016)
;;     l = 18
;;
;; Note that 32-bit implementations of the Mersenne Twister generally
;; have d = FFFFFFFF16. As a result, the d is occasionally omitted
;; from the algorithm description, since the bitwise and with d in
;; that case has no effect.
;;
;; The coefficients for MT19937-64 are:[43]
;;
;;     (w, n, m, r) = (64, 312, 156, 31)
;;     a = B5026F5AA96619E916
;;     (u, d) = (29, 555555555555555516)
;;     (s, b) = (17, 71D67FFFEDA6000016)
;;     (t, c) = (37, FFF7EEE00000000016)
;;     l = 43
;;
;; f:
;; (typep 1812433253 'fixnum)
;; (typep 6364136223846793005 'fixnum)
;;

(deftype word () '(unsigned-byte 32))

(defstruct (random-state
            (:constructor %make-random-state))
  (i  0)
  (mt (make-array 624 :initial-element 0 :element-type 'word)))

(defvar *random-state*)

(defun Makoto-Matsumoto (seed)
  ;; Mersenne Twistter seed function.
  (check-type seed word)
  (let ((mt (random-state-mt *random-state*)))
    (setf (aref mt 0) seed)
    (do ((i 1 (1+ i)))
        ((> i 623))
      (setf (aref  mt i) (ldb (byte 32 0) (+ 1 (* 69069 (aref mt (- i 1)))))))
    (setf (random-state-i *random-state*) 624)
    (values)))

(defun Takuji-Nishimura ()
  ;; Mersenne Twistter random generator function.
  (let ((mt (random-state-mt *random-state*))
        (i  (random-state-i  *random-state*)))
    (setf i (mod (1+ i) 624))
    (when (zerop i)
      (do ((i 0 (+ 1 i)))
          ((> i 622))
        (setf (aref mt i)
              (let ((y (ldb (byte 31 0) (aref mt (+ i 1)))))
                (the word
                     (if (evenp (ldb (byte 1 31) (aref mt i)))
                         (logxor (aref mt (mod (+ i 397) 624)) y)
                         (logxor (aref mt (mod (+ i 397) 624)) y 2567483615))))))
      (setf (aref mt 623)
            (let ((y (ldb (byte 31 0) (aref mt 0))))
              (the word (if (evenp (ldb (byte 1 31) (aref mt 623)))
                            (logxor (aref mt 396) y)
                            (logxor (aref mt 396) y 2567483615))))))
    (let ((y (aref mt i)))
      (declare (type word y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (logand (ash y 7) 2636928640)))
      (setf y (logxor y (logand (ash y 11) 4022730752)))
      (setf y (logxor y (ash y -18)))
      (setf (random-state-i *random-state*) i)
      y)))

(defun make-random-state (&optional (state nil))
  (check-type state (or (member nil t) random-state))
  (case state
    ((nil)
     (make-random-state *random-state*))
    ((t)
     (let ((*random-state* (%make-random-state)))
       (Makoto-Matsumoto (mod (get-universal-time) 4294967291))
       *random-state*))
    (otherwise
     (let ((new-state (copy-random-state state)))
       (setf (random-state-mt new-state) (copy-seq (random-state-mt state)))
       new-state))))

(setf *random-state* (make-random-state t))

(defun random (limit &optional (*random-state* *random-state*))
  "Returns a pseudo-random integer in [0,LIMIT[."
  (check-type limit (and (or (integer 1) (real 0))
                         (not (real 0 0))))
  (if (integerp limit)
      (loop
        :with llen := (integer-length limit)
        :with wlen := 32
        :for position :from 0 :to llen :by wlen
        :for random := (Takuji-Nishimura)
          :then (dpb (Takuji-Nishimura) (byte wlen position) random)
        :finally (return (mod random limit)))
      (let* ((b (truncate (scale-float (float 1 limit) 1)))
             (e (loop
                  :for e :from (nth-value 1 (decode-float limit)) :downto 0
                  :for r := (random b)
                  :while (zerop r)
                  :finally (return e)))
             (s (1+ (integer-decode-float
                     (float-typecase 1d100
                       (short-float  most-positive-short-float)
                       (single-float most-positive-single-float)
                       (double-float most-positive-double-float)
                       (long-float   most-positive-long-float)))))
             (m (random s)))
        (scale-float (/ (float m limit) s) e))))


(defun srand (seed)
  (setf *random-state*  (%make-random-state))
  (Makoto-Matsumoto (mod seed 4294967291)))

(defun rand (seed)
  (random 4294967296))


#-(and)
(progn
  (loop
    repeat 10 collect (random 1000))
  (73 381 773 143 193 815 324 777 801 439)
  (loop
    repeat 10 collect (random 1000.0f0))
  (384.27188 387.2843 72.70929 437.53748 212.09105 39.057716 502.98566 808.5497 27.742487 915.78644)
  (loop
    repeat 10 collect (random 1000.0f20))
  (4.941074E+22 4.378368E+21 2.342984E+22 1.1714196E+22 1.3757518E+22 9.1934555E+22 7.97674E+22 1.0366667E+23 8.556502E+22 6.509138E+22)
  )

;;;; THE END ;;;;
