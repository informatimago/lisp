;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               xoroshiro128.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements Xoroshiro128+ PRNG
;;;;    https://en.wikipedia.org/wiki/Xoroshiro128%2B
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-06-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.XOROSHIRO128"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "RANDOM-STATE" "RANDOM-STATE-P" "MAKE-RANDOM-STATE"
           "*RANDOM-STATE*" "RANDOM")
  (:export "RANDOM-STATE" "RANDOM-STATE-P" "MAKE-RANDOM-STATE"
           "*RANDOM-STATE*" "RANDOM")
  (:documentation "
Implements the Xoroshiro128+ Pseudo Random Number Generator.
(Not cryptographically secure).
AGPL3
Copyright Pascal J. Bourguignon 2018 - 2018
"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.XOROSHIRO128")


(deftype uint64 () '(unsigned-byte 64))
(defconstant +max-uint64+ #xFFFFFFFFFFFFFFFF)
(defun uint64 (word) (the uint64 (logand +max-uint64+ word)))

(defun rotl64 (word k)
  (declare (type uint64 word)
           (type fixnum k))
  (the uint64 (let ((p (- 64 k)))
                (dpb (ldb (byte k 0) word)
                     (byte k p)
                     (ldb (byte p k) word)))))


(defstruct (random-state
            (:constructor %make-random-state)
            (:conc-name %rs-))
  s0 s1)




(defvar *random-state*)

(defun jump ()
  "This is the jump function for the generator. It is equivalent
to 2^64 calls to next(); it can be used to generate 2^64
non-overlapping subsequences for parallel computations."
  (let ((j  #*00111111100000000100110011010010111110111010011000010000111010001010010100101010101011110001101100101001010000000000100111111011)
        (s0 0)
        (s1 0))
    (loop :for b :below 128
          :do (when (aref j b)
                (setf s0 (logxor s0 (%rs-s0 *random-state*))
                      s1 (logxor s1 (%rs-s1 *random-state*))))
              (next))
    (setf (%rs-s0 *random-state*) s0
          (%rs-s1 *random-state*) s1)
    *random-state*))

(defun next ()
  (let* ((s0     (%rs-s0 *random-state*))
         (s1     (%rs-s1 *random-state*))
         (result (uint64 (+ s0 s1))))
    (setf s1                      (logxor s1 s0)
          (%rs-s0 *random-state*) (uint64 (logxor (rotl64 s0 24) (logxor s1 (ash s1 16))))
          (%rs-s1 *random-state*) (the uint64 (rotl64 s1 37)))
    result))

(defun make-random-state (&optional (state nil))
  "As an extension, the state can also be a (unsigned-byte 128) used to initialize the random-state.
The first random-state is initialized with get-universal-time and get-internal-real-time,
the next time (make-random-state t) is called, the random state obtained with JUMP is returned.
"
  (check-type state (or (member nil t)
                        random-state
                        (unsigned-byte 128)))
  (case state
    ((nil)
     (make-random-state *random-state*))
    ((t)
     (if (boundp '*random-state*)
         (let ((*random-state* (copy-random-state *random-state*)))
           (jump))
         (%make-random-state
           :s0 (uint64 (get-universal-time))
           :s1 (uint64 (get-internal-real-time)))))
    (otherwise
     (if (integerp state)
         (%make-random-state
          :s0 (ldb (byte 64 64) state)
          :s1 (ldb (byte 64  0) state))
         (%make-random-state
          :s0 (%rs-s0 state)
          :s1 (%rs-s1 state))))))

(defparameter *random-state* (make-random-state t))



(defun random (limit &optional (*random-state* *random-state*))
  (etypecase limit
    ((integer 1)
     (loop
       :with drop := -4
       :with max := (ash +max-uint64+ drop)
       :with m := (* limit (truncate max limit))
       :for r := (if (<= limit max)
                     (ash (next) drop)
                     (loop
                       :named bignum
                       :for p :below (ceiling (log limit (1+ max)))
                       :for r := (ash (next) drop)
                         :then (dpb (ash (next) drop) (byte (+ 64 drop) (* (+ 64 drop) p)) r)
                       :finally (return r)))
       :while (and (plusp m) (< m r))
       :finally (return (mod r limit))))
    ((float 0.0)
     (assert (plusp limit)) ; exclude 0.
     (let ((m (expt 2 (float-digits limit))))
       (* limit (/ (float (random m) limit)
                   (float m limit)))))))


(defmacro with-specific-random-state (&body body)
  `(let ((*random-state* (make-random-state 321047098306122495840621590880868556385)))
     ,@body))

(defun test ()
  (assert
   (equalp
    (with-specific-random-state
      (list (loop repeat 10 collect (random 100))
            (loop repeat 10 collect (random 100.0s0))
            (loop repeat 10 collect (random 100.0d0))))
    '((93 63 20 52 61 21 92 28 20 90)
      (41.85024 2.6022136 5.2173853 95.012344 44.178146 92.49861 97.987236 78.110275 53.780884 30.602669)
      (61.45277314022255D0 26.579989800929106D0 28.86112251909987D0 91.21929348679451D0 76.0525998133693D0 26.43927281798557D0 0.22559720367546499D0 94.67635880985642D0 75.968619819787D0 62.1822518499491D0))))
  :success)

#-clisp(test)



;;;; THE END ;;;;
