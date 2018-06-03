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
  (let ((j  #xDF900294D8F554A5170865DF4B3201FC)
        (s0 0)
        (s1 0))
    (loop :for b :below 128
          :do (when (logbitp b j)
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
       :with m := (* limit (truncate +max-uint64+ limit))
       :for r := (if (<= limit +max-uint64+)
                     (next)
                     (loop
                       :named bignum
                       :for p :below (ceiling (log limit (1+ +max-uint64+)))
                       :for r := (next)
                         :then (dpb (next) (byte 64 (* 64 p)) r)
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


(assert
 (equalp
  (with-specific-random-state
    (list (loop repeat 10 collect (random 100))
          (loop repeat 10 collect (random 100.0s0))
          (loop repeat 10 collect (random 100.0d0))))
  '((89 18 35 33 85 40 84 59 21 49)
    (69.60391 41.635468 83.47823 20.19757 6.850362
     79.97781 67.79578 49.764473 60.49419 89.642746)
    (83.24437024356094D0 25.27983681486573D0 61.777960305597965D0 59.50869578871241D0
     16.841597013908803D0 23.02836508776924D0 3.609555258807451D0 14.821740957703033D0
     28.79294131232856D0 15.497917116592108D0))))



;;;; THE END ;;;;
