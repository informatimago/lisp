;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               version.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to get and compare lisp
;;;;    implementation versions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-18 <PJB> Extracted from script.lisp (extracted from .clisprc).
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.VERSION"
  (:use "COMMON-LISP")
  (:export
   "VERSION"
   "VERSION="     "VERSION<"    "VERSION<="
   "VERSION/="    "VERSION>"    "VERSION>="
   "RT-VERSION="  "RT-VERSION<" "RT-VERSION<="
   "RT-VERSION/=" "RT-VERSION>" "RT-VERSION>="
   ))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.VERSION")


(defun version (&optional (version-string (LISP-IMPLEMENTATION-VERSION)))
  "
Parse the version in the version-string or (lisp-implementation-version)
This is expected to be a sequence of integers separated by dots.
Return it as a list of integers.
"
  (loop
     :with r = '()
     :with start = 0
     :do (multiple-value-bind (n p)
             (parse-integer version-string :start start :junk-allowed t)
           (push n r)
           (if (or (<= (length version-string) p)
                   (char= #\space (aref version-string p)))
               (return-from version (nreverse r))   
            (setf start (1+ p))))))

(defun version= (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (let ((a (if (stringp a) (version a) a))
        (b (if (stringp b) (version b) b)))
    (equal a b)))


(defun version/= (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (not (version= a b)))


(defun version< (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (let ((a (if (stringp a) (version a) a))
        (b (if (stringp b) (version b) b)))
    (cond
      ((null a)            (not (null b)))
      ((null b)            nil)
      ((< (car a) (car b)) t)
      ((= (car a) (car b)) (version< (cdr a) (cdr b)))
      (t                   nil))))


(defun version<= (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (let ((a (if (stringp a) (version a) a))
        (b (if (stringp b) (version b) b)))
    (or (version= a b) (version< a b))))


(defun version>  (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (version<  b a))


(defun version>= (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string."
  (version<= b a))


(defun rt-version=  (a b)
  "Compare versions A and B, which may be either lists of integers, or a dotted string,
returning either (:AND) or (:OR), for use as a read-time feature expression.
   #.#+(version:rt-version= \"1.0\" (version:version)) read-only-in-version-1.0
"
  (if (version=  a b) '(:and) '(:or)))


(defun rt-version<  (a b)
  "
Compare versions A and B, which may be either lists of integers, or a dotted string,
returning either (:AND) or (:OR), for use as a read-time feature expression.
   #.#+(version:rt-version= \"1.0\" (version:version)) read-only-in-version-1.0
"
  (if (version<  a b) '(:and) '(:or)))


(defun rt-version<= (a b)
  "
Compare versions A and B, which may be either lists of integers, or a dotted string,
returning either (:AND) or (:OR), for use as a read-time feature expression.
   #.#+(version:rt-version= \"1.0\" (version:version)) read-only-in-version-1.0
"
  (if (version<= a b) '(:and) '(:or)))


(defun rt-version>  (a b)
  "
Compare versions A and B, which may be either lists of integers, or a dotted string,
returning either (:AND) or (:OR), for use as a read-time feature expression.
   #.#+(version:rt-version= \"1.0\" (version:version)) read-only-in-version-1.0
"
  (rt-version< b a))


(defun rt-version>= (a b)
  "
Compare versions A and B, which may be either lists of integers, or a dotted string,
returning either (:AND) or (:OR), for use as a read-time feature expression.
   #.#+(version:rt-version= \"1.0\" (version:version)) read-only-in-version-1.0
"
  (rt-version<= b a))

;;;; THE END ;;;;
