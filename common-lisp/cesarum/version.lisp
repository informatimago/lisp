;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               version.lisp
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
;;;;    2015-01-25 <PJB> Improved parsing of lisp-implementation-version strings.
;;;;    2010-07-18 <PJB> Extracted from script.lisp (extracted from .clisprc).
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.VERSION"
  (:use "COMMON-LISP")
  (:export
   "VERSION"
   "VERSION="     "VERSION<"    "VERSION<="
   "VERSION/="    "VERSION>"    "VERSION>="
   "RT-VERSION="  "RT-VERSION<" "RT-VERSION<="
   "RT-VERSION/=" "RT-VERSION>" "RT-VERSION>="
   )
  (:documentation "

This package exports functions to get and compare lisp implementation
versions.



License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2010 - 2015
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.VERSION")




(defun version (&optional (version-string (lisp-implementation-version)))
  "
Parse the version in the version-string or (lisp-implementation-version)
This is expected to be a sequence of integers separated by dots.
Return it as a list of integers.
"
  ;; version ::= [a-z]* integer ('.' integer)* ('.' [a-z]+)? (+|[a-z]|'r' integer)? ('(' /.*/)?
  (let ((pos 0)
        (len (length version-string)))
    (labels ((peek   (&optional (offset 0))
               (when (< (+ pos offset) len)
                 (aref version-string (+ pos offset))))
             (eat    (&optional (n 1))
               (when (<= (+ pos n) len)
                 (prog1 (aref version-string pos)
                   (incf pos n))))
             (spacep (ch)
               (and ch (char= #\space ch)))
             (prefix ()
               (loop :for ch = (peek)
                     :while (and ch (or (spacep ch) (alpha-char-p ch)))
                     :do (eat))
               '())
             (word ()
               (loop :for ch = (peek)
                     :while (and ch (alpha-char-p ch))
                     :do (eat))
               '())
             (int ()
               (let ((ch (peek)))
                 (when (digit-char-p ch)
                   (multiple-value-bind (n p)
                       (parse-integer version-string
                                      :start pos
                                      :junk-allowed t)
                     (setf pos p)
                     n))))
             (dot ()
               (let ((ch (peek)))
                 (when (and ch (char= #\. ch))
                   (eat))))
             (rel ()
               (let ((ch (peek)))
                 (cond
                   ((null ch)
                    '())
                   ((char= #\+ ch)                     ; clisp 2.49+ --> 2.49.1
                    (eat)
                    (list 1))
                   ((and (char= #\- ch) (char= #\r (peek 1))) ; ccl 1.10-r16196
                    (eat 2)
                    (list (int)))
                   ((alpha-char-p ch)                       ; cmucl 20d -> 20.3
                    (let ((p (position (eat) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                       :test (function char-equal))))
                      (when p (list p))))
                   (t
                    '())))))
      (prefix)
      (nconc (list (int))
             (loop
               :with vers = '()
               :while (dot)
               :do (let ((item (int)))
                     (if item
                         (push item vers)
                         (word)))
               :finally (return (nreverse vers)))
             (rel)))))


(assert (equal (mapcar (function version)
                       '("1.3.1"
                         "Version 1.10-r16196  (LinuxX8664)"
                         "2.49+ (2010-07-17) (built 3603386203) (memory 3603386298)"
                         "20d (20D Unicode)"
                         "13.5.1"
                         "1.0.57.0.debian"))
               '((1 3 1) (1 10 16196) (2 49 1) (20 3) (13 5 1) (1 0 57 0))))


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
