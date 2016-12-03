;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               a-star-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Tests a-star.lisp.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from a-star.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.A-STAR.TEST")


(defun g1-successors (node)
  "Represents the graph http://gabrielgambetta.com/path1.html"
  (ecase node
    (a '(b f))
    (b '(a c))
    (c '(b))
    (d '())
    (e '(j))
    (f '(a k))
    (g '())
    (h '())
    (j '(e o))
    (k '(f l))
    (l '(k m q))
    (m '(l n))
    (n '(m o))
    (o '(n j t))
    (p '())
    (q '(l v))
    (r '())
    (s '())
    (t '(o y))
    (u '())
    (v '(q w))
    (w '(v x))
    (x '(w y))
    (y '(x t))))

(defun g1-coordinates (node)
  (ecase node
    (a #c(0 0))
    (b #c(1 0))
    (c #c(2 0))
    (d #c(3 0))
    (e #c(4 0))
    (f #c(0 1))
    (g #c(1 1))
    (h #c(2 1))
    (i #c(3 1))
    (j #c(4 1))
    (k #c(0 2))
    (l #c(1 2))
    (m #c(2 2))
    (n #c(3 2))
    (o #c(4 2))
    (p #c(0 3))
    (q #c(1 3))
    (r #c(2 3))
    (s #c(3 3))
    (t #c(4 3))
    (u #c(0 4))
    (v #c(1 4))
    (w #c(2 4))
    (x #c(3 4))
    (y #c(4 4))))

(defun g1-distance (a b)
  (abs (- (g1-coordinates a) (g1-coordinates b))))

(defun test/g1 (&key (start 'a) (goal 't))
  (multiple-value-call (function find-path)
    (function g1-successors)
    (let ((p (make-hash-table)))
      (values (lambda (node) (gethash node p))
              (lambda (new-previous node) (setf (gethash node p) new-previous))))
    (let ((c (make-hash-table)))
      (values (lambda (node) (gethash node c +infinity+))
              (lambda (new-cost node) (setf (gethash node c) new-cost))))
    (function g1-distance)
    (lambda (node) (eql node goal))
    start goal))

(define-test test/g1-paths ()
  (assert-true (equal (test/g1 :start 'a :goal 'x)
                      '(a f k l q v w x)))
  (assert-true (equal (test/g1 :start 'a :goal 'y)
                      '(a f k l m n o t y))))

(define-test test/all ()
  (test/g1))

;; (test)


