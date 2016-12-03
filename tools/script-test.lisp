;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               script-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test script.lisp
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-23 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.TOOLS.SCRIPT.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.TOOLS.SCRIPT")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.TOOLS.SCRIPT.TEST")

(define-test test/mapconcat ()
  (loop :for (expression expected)
          :in '(((mapconcat (lambda (x) (and x (string-downcase x))) '("one" two three nil "five") "-")
                 "one-two-three--five")
                ((mapconcat (lambda (x) (and x (string-downcase x))) '("one") "-")
                 "one")
                ((mapconcat (lambda (x) (and x (string-downcase x))) '(nil) "-")
                 "")
                ((mapconcat (lambda (x) (and x (string-downcase x))) '() "-")
                 "")
                ((mapconcat (lambda (x) (and x (string-downcase x))) #("one" two three nil "five") "-")
                 "one-two-three--five")
                ((mapconcat (lambda (x) (and x (string-downcase x))) #("one") "-")
                 "one")
                ((mapconcat (lambda (x) (and x (string-downcase x))) #(nil) "-")
                 "")
                ((mapconcat (lambda (x) (and x (string-downcase x))) #() "-")
                 ""))
        :do (assert-true (equal (eval expression) expected)
                         ()
                         "~%Expression: ~S~%Expected: ~S~%Got: ~S~%"
                         expression expected (eval expression))))


(define-test test/all ()
  (test/mapconcat))

;;;; THE END ;;;;
