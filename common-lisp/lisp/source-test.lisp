;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               source-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test source.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from source.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE"
                "NORMALIZE-OBJECT-AND-SOURCE-TYPE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE.TEST")


(defun test/normalize-object-and-source-type ()
  (assert-true (equal (list
                       (multiple-value-list
                        (normalize-object-and-source-type 'f 'function))
                       (multiple-value-list
                        (normalize-object-and-source-type '(setf f) 'function))
                       (multiple-value-list
                        (normalize-object-and-source-type 'f 'setf-function))
                       (ignore-errors
                        (multiple-value-list
                         (normalize-object-and-source-type '(setf f) 'setf-function))))
                      '((f :function)
                        (f :setf-function)
                        (f :setf-function)
                        nil)))
  (assert-true (equal (list
                       (multiple-value-list
                        (normalize-object-and-source-type '(m () (t t)) :method))
                       (multiple-value-list
                        (normalize-object-and-source-type '((setf m) () (t t)) ':method))
                       (multiple-value-list
                        (normalize-object-and-source-type '(m () (t t)) 'setf-method))
                       (ignore-errors
                        (multiple-value-list
                         (normalize-object-and-source-type '((setf m) () (t t)) :setf-method))))
                      '((m :method (() (t t)))
                        (m :setf-method (() (t t)))
                        (m :setf-method (() (t t)))
                        nil))))

(define-test test/all ()
  (test/normalize-object-and-source-type))


;;;; THE END ;;;;
