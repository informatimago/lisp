;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               regexp-posix-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test regexp-posix.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from regexp-posix.lisp
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX"
                "RANGE-SET-UNION" "RANGE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP-POSIX.TEST")

(define-test test/range-set-union ()
  (map nil (lambda (test)
             (let ((u (range-set-union (first test)  (second test)))
                   (v (range-set-union (second test) (first test))))
               (if (and (equal u v) (equal u (third test)))
                   (progress-success)
                   (progress-failure-message
                    '(and (equal u v) (equal u (third test)))
                    "ERROR:~%a=  ~S~%b=  ~S~%e=  ~S~%u=  ~S~%v=  ~S~2%"
                    (first test)  (second test) (third test) u v))))
    '(
      ((1 (nil))
       (3 (nil))
       (1 3 (nil)))
      ((1 (nil))
       (2 (nil))
       ((1 . 2) (nil)))
      (((1 . 3) (nil))
       ((5 . 7) (nil))
       ((1 . 3) (5 . 7) (nil)))
      (((1 . 3) (nil))
       ((4 . 6) (nil))
       ((1 . 6) (nil)))
      (((1 . 4) (nil))
       ((4 . 6) (nil))
       ((1 . 6) (nil)))
      (((1 . 4) (nil))
       ((3 . 6) (nil))
       ((1 . 6) (nil)))
      (((1 . 4) (nil))
       ((1 . 6) (nil))
       ((1 . 6) (nil)))
      (((1 . 4) (nil))
       ((0 . 6) (nil))
       ((0 . 6) (nil)))
      (((1 . 3) (5 . 7) (9 . 11) (nil))
       ((3 . 5) (7 . 9) (11 . 13) (nil))
       ((1 . 13) (nil)))
      (((1 . 3) (5 . 7) (9 . 11) (nil))
       (4 8 12 (nil))
       ((1 . 12) (nil)))
      (((2 . 6) (10 . 14) (18 . 22) (nil))
       (0  8 16 34 (nil))
       (0 (2 . 6) 8 (10 . 14) 16 (18 . 22) 34 (nil))))))

(define-test test/all ()
  (test/range-set-union))

;;;; THE END ;;;;
