;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               index-set-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The tests for index-set.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-08 <PJB> Extracted from index-set.lisp
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET"
                          "UNION" "INTERSECTION" "MERGE" "INCLUDE")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET"
                "EQUAL-RANGES" "COMPLEMENT-RANGES")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET.TEST")


;;----------------------------------------------------------------------
;; RANGE TESTS

(define-test test/range ()
  (assert-true (range-emptyp (make-range :start 1 :count 0)))
  (assert-true (range-emptyp (make-range :start 1 :last 0)))
  (assert-true (range-emptyp (make-range :start 1 :end 1)))
  (assert-true (not (range-emptyp (make-range :start 1 :count 1))))
  (assert-true (not (range-emptyp (make-range :start 1 :last 1))))
  (assert-true (not (range-emptyp (make-range :start 1 :end 2))))
  (assert-true (equal-range (make-range :start 10 :end 21)
                            (make-range :first 10 :last 20)))
  (assert-true (equal-range (make-range :start 0 :end -1)
                            (make-range :first 0 :last -1)))
  
  (test = (range-start (make-range :start 1 :count 3)) 1)
  (test = (range-last  (make-range :start 1 :count 3)) 3)
  (test = (range-end   (make-range :start 1 :count 3)) 4)
  (test = (range-count (make-range :start 1 :count 3)) 3)
  (test = (range-start (copy-range (make-range :start 1 :count 3))) 1)
  (test = (range-last  (copy-range (make-range :start 1 :count 3))) 3)
  (test = (range-end   (copy-range (make-range :start 1 :count 3))) 4)
  (test = (range-count (copy-range (make-range :start 1 :count 3))) 3)

  (test = (range-start (make-range :start 11 :last 13)) 11)
  (test = (range-last  (make-range :start 11 :last 13)) 13)
  (test = (range-end   (make-range :start 11 :last 13)) 14)
  (test = (range-count (make-range :start 11 :last 13))  3)
  (test = (range-start (copy-range (make-range :start 11 :last 13))) 11)
  (test = (range-last  (copy-range (make-range :start 11 :last 13))) 13)
  (test = (range-end   (copy-range (make-range :start 11 :last 13))) 14)
  (test = (range-count (copy-range (make-range :start 11 :last 13)))  3)

  (test = (range-start (make-range :start 11 :end 14)) 11)
  (test = (range-last  (make-range :start 11 :end 14)) 13)
  (test = (range-end   (make-range :start 11 :end 14)) 14)
  (test = (range-count (make-range :start 11 :end 14))  3)
  (test = (range-start (copy-range (make-range :start 11 :end 14))) 11)
  (test = (range-last  (copy-range (make-range :start 11 :end 14))) 13)
  (test = (range-end   (copy-range (make-range :start 11 :end 14))) 14)
  (test = (range-count (copy-range (make-range :start 11 :end 14)))  3)

  (test = (range-start (make-range :count 3 :last 13)) 11)
  (test = (range-last  (make-range :count 3 :last 13)) 13)
  (test = (range-end   (make-range :count 3 :last 13)) 14)
  (test = (range-count (make-range :count 3 :last 13))  3)
  (test = (range-start (copy-range (make-range :count 3 :last 13))) 11)
  (test = (range-last  (copy-range (make-range :count 3 :last 13))) 13)
  (test = (range-end   (copy-range (make-range :count 3 :last 13))) 14)
  (test = (range-count (copy-range (make-range :count 3 :last 13)))  3)

  (test = (range-start (make-range :count 3 :end 14)) 11)
  (test = (range-last  (make-range :count 3 :end 14)) 13)
  (test = (range-end   (make-range :count 3 :end 14)) 14)
  (test = (range-count (make-range :count 3 :end 14))  3)
  (test = (range-start (copy-range (make-range :count 3 :end 14))) 11)
  (test = (range-last  (copy-range (make-range :count 3 :end 14))) 13)
  (test = (range-end   (copy-range (make-range :count 3 :end 14))) 14)
  (test = (range-count (copy-range (make-range :count 3 :end 14)))  3))


(define-test test/range/complement ()
  
  (test equal-ranges
        (complement-ranges (vector) 0 100)
        (vector (make-range :start 0 :end 100)))

  (test equal-ranges
        (complement-ranges (vector  (make-range :start 0 :end 100)) 0 100)
        (vector))

  (test equal-ranges
        (complement-ranges (vector (make-range :start 0 :end 90)) 0 100)
        (vector (make-range :start 90 :end 100)))
  
  (test equal-ranges
        (complement-ranges (vector (make-range :start 10 :end 100)) 0 100)
        (vector (make-range :start 0 :end 10)))

  (test equal-ranges
        (complement-ranges (vector (make-range :start 10 :end 90)) 0 100)
        (vector (make-range :start 0 :end 10)  (make-range :start 90 :end 100)))

  (expect-condition error
                    (complement-ranges (vector  (make-range :start 0 :end 100)) 10 90)))

;;----------------------------------------------------------------------
;; INDEX-SET TESTS

(define-test test/all ()
  (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET")))
   (test/range)
   (test/range/complement)
   (com.informatimago.common-lisp.cesarum.set.test:test/all/class 'index-set)))

;;;; THE END ;;;;




