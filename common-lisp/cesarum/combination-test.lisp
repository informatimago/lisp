;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               combination-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests combination.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-28 <PJB> Extracted from combination.lisp.
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.COMBINATION.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.COMBINATION")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.COMBINATION.TEST")

(define-test test/arrangement-with-repeat/take-3-of-5 ()
  (let ((awr (make-instance 'arrangement-with-repeat)))
    (setf (base-cardinal awr) 5
          (element-size  awr) 3)
    (reset awr)
    (assert-true (= 125 (cardinal awr)))
    (assert-true (equalp (loop :named awr
                               :with results = '()
                               :do (multiple-value-bind (arrangement done) (get-next-element awr)
                                     (when done (return-from awr (nreverse results)))
                                     (push arrangement results)))
                         '(#(0 0 0)  #(0 0 1)  #(0 0 2)  #(0 0 3)  #(0 0 4)  #(0 1 0)  #(0 1 1)
                           #(0 1 2)  #(0 1 3)  #(0 1 4)  #(0 2 0)  #(0 2 1)  #(0 2 2)  #(0 2 3)
                           #(0 2 4)  #(0 3 0)  #(0 3 1)  #(0 3 2)  #(0 3 3)  #(0 3 4)  #(0 4 0)
                           #(0 4 1)  #(0 4 2)  #(0 4 3)  #(0 4 4)  #(1 0 0)  #(1 0 1)  #(1 0 2)
                           #(1 0 3)  #(1 0 4)  #(1 1 0)  #(1 1 1)  #(1 1 2)  #(1 1 3)  #(1 1 4)
                           #(1 2 0)  #(1 2 1)  #(1 2 2)  #(1 2 3)  #(1 2 4)  #(1 3 0)  #(1 3 1)
                           #(1 3 2)  #(1 3 3)  #(1 3 4)  #(1 4 0)  #(1 4 1)  #(1 4 2)  #(1 4 3)
                           #(1 4 4)  #(2 0 0)  #(2 0 1)  #(2 0 2)  #(2 0 3)  #(2 0 4)  #(2 1 0)
                           #(2 1 1)  #(2 1 2)  #(2 1 3)  #(2 1 4)  #(2 2 0)  #(2 2 1)  #(2 2 2)
                           #(2 2 3)  #(2 2 4)  #(2 3 0)  #(2 3 1)  #(2 3 2)  #(2 3 3)  #(2 3 4)
                           #(2 4 0)  #(2 4 1)  #(2 4 2)  #(2 4 3)  #(2 4 4)  #(3 0 0)  #(3 0 1)
                           #(3 0 2)  #(3 0 3)  #(3 0 4)  #(3 1 0)  #(3 1 1)  #(3 1 2)  #(3 1 3)
                           #(3 1 4)  #(3 2 0)  #(3 2 1)  #(3 2 2)  #(3 2 3)  #(3 2 4)  #(3 3 0)
                           #(3 3 1)  #(3 3 2)  #(3 3 3)  #(3 3 4)  #(3 4 0)  #(3 4 1)  #(3 4 2)
                           #(3 4 3)  #(3 4 4)  #(4 0 0)  #(4 0 1)  #(4 0 2)  #(4 0 3)  #(4 0 4)
                           #(4 1 0)  #(4 1 1)  #(4 1 2)  #(4 1 3)  #(4 1 4)  #(4 2 0)  #(4 2 1)
                           #(4 2 2)  #(4 2 3)  #(4 2 4)  #(4 3 0)  #(4 3 1)  #(4 3 2)  #(4 3 3)
                           #(4 3 4)  #(4 4 0)  #(4 4 1)  #(4 4 2)  #(4 4 3)  #(4 4 4))))))



(define-test test/arrangement-sans-repeat/take-3-of-5-distinct ()
   (let ((asr (make-instance 'arrangement-sans-repeat)))
     (setf (base-cardinal asr) 5
           (element-size  asr) 3)
     (reset asr)
     (assert-true (= 60 (cardinal asr)))
     (assert-true (equalp (loop :with results = '()
                         :named asr
                         :do (multiple-value-bind (arrangement done) (get-next-element asr)
                               (when done (return-from asr (nreverse results)))
                               (push arrangement results)))

                          
                          '(#(0 1 2)  #(0 1 3)  #(0 1 4)  #(0 2 1) #(0 2 3)  #(0 2 4)  #(0 3 1)
                            #(0 3 2)  #(0 3 4)  #(0 4 1)  #(0 4 2) #(0 4 3)  #(1 0 2)  #(1 0 3)
                            #(1 0 4)  #(1 2 0)  #(1 2 3)  #(1 2 4) #(1 3 0)  #(1 3 2)  #(1 3 4)
                            #(1 4 0)  #(1 4 2)  #(1 4 3)  #(2 0 1) #(2 0 3)  #(2 0 4)  #(2 1 0)
                            #(2 1 3)  #(2 1 4)  #(2 3 0)  #(2 3 1) #(2 3 4)  #(2 4 0)  #(2 4 1)
                            #(2 4 3)  #(3 0 1)  #(3 0 2)  #(3 0 4) #(3 1 0)  #(3 1 2)  #(3 1 4)
                            #(3 2 0)  #(3 2 1)  #(3 2 4)  #(3 4 0) #(3 4 1)  #(3 4 2)  #(4 0 1)
                            #(4 0 2)  #(4 0 3)  #(4 1 0)  #(4 1 2) #(4 1 3)  #(4 2 0)  #(4 2 1)
                            #(4 2 3)  #(4 3 0)  #(4 3 1)  #(4 3 2))))))


(define-test test/combination/3-from-5 ()
  (let ((com (make-instance 'combination)))
    (setf (base-cardinal com) 5
          (element-size  com) 3)
    (reset com)
    (assert-true (= 10 (cardinal com)))
    (assert-true (equalp (loop :with results = '()
                               :named com
                               :do (multiple-value-bind (arrangement done) (get-next-element com)
                                     (when done  (return-from com (nreverse results)))
                                     (push arrangement results)))
                         '(#(0 1 2)  #(0 1 3)  #(0 1 4)  #(0 2 3)  #(0 2 4)  #(0 3 4)  #(1 2 3)
                           #(1 2 4)  #(1 3 4)  #(2 3 4))))))


(define-test test/all ()
  (test/arrangement-with-repeat/take-3-of-5)
  (test/arrangement-sans-repeat/take-3-of-5-distinct)
  (test/combination/3-from-5))


;;;; THE END ;;;;
