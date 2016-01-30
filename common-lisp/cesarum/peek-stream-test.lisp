;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               peek-stream-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests peek-stream.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from peek-stream.lisp.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM.TEST")


(define-test test/peek-stream/get-future-char ()
  (dotimes (n 10)
    (with-input-from-string (in "ComMon-Lisp")
      (let* ((ps (make-instance 'peek-stream :stream in))
             (nc (loop
                   :for ch = (get-future-char ps)
                   :repeat n
                   :collect ch :into result :finally (return result)))
             (gc (loop
                   :for ch = (getchar  ps)
                   :repeat n
                   :collect ch :into result :finally (return result))))
        (assert-true (equal nc gc))))))


(define-test test/peek-stream/nextchar/1 ()
  (with-input-from-string (in "ComMon-Lisp")
    (let ((ps (make-instance 'peek-stream :stream in))
          c1 c2 c3)
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\C #\o #\m)))
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3 (nextchar ps))
                          '(#\M #\o #\n #\-)))
      (ungetchar ps c3) (ungetchar ps c2) (ungetchar ps c1)
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\M #\o #\n)))
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\- #\L #\i))))))


(define-test test/peek-stream/nextchar/2 ()
  (with-input-from-string (in "Common-Lisp")
    (let ((ps (make-instance 'peek-stream :stream in))
          c1 c2 c3)
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\C #\o #\m)))
      (setf c1 (getchar ps) c2 (getchar ps))
      (assert-true (equal (list c1 c2 (nextchar ps))
                          '(#\m #\o #\n)))
      (setf c3 (getchar ps))
      (assert-true (equal (list c3 (nextchar ps))
                          '(#\n #\-)))
      (ungetchar ps c3) (ungetchar ps c2) (ungetchar ps c1)
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\m #\o #\n)))
      (setf c1 (getchar ps) c2 (getchar ps) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3)
                          '(#\- #\L #\i))))))

(define-test test/peek-stream/nextchar/3 ()
  (with-input-from-string (in "  Common   Lisp")
    (let ((ps (make-instance 'peek-stream :stream in))
          c1 c2 c3)
      (setf c1 (getchar ps) c2 (getchar ps) c3 (nextchar ps))
      (assert-true (equal (list c1 c2 c3) '(#\space #\space #\C)))
      (setf c1 (getchar ps) c2 (getchar ps) c3 (nextchar ps #\n))
      (assert-true (equal (list c1 c2 c3) '(#\C #\o #\n)))
      (setf c1 (getchar ps) c2 (nextchar ps t) c3 (getchar ps))
      (assert-true (equal (list c1 c2 c3) '(#\n #\L #\L))))))


(define-test test/all ()
  (test/peek-stream/get-future-char)
  (test/peek-stream/nextchar/1)
  (test/peek-stream/nextchar/2)
  (test/peek-stream/nextchar/3))


;;;; THE END ;;;;
