;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sparse-vector-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test sparse-vector.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-25 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SPARSE-VECTOR.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SPARSE-VECTOR")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SPARSE-VECTOR.TEST")


;; (let* ((size   1000000)
;;        (primes (com.informatimago.common-lisp.arithmetic.primes:compute-primes-to size)))
;;   (loop :for node :in '(2 5 7 8 13 16 29 32 64 128 256 1000 1024 2048)
;;         :do (let ((v (make-vector-tree node size :element-type 'fixnum :initial-element -1)))
;;               (loop :for i :below (length primes)
;;                     :do (setf (vector-ref v i) (* i i)))
;;               (format t "n=~4D  len=~8D   cnt=~8D  spc=~8D  l/s=~5,3F  c/l=~5,3F  c/s=~5,3F~%"
;;                       node
;;                       (vector-length v)
;;                       (vector-count v)
;;                       (vector-storage-size v)
;;                       (float (/ (vector-storage-size v)
;;                                 (vector-length v)))
;;                       (float (/ (vector-count v)
;;                                 (vector-length v)))
;;                       (float (/ (vector-count v)
;;                                 (vector-storage-size v)))))))
;; 
;; n=   2  len= 1000000   cnt=   78498  spc=  157033  l/s=0.157  c/l=0.078  c/s=0.500
;; n=   5  len= 1000000   cnt=   78498  spc=   98171  l/s=0.098  c/l=0.078  c/s=0.800
;; n=   7  len= 1000000   cnt=   78498  spc=   91621  l/s=0.092  c/l=0.078  c/s=0.857
;; n=   8  len= 1000000   cnt=   78498  spc=   89782  l/s=0.090  c/l=0.078  c/s=0.874
;; n=  13  len= 1000000   cnt=   78498  spc=   85119  l/s=0.085  c/l=0.078  c/s=0.922
;; n=  16  len= 1000000   cnt=   78498  spc=   83836  l/s=0.084  c/l=0.078  c/s=0.936
;; n=  29  len= 1000000   cnt=   78498  spc=   81497  l/s=0.081  c/l=0.078  c/s=0.963
;; n=  32  len= 1000000   cnt=   78498  spc=   81178  l/s=0.081  c/l=0.078  c/s=0.967
;; n=  64  len= 1000000   cnt=   78498  spc=   80096  l/s=0.080  c/l=0.078  c/s=0.980
;; n= 128  len= 1000000   cnt=   78498  spc=   79509  l/s=0.080  c/l=0.078  c/s=0.987
;; n= 256  len= 1000000   cnt=   78498  spc=   79819  l/s=0.080  c/l=0.078  c/s=0.983
;; n=1000  len= 1000000   cnt=   78498  spc=   80921  l/s=0.081  c/l=0.078  c/s=0.970
;; n=1024  len= 1000000   cnt=   78498  spc=   80819  l/s=0.081  c/l=0.078  c/s=0.971
;; n=2048  len= 1000000   cnt=   78498  spc=   83929  l/s=0.084  c/l=0.078  c/s=0.935
;; 
;; 
;; (let ((size   1000000))
;;   (loop :for node :in '(2 5 7 8 13 16 29 32 64 128 256 1000 1024 2048)
;;         :do (let ((v (make-vector-tree node size :element-type 'fixnum :initial-element -1)))
;;               (loop :for i :below 1000
;;                     :do (setf (vector-ref v i) (* i i)))
;;               (loop :for i :from 900000 :by 7 :repeat 1000
;;                     :do (setf (vector-ref v i) (* i i)))
;;               (format t "n=~4D  len=~8D   cnt=~8D  spc=~8D  l/s=~5,3F  c/l=~5,3F  c/s=~5,3F~%"
;;                       node
;;                       (vector-length v)
;;                       (vector-count v)
;;                       (vector-storage-size v)
;;                       (float (/ (vector-storage-size v)
;;                                 (vector-length v)))
;;                       (float (/ (vector-count v)
;;                                 (vector-length v)))
;;                       (float (/ (vector-count v)
;;                                 (vector-storage-size v)))))))
;; 
;; n=   2  len= 1000000   cnt=    2000  spc=   11326  l/s=0.011  c/l=0.002  c/s=0.177
;; n=   5  len= 1000000   cnt=    2000  spc=    8491  l/s=0.008  c/l=0.002  c/s=0.236
;; n=   7  len= 1000000   cnt=    2000  spc=    9485  l/s=0.009  c/l=0.002  c/s=0.211
;; n=   8  len= 1000000   cnt=    2000  spc=    9281  l/s=0.009  c/l=0.002  c/s=0.215
;; n=  13  len= 1000000   cnt=    2000  spc=    8843  l/s=0.009  c/l=0.002  c/s=0.226
;; n=  16  len= 1000000   cnt=    2000  spc=    8725  l/s=0.009  c/l=0.002  c/s=0.229
;; n=  29  len= 1000000   cnt=    2000  spc=    8726  l/s=0.009  c/l=0.002  c/s=0.229
;; n=  32  len= 1000000   cnt=    2000  spc=    8538  l/s=0.009  c/l=0.002  c/s=0.234
;; n=  64  len= 1000000   cnt=    2000  spc=    8828  l/s=0.009  c/l=0.002  c/s=0.227
;; n= 128  len= 1000000   cnt=    2000  spc=    9022  l/s=0.009  c/l=0.002  c/s=0.222
;; n= 256  len= 1000000   cnt=    2000  spc=    9694  l/s=0.010  c/l=0.002  c/s=0.206
;; n=1000  len= 1000000   cnt=    2000  spc=    9992  l/s=0.010  c/l=0.002  c/s=0.200
;; n=1024  len= 1000000   cnt=    2000  spc=   11255  l/s=0.011  c/l=0.002  c/s=0.178
;; n=2048  len= 1000000   cnt=    2000  spc=   14331  l/s=0.014  c/l=0.002  c/s=0.140



(define-test test/vector-tree (&key verbose)
  (flet ((report (v)
           (when verbose
            (format *trace-output* "~&length=~3D, count=~3D, space=~3D, ratio=~5,2F~%"
                    (vector-length v) (vector-count v) (vector-storage-size v)
                    (/ (vector-count v) (vector-storage-size v))))))
   ;; with fixnum:
   (let* ((element-type 'fixnum)
          (default      -1)
          (length       18)
          (node-size     4)
          (v (make-vector-tree node-size length :initial-element default :element-type element-type))
          (e (make-array                 length :initial-element default :element-type element-type))
          (s 0))
     (check equal (vector-element-type v) element-type)
     (check equal (vector-length       v) length)
     (check equal (vector-element-type e) element-type)
     (check equal (vector-length       e) length)
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "f0")
     (check < s (vector-storage-size v))
     (setf s (vector-storage-size v))
     (report v)
     (loop :for i :from 3 :by 7 :to 18
           :do (setf (vector-ref v i) i
                     (vector-ref e i) i))
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "f1")
     (check < s (vector-storage-size v))
     (setf s (vector-storage-size v))
     (report v)
     (loop :for i :from 0 :to 6
           :do (setf (vector-ref v i) i
                     (vector-ref e i) i))
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "f2")
     (check < s (vector-storage-size v))
     (setf s (vector-storage-size v))
     (report v)
     (loop :for i :from 16 :to 17
           :do (setf (vector-ref v i) i
                     (vector-ref e i) i))
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "f3")
     (check <= s (vector-storage-size v))
     (setf s (vector-storage-size v))
     (report v)
     (loop :for i :from 0 :to 7
           :do (setf (vector-ref v i) default
                     (vector-ref e i) default))
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "f4")
     (check > s (vector-storage-size v))
     (setf s (vector-storage-size v))
     (report v))
   ;; with characters:
   (let* ((element-type 'character)
          (default      #\space)
          (length       128)
          (node-size    11)
          (v (make-vector-tree node-size length :initial-element default :element-type element-type))
          (e (make-array                 length :initial-element default :element-type element-type)))
     (check equal (vector-element-type v) element-type)
     (check equal (vector-length       v) length)
     (check equal (vector-element-type e) element-type)
     (check equal (vector-length       e) length)
     (check equalp (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "c0")
     (report v)
     (loop :for i :from 33 :by 7 :to 127
           :do (setf (vector-ref v i) (code-char i)
                     (vector-ref e i) (code-char i)))
     (check equal (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "c1")
     (report v)
     (loop :for i :from 33 :to 63
           :do (setf (vector-ref v i) (code-char i)
                     (vector-ref e i) (code-char i)))
     (check equal (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "c2")
     (report v)
     (loop :for i :from 64 :to 95
           :do (setf (vector-ref v i) (code-char i)
                     (vector-ref e i) (code-char i)))
     (check equal (vector-elements v) e)
     (check = (vector-count v) (count default e :test-not (function eql)) (v) "c3")
     (report v))))


(define-test test/all ()
  (test/vector-tree))


;;;; THE END ;;;;
