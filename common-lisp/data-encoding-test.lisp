;;;; -*- coding:utf-8 -*-
;;****************************************************************************
;;FILE:               data-encoding-test.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    Some test cases of COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.
;;    
;;AUTHORS
;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2004-06-20 <PJB> Created.
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal J. Bourguignon 2004 - 2004
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(package:load-package "data-encoding.lisp")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING")

(progn
  (def-enctype buint8        ()    (number unsigned       1 big-endian))
  (def-enctype buint16       ()    (number unsigned       2 big-endian))
  (def-enctype buint24       ()    (number unsigned       3 big-endian))
  (def-enctype buint32       ()    (number unsigned       4 big-endian))
  (def-enctype bint8         ()    (number two-complement 1 big-endian))
  (def-enctype bint16        ()    (number two-complement 2 big-endian))
  (def-enctype bint24        ()    (number two-complement 3 big-endian))
  (def-enctype bint32        ()    (number two-complement 4 big-endian))
  (def-enctype bbcd          (siz) (number binary-coded-decimal siz big-endian))
  (def-enctype luint8        ()    (number unsigned       1 little-endian))
  (def-enctype luint16       ()    (number unsigned       2 little-endian))
  (def-enctype luint24       ()    (number unsigned       3 little-endian))
  (def-enctype luint32       ()    (number unsigned       4 little-endian))
  (def-enctype lint8         ()    (number two-complement 1 little-endian))
  (def-enctype lint16        ()    (number two-complement 2 little-endian))
  (def-enctype lint24        ()    (number two-complement 3 little-endian))
  (def-enctype lint32        ()    (number two-complement 4 little-endian))
  (def-enctype lbcd          (siz) (number binary-coded-decimal siz little-endian))
  (def-enctype m2-string     (len) (string len null  :terminated :if-smaller))
  (def-enctype cobol-string  (len) (string len space :padded :strip))
  (def-enctype pascal-string (len) (string len :green-length (number unsigned 1)))
  (def-enctype c-string      (len) (string len null  :terminated))
  )




(def-encrecord test-rec
  (buint8   buint8)
  (buint24  buint24)
  (luint8   luint8)
  (luint24  luint24)
  (buint16  buint16)
  (luint16  luint16)
  (buint32  buint32)
  (luint32  luint32)
  (bint8    bint8)
  (bint24   bint24)
  (lint8    lint8)
  (lint24   lint24)
  (bint16   bint16)
  (lint16   lint16)
  (bint32   bint32)
  (lint32   lint32)
  (lbcd1    (lbcd 1))
  (lbcd7    (lbcd 7))
  (bbcd1    (bbcd 1))
  (bbcd7    (bbcd 7))
  (cstr1    (c-string 16) :offset 64)
  (cstr2    (c-string 16))
  (cstr3    (c-string 16))
  (m2str1   (m2-string 16))
  (m2str2   (m2-string 16))
  (m2str3   (m2-string 16)) 
  (cbstr1   (cobol-string 16))
  (cbstr2   (cobol-string 16))
  (cbstr3   (cobol-string 16))
  (pstr1    (pascal-string 16))
  (pstr2    (pascal-string 16))
  (pstr3    (pascal-string 16))
  (bstr1    (string 16 :green-length (number unsigned 2)))
  (bstr2    (string 16 :green-length (number unsigned 2)))
  (bstr3    (string 16 :green-length (number unsigned 2)))
  );;test-rec








(defun test-write-read (record write read)
  (with-open-file (file "/tmp/test.data"
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))
    (funcall write record file))
  (assert (equalp record
                  (with-open-file (file "/tmp/test.data"
                                        :direction :input
                                        :if-does-not-exist :error
                                        :element-type '(unsigned-byte 8))
                    (funcall read file)))));;test-write-read



(test-write-read (make-test-rec)
                 (function write-test-rec)
                 (function read-test-rec))




(dotimes (i 192)
  (test-write-read 
   (make-TEST-REC
    :BUINT8  i
    :BUINT24 (* (+ 32768 512 3) i)
    :LUINT8  i
    :LUINT24 (* (+ 32768 512 3) i)
    :BUINT16 (* (+ 128 2) i)
    :LUINT16 (* (+ 128 2) i)
    :BUINT32 (* (+ 4194304 32768 512 3) i)
    :LUINT32 (* (+ 4194304 32768 512 3) i)
    :BINT8   (- i 128)
    :BINT24  (- (* (+ 32768 512 3) i) (expt 2 23))
    :LINT8   (- i 128)
    :LINT24  (- (* (+ 32768 512 3) i) (expt 2 23))
    :BINT16  (- (* (+ 128 2) i) (expt 2 15))
    :LINT16  (- (* (+ 128 2) i) (expt 2 15))
    :BINT32  (- (* (+ 4194304 32768 512 3) i) (expt 2 31))
    :LINT32  (- (* (+ 4194304 32768 512 3) i) (expt 2 31))
    :LBCD1   (- (mod i 19) 9)
    :LBCD7   (- (mod (* 12345678 i) 1999999) 999999)
    :BBCD1   (- (mod i 19) 9)
    :BBCD7   (- (mod (* 12345678 i) 1999999) 999999)
    :CSTR1   (format nil "~10,'0D" i)
    :CSTR2   (format nil "~15,'0D" i)
    :CSTR3   ""
    :M2STR1  (format nil "~10,'0D" i) 
    :M2STR2  (format nil "~15,'0D" i) 
    :M2STR3  (format nil "~16,'0D" i)
    :CBSTR1  (format nil "~10,'0D" i)
    :CBSTR2  (format nil "~15,'0D" i) 
    :CBSTR3  (format nil "~16,'0D" i)
    :PSTR1   (format nil "~10,'0D" i) 
    :PSTR2   (format nil "~15,'0D" i)
    :PSTR3   "" 
    :BSTR1   (format nil "~10,'0D" i) 
    :BSTR2   (format nil "~14,'0D" i)
    :BSTR3   "")
   (function write-test-rec)
   (function read-test-rec)))


 
(test-write-read 
 (make-TEST-REC
  :BUINT8  #x12
  :BUINT24 #x123456
  :LUINT8  #x12
  :LUINT24 #x123456
  :BUINT16 #x1234
  :LUINT16 #x1234
  :BUINT32 #x12345678
  :LUINT32 #x12345678
  :BINT8   (- #xfe #x100)
  :BINT24  (- #xfedcba #x1000000)
  :LINT8   (- #xfe #x100)
  :LINT24  (- #xfedcba #x1000000)
  :BINT16  (- #xfedc #x10000)
  :LINT16  (- #xfedc #x10000)
  :BINT32  (- #xfedcba98 #x100000000)
  :LINT32  (- #xfedcba98 #x100000000)
  :LBCD1   -7
  :LBCD7   1234567
  :BBCD1   -7
  :BBCD7   1234567
  :CSTR1   "cstr1"
  :CSTR2   "Pascal"
  :CSTR3   ""
  :M2STR1  "m2str1"
  :M2STR2  "Jacques"
  :M2STR3  ""
  :CBSTR1  "cbstr1"
  :CBSTR2  "Bourguignon"
  :CBSTR3  ""
  :PSTR1   "pstr1"
  :PSTR2   "La Manga"
  :PSTR3   "" 
  :BSTR1   "bstr1"
  :BSTR2   "del Mar Menor"
  :BSTR3   "")
 (function write-test-rec)
 (function read-test-rec))


(assert 
 (equalp
  (with-open-file (file "/tmp/test.data"
                        :direction :input
                        :if-does-not-exist :error
                        :element-type '(unsigned-byte 8))
    (let ((buffer  (make-array '(304) :element-type '(unsigned-byte 8))))
      (assert (= 304 (read-sequence buffer file)))
      buffer))
  #(18 18 52 86 18 86 52 18 18 52 52 18 18 52 86 120 120 86 52 18 254
       254 220 186 254 186 220 254 254 220 220 254 254 220 186 152 152 186
       220 254 125 124 86 52 18 0 0 0 125 0 0 0 18 52 86 124 0 0 0 0 0 0 0 0
       99 115 116 114 49 0 0 0 0 0 0 0 0 0 0 0 80 97 115 99 97 108 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 109 50 115 116 114 49 0 0 0
       0 0 0 0 0 0 0 74 97 99 113 117 101 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 99 98 115 116 114 49 32 32 32 32 32 32 32 32 32 32
       66 111 117 114 103 117 105 103 110 111 110 32 32 32 32 32 32 32 32 32
       32 32 32 32 32 32 32 32 32 32 32 32 5 112 115 116 114 49 0 0 0 0 0 0 0
       0 0 0 8 76 97 32 77 97 110 103 97 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 5 98 115 116 114 49 0 0 0 0 0 0 0 0 0 0 13 100 101 108 32
       77 97 114 32 77 101 110 111 114 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))



;;;; data-encoding-test.lisp          --                     --          ;;;;
