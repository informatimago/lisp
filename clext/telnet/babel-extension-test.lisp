;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               babel-extension-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Tests decode-character.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(defpackage "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION.TEST")


(define-test test/decode-character/us-ascii ()
  (let ((encoding :us-ascii)
        (octets (make-array 10 :element-type '(unsigned-byte 8) :initial-element 32)))

    (check equal
           (multiple-value-list (decode-character octets :start 0 :end 0 :encoding encoding))
           '(nil t 1))

    (loop :for code :from 0 :to 127
          :do (setf (aref octets 0) code)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 1 :encoding encoding))
                     (list (code-char code) t 1)
                     (encoding code octets)))

    (loop :for code :from 128 :to 255
          :do (setf (aref octets 0) code)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 1 :encoding encoding))
                     '(nil nil 1)
                     (encoding code octets)))

    (loop :for code :from 0 :to 127
          :do (setf (aref octets 0) code)
              (setf (aref octets 1) 65)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 2 :encoding encoding))
                     (list (code-char code) t 1)
                     (encoding code octets)))

    (loop :for code :from 128 :to 255
          :do (setf (aref octets 0) code)
              (setf (aref octets 1) 65)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 2 :encoding encoding))
                     '(nil nil 1)
                     (encoding code octets)))))


(define-test test/decode-character/iso-8859-1 ()
  (let ((encoding :iso-8859-1)
        (octets (make-array 10 :element-type '(unsigned-byte 8) :initial-element 32)))

    (check equal
           (multiple-value-list (decode-character octets :start 0 :end 0 :encoding encoding))
           '(nil t 1))

    (loop :for code :from 0 :to 255
          :do (setf (aref octets 0) code)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 1 :encoding encoding))
                     (list (code-char code) t 1)
                     (encoding code octets)))

    (loop :for code :from 0 :to 255
          :do (setf (aref octets 0) code)
              (setf (aref octets 1) 65)
              (check equal
                     (multiple-value-list (decode-character octets :start 0 :end 2 :encoding encoding))
                     (list (code-char code) t 1)
                     (encoding code octets)))))


(defun utf-8-to-octets (code octets &key (start 0) end)
  (assert (<= code char-code-limit) (code)
          "Code ~D should be a unicode code point between 0 and ~A"
          code char-code-limit)
  (cond
    ((<= code #x7f)
     (assert (<= (+ start 1) (or end (length octets))))
     (setf (aref octets start) code)
     (incf start))
    ((<= code #x7ff)
     (assert (<= (+ start 2) (or end (length octets))))
     (setf (aref octets start) (dpb (ldb (byte 5  6) code) (byte 5 0) #b11000000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6  0) code) (byte 6 0) #b10000000))
     (incf start))
    ((<= code #xffff)
     (assert (<= (+ start 3) (or end (length octets))))
     (setf (aref octets start) (dpb (ldb (byte 4 12) code) (byte 4 0) #b11100000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6  6) code) (byte 6 0) #b10000000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6  0) code) (byte 6 0) #b10000000))
     (incf start))
    ((<= code #x10ffff)
     (assert (<= (+ start 4) (or end (length octets))))
     (setf (aref octets start) (dpb (ldb (byte 3 18) code) (byte 3 0) #b11110000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6 12) code) (byte 6 0) #b10000000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6  6) code) (byte 6 0) #b10000000))
     (incf start)
     (setf (aref octets start) (dpb (ldb (byte 6  0) code) (byte 6 0) #b10000000))
     (incf start))
    (t
     (error "Invalid unicode code-point for utf-8 encoding ~D (#x~:*~X)" code)))
  (values  start octets))

(define-test test/utf-8-to-octets ()
  (let ((octets (make-array 10 :element-type '(unsigned-byte 8) :initial-element 32)))
    (assert-true (= 1 (utf-8-to-octets     #x45 octets)))
    (assert-true (= 2 (utf-8-to-octets    #x745 octets)))
    (assert-true (= 3 (utf-8-to-octets   #x7045 octets)))
    (assert-true (= 4 (utf-8-to-octets #x100045 octets)))))

(define-test test/decode-character/utf-8 ()
  (let ((encoding :utf-8)
        (octets (make-array 10 :element-type '(unsigned-byte 8) :initial-element 32)))

    (check equal
           (multiple-value-list (decode-character octets :start 0 :end 0 :encoding encoding))
           '(nil t 1))

    ;; Note: this includes the cases where code-char returns NIL:
    (loop :for code :from 0 :below char-code-limit
          :for next := (utf-8-to-octets code octets :start 0    :end (length octets))
          :do (utf-8-to-octets 65   octets :start next :end (length octets))
              (if (<= code #x7f)
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 1 :encoding encoding))
                         (list (code-char code) t next)
                         (encoding code octets))
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 1 :encoding encoding))
                         (list nil t next)
                         (encoding code octets)))
              (if (<= code #x7ff)
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 2 :encoding encoding))
                         (list (code-char code) t next)
                         (encoding code octets))
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 2 :encoding encoding))
                         (list nil t next)
                         (encoding code octets)))
              (if (<= code #xffff)
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 3 :encoding encoding))
                         (list (code-char code) t next)
                         (encoding code octets))
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 3 :encoding encoding))
                         (list nil t next)
                         (encoding code octets)))
              (if (<= code #x10ffff)
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 4 :encoding encoding))
                         (list (code-char code) t next)
                         (encoding code octets))
                  (check equal
                         (multiple-value-list (decode-character octets :start 0 :end 4 :encoding encoding))
                         (list nil t next)
                         (encoding code octets))))

    ;; Testing invalid utf-8 code sequences:

    (check equal (multiple-value-list (decode-character (replace octets #(130)) :encoding encoding))
           '(nil nil 1)
           (encoding octets))

    (check equal (multiple-value-list (decode-character (replace octets #(#b11000000 #b00100001)) :encoding encoding))
           '(nil nil 2)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11000000 #b11100001)) :encoding encoding))
           '(nil nil 2)
           (encoding octets))

    (check equal (multiple-value-list (decode-character (replace octets #(#b11100000 #b10110011 #b00100001)) :encoding encoding))
           '(nil nil 3)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11100000 #b10110011 #b11100001)) :encoding encoding))
           '(nil nil 3)
           (encoding octets))

    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b00100001 #b10110011 #b10110011)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b10110011 #b00100001 #b10110011)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b10110011 #b10110011 #b00100001)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b11100001 #b10110011 #b10110011)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b10110011 #b11100001 #b10110011)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))
    (check equal (multiple-value-list (decode-character (replace octets #(#b11110000 #b10110011 #b10110011 #b11100001)) :encoding encoding))
           '(nil nil 4)
           (encoding octets))))

(define-test test/decode-character/eucjp ()
  (let* ((encoding :eucjp)
         (string "こんにちは / ｺﾝﾆﾁﾊ")
         (octets (babel:string-to-octets string :encoding :eucjp)))
    (loop
      :for expected :across string
      :for start := 0 :then (+ start size)
      :for (character validp size) := (multiple-value-list (decode-character octets :start start :encoding encoding))
      :do (assert-true character (character) "decode-character should have decoded a ~S character from ~A" encoding start)
          (assert-true validp (validp) "decode-character should have decoded a valid ~S code sequence from ~A" encoding start)
          (check char= character expected (encoding start octets character expected))
      :finally (incf start size)
               (check = start (length octets) (encoding start octets)))))


(define-test test/all ()
  (test/decode-character/us-ascii)
  (test/decode-character/iso-8859-1)
  (test/utf-8-to-octets)
  (test/decode-character/utf-8)
  (test/decode-character/eucjp))

;; (test/all)
;;;; THE END ;;;;
