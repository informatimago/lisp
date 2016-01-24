;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ieee-754-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test ieee-754.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from ieee-754.lisp
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.IEEE-754.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.IEEE-754")
  (:export
   "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DATA-ENCODING.IEEE-754.TEST")


(defun test-ieee-read-double ()
  (with-open-file (in "value.ieee-754-double" 
                      :direction :input :element-type '(unsigned-byte 8))
    (loop :while (< (file-position in) (file-length in))
          :do (loop  :for i = 1 :then (* i 256)
                     :for v = (read-byte in) :then (+ v (* i (read-byte in)))
                     :repeat 8
                     :finally (let ((*print-base* 16)) (princ v))
                              (princ " ")
                              (princ (ieee-754-to-float-64 v))
                              (terpri)))))

(defun test-ieee-read-single ()
  (with-open-file (in "value.ieee-754-single" 
                      :direction :input :element-type '(unsigned-byte 8))
    (loop :while (< (file-position in) (file-length in))
          :do (loop :for i = 1 :then (* i 256)
                    :for v = (read-byte in) :then (+ v (* i (read-byte in)))
                    :repeat 4
                    :finally (let ((*print-base* 16)) (princ v))
                             (princ " ")
                             (princ (ieee-754-to-float-32 v))
                             (terpri)))))


(defun test-single-to-ieee (&rest args)
  (dolist (arg args)
    (format t "(#x~16,8,'0R ~A)~%" 
            (float-32-to-ieee-754 (coerce arg 'single-float)) arg)))

(defun test-double-to-ieee (&rest args)
  (dolist (arg args)
    (format t "(#x~16,16,'0R ~A)~%" 
            (float-64-to-ieee-754 (coerce arg 'double-float)) arg)))



(defparameter *singles* '((#x00000000 0.0f0)
                          (#x00000000 -0.0f0)
                          (#x0DA24260 1.0F-30)
                          (#x1E3CE508 1.0F-20)
                          (#x2EDBE6FF 1.0F-10)
                          (#x3F800000 1.0)
                          (#x3F99999A 1.2f0)
                          (#x41400000 12.0f0)
                          (#x42F00000 120.0f0)
                          (#x44960000 1200.0f0)
                          (#x449A5225 1234.567f0)
                          (#x501502F9 1.0F+10)
                          (#x60AD78EC 1.0F+20)
                          (#x7149F2CA 1.0F+30)
                          (#x8DA24260 -1.0F-30)
                          (#x9E3CE508 -1.0F-20)
                          (#xAEDBE6FF -1.0F-10)
                          (#xBF800000 -1.0f0)
                          (#xBFC00000 -1.5f0)
                          (#xC1700000 -15.0f0)
                          (#xC3160000 -150.0f0)
                          (#xC49A5225 -1234.567f0)
                          (#xC4BB8000 -1500.0f0)
                          (#xD01502F9 -1.0F+10)
                          (#xE0AD78EC -1.0F+20)
                          (#xF149F2CA -1.0F+30)))

(define-test test/encode-single ()
  (loop :for (expected single) :in *singles*
        :for converted = (float-32-to-ieee-754 single)
        :do (assert-true (= expected converted)
                         (converted expected single))))

(define-test test/decode-single ()
  (loop :for (ieee expected) :in *singles*
        :for converted = (ieee-754-to-float-32 ieee)
        :do (assert-true (= expected converted)
                         (converted expected ieee))))


(defparameter *doubles* '((#x0000000000000000 0.0d0)
                          (#x0000000000000000 -0.0d0)
                          (#x3FF3333333333333 1.2d0)
                          (#x4028000000000000 12.0d0)
                          (#x405E000000000000 120.0d0)
                          (#x4092C00000000000 1200.0d0)
                          (#x40934A449BA5E354 1234.567d0)
                          (#xC202A05F20000000 -1.0D+10)
                          (#x4202A05F20000000 1.0D+10)
                          (#xC415AF1D78B58C40 -1.0D+20)
                          (#x4415AF1D78B58C40 1.0D+20)
                          (#xC6293E5939A08CEA -1.0D+30)
                          (#x46293E5939A08CEA 1.0D+30)
                          (#xBDDB7CDFD9D7BDBB -1.0D-10)
                          (#x3DDB7CDFD9D7BDBB 1.0D-10)
                          (#xBBC79CA10C924223 -1.0D-20)
                          (#x3BC79CA10C924223 1.0D-20)
                          (#xB9B4484BFEEBC2A0 -1.0D-30)
                          (#x39B4484BFEEBC2A0 1.0D-30)))

(define-test test/encode-double ()
  (loop :for (expected double) :in *doubles*
        :for converted = (float-64-to-ieee-754 double)
        :do (assert-true (= expected converted)
                         (converted expected double))))

(define-test test/decode-double ()
  (loop :for (ieee expected) :in *doubles*
        :for converted = (ieee-754-to-float-64 ieee)
        :do (assert-true (= expected converted)
                         (converted expected ieee))))



#-(and) (progn
          (test-single-to-ieee -1f10 1f10 -1f20 1f20 -1f30 1f30
                               -1f-10 1f-10 -1f-20 1f-20 -1f-30 1f-30)
          (test-double-to-ieee 1.2d0 12.0d0 120.0d0 1200.0d0 1234.567d0
                               -1d10 1d10 -1d20 1d20 -1d30 1d30
                               -1d-10 1d-10 -1d-20 1d-20 -1d-30 1d-30)

          )



(define-test test/all ()
  (test/encode-single)
  (test/decode-single)
  (test/encode-double)
  (test/decode-double))


;;;; THE END ;;;;
