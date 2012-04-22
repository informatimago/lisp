;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               float-binio.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package encodes and decodes arrays of float into arrays
;;;;    of signed-byte 32 in order to do binary I/O.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-05 <PJB> Created.
;;;;BUGS
;;;;    Handling of SHORT-FLOAT and LONG-FLOAT is not complete.
;;;;    Notably, for SHORT-FLOAT, (and for exponents), we may want to
;;;;    encode into an array of (signed-byte 8).
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FLOAT-BINIO"
  (:use "COMMON-LISP")
  (:documentation
   "This package encodes and decodes arrays of float into arrays
    of signed-byte 32 in order to do binary I/O.
    BUGS: Handling of SHORT-FLOAT and LONG-FLOAT is not complete.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:export "BIOFA-DECODE" "BIOFA-ENCODE" "BIOFA-SETREF" "BIOFA-REF"
           "BIOFA-COUNT"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FLOAT-BINIO")




;;; (defparameter seq (make-array '(1000)
;;;                               :element-type '(signed-byte 32)
;;;                               :initial-element 123456789));;seq

;;; (time
;;;   (with-open-file (out "/tmp/file.dat"
;;;                        :direction :output
;;;                        :if-exists :supersede :if-does-not-exist :create
;;;                        :element-type '(signed-byte 32))
;;;     (dotimes (i 1000)
;;;       (write-sequence seq out))))


;;; (time
;;;   (with-open-file (in  "/tmp/file.dat"
;;;                        :direction :input
;;;                        :if-does-not-exist :create
;;;                        :element-type '(signed-byte 32))
;;;     (dotimes (i 1000)
;;;       (read-sequence seq in))))


;; integer-decode-float


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant  +header-size+ 2 "NUMBER OF HEADER ELEMENTS IN A BIOFA ARRAY"))

(defun biofa-float-size (biofa-array)
  "
PRIVATE
RETURN: THE NUMBER OF (SIGNED-BYTE 32) NEEDED TO STORE THE MANTISSA
        OF THE FLOATS STORED IN BIOFA-ARRAY.
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (aref biofa-array 0))



(defun biofa-set-float-size (biofa-array float-size)
  "
PRIVATE
POST: (= (BIOFA-FLOAT-SIZE BIOFA-ARRAY) FLOAT-SIZE)
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (setf (aref biofa-array 0) float-size))



(defun biofa-radix (biofa-array)
  "
PRIVATE
RETURN: The radix used to decode/encode that BIOFA-ARRAY.
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (aref biofa-array 1))



(defun biofa-set-radix (biofa-array radix)
  "
PRIVATE
RETURN: The radix used to decode/encode that BIOFA-ARRAY.
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (setf (aref biofa-array 1) radix))



(defun float-size (fvalue)
  "
PRIVATE
RETURN: THE NUMBER OF (SIGNED-BYTE 32) NEEDED TO STORE THE MANTISSA OF FVALUE.
"
  (values (truncate (+ 31 (float-precision fvalue)) 32)))



(defun biofa-count (biofa-array)
  "
RETURN: The number of floats encoded into the biofa-array.
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (/ (- (length biofa-array)  +header-size+)
     (1+ (biofa-float-size biofa-array))))



(defvar +float-types+ '((1 . single-float) (2 . double-float))
  ;; SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT
  "AN A-LIST MAPPING FLOAT-SIZE TO FLOAT TYPE.") ;;+FLOAT-TYPES+



(defmacro biofa-fetch (biofa-array radix float-size type base)
  "
PRIVATE
"
  `(let* ((exps (prog1 (aref ,biofa-array ,base) (incf ,base)))
          (nega (oddp exps))
          (expo (truncate (- exps (if nega 1 0)) 2))
          (mant (case ,float-size
                  (1 (prog1 (aref ,biofa-array ,base) (incf ,base)))
                  (2 (let ((l (prog1 (aref ,biofa-array ,base) (incf ,base)))
                           (h (prog1 (aref ,biofa-array ,base) (incf ,base))))
                       (dpb h (byte 32 32) l) ))
                  (otherwise
                   (do ((i 0 (1+ i))
                        (b 0 (+ b 32))
                        (m)
                        (r 0))
                       ((< i fs) r)
                     (setq m (aref ,biofa-array ,base))
                     (incf ,base)
                     (dpb m (byte 32 b) r)))))
          (fvalue (* (coerce mant ,type) (expt (coerce ,radix ,type) expo))))
     (if nega (- fvalue) fvalue)))



(defmacro biofa-stash (biofa-array float-size base fvalue)
  "
PRIVATE
"
  `(multiple-value-bind (mant expo sign) (integer-decode-float ,fvalue)
     (setq expo (+ (* 2 expo) (if (< sign 0) 1 0)))
     (setf (aref ,biofa-array ,base) expo)
     (incf ,base)
     (case ,float-size
       (1 (setf (aref ,biofa-array ,base) mant)
          (incf ,base))
       (2 (setf (aref ,biofa-array ,base) (ldb (byte 32  0) mant))
          (incf ,base)
          (setf (aref ,biofa-array ,base) (ldb (byte 32 32) mant))
          (incf ,base))
       (otherwise
        (do ((i 0 (1+ i))
             (b 0 (+ b 32)))
            ((< i ,float-size))
          (setf (aref ,biofa-array ,base) (ldb (byte 32 b) mant))
          (incf ,base))))))



(defun biofa-ref (biofa-array index)
  "
RETURN: The float at INDEX.
"
  (declare (type (array (signed-byte 32)) biofa-array)
           (type integer index))
  (assert (and (<= 0 index) (< index (biofa-count biofa-array))))
  (let* ((radix (biofa-radix biofa-array))
         (fs    (biofa-float-size biofa-array))
         (type  (or (cdr (assoc fs +float-types+)) 'long-float))
         (base  (+ +header-size+ (* (1+ fs) index))) )
    (biofa-fetch biofa-array radix fs type base)))



(defun biofa-setref (biofa-array index fvalue)
  "
POST:  (= (biofa-ref biofa-array index) fvalue)
"
  (declare (type (array (signed-byte 32)) biofa-array)
           (type integer index)
           (type float   fvalue))
  (assert (and (<= 0 index) (< index (biofa-count biofa-array))))
  (let* ((fs   (biofa-float-size biofa-array))
         (base (+ +header-size+ (* (1+ fs) index))))
    (biofa-stash biofa-array fs base fvalue)))



(defun biofa-encode (float-array)
  "
RETURN:  An array of (SIGNED-BYTE 32) or of containing
         the data from FLOAT-ARRAY.
"
  (declare (type (array float) float-array))
  (let* ((fs (apply (function max)
                    (map 'list (function float-size) float-array)))
         (size (+ +header-size+ (* (1+ fs) (length float-array))))
         (biofa  (make-array (list size) :element-type '(signed-byte 32)))
         (base +header-size+))
    (dotimes (i (length float-array))
      (let ((fvalue (aref float-array i)))
        (biofa-stash biofa fs base fvalue)))
    (biofa-set-radix      biofa (float-radix (aref float-array 0)))
    (biofa-set-float-size biofa fs)
    biofa))



(defun biofa-decode (biofa-array)
  "
RETURN: AN ARRAY OF FLOAT (DEPENDING ON BIOFA-FLOAT-SIZE AND +FLOAT-TYPE+)
        CONTAINING THE SAME VALUE AS ENCODED INTO BIOFA-ARRAY.
"
  (declare (type (array (signed-byte 32)) biofa-array))
  (let* ((radix (biofa-radix biofa-array))
         (fs    (biofa-float-size biofa-array))
         (type  (or (cdr (assoc fs +float-types+)) 'long-float))
         (base  +header-size+)                      
         (float-array (make-array (list (biofa-count biofa-array))
                                  :element-type type)))
    (dotimes (i (length float-array))
      (setf (aref float-array i) (biofa-fetch biofa-array radix fs type base)))
    float-array))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS

(defparameter lstest
  '( 0.0 1.0 -1.0 2.0 -2.0 3.141592 -3.141592
    1.0e1 -1.0e1 2.0e1 -2.0e1 3.141592e1 -3.141592e1
    1.0e-1 -1.0e-1 2.0e-1 -2.0e-1 3.141592e-1 -3.141592e-1
    1.0e20 -1.0e20 2.0e20 -2.0e20 3.141592e20 -3.141592e20
    1.0e-20 -1.0e-20 2.0e-20 -2.0e-20 3.141592e-20 -3.141592e-20))
(defparameter fstest (make-array (list (length lstest))
                                 :initial-contents lstest))
(defparameter ldtest
  '( 0.0d0 1.0d0 -1.0d0 2.0d0 -2.0d0
    3.1415926535897932385d0 -3.1415926535897932385d0
    1.0d1 -1.0d1 2.0d1 -2.0d1
    3.1415926535897932385d1 -3.1415926535897932385d1
    1.0d-1 -1.0d-1 2.0d-1 -2.0d-1
    3.1415926535897932385d-1 -3.1415926535897932385d-1
    1.0d20 -1.0d20 2.0d20 -2.0d20
    3.1415926535897932385d20 -3.1415926535897932385d20
    1.0d-20 -1.0d-20 2.0d-20 -2.0d-20
    3.1415926535897932385d-20 -3.1415926535897932385d-20))
(defparameter fdtest (make-array (list (length ldtest))
                                 :initial-contents ldtest))
(defvar estest)
(defvar dstest)
(defvar edtest)
(defvar ddtest)

(defun test ()
  (setq estest (biofa-encode fstest))
  (setq dstest (biofa-decode estest))
  (format t "~&SINGLE: ~A~%" (equalp dstest fstest))
  (setq edtest (biofa-encode fdtest))
  (setq ddtest (biofa-decode edtest))
  (format t "~&DOUBLE: ~A~%" (equalp ddtest fdtest))
  (assert (and (equalp dstest fstest)  (equalp ddtest fdtest))))



;;; <http://www.mail-archive.com/cmucl-help@cons.org/msg00547.html>.
;;; Please excuse the rather lame temporary filename generation :)
;;;
;;; (defpackage #:faslstore
;;;   (:export #:bindump #:binload)
;;;   (:nicknames #:fs)
;;;   (:use :cl))
;;;
;;; (in-package #:faslstore)
;;;
;;; (defparameter *hook* nil)
;;;
;;; (defun gentempname nil
;;;   "Generate a rather unlikely filename."
;;;   (format nil "~Afaslize.lisp" (get-universal-time)))
;;;
;;; (defun bindump (data fname)
;;;   (let ((tmp (gentempname)))
;;;     (setq *hook* data)
;;;     (with-open-file (str tmp
;;; 			 :direction :output
;;; 			 :if-exists :supersede) ;:error)
;;; 	(format str "(in-package #:faslstore)~%~
;;;                      (let (c)~%~
;;;                        (defun returner nil~%~
;;;                        (cond ((not c) (setf c t) '#.*hook*)~%~
;;;                              (t nil))))~%"))
;;;     (compile-file tmp :output-file fname)
;;;     (delete-file tmp)))
;;;
;;; (defun returner nil nil)
;;;
;;; (defun binload (fname)
;;;   (load fname)
;;;   (returner))
;;;
;;;

;;;; float-binio.lisp                 --                     --          ;;;;
