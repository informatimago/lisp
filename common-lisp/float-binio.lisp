;;;; -*- coding:utf-8 -*-
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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.FLOAT-BINIO"
  (:USE "COMMON-LISP")
  (:DOCUMENTATION
   "This package encodes and decodes arrays of float into arrays
    of signed-byte 32 in order to do binary I/O.
    BUGS: Handling of SHORT-FLOAT and LONG-FLOAT is not complete.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:EXPORT "BIOFA-DECODE" "BIOFA-ENCODE" "BIOFA-SETREF" "BIOFA-REF"
           "BIOFA-COUNT"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.FLOAT-BINIO")




;;; (DEFPARAMETER SEQ (MAKE-ARRAY '(1000)
;;;                               :ELEMENT-TYPE '(SIGNED-BYTE 32)
;;;                               :INITIAL-ELEMENT 123456789));;SEQ

;;; (TIME
;;;   (WITH-OPEN-FILE (OUT "/tmp/file.dat"
;;;                        :DIRECTION :OUTPUT
;;;                        :IF-EXISTS :SUPERSEDE :IF-DOES-NOT-EXIST :CREATE
;;;                        :ELEMENT-TYPE '(SIGNED-BYTE 32))
;;;     (DOTIMES (I 1000)
;;;       (WRITE-SEQUENCE SEQ OUT))))


;;; (TIME
;;;   (WITH-OPEN-FILE (IN  "/tmp/file.dat"
;;;                        :DIRECTION :INPUT
;;;                        :IF-DOES-NOT-EXIST :CREATE
;;;                        :ELEMENT-TYPE '(SIGNED-BYTE 32))
;;;     (DOTIMES (I 1000)
;;;       (READ-SEQUENCE SEQ IN))))


;; integer-decode-float


(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFCONSTANT  +HEADER-SIZE+ 2 "NUMBER OF HEADER ELEMENTS IN A BIOFA ARRAY")
  );;eval-when

(DEFUN BIOFA-FLOAT-SIZE (BIOFA-ARRAY)
  "
PRIVATE
RETURN: THE NUMBER OF (SIGNED-BYTE 32) NEEDED TO STORE THE MANTISSA
        OF THE FLOATS STORED IN BIOFA-ARRAY.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (AREF BIOFA-ARRAY 0)
  ) ;;BIOFA-FLOAT-SIZE



(DEFUN BIOFA-SET-FLOAT-SIZE (BIOFA-ARRAY FLOAT-SIZE)
  "
PRIVATE
POST: (= (BIOFA-FLOAT-SIZE BIOFA-ARRAY) FLOAT-SIZE)
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (SETF (AREF BIOFA-ARRAY 0) FLOAT-SIZE)
  ) ;;BIOFA-SET-FLOAT-SIZE



(DEFUN BIOFA-RADIX (BIOFA-ARRAY)
  "
PRIVATE
RETURN: The radix used to decode/encode that BIOFA-ARRAY.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (AREF BIOFA-ARRAY 1)
  ) ;;BIOFA-RADIX



(DEFUN BIOFA-SET-RADIX (BIOFA-ARRAY RADIX)
  "
PRIVATE
RETURN: The radix used to decode/encode that BIOFA-ARRAY.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (SETF (AREF BIOFA-ARRAY 1) RADIX)
  ) ;;BIOFA-SET-RADIX



(DEFUN FLOAT-SIZE (FVALUE)
  "
PRIVATE
RETURN: THE NUMBER OF (SIGNED-BYTE 32) NEEDED TO STORE THE MANTISSA OF FVALUE.
"
  (VALUES (TRUNCATE (+ 31 (FLOAT-PRECISION FVALUE)) 32))
  ) ;;FLOAT-SIZE



(DEFUN BIOFA-COUNT (BIOFA-ARRAY)
  "
RETURN: The number of floats encoded into the biofa-array.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (/ (- (LENGTH BIOFA-ARRAY)  +HEADER-SIZE+)
     (1+ (BIOFA-FLOAT-SIZE BIOFA-ARRAY)))
  ) ;;BIOFA-COUNT



(defvar +FLOAT-TYPES+ '((1 . SINGLE-FLOAT) (2 . DOUBLE-FLOAT))
  ;; SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT
  "AN A-LIST MAPPING FLOAT-SIZE TO FLOAT TYPE.") ;;+FLOAT-TYPES+



(DEFMACRO BIOFA-FETCH (BIOFA-ARRAY RADIX FLOAT-SIZE TYPE BASE)
  "
PRIVATE
"
  `(LET* ((EXPS (PROG1 (AREF ,BIOFA-ARRAY ,BASE) (INCF ,BASE)))
          (NEGA (ODDP EXPS))
          (EXPO (TRUNCATE (- EXPS (IF NEGA 1 0)) 2))
          (MANT (CASE ,FLOAT-SIZE
                  (1 (PROG1 (AREF ,BIOFA-ARRAY ,BASE) (INCF ,BASE)))
                  (2 (LET ((L (PROG1 (AREF ,BIOFA-ARRAY ,BASE) (INCF ,BASE)))
                           (H (PROG1 (AREF ,BIOFA-ARRAY ,BASE) (INCF ,BASE))))
                       (DPB H (BYTE 32 32) L) ))
                  (OTHERWISE
                   (DO ((I 0 (1+ I))
                        (B 0 (+ B 32))
                        (M)
                        (R 0))
                       ((< I FS) R)
                     (SETQ M (AREF ,BIOFA-ARRAY ,BASE))
                     (INCF ,BASE)
                     (DPB M (BYTE 32 B) R)))))
          (FVALUE (* (COERCE MANT ,TYPE) (EXPT (COERCE ,RADIX ,TYPE) EXPO))))
     (IF NEGA (- FVALUE) FVALUE))
  ) ;;BIOFA-FETCH



(DEFMACRO BIOFA-STASH (BIOFA-ARRAY FLOAT-SIZE BASE FVALUE)
  "
PRIVATE
"
  `(MULTIPLE-VALUE-BIND (MANT EXPO SIGN) (INTEGER-DECODE-FLOAT ,FVALUE)
     (SETQ EXPO (+ (* 2 EXPO) (IF (< SIGN 0) 1 0)))
     (SETF (AREF ,BIOFA-ARRAY ,BASE) EXPO)
     (INCF ,BASE)
     (CASE ,FLOAT-SIZE
       (1 (SETF (AREF ,BIOFA-ARRAY ,BASE) MANT)
          (INCF ,BASE))
       (2 (SETF (AREF ,BIOFA-ARRAY ,BASE) (LDB (BYTE 32  0) MANT))
          (INCF ,BASE)
          (SETF (AREF ,BIOFA-ARRAY ,BASE) (LDB (BYTE 32 32) MANT))
          (INCF ,BASE))
       (OTHERWISE
        (DO ((I 0 (1+ I))
             (B 0 (+ B 32)))
            ((< I ,FLOAT-SIZE))
          (SETF (AREF ,BIOFA-ARRAY ,BASE) (LDB (BYTE 32 B) MANT))
          (INCF ,BASE)))))
  ) ;;BIOFA-STASH



(DEFUN BIOFA-REF (BIOFA-ARRAY INDEX)
  "
RETURN: The float at INDEX.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY)
           (TYPE INTEGER INDEX))
  (ASSERT (AND (<= 0 INDEX) (< INDEX (BIOFA-COUNT BIOFA-ARRAY))))
  (LET* ((RADIX (BIOFA-RADIX BIOFA-ARRAY))
         (FS    (BIOFA-FLOAT-SIZE BIOFA-ARRAY))
         (TYPE  (OR (CDR (ASSOC FS +FLOAT-TYPES+)) 'LONG-FLOAT))
         (BASE  (+ +HEADER-SIZE+ (* (1+ FS) INDEX))) )
    (BIOFA-FETCH BIOFA-ARRAY RADIX FS TYPE BASE))
  ) ;;BIOFA-REF



(DEFUN BIOFA-SETREF (BIOFA-ARRAY INDEX FVALUE)
  "
POST:  (= (biofa-ref biofa-array index) fvalue)
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY)
           (TYPE INTEGER INDEX)
           (TYPE FLOAT   FVALUE))
  (ASSERT (AND (<= 0 INDEX) (< INDEX (BIOFA-COUNT BIOFA-ARRAY))))
  (LET* ((FS   (BIOFA-FLOAT-SIZE BIOFA-ARRAY))
         (BASE (+ +HEADER-SIZE+ (* (1+ FS) INDEX))))
    (BIOFA-STASH BIOFA-ARRAY FS BASE FVALUE))
  ) ;;BIOFA-SETREF

  
 
(DEFUN BIOFA-ENCODE (FLOAT-ARRAY)
  "
RETURN:  An array of (SIGNED-BYTE 32) or of containing
         the data from FLOAT-ARRAY.
"
  (DECLARE (TYPE (ARRAY FLOAT) FLOAT-ARRAY))
  (LET* ((FS (APPLY (FUNCTION MAX)
                    (MAP 'LIST (FUNCTION FLOAT-SIZE) FLOAT-ARRAY)))
         (SIZE (+ +HEADER-SIZE+ (* (1+ FS) (LENGTH FLOAT-ARRAY))))
         (BIOFA  (MAKE-ARRAY (LIST SIZE) :ELEMENT-TYPE '(SIGNED-BYTE 32)))
         (BASE +HEADER-SIZE+))
    (DOTIMES (I (LENGTH FLOAT-ARRAY))
      (LET ((FVALUE (AREF FLOAT-ARRAY I)))
        (BIOFA-STASH BIOFA FS BASE FVALUE)))
    (BIOFA-SET-RADIX      BIOFA (FLOAT-RADIX (AREF FLOAT-ARRAY 0)))
    (BIOFA-SET-FLOAT-SIZE BIOFA FS)
    BIOFA)
  ) ;;BIOFA-ENCODE



(DEFUN BIOFA-DECODE (BIOFA-ARRAY)
  "
RETURN: AN ARRAY OF FLOAT (DEPENDING ON BIOFA-FLOAT-SIZE AND +FLOAT-TYPE+)
        CONTAINING THE SAME VALUE AS ENCODED INTO BIOFA-ARRAY.
"
  (DECLARE (TYPE (ARRAY (SIGNED-BYTE 32)) BIOFA-ARRAY))
  (LET* ((RADIX (BIOFA-RADIX BIOFA-ARRAY))
         (FS    (BIOFA-FLOAT-SIZE BIOFA-ARRAY))
         (TYPE  (OR (CDR (ASSOC FS +FLOAT-TYPES+)) 'LONG-FLOAT))
         (BASE  +HEADER-SIZE+)                      
         (FLOAT-ARRAY (MAKE-ARRAY (LIST (BIOFA-COUNT BIOFA-ARRAY))
                                  :ELEMENT-TYPE TYPE)))
    (DOTIMES (I (LENGTH FLOAT-ARRAY))
      (SETF (AREF FLOAT-ARRAY I) (BIOFA-FETCH BIOFA-ARRAY RADIX FS TYPE BASE))
      )
    FLOAT-ARRAY)
  ) ;;BIOFA-DECODE




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS

(DEFPARAMETER LSTEST
  '( 0.0 1.0 -1.0 2.0 -2.0 3.141592 -3.141592
    1.0E1 -1.0E1 2.0E1 -2.0E1 3.141592E1 -3.141592E1
    1.0E-1 -1.0E-1 2.0E-1 -2.0E-1 3.141592E-1 -3.141592E-1
    1.0E20 -1.0E20 2.0E20 -2.0E20 3.141592E20 -3.141592E20
    1.0E-20 -1.0E-20 2.0E-20 -2.0E-20 3.141592E-20 -3.141592E-20
    )) ;;LSTEST
(DEFPARAMETER FSTEST (MAKE-ARRAY (LIST (LENGTH LSTEST))
                                 :INITIAL-CONTENTS LSTEST))
(DEFPARAMETER LDTEST
  '( 0.0d0 1.0d0 -1.0d0 2.0d0 -2.0d0
    3.1415926535897932385d0 -3.1415926535897932385d0
    1.0D1 -1.0D1 2.0D1 -2.0D1
    3.1415926535897932385D1 -3.1415926535897932385D1
    1.0D-1 -1.0D-1 2.0D-1 -2.0D-1
    3.1415926535897932385D-1 -3.1415926535897932385D-1
    1.0D20 -1.0D20 2.0D20 -2.0D20
    3.1415926535897932385D20 -3.1415926535897932385D20
    1.0D-20 -1.0D-20 2.0D-20 -2.0D-20
    3.1415926535897932385D-20 -3.1415926535897932385D-20
    )) ;;LDTEST
(DEFPARAMETER FDTEST (MAKE-ARRAY (LIST (LENGTH LDTEST))
                                 :INITIAL-CONTENTS LDTEST))
(defvar estest)
(defvar dstest)
(defvar edtest)
(defvar ddtest)

(defun test ()
  (SETQ ESTEST (BIOFA-ENCODE FSTEST))
  (SETQ DSTEST (BIOFA-DECODE ESTEST))
  (FORMAT T "~&SINGLE: ~A~%" (EQUALP DSTEST FSTEST))
  (SETQ EDTEST (BIOFA-ENCODE FDTEST))
  (SETQ DDTEST (BIOFA-DECODE EDTEST))
  (FORMAT T "~&DOUBLE: ~A~%" (EQUALP DDTEST FDTEST))
  (assert (and (EQUALP DSTEST FSTEST)  (EQUALP DDTEST FDTEST)))
  ) ;;test



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
