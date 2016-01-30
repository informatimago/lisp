;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               raw-memory.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Peek and Poke.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-11-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "linux"))
(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "LINUX"))
(defpackage "COM.INFORMATIMAGO.CLISP.RAW-MEMORY"
  (:documentation "Peek and Poke.")
  (:use "COMMON-LISP" "FFI")
  (:shadowing-import-from "FFI" "SIN")
  (:export
   "*LIBRARY*"
   "PEEK" "POKE" "DUMP"
   "WITH-SIGSEG-HANDLER"
   ;; The following are low-level function, not protected by signal handler.
   ;; Install you own!
   "PEEK-UINT8"  "PEEK-SINT8"  "POKE-UINT8"  "POKE-SINT8"
   "PEEK-UINT16" "PEEK-SINT16" "POKE-UINT16" "POKE-SINT16"
   "PEEK-UINT32" "PEEK-SINT32" "POKE-UINT32" "POKE-SINT32"
   "PEEK-UINT64" "PEEK-SINT64" "POKE-UINT64" "POKE-SINT64"))
(in-package  "COM.INFORMATIMAGO.CLISP.RAW-MEMORY")

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *library* "/usr/local/lib/libraw-memory.so"
   "The namestring of the pathname to the raw-memory library file.")) 

(defun install-signal-handler (signum handler)
  (let ((oldhan (linux:|set-signal-handler| signum handler))
        (sigset (second (multiple-value-list
                         (linux:|sigaddset| (second (multiple-value-list
                                                     (linux:|sigemptyset|)))
                                signum)))))
    (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset)
    (values signum oldhan sigset)))


(defun restore-signal-handler (signum oldhan sigset)
  (linux:|set-signal-handler| signum oldhan)
  (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset))

  

(defmacro with-signal-handler (signum handler &body body)
  (let ((voldhan (gensym))
        (vsignum (gensym))
        (vsigset (gensym)))
    `(let* ((,vsignum ,signum)
            (,voldhan (linux:|set-signal-handler| ,vsignum ,handler))
            (,vsigset (second (multiple-value-list
                               (linux:|sigaddset| 
                                      (second (multiple-value-list
                                               (linux:|sigemptyset|)))
                                      ,vsignum)))))
       (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset)
       (unwind-protect (progn ,@body)
         (linux:|set-signal-handler| ,vsignum ,voldhan)
         (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset)))))


(defmacro with-sigseg-handler (&body body)
  `(with-signal-handler linux:|SIGSEGV| 
     (lambda (signum) 
       (declare (ignore signum))
       (error "Got Segment Violation Signal while accessing raw memory"))
     ,@body))


;; Scalar:

(defmacro generate-peek-and-poke ()
  (loop with code = '()
     for size in '(8 16 32) ;; peek and poke of 64-bit don't work.
     for c-peek-name = (format nil "peek~D" size)
     for c-poke-name = (format nil "poke~D" size) do
     (loop for type in '(uint sint) 
        for l-peek-name = (intern (with-standard-io-syntax
                                    (format nil"PEEK-~A~D" type size))
                                  "COM.INFORMATIMAGO.CLISP.RAW-MEMORY")
        for l-poke-name = (intern (with-standard-io-syntax
                                    (format nil"POKE-~A~D" type size))
                                  "COM.INFORMATIMAGO.CLISP.RAW-MEMORY")
        for l-type      = (intern (with-standard-io-syntax
                                    (format nil"~A~D" type size))
                                  "FFI")
        do 
        (push `(ffi:def-call-out ,l-peek-name
                   (:name ,c-peek-name)
                 (:arguments (address ffi:ulong))
                 (:return-type ,l-type)
                 (:library  *library*) (:language :stdc)) code)
        (push `(ffi:def-call-out ,l-poke-name
                   (:name ,c-poke-name)
                 (:arguments (address ffi:ulong) (value ,l-type))
                 (:return-type nil)
                 (:library  *library*) (:language :stdc)) code))
     finally (return `(progn ,@ code))))



(eval-when (:load-toplevel :execute)
  (generate-peek-and-poke))


(defun peek-uint64 (address)
  (dpb (peek-uint32 (+ 4 address))
       (byte 32 32)
       (peek-uint32 address)))

(defun peek-sint64 (address)
  (dpb (peek-uint32 (+ 4 address))
       (byte 32 32)
       (peek-uint32 address)))

(defun poke-uint64 (address object)
  (poke-uint32      address  (ldb (byte 32  0) object))
  (poke-uint32 (+ 4 address) (ldb (byte 32 32) object)))

(defun poke-sint64 (address object)
  (poke-uint32      address  (ldb (byte 32  0) object))
  (poke-uint32 (+ 4 address) (ldb (byte 32 32) object)))



(defun get-function (type peek-or-poke)
  (case peek-or-poke
    ((:peek)
     (when (atom type) (error "Can't peek this type ~S" type))
     (case (first type)
       ((unsigned-byte)
        (case (second type)
          ((8)  (values (function peek-uint8 ) 1))
          ((16) (values (function peek-uint16) 2))
          ((32) (values (function peek-uint32) 4))
          ((64) (values (function peek-uint64) 8))
          (otherwise (error "Can't peek this type ~S" type))))
       ((signed-byte)
        (case (second type)
          ((8)  (values (function peek-sint8 ) 1))
          ((16) (values (function peek-sint16) 2))
          ((32) (values (function peek-sint32) 4))
          ((64) (values (function peek-sint64) 8))
          (otherwise (error "Can't peek this type ~S" type))))
       (otherwise (error "Can't peek this type ~S" type))))
    ((:poke)
     (when (atom type) (error "Can't poke this type ~S" type))
     (case (first type)
       ((unsigned-byte)
        (case (second type)
          ((8)  (values (function poke-uint8 ) 1))
          ((16) (values (function poke-uint16) 2))
          ((32) (values (function poke-uint32) 4))
          ((64) (values (function poke-uint64) 8))
          (otherwise (error "Can't poke this type ~S" type))))
       ((signed-byte)
        (case (second type)
          ((8)  (values (function poke-sint8 ) 1))
          ((16) (values (function poke-sint16) 2))
          ((32) (values (function poke-sint32) 4))
          ((64) (values (function poke-sint64) 8))
          (otherwise (error "Can't poke this type ~S" type))))
       (otherwise (error "Can't poke this type ~S" type))))
    (otherwise
     (error "PEEK-OR-POKE must be either :PEEK or :POKE, not ~S" 
            peek-or-poke))))


;; type: simtype | comtype.
;; simtype:
;;     (unsigned-byte  8)
;;     (signed-byte    8)
;;     (unsigned-byte 16)
;;     (signed-byte   16)
;;     (unsigned-byte 32)
;;     (signed-byte   32)
;;     (unsigned-byte 64)
;;     (signed-byte   64)
;;     single-float ; not implemented yet.
;;     double-float ; not implemented yet.
;; comtype:
;;     (array  simtype size)
;;     (vector simtype size)


(defun peek (address type)
  (with-signal-handler linux:|SIGSEGV| 
    (lambda (signum) 
      (declare (ignore signum))
      (error "Got Segment Violation Signal while peeking ~8,'0X" address))
    (if (and (listp type)
             (or (eq (first type) 'array)  (eq (first type) 'vector)))
        (multiple-value-bind (peek incr) (get-function (second type) :peek)
          (do ((data (make-array (list (third type))
                                 :element-type (second type)
                                 :initial-element 0))
               (address address (+ address incr))
               (i 0 (1+ i)))
              ((>= i (third type)) data)
            (setf (aref data i) (funcall peek address))))
        (funcall (get-function type :peek) address)))) ;;peek


(defun poke (address type value)
  (with-signal-handler linux:|SIGSEGV| 
    (lambda (signum) 
      (declare (ignore signum))
      (error "Got Segment Violation Signal while poking ~8,'0X" address))
    (if (and (listp type)
             (or (eq (first type) 'array)  (eq (first type) 'vector)))
        (multiple-value-bind (poke incr) (get-function (second type) :poke)
          (do ((address address (+ address incr))
               (i 0 (1+ i)))
              ((>= i (third type)) (values))
            (funcall poke address (aref value i))))
        (funcall (get-function type :poke) address value)))) ;;poke


(defun dump (address type &optional (stream t) (margin ""))
  (with-signal-handler linux:|SIGSEGV| 
    (lambda (signum) 
      (declare (ignore signum))
      (error "Got Segment Violation Signal while peeking ~8,'0X" address))
    (if (and (listp type)
             (or (eq (first type) 'array)  (eq (first type) 'vector)))
        (multiple-value-bind (peek incr) (get-function (second type) :peek)
          (do ((address address (+ address incr))
               (i 0 (1+ i)))
              ((>= i (third type)) (format stream "~&") (values))
            (when (zerop (mod i (/ 16 incr)))
              (format stream "~&~A~8,'0X: " margin (+ address i)))
            (format stream "~V,'0X " (* 2 incr) 
                    (funcall peek address))))
        (multiple-value-bind (peek incr) (get-function type :peek)
          (format stream "~&~A~8,'0X: ~V,'0X ~&"
                  margin address (* 2 incr)
                  (funcall peek address))))))




;; (in-package "COMMON-LISP-USER")
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (unless (find-package "LINUX")
;;     (warn "Package LINUX is not available.  Signal handling is disabled.")
;;     (defpackage "LINUX"
;;       (:use "COMMON-LISP")
;;       (:export "set-signal-handler" "sigaddset" "sigemptyset"
;;        "sigprocmask-set-n-save" "SIG_UNBLOCK" "SIGSEGV"))
;;     (in-package "LINUX")
;;     (defmacro null-op (name)
;;       `(defmacro ,name (&rest args) (declare (ignore args)) nil))
;;     (eval-when (:compile-toplevel :load-toplevel :execute)
;;       (null-op |set-signal-handler|)
;;       (null-op |sigaddset|)
;;       (null-op |sigemptyset|)
;;       (null-op |sigprocmask-set-n-save|)
;;       (defparameter |SIG_UNBLOCK| 0)
;;       (defparameter |SIGSEGV|     0))))
;; 
;; (in-package "COMMON-LISP-USER")

;;;; THE END ;;;;
