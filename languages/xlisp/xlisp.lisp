;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               xlisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This is an implementation of xlisp 3.0 (1997) written as a
;;;;    Common Lisp package.
;;;;
;;;;    To keep it simple, we don't support continuations and
;;;;    environments, but they could be added in a later version.  We
;;;;    don't export internal functions either (whose names start with %).
;;;;
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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

(defpackage "COM.INFORMATIMAGO.XLISP"
  (:nickname "XLISP")
  (:use)
  (:import-from "COMMON-LISP"
                "T"
                "NIL"
                "*PACKAGE*"
                "*READTABLE*"
                "*STANDARD-INPUT*"
                "*STANDARD-OUTPUT*"
                "*ERROR-OUTPUT*"
                "*PRINT-CASE*"
                )
  (:export "T"
           "NIL"
           "*PACKAGE*"
           "*READTABLE*"
           "*STANDARD-INPUT*"
           "*STANDARD-OUTPUT*"
           "*ERROR-OUTPUT*"
           "*PRINT-CASE*"

           "*ERROR-HANDLER*"
           "*UNBOUND-HANDLER*"
           "*LOAD-PATH*"
           "*FIXNUM-FORMAT*"
           "*HEXNUM-FORMAT*"
           "*FLONUM-FORMAT*"
           "*SOFTWARE-TYPE*"
           "WIN95"
           "DOS32"
           "UNIX"
           "MAC"
           "OBJECT"
           "CLASS"


           ))

(defpackage "COM.INFORMATIMAGO.XLISP.IMPLEMENTATION"
  (:use "COMMON-LISP"))
(in-package "COM.INFORMATIMAGO.XLISP.IMPLEMENTATION")



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun sharp-f (stream ch)
    (declare (ignore stream ch))
    'nil)

  (defun sharp-t (stream ch)
    (declare (ignore stream ch))
    't)

  (defun sharp-exclaim (stream ch)
    (declare (ignore ch))
    (let ((*package* (find-package "KEYWORD")))
      (ecase (read stream)
        (:true     't)
        (:false    'nil)
        (:optional '&optional)
        (:rest     '&rest))))
  
  );;eval-when


(defmacro enable-xlisp-readtable ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-dispatch-macro-character #\# #\f 'sharp-f)
     (set-dispatch-macro-character #\# #\t 'sharp-t)
     (set-dispatch-macro-character #\# #\! 'sharp-exclaim)))


(define-condition xlisp-error (error)
  ((function :initarg :function :reader xlisp-error-function)
   (environment :initarg :environment :reader xlisp-error-environment))
  (:report (lambda (condition stream)
             (format stream "XLISP ERROR in function ~S, environment ~S"
                     (xlisp-error-function  condition)
                     (xlisp-error-environment condition)))))


(defun default-error-handler (erroneous-function error-environment)
  (error 'xlisp-error
         :function erroneous-function
         :environment error-environment))


(defvar xlisp:*error-handler*   (function default-error-handler)
  "Bound to a function to handle errors.  The function should take two
arguments, the function where the error occured and the environment at
the time of the error.  It shouldn't return.")


(defun default-unbound-handler (symbol continuation)
  (error "~S NOT IMPLEMENTED YET" 'default-unbound-handler))

(defvar xlisp:*unbound-handler* (function default-unbound-handler)
  "Bound to a function to handle unbound symbol errors.  The function
should take two arguments, the symbol that is unbound and a
continuation to call after correcting the error.")

;; TODO: See what format environment variable XLISP has?
(defvar xlisp:*load-path* (or (when (getenv "XLISP") (read-from-string (getenv "XLISP")))
                              (when *load-pathname* (list *load-pathname*))
                              '("."))
  "Bound to the path used by the LOAD function.  This is initialized to
the contents of the XLISP environment variable or, if that is not
defined, to the path where the XLISP executable was found.  The value
consists of a list of strings that should be paths to directories
XLISP should search for files being loaded.  Each string should end
with an appropriate directory terminator (the backslash under MS-DOS,
the slash under UNIX or a colon on the Macintosh.")


(defvar xlisp:*FIXNUM-FORMAT* "%ld"
  "A printf style format string for printing fixed point numbers.
FIXNUMs are generally represented by long integers so this should
usually be set to \"%ld\".")


(defvar xlisp:*HEXNUM-FORMAT* "%lx"
  "A printf style format string for printing fixed point numbers in
hexadecimal.  FIXNUMs are generally represented by long integers so
this should usually be set to \"%lx\".")

(defvar xlisp:*FLONUM-FORMAT* "%.15g"
  "A printf style format string for printing floating point numbers.
This is usually set to \"%.15g\".")


