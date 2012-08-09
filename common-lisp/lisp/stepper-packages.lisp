;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               stepper-packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An internal packageo of the Common Lisp stepper.
;;;;    This package exports the stepper interactive functions,
;;;;    and the stepper generator functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-08-09 <PJB> Extracted from stepper.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERACTIVE"
  
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")

  (:export
   "*STEP-PACKAGE*"
   "*STEP-PRINT-LENGTH*"   
   "*STEP-PRINT-LEVEL*"     
   "*STEP-PRINT-READABLY*" 
   "*STEP-PRINT-CASE*"    


   "*STEP-MODE*" "*STEP-LEVEL*"

   "WILL-STEP" "DID-BIND" "PRINT-STEP-RESULTS" "DID-STEP" "DID-TAG"
   
   "STEP-CONDITION" "STEP-MESSAGE" "STEP-CHOICE"

   "SIMPLE-STEP"
   "STEP-EXPRESSION"
   "STEP-BODY"
   "STEP-FUNCTION"
   "STEP-LAMBDA"
   "STEP-BINDINGS")
  
  (:documentation "
An internal package of the Common Lisp stepper.
This package exports the stepper interactive functions,
and the stepper generator functions.
"))


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.COMMON-LISP-STEPPER"

  (:nicknames "COMMON-LISP-STEPPER"
              "CL-STEPPER"
              "STEPPER")
  
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERACTIVE")

  (:shadow ;; macros
   "DEFUN" "DEFGENERIC" "DEFMETHOD" "LAMBDA"
   "STEP")
  
  (:shadow ;; special operators
   "BLOCK" "CATCH" "EVAL-WHEN" "FLET" "FUNCTION" "GO" "IF" "LABELS" "LET" "LET*"
   "LOAD-TIME-VALUE" "LOCALLY" "MACROLET" "MULTIPLE-VALUE-CALL"
   "MULTIPLE-VALUE-PROG1" "PROGN" "PROGV" "QUOTE" "RETURN-FROM" "SETQ"
   "SYMBOL-MACROLET" "TAGBODY" "THE" "THROW" "UNWIND-PROTECT")
  
  (:export ;; everything from COMMON-LISP
   . #.(cl:let ((e '()))
         (cl:do-external-symbols (s "COMMON-LISP" e)
           (push (string s) e))))

  
  (:documentation "
Implements a Common Lisp stepper.
"))


;;;; THE END ;;;;
