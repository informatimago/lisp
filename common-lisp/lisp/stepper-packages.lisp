;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               stepper-packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the Common Lisp Stepper packages.
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


;; We define an internal package that uses COMMON-LISP to easily
;; define functions and macros, since the CL-STEPPER package shadows
;; all the special operators and defun, defgeneric and defmacro
;; macros.
;;
;; The CL-STEPPER package only contains those shadowed macro
;; definitions.


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERNAL"
  
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")

  (:export
   "*STEP-PACKAGE*"
   "*STEP-PRINT-READABLY*" 
   "*STEP-PRINT-LENGTH*"   
   "*STEP-PRINT-LEVEL*"     
   "*STEP-PRINT-CASE*"    

   "STEP-TRACE" "STEP-UNTRACE"
   "STEP-BREAK-ENTRY" "STEP-UNBREAK-ENTRY"
   "STEP-BREAK-EXIT" "STEP-UNBREAK-EXIT"

   "*STEP-MODE*" "*STEP-LEVEL*"

   "WILL-STEP" "DID-BIND" "PRINT-STEP-RESULTS" "DID-STEP" "DID-TAG"
   
   "STEP-CONDITION" "STEP-MESSAGE" "STEP-CHOICE"

   "SUBSTITUTE-IGNORABLE"
   "SIMPLE-STEP"
   "STEP-EXPRESSION"
   "STEP-BODY"
   "STEP-FUNCTION"
   "STEP-LAMBDA"
   "STEP-BINDINGS")
  
  (:documentation "
An internal package of the Common Lisp stepper.
This package exports the  stepper generator functions,
and defines stepper interactive functions (not exported).

BUG: we should probably design it with hooks so that client may define
     the stepping/tracing user interface.
"))


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER"

  (:nicknames "COMMON-LISP-STEPPER"
              "CL-STEPPER"
              "STEPPER")
  
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.INTERNAL")

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

  (:export
   "*STEP-MODE*"
   "*STEP-PRINT-LENGTH*"   
   "*STEP-PRINT-LEVEL*"     
   "*STEP-PRINT-CASE*"    
   "*STEP-PACKAGE*"

   "STEP-TRACE"       "STEP-UNTRACE"
   "STEP-BREAK-ENTRY" "STEP-UNBREAK-ENTRY"
   "STEP-BREAK-EXIT"  "STEP-UNBREAK-EXIT")
  
  (:documentation "
Implements a Common Lisp Stepper.

This package should be used instead of COMMON-LISP, and the code
recompiled or reloaded.  This will instrumentablize the functions so
that tracing and stepping is available.

To start running some code step-by-step, you can use:

    (step (some-expression))

Or you may use STEP-TRACE, to activate tracing of some functions (that
must have been compiled with CL-STEPPER), or STEP-BREAK-ENTRY or
STEP-BREAK-EXIT to enter the stepper upon entry or exit of the named
functions.

It is also possible to run the tracer on all code (that has been
compiled with CL-STEPPER) with:

   (setf *step-mode* :trace)

Reset it with:

   (setf *step-mode* :run)


The stepper menu is:

    Step Into (s, si, RET), Step over (so), Trace (t), Run (r), Debugger (d),
    Abort (a, q)?

Step Into:

    Continue evaluating each forms and subforms step by step.

Step Over:

    Evaluate the current form in one step. 

Trace:

    The code is executed, and all the instrumented code produces traces.

Run:

    The code is executed silently.

Debugger:

    The debugger is invoked with a STEP-CONDITION.  There are restarts
    installed to invoke all the stepper menu commands.

Abort:

    The evaluation of the STEP form is aborted.

With the Step Over Trace, and Run commands,  if a function with a
break-point or an active trace is reached, it will still enter the
stepper menu again, or trace it.

"))


;;;; THE END ;;;;
