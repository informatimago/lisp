;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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
;;;;**************************************************************************

#+(and ccl ccl-1.9)
(let ((initialized nil))
 (defun ccl::open-main-bundle ()
   (if initialized
       (#/mainBundle ns:ns-bundle)
       (let ((mainBundle (#/mainBundle ns:ns-bundle)))
         (#/initWithPath: mainBundle (namestring (truename ccl::*cocoa-ide-path*)))))))

#+ccl (eval-when (:compile-toplevel :load-toplevel :execute)
        (require :cocoa))


(defpackage "COM.INFORMATIMAGO.SIMPLE-TEST"
  (:use "CL")
  (:export "*DEBUG-ON-ERROR*" "WITH-DEBUGGER-ON-ERROR"
           "DEFINE-TEST" "TEST")
  (:documentation "
This package defines a simple test tool.
"))


(defpackage "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER"
  (:nicknames "COM.INFORMATIMAGO.OCLO"
              "OCLO")
  (:use "CL")

  #+ccl
  (:shadowing-import-from "OBJC"
                          "*OBJC-DESCRIPTION-MAX-LENGTH*"
                          "@CLASS"
                          "@SELECTOR" 
                          "DEFINE-OBJC-CLASS-METHOD"
                          "DEFINE-OBJC-METHOD"
                          "DEFMETHOD" 
                          "LISP-STRING-FROM-NSSTRING"
                          "LOAD-FRAMEWORK"
                          "MAKE-NSSTRING" 
                          "MAKE-OBJC-INSTANCE"
                          "OBJC-CLASS"
                          "OBJC-CLASS-OBJECT" 
                          "OBJC-MESSAGE-SEND"
                          "OBJC-MESSAGE-SEND-STRET" 
                          "OBJC-MESSAGE-SEND-SUPER"
                          "OBJC-MESSAGE-SEND-SUPER-STRET" 
                          "OBJC-METACLASS"
                          "OBJC-OBJECT"
                          "REMOVE-LISP-SLOTS" 
                          "RETURNING-FOREIGN-STRUCT"
                          "SEND"
                          "SEND-SUPER" 
                          "SEND-SUPER/STRET"
                          "SEND/STRET"
                          "WITH-AUTORELEASE-POOL"
                          "WITH-AUTORELEASED-NSSTRINGS")

  #+ccl
  (:shadowing-import-from "CCL"
                          "*COCOA-APPLICATION-FRAMEWORKS*" 
                          "@"
                          "DEFINE-CLASSNAME-TRANSLATION"
                          "LISP-TO-OBJC-CLASSNAME"
                          "LISP-TO-OBJC-MESSAGE"
                          "OBJC-TO-LISP-CLASSNAME"
                          "OBJC-TO-LISP-MESSAGE"
                          "SLET"
                          "UPDATE-OBJC-METHOD-INFO")

  (:export
   "SELF" "SUPER"
   
   ;; from objc.
   "*OBJC-DESCRIPTION-MAX-LENGTH*"
   "@CLASS"
   "@SELECTOR" 
   "DEFINE-OBJC-CLASS-METHOD"
   "DEFINE-OBJC-METHOD"
   "DEFMETHOD" 
   "LISP-STRING-FROM-NSSTRING"
   "LOAD-FRAMEWORK"
   "MAKE-NSSTRING" 
   "MAKE-OBJC-INSTANCE"
   "OBJC-CLASS"
   "OBJC-CLASS-OBJECT" 
   "OBJC-MESSAGE-SEND"
   "OBJC-MESSAGE-SEND-STRET" 
   "OBJC-MESSAGE-SEND-SUPER"
   "OBJC-MESSAGE-SEND-SUPER-STRET" 
   "OBJC-METACLASS"
   "OBJC-OBJECT"
   "REMOVE-LISP-SLOTS" 
   "RETURNING-FOREIGN-STRUCT"
   "SEND"
   "SEND-SUPER" 
   "SEND-SUPER/STRET"
   "SEND/STRET"
   "SLET"
   "WITH-AUTORELEASE-POOL"
   "WITH-AUTORELEASED-NSSTRINGS"

   ;; from ccl.
   "*COCOA-APPLICATION-FRAMEWORKS*" 
   "@"
   "DEFINE-CLASSNAME-TRANSLATION"
   "LISP-TO-OBJC-CLASSNAME"
   "LISP-TO-OBJC-MESSAGE"
   "OBJC-TO-LISP-CLASSNAME"
   "OBJC-TO-LISP-MESSAGE"
   "UPDATE-OBJC-METHOD-INFO"

   ;; implemented in oclo.lisp
   "STRET"
   ;; implemented in oclo-<implementation>.lisp
   "LISP-TO-OBJC-CLASSNAME-P"
   "OBJC-TO-LISP-CLASSNAME-P"
   "*NULL*" "NULLP" 
   "SELECTOR")
  
  (:documentation "
This package exports low level Objective-C stuff,
basically the ccl Objective-C bridge, in a nifty
single package exporting all these symbols.
"))



(defpackage "COM.INFORMATIMAGO.OBJECTIVE-CL"
  (:nicknames "COM.INFORMATIMAGO.OBJCL"
              "OBJCL")
  (:use "CL" "COM.INFORMATIMAGO.SIMPLE-TEST")
  ;; also use "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER" as "OCLO".
  (:export
   "*OBJECTIVE-CL-READTABLE*"
   "DISABLE-OBJCL-READER-MACROS"
   "ENABLE-OBJCL-READER-MACROS"
   "SET-OBJECTIVE-CL-SYNTAX" ; deprecated; use (enable-objc-reader-macros).
   "READ-ERROR" "READ-ERROR-CONTROL-STRING" "READ-ERROR-ARGUMENTS"
   "OBJC-DEFINITION-READER-MACRO" ; #\@
   "OBJC-EXPRESSION-READER-MACRO" ; \#[
   "@" ; macro to make NSString literals with unicode.
   "OBJCL-STRING" "LISP-STRING"
   "YES" "NO")
  (:documentation "
This package exports a readtable with a couple of reader macros to
read Objective-C bracketed expressions, and @\"\" strings.
"))


;;;; THE END ;;;;
