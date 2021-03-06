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
;;;;    Copyright Pascal J. Bourguignon 2010 - 2017
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")

;; #+(and ccl ccl-1.9)
;; (let ((initialized nil))
;;  (defun ccl::open-main-bundle ()
;;    (if initialized
;;        (#/mainBundle ns:ns-bundle)
;;        (let ((mainBundle (#/mainBundle ns:ns-bundle)))
;;          (#/initWithPath: mainBundle (namestring (truename ccl::*cocoa-ide-path*)))))))
;; #+(and ccl darwin)

(defpackage "COM.INFORMATIMAGO.OBJECTIVE-CL.MAC-ROMAN"
  (:nicknames  "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN")
  (:use "COMMON-LISP")
  (:export "MAC-ROMAN-CHAR-P" "MAC-ROMAN-STRING-P")
  (:documentation "
This package exports a function to check if a character is in the
Mac-Roman repertoire.

Copyright Pascal J. Bourguignon 2011 - 2017

This package is licensed under the GPL.
See source file for details.
"))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *oclo-symbol-names*
    '("*OBJC-DESCRIPTION-MAX-LENGTH*"
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
      "SEND-SUPER"
      "SEND-SUPER/STRET"
      "SEND"
      "SEND/STRET"
      "WITH-AUTORELEASE-POOL"
      "WITH-AUTORELEASED-NSSTRINGS"

      #-ccl-1.9 "*COCOA-APPLICATION-FRAMEWORKS*"

      "@"
      "DEFINE-CLASSNAME-TRANSLATION"
      "LISP-TO-OBJC-CLASSNAME"
      "LISP-TO-OBJC-MESSAGE"
      "OBJC-TO-LISP-CLASSNAME"
      "OBJC-TO-LISP-MESSAGE"
      "SLET"
      "UPDATE-OBJC-METHOD-INFO"))

  (defparameter *oclo-ccl-symbol-names*
    '("UPDATE-OBJC-METHOD-INFO" "SLET" "OBJC-TO-LISP-MESSAGE"
      "OBJC-TO-LISP-CLASSNAME" "LISP-TO-OBJC-MESSAGE"
      "LISP-TO-OBJC-CLASSNAME" "DEFINE-CLASSNAME-TRANSLATION" "@"
      "WITH-AUTORELEASED-NSSTRINGS" "WITH-AUTORELEASE-POOL"
      "SEND/STRET" "SEND" "SEND-SUPER/STRET" "SEND-SUPER"
      "OBJC-MESSAGE-SEND-SUPER-STRET" "OBJC-MESSAGE-SEND-SUPER"
      "OBJC-MESSAGE-SEND-STRET" "OBJC-MESSAGE-SEND"
      "MAKE-OBJC-INSTANCE" "LISP-STRING-FROM-NSSTRING" "DEFMETHOD"
      "DEFINE-OBJC-METHOD" "DEFINE-OBJC-CLASS-METHOD" "@SELECTOR"
      "@CLASS" "*OBJC-DESCRIPTION-MAX-LENGTH*"))

  (defparameter *oclo-objc-symbol-names*
    '("RETURNING-FOREIGN-STRUCT" "REMOVE-LISP-SLOTS" "OBJC-OBJECT"
      "OBJC-METACLASS" "OBJC-CLASS-OBJECT" "OBJC-CLASS" "MAKE-NSSTRING"
      "LOAD-FRAMEWORK"))

  #-(and)
  (dolist (s *oclo-symbol-names*)
    (cond
      ((find-symbol s "CCL")  (push s *oclo-ccl-symbol-names*))
      ((find-symbol s "OBJC") (push s *oclo-objc-symbol-names*))
      (t (error "~A is not found in OBJC or CCL" s))))

  ) ;;eval-when

(defpackage "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER"
   (:nicknames "COM.INFORMATIMAGO.OCLO"
               "OCLO")
   (:use "CL")

   #+(and ccl objc-support)
   (:shadowing-import-from "OBJC" . #.*oclo-objc-symbol-names*)

   #+(and ccl objc-support)
   (:shadowing-import-from "CCL"  . #.*oclo-ccl-symbol-names*)

   (:export . #.*oclo-symbol-names*)

   (:export
    "SELF" "SUPER"
    ;; implemented in oclo.lisp:
    "STRET"
    ;; implemented in oclo-<implementation>.lisp:
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
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.OBJCL.MAC-ROMAN")
  ;; also use "COM.INFORMATIMAGO.OBJECTIVE-C.LOWER" as "OCLO".
  (:export
   "*OBJECTIVE-CL-READTABLE*"
   "*OBJC-READTABLE*"
   "DISABLE-OBJCL-READER-MACROS"
   "ENABLE-OBJCL-READER-MACROS"
   "SET-OBJECTIVE-CL-SYNTAX" ; deprecated; use (enable-objc-reader-macros).
   "READ-ERROR" "READ-ERROR-CONTROL-STRING" "READ-ERROR-ARGUMENTS"
   "OBJC-DEFINITION-READER-MACRO"       ; #\@
   "OBJC-EXPRESSION-READER-MACRO"       ; #\[
   "@"                 ; macro to make NSString literals with unicode.
   "OBJC-STRING" "LISP-STRING" #|deprecated:|# "OBJCL-STRING"
   "YES" "NO")
  (:documentation "
This package exports a readtable with a couple of reader macros to
read Objective-C bracketed expressions, and @\"\" strings.
"))

;;;; THE END ;;;;
