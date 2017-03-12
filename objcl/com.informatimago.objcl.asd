;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.objcl.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Reader macros and tools to program with Objective-C object libraries.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-22 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(asdf:defsystem "com.informatimago.objcl"
  ;; system attributes:
  :description "Reader macros to implement an Objective-CL syntax."
  :long-description "

Defines readers macros to provide an Objective-C -like syntax to wrap
over the Objective-C FFI.

Current implementation work only on ccl, but it should be extended to
cover generic FFI to both Apple and GNUstep objc2 runtimes.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence    "AGPL3"
  ;; component attributes:
  :version    "1.12.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2014")
               ((#:albert #:output-dir)          . "../documentation/com.informatimago.objc/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp.cesarum")
  :components ((:file "packages"           :depends-on ("objc-support"))
               (:file "objc-support"       :depends-on ())
               (:file "mac-roman"          :depends-on ("packages"))
               . #+(and ccl darwin)
               ((:file "oclo-ccl"           :depends-on ("packages" "objc-support"))
                (:file "oclo"               :depends-on ("packages" "objc-support" "oclo-ccl"))
                (:file "objcl"              :depends-on ("packages" "objc-support" "oclo" "mac-roman")))
               #-(and ccl darwin)
               ())
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op
                 (asdf:test-op "com.informatimago.objcl.test"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(and ccl darwin) (warn "System ~A is incomplete on ~A" :com.informatimago.objcl (lisp-implementation-type)))

;;;; THE END ;;;;
