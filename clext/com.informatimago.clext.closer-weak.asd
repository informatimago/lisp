;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clext.closer-weak.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to load the com.informatimago.clext.closer-weak library.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-12 <PJB> Created this .asd file.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

#-(or ccl clisp sbcl cmu)
(asdf:defsystem "com.informatimago.clext.closer-weak"
  ;; system attributes:
  :description "Dummy Informatimago Common Lisp Extensions: Closer-Weaks."
  :long-description "

This system requires weak references, which either
are not available on this implementation,
or it hasn't been ported to this implementation yet.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.2.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clext/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ()
  :components ())

#+(or ccl clisp sbcl cmu)
(asdf:defsystem "com.informatimago.clext.closer-weak"
  ;; system attributes:
  :description "Informatimago Common Lisp Extensions: Closer-Weak."
  :long-description "

Closer to Weak objects.
Implements the specification: <http://clisp.cons.org/impnotes/weak.html>
for Common Lisp implementations that have weak-pointers.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.2.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clext/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ()
  :components ((:file "closer-weak"))
  #+adsf3 :in-order-to #+adsf3 ((test-op (test-op "com.informatimago.clext.closer-weak.test")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


;;;; THE END ;;;;

