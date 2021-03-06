;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to load com.informatimago libraries.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-12-23 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(in-package "COMMON-LISP-USER")

(asdf:defsystem "com.informatimago"
  ;; system attributes:
  :description  "Informatimago Systems Agregate"
  :long-description  "This system gathers most of the Informatimago systems."
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.6.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Summer 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp"
               "com.informatimago.clext"
               "com.informatimago.clmisc"
               "com.informatimago.rdp"
               "com.informatimago.tools"
               "com.informatimago.xcode"
               "com.informatimago.lispdoc"
               "com.informatimago.small-cl-pgms"
               "com.informatimago.languages"
               "com.informatimago.future"
               "com.informatimago.editor" ; future
               "com.informatimago.objcl"  ; empty shell on non-ccl darwin
               "com.informatimago.susv3"  ; empty shell on non-clisp.
               "com.informatimago.clisp"  ; empty shell on non-clisp linux
               )
  :components ()
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op (asdf:test-op "com.informatimago.test"))))


(defun test-all-com.informatimago ()
  (asdf:oos 'asdf:load-op :com.informatimago.test)
  (eval (read-from-string "
         (let ((com.informatimago.common-lisp.cesarum.simple-test::*current-test-name*       'test-all-com.informatimago)
               (com.informatimago.common-lisp.cesarum.simple-test::*current-test-parameters* '()))
           (com.informatimago.common-lisp.cesarum.simple-test:testing
             (asdf:oos 'asdf:test-op :com.informatimago.test)))")))

;;;; THE END ;;;;
