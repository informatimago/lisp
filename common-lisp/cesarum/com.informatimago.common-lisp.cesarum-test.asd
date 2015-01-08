;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.cesarum-test.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to test the com.informatimago.common-lisp.cesarum library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-08 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

#+clisp
(unless custom:*ansi*
  (warn "System ~A: clisp should be used with -ansi or (setf custom:*ansi* t) in ~/.clisprc"
        :com.informatimago.common-lisp.cesarum))

(asdf:defsystem "com.informatimago.common-lisp.cesarum-test"

  ;; system attributes:
  
  :description  "Tests the cesarum library."

  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  
  :licence "AGPL3"

  ;; component attributes:
  
  :name "com.informatimago.common-lisp.cesarum-test"

  :version "1.3.3"

  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.cesarum-test/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp.cesarum")
  :perform (test-op (o s)
                    (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SET" "TEST/ALL")
                    (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.INDEX-SET" "TEST/ALL"))
  :components ((:file "set-test"       :depends-on ())
               (:file "index-set-test" :depends-on ("set-test"))))


;;;; THE END ;;;;
