;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.lisp-reader-test.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to test the com.informatimago.common-lisp.lisp-reader library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-25 <PJB> Created this .asd file.
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

(asdf:defsystem "com.informatimago.common-lisp.lisp-reader-test"

  ;; system attributes:
  
  :description  "Tests the lisp-reader library."

  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  
  :licence "AGPL3"

  ;; component attributes:
  
  :name "com.informatimago.common-lisp.lisp-reader-test"

  :version "1.0.0"

  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2015")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.lisp-reader-test/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp.lisp-reader")
  :perform (asdf:test-op
            (o s)
            (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST")))
              (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER.TEST" "TEST/ALL")))
  :components ((:file "reader-test"    :depends-on ())))


;;;; THE END ;;;;
