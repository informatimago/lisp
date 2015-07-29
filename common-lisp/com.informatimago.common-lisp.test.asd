;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:                com.informatimago.common-lisp.test.asd
;;;;LANGUAGE:            Common-Lisp
;;;;SYSTEM:              None
;;;;USER-INTERFACE:      None
;;;;DESCRIPTION:
;;;;
;;;;    This file defines the com.informatimago.common-lisp.test system.
;;;;    Tests the com.informatimago.common-lisp system.
;;;;
;;;;USAGE:
;;;;
;;;;AUTHORS:
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS:
;;;;    2015-02-23 <PJB> Created.
;;;;BUGS:
;;;;
;;;;LEGAL:
;;;;
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;
;;;;***************************************************************************

(asdf:defsystem "com.informatimago.common-lisp.test"
  ;; system attributes:
  :description    "Tests the com.informatimago.common-lisp system."
  :author         "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence        "AGPL3"
  ;; component attributes:
  :version        "1.0.4"
  :properties     ((#:author-email . "pjb@informatimago.com")
                   (#:date . "Winter 2015")
                   ((#:albert #:output-dir)
                    . "/tmp/documentation/com.informatimago.common-lisp.test/")
                   ((#:albert #:formats) "docbook")
                   ((#:albert #:docbook #:template) . "book")
                   ((#:albert #:docbook #:bgcolor) . "white")
                   ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on     ("com.informatimago.common-lisp.cesarum"
                   "com.informatimago.common-lisp")
  :components     ()
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op
                 (asdf:test-op "com.informatimago.common-lisp.lisp-sexp")
                 (asdf:test-op "com.informatimago.common-lisp.lisp-reader")
                 (asdf:test-op "com.informatimago.common-lisp.lisp-text")
                 (asdf:test-op "com.informatimago.common-lisp.cesarum")
                 (asdf:test-op "com.informatimago.common-lisp.picture")
                 (asdf:test-op "com.informatimago.common-lisp.arithmetic")
                 (asdf:test-op "com.informatimago.common-lisp.data-encoding")
                 (asdf:test-op "com.informatimago.common-lisp.heap")
                 (asdf:test-op "com.informatimago.common-lisp.html-base")
                 (asdf:test-op "com.informatimago.common-lisp.html-generator")
                 (asdf:test-op "com.informatimago.common-lisp.html-parser")
                 (asdf:test-op "com.informatimago.common-lisp.http")
                 (asdf:test-op "com.informatimago.common-lisp.bank")
                 (asdf:test-op "com.informatimago.common-lisp.csv")
                 (asdf:test-op "com.informatimago.common-lisp.diagram")
                 (asdf:test-op "com.informatimago.common-lisp.regexp")
                 (asdf:test-op "com.informatimago.common-lisp.ed")
                 (asdf:test-op "com.informatimago.common-lisp.graphviz")
                 (asdf:test-op "com.informatimago.common-lisp.invoice")
                 (asdf:test-op "com.informatimago.common-lisp.interactive")
                 (asdf:test-op "com.informatimago.common-lisp.parser")
                 (asdf:test-op "com.informatimago.common-lisp.rfc2822")
                 (asdf:test-op "com.informatimago.common-lisp.rfc3548")
                 ;; not yet (asdf:test-op "com.informatimago.common-lisp.telnet")
                 (asdf:test-op "com.informatimago.common-lisp.unix"))))

;;;; THE END ;;;;
