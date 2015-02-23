;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:                com.informatimago.test.asd
;;;;LANGUAGE:            Common-Lisp
;;;;SYSTEM:              None
;;;;USER-INTERFACE:      None
;;;;DESCRIPTION:
;;;;
;;;;    This file defines the com.informatimago.test system.
;;;;    Tests the com.informatimago system.
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

(asdf:defsystem "com.informatimago.test"
  ;; system attributes:
  :description    "Tests the com.informatimago system."
  :author         "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence        "AGPL3"
  ;; component attributes:
  :version        "1.0.0"
  :properties     ((#:author-email . "pjb@informatimago.com")
                   (#:date . "Winter 2015")
                   ((#:albert #:output-dir)
                    . "/tmp/documentation/com.informatimago.test/")
                   ((#:albert #:formats) "docbook")
                   ((#:albert #:docbook #:template) . "book")
                   ((#:albert #:docbook #:bgcolor) . "white")
                   ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "com.informatimago"
               "com.informatimago.common-lisp.test"
               "com.informatimago.clext.test"
               "com.informatimago.clmisc.test"
               "com.informatimago.rdp.test"
               "com.informatimago.tools.test"
               "com.informatimago.xcode.test"
               "com.informatimago.lispdoc.test"
               "com.informatimago.small-cl-pgms.test"
               "com.informatimago.future.test"
               "com.informatimago.objcl.test" ; empty shell on non-ccl darwin
               "com.informatimago.susv3.test" ; empty shell on non-clisp.
               "com.informatimago.clisp.test" ; empty shell on non-clisp linux
               )
  :components  ()
  :in-order-to ((asdf:test-op
                 (asdf:test-op "com.informatimago.common-lisp.test")
                 (asdf:test-op "com.informatimago.clext.test")
                 (asdf:test-op "com.informatimago.clmisc.test")
                 (asdf:test-op "com.informatimago.rdp.test")
                 (asdf:test-op "com.informatimago.tools.test")
                 (asdf:test-op "com.informatimago.xcode.test")
                 (asdf:test-op "com.informatimago.lispdoc.test")
                 (asdf:test-op "com.informatimago.small-cl-pgms.test")
                 (asdf:test-op "com.informatimago.future.test")
                 (asdf:test-op "com.informatimago.objcl.test")
                 (asdf:test-op "com.informatimago.susv3.test")
                 (asdf:test-op "com.informatimago.clisp.test"))))

;;;; THE END ;;;;
