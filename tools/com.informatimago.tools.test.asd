;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:                com.informatimago.tools.test.asd
;;;;LANGUAGE:            Common-Lisp
;;;;SYSTEM:              None
;;;;USER-INTERFACE:      None
;;;;DESCRIPTION:
;;;;
;;;;    This file defines the com.informatimago.tools.test system.
;;;;    Tests the com.informatimago.tools systems.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;
;;;;***************************************************************************

(asdf:defsystem "com.informatimago.tools.test"
  ;; system attributes:
  :description    "Tests the com.informatimago.tools system."
  :author         "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence        "AGPL3"
  ;; component attributes:
  :version        "1.2.0"
  :properties     ((#:author-email . "pjb@informatimago.com")
                   (#:date . "Winter 2015")
                   ((#:albert #:output-dir)
                    . "/tmp/documentation/com.informatimago.tools.test/")
                   ((#:albert #:formats) "docbook")
                   ((#:albert #:docbook #:template) . "book")
                   ((#:albert #:docbook #:bgcolor) . "white")
                   ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on     ("com.informatimago.tools.check-asdf.test"
                   "com.informatimago.tools.make-depends.test"
                   "com.informatimago.tools.manifest.test"
                   "com.informatimago.tools.pathname.test"
                   "com.informatimago.tools.quicklisp.test"
                   "com.informatimago.tools.source.test"
                   "com.informatimago.tools.summary.test"
                   "com.informatimago.tools.symbol.test"
                   "com.informatimago.tools.script.test"
                   "com.informatimago.tools.undefmethod.test")
  :components     ()
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  #+adsf3 :in-order-to #+adsf3 ((asdf:test-op
                 (asdf:test-op "com.informatimago.tools.check-asdf.test")
                 (asdf:test-op "com.informatimago.tools.make-depends.test")
                 (asdf:test-op "com.informatimago.tools.manifest.test")
                 (asdf:test-op "com.informatimago.tools.pathname.test")
                 (asdf:test-op "com.informatimago.tools.quicklisp.test")
                 (asdf:test-op "com.informatimago.tools.source.test")
                 (asdf:test-op "com.informatimago.tools.summary.test")
                 (asdf:test-op "com.informatimago.tools.symbol.test")
                 (asdf:test-op "com.informatimago.tools.script.test")
                 (asdf:test-op "com.informatimago.tools.undefmethod.test"))))

;;;; THE END ;;;;
