;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:                com.informatimago.tools.check-asdf.test.asd
;;;;LANGUAGE:            Common-Lisp
;;;;SYSTEM:              None
;;;;USER-INTERFACE:      None
;;;;DESCRIPTION:
;;;;
;;;;    This file defines the com.informatimago.tools.check-asdf.test system.
;;;;    Tests the com.informatimago.tools.check-asdf system.
;;;;
;;;;USAGE:
;;;;
;;;;AUTHORS:
;;;;    <PJB> Pascal J. Bourguignon
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

(asdf:defsystem "com.informatimago.tools.check-asdf.test"
  ;; system attributes:
  :description    "Tests the com.informatimago.tools.check-asdf system."
  :author         "Pascal J. Bourguignon"
  :maintainer     "Pascal J. Bourguignon"
  :licence        "AGPL3"
  ;; component attributes:
  :version        "1.0.0"
  :properties     ((#:author-email . "pjb@informatimago.com")
                   (#:date . "Winter 2015")
                   ((#:albert #:output-dir)
                    . "/tmp/documentation/com.informatimago.tools.check-asdf.test/")
                   ((#:albert #:formats) "docbook")
                   ((#:albert #:docbook #:template) . "book")
                   ((#:albert #:docbook #:bgcolor) . "white")
                   ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on     ("com.informatimago.common-lisp.cesarum"
                   "com.informatimago.tools.check-asdf")
  :components     ((:file "script-test" :depends-on nil)
                   (:file "dependency-cycles-test" :depends-on nil))
  :perform        (asdf/lisp-action:test-op
                   (operation system)
                   (declare (ignore operation system))
                   (let ((*package* (find-package "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES.TEST")))
                     (uiop/package:symbol-call "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES.TEST"
                                               "TEST/ALL"))
                   (let ((*package* (find-package "COM.INFORMATIMAGO.TOOLS.CHECK-ASDF.TEST")))
                     (uiop/package:symbol-call "COM.INFORMATIMAGO.TOOLS.CHECK-ASDF.TEST"
                                               "TEST/ALL"))
                   (let ((*package* (find-package "COM.INFORMATIMAGO.TOOLS.SCRIPT.TEST")))
                     (uiop/package:symbol-call "COM.INFORMATIMAGO.TOOLS.SCRIPT.TEST"
                                               "TEST/ALL"))))

;;;; THE END ;;;;
