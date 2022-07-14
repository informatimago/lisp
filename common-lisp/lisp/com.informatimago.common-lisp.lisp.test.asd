;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:                com.informatimago.common-lisp.lisp.test.asd
;;;;LANGUAGE:            Common-Lisp
;;;;SYSTEM:              None
;;;;USER-INTERFACE:      None
;;;;DESCRIPTION:
;;;;
;;;;    This file defines the com.informatimago.common-lisp.lisp.test system.
;;;;    Tests the com.informatimago.common-lisp.lisp system.
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

(asdf:defsystem "com.informatimago.common-lisp.lisp.test"
  ;; system attributes:
  :description    "Tests the com.informatimago.common-lisp.lisp system."
  :author         "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence        "AGPL3"
  ;; component attributes:
  :version        "1.2.0"
  :properties     ((#:author-email . "pjb@informatimago.com")
                   (#:date . "Winter 2015")
                   ((#:albert #:output-dir)
                    . "/tmp/documentation/com.informatimago.common-lisp.lisp.test/")
                   ((#:albert #:formats) "docbook")
                   ((#:albert #:docbook #:template) . "book")
                   ((#:albert #:docbook #:bgcolor) . "white")
                   ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on     ("com.informatimago.common-lisp.cesarum"
                   "com.informatimago.common-lisp.lisp")
  :components     ((:file "source-test")
                   (:file "stepper-test"))
  #+asdf3 :perform #+asdf3 (asdf:test-op
                            (operation system)
                            (declare (ignore operation system))
                            (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE.TEST")))
                              (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE.TEST"
                                                "TEST/ALL"))
                            (let ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.TEST")))
                              (uiop:symbol-call "COM.INFORMATIMAGO.COMMON-LISP.LISP.STEPPER.TEST"
                                                "TEST/ALL"))))

;;;; THE END ;;;;
