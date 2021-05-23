;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.clext.telnet.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to load the Telnet REPL server.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-13 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(asdf:defsystem "com.informatimago.clext.telnet.repl.test"
  ;; system attributes:
  :description "Tests the Telnet REPL Server."
  :long-description "

This system tests the Telnet REPL Server.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2021")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.clext.telnet.repl.test/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("babel"
               "usocket"
               "bordeaux-threads"
               "trivial-gray-streams"
               "com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.interactive"
               "com.informatimago.common-lisp.telnet"
               "com.informatimago.clext.telnet.repl")
  :components ((:file "packages"        :depends-on ())

               (:file "telnet-stream"   :depends-on ("packages"))
               (:file "babel-extension" :depends-on ("packages"))
               (:file "telnet-repl"     :depends-on ("packages"
                                                     "telnet-stream"
                                                     "babel-extension"))

               (:file "babel-extension-test" :depends-on ("packages" "babel-extension"))
               (:file "test-stub-nvt"        :depends-on ("packages"))
               (:file "telnet-stream-test"   :depends-on ("packages"
                                                          "telnet-stream"
                                                          "test-stub-nvt"))))


;;;; THE END ;;;;
