;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The package definitions of the telnet REPL server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-13 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION"
  (:use "COMMON-LISP"
        "BABEL")
  (:export "DECODE-CHARACTER"
           "REPLACE-OCTETS-BY-STRING"))

(defpackage "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST")
  (:export "TEST/ALL"))

(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS"
        "TRIVIAL-GRAY-STREAMS"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.CLEXT.BABEL-EXTENSION")
  (:export "WITH-TELNET-ON-STREAM"
           "TELNET-STREAM"))

(defpackage "COM.INFORMATIMAGO.CLEXT.TELNET.REPL"
  (:use "COMMON-LISP"
        "BABEL"
        "USOCKET"
        "BORDEAUX-THREADS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.TELNET"
        ;; "com.informatimago.common-lisp.cesarum"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
        "COM.INFORMATIMAGO.CLEXT.TELNET.STREAM")
  (:export "REPL-SERVER"
           "REPL-SERVER-THREAD"
           "REPL-SERVER-PORT"
           "REPL-SERVER-INTERFACE"
           "REPL-SERVER-MAX-CLIENTS"
           "REPL-SERVER-CLIENT-THREADS"
           "START-REPL-SERVER" "STOP-REPL-SERVER"))

;;;; THE END ;;;;
