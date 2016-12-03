;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Define the rdp package.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-06-16 <PJB> Extracted defpackage form.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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
(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.RDP"
  (:use "COMMON-LISP"
        ;; "CL-STEPPER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CONSTRAINTS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
        )
  (:export "DEFGRAMMAR" "-->" "SEQ" "REP" "OPT" "ALT" "GRAMMAR-NAMED"
           "GENERATE-GRAMMAR"

           "GRAMMAR" "MAKE-GRAMMAR" "COPY-GRAMMAR"
           "REGISTER-GRAMMAR"
           "GRAMMAR-NAME" "GRAMMAR-TERMINALS" "GRAMMAR-START" "GRAMMAR-RULES"
           "GRAMMAR-ALL-TERMINALS" "GRAMMAR-ALL-NON-TERMINALS"
           "GRAMMAR-SKIP-SPACES"

           "FIND-RHSES" "FIND-RHS" "TERMINALP" "NON-TERMINAL-P"
           "FIRSTS-SET" "FOLLOW-SET" "NULLABLEP"
           "SENTENCE-FIRST-SET"

           "CLEAN-RULES"
           "NORMALIZE-GRAMMAR" "COMPUTE-FIRST-SETS" "COMPUTE-FOLLOW-SETS"

           "*NON-TERMINAL-STACK*"
           ;; Re-export form com.informatimago.common-lisp.parser.scanner:
           "TOKEN" "TOKEN-KIND" "TOKEN-TEXT" "TOKEN-LINE" "TOKEN-COLUMN"
           "*SPACE*" "WORD-EQUAL"
           "RDP-SCANNER"
           "SCANNER-LINE" "SCANNER-COLUMN" "SCANNER-STATE" "SCANNER-CURRENT-TOKEN"
           "SCANNER-SPACES" "SCANNER-TAB-WIDTH"
           "SKIP-SPACES" "SCAN-NEXT-TOKEN"
           "SCANNER-BUFFER" "SCANNER-CURRENT-TEXT"
           "SCANNER-END-OF-SOURCE-P" "ADVANCE-LINE" "ACCEPT"
           "PARSER-ERROR"
           "PARSER-ERROR-LINE"
           "PARSER-ERROR-COLUMN"
           "PARSER-ERROR-GRAMMAR"
           "PARSER-ERROR-SCANNER"
           "PARSER-ERROR-NON-TERMINAL-STACK"
           "PARSER-ERROR-FORMAT-CONTROL"
           "PARSER-ERROR-FORMAT-ARGUMENTS"
           "PARSER-END-OF-SOURCE-NOT-REACHED"
           ;; "PARSER-ERROR-UNEXPECTED-TOKEN"
           ;; "PARSER-ERROR-EXPECTED-TOKEN"
           "UNEXPECTED-TOKEN-ERROR"
           "UNEXPECTED-TOKEN-ERROR-EXPECTED-TOKENS"
           "UNEXPECTED-TOKEN-ERROR-NON-TERMINAL-STACK"
           )
  (:documentation "
This package implements a simple recursive descent parser.

Copyright Pascal J. Bourguignon 2006 - 2015

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
"))



;;;; THE END ;;;;
