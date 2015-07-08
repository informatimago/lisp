;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Extracted.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "COMMON-LISP-USER")

(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP")
  (:export
   "DEFINE-SCANNER"
   ;; TOKEN:
   "TOKEN" "TOKEN-KIND" "TOKEN-TEXT" "TOKEN-LINE" "TOKEN-COLUMN"
   "*SPACE*"
   ;; SCANNER:
   "SCANNER" "SCANNER-CURRENT-TOKEN" 
   "SCANNER-SOURCE" "SCANNER-LINE" "SCANNER-COLUMN" "SCANNER-STATE"
   "SCANNER-SPACES" "SCANNER-TAB-WIDTH"
   ;; SCANNER-ERROR condition:
   "SCANNER-ERROR" "SCANNER-ERROR-LINE" "SCANNER-ERROR-COLUMN"
   "SCANNER-ERROR-STATE" "SCANNER-ERROR-CURRENT-TOKEN"
   "SCANNER-ERROR-SCANNER"
   "SCANNER-ERROR-FORMAT-CONTROL" "SCANNER-ERROR-FORMAT-ARGUMENTS"
   "SCANNER-ERROR-INVALID-CHARACTER"
   ;; SCANNER methods:
   "SKIP-SPACES" "SCAN-NEXT-TOKEN"
   ;; PEEK-STREAM methods specialized on SCANNER:
   "NEXTCHAR" "UNGETCHAR" "GETCHAR"
   "BUFFERED-SCANNER" "SCANNER-BUFFER" "SCANNER-CURRENT-TEXT"
   "ADVANCE-LINE"
   "SCANNER-END-OF-SOURCE-P"
   "SCANNER-END-OF-LINE-P"
   "SCANNER-CURRENT-TOKEN"
   "ACCEPT"
   "WORD-EQUAL"
   "GENERATE-SCANNER" "READLINE"
   "SCANNER-TOKEN-PACKAGE" "ADVANCE-LINE")
  (:documentation
   "
An abstract scanner class.

A method to the SCAN-NEXT-TOKEN generic function needs to be provided.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>
"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.PARSER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "PARSER" "PARSER-SCANNER"
           "PARSER-NEXT-TOKEN" "PARSER-TOKEN"
           "ADVANCE"
           "REPORT-ERROR"
           "PARSER-ERROR"
           "PARSER-ERROR-PARSER"
           "PARSER-ERROR-TOKEN"
           "PARSER-ERROR-NEXT-TOKEN"
           "PARSER-ERROR-FORMAT-CONTROL"
           "PARSER-ERROR-FORMAT-ARGUMENTS")
  (:documentation "

An abstract parser class.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>



"))

;;;; THE END ;;;;
