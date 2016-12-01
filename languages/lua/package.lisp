;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the package for the LUA system.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or
;;;;    modify it under the terms of the GNU Affero General Public
;;;;    License as published by the Free Software Foundation, either
;;;;    version 3 of the License, or (at your option) any later
;;;;    version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.LANGUAGES.LUA.SCANNER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export
   "LUA-TOKEN"
   "TOKEN-VALUE"
   "TOK-COMMENT"
   "TOK-IDENTIFIER"
   "TOK-KEYWORD"
   "TOK-NUMBER"
   "TOK-SPECIAL"
   "TOK-STRING"
   "LUA-SCANNER" "LUA-SCANNER-KEEP-COMMENTS")
  (:documentation "

Implements a LUA scanner.

The LUA-SCANNER is a subclass of
COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER:SCANNER.

The LUA-TOKEN is a subclass of
COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER:TOKEN.


Copyright Pascal J. Bourguignon 2012 - 2012

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
"))



(defpackage "COM.INFORMATIMAGO.LANGUAGES.LUA.PARSER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.LANGUAGES.LUA.SCANNER"
        "COM.INFORMATIMAGO.RDP")
  (:export
   )
  (:documentation "

Implements a LUA parser.

The parser is generated with COM.INFORMATIMAGO.RDP.


Copyright Pascal J. Bourguignon 2012 - 2012

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.
"))



;;;; THE END ;;;;

