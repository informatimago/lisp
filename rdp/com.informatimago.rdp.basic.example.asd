;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.rdp.basic.example.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Define the com.informatimago.rdp.basic.example system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-24 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(asdf:defsystem :com.informatimago.rdp.basic.example
    :name "An example of parser generated in BASIC with the Recursive Descent Parser Generator."
    :description "An example of parser generated in BASIC with the Recursive Descent Parser Generator."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "1.0.0"
    :licence "AGPLv3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp.basic.example/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("com.informatimago.rdp"
                 "com.informatimago.rdp.basic")
    :components ((:file "example-basic")))

;;;; THE END ;;;;
