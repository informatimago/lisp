;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.rdp.basic.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Define the com.informatimago.rdp.basic system.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(asdf:defsystem :com.informatimago.rdp.basic

    ;; system attributes:
    
    :description "Methods to generate the parsers in BASIC using the Recursive Descent Parser Generator."

    :long-description "

Provides methods specialized on the :basic target-language, to emit a generator in BASIC.

"

    :author "Pascal J. Bourguignon <pjb@informatimago.com>"

    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    

    :licence "AGPL3"

    ;; component attributes:
    
    :name "Recursive Descent Parser Generator -- BASIC generator"

    :version "1.0.1"
    
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp.basic/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on ("com.informatimago.rdp")
    
    :components ((:file "rdp-basic-gen")))

;;;; THE END ;;;;
