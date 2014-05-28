;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.rdp.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the com.informatimago.rdp system.
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

(asdf:defsystem :com.informatimago.rdp

    ;; system attributes:
    
    :description "A simple Recursive Descent Parser generator."

    :long-description "

A simple Recursive Descent Parser generator.
The client may define methods to generate the code of the parser in
different languages than lisp.

"
    
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :licence "AGPL3"

    ;; component attributes:
    
    :name "Recursive Descent Parser Generator"
    
    :version "1.0.2"

    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.rdp/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on ("cl-ppcre" 
                 "com.informatimago.common-lisp.cesarum"
                 "com.informatimago.common-lisp.parser")
    
    :components ((:file "packages")
                 (:file "rdp"                  :depends-on ("packages"))
                 (:file "rdp-lisp-boilerplate" :depends-on ("packages" "rdp"))
                 (:file "rdp-macro"            :depends-on ("packages" "rdp"))))

;;;; THE END ;;;;
