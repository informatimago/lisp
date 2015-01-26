;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.rfc2822.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.rfc2822 library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2015
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

(asdf:defsystem :com.informatimago.common-lisp.rfc2822

    :description  "RFC0822/RFC2822 support funtions."

    :long-description "

A few RFC0822/RFC2822 utility fucntions.

"
        
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"

    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :licence "AGPL3"

    ;; component attributes:
    
    :name "Informatimago Common Lisp RFC2822 Utilities"
    
    :version "1.2.1"
    
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.rfc2822/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on ("com.informatimago.common-lisp.cesarum")
    
    :components ((:file "rfc2822" :depends-on ()))) 

;;;; THE END ;;;;
