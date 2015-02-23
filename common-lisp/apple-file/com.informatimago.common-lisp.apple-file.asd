;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.apple-file.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.apple-file library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-05-26 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(asdf:defsystem :com.informatimago.common-lisp.apple-file
    ;; system attributes:
    :description  "Apple-Single and Apple-Double File Formats."
    :long-description "

Read Apple-Single and Apple-Double file formats.

"
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    :licence "AGPL3"
    ;; component attributes:
    :version "1.0.0"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2013")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.apple-file/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :depends-on ("com.informatimago.common-lisp.cesarum")
    :components ((:file "apple-file" :depends-on ())))

;;;; THE END ;;;;
