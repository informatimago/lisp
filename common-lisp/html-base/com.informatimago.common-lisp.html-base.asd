;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.html-base.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.html-base library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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

(asdf:defsystem :com.informatimago.common-lisp.html-base

    ;; system attributes:
    
    :description  "Lisp description of the HTML 4.01 standard and entities."

    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"

    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"

    :licence "AGPL3"

    
    ;; component attributes:

    :name "Informatimago Common Lisp HTML Base Utilities"

    :version "1.2.1"

    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.html-base/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on ("com.informatimago.common-lisp.cesarum"
                 "com.informatimago.common-lisp.lisp-sexp")
    
    :components ((:file "html-entities"  :depends-on ())
                 ;; The html401.lisp file is designed to be loaded in various packages
                 ;; where different definitions of the DEFELEMENT and DEFATTRIBUTE macros
                 ;; will process it.
                 ;; (:file "html401"        :depends-on ())
                 ))

;;;; THE END ;;;;
