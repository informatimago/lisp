;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.lisp.ibcl.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.lisp.ibcl library.
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

(asdf:defsystem :com.informatimago.common-lisp.lisp.ibcl

    ;; system attributes:
    
    :description  "IBCL and utilities."

    :long-description "

This system provides three packages:

COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE              keep sources sexps.
COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-SAVING-DEFINES   def* macro saving sources.
COM.INFORMATIMAGO.COMMON-LISP.LISP.IBCL                IBCL package.

"
    
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"

    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"

    :licence "AGPL3"

    
    ;; component attributes
    
    :name "Informatimago Common Lisp Lisp Language Stuff"

    :version "1.0.0"
    
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Summer 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.lisp.ibcl/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on ("com.informatimago.common-lisp.lisp-sexp")
    
    :components ((:file "source")
                 (:file "cl-saving-defines" :depends-on ("source"))
                 (:file "ibcl"              :depends-on ("source" "cl-saving-defines"))))

;;;; THE END ;;;;
