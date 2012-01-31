;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp libraries.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(asdf:defsystem :com.informatimago.common-lisp
    :description  "Various Common Lisp libraries."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.0"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :depends-on (:com.informatimago.common-lisp.lisp-sexp
                 :com.informatimago.common-lisp.lisp-reader
                 :com.informatimago.common-lisp.lisp-text
                 
                 ;; not yet  :com.informatimago.common-lisp.lisp

                 :com.informatimago.common-lisp.cesarum
                 :com.informatimago.common-lisp.picture
                 :com.informatimago.common-lisp.arithmetic
                 :com.informatimago.common-lisp.data-encoding
                 :com.informatimago.common-lisp.heap
                 
                 :com.informatimago.common-lisp.html-base
                 :com.informatimago.common-lisp.html-generator
                 :com.informatimago.common-lisp.html-parser
                 :com.informatimago.common-lisp.http

                 :com.informatimago.common-lisp.bank
                 :com.informatimago.common-lisp.csv
                 :com.informatimago.common-lisp.cxx
                 :com.informatimago.common-lisp.diagram
                 :com.informatimago.common-lisp.regexp
                 :com.informatimago.common-lisp.ed
                 :com.informatimago.common-lisp.graphviz
                 :com.informatimago.common-lisp.invoice
                 :com.informatimago.common-lisp.interactive
                 :com.informatimago.common-lisp.parser
                 :com.informatimago.common-lisp.rfc2822
                 :com.informatimago.common-lisp.rfc3548
                 :com.informatimago.common-lisp.unix))


;;;; THE END ;;;;
