;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.data-encoding.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.data-encoding library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-05 <PJB> Added ecp.
;;;;    2010-12-09 <PJB> Added bencode.
;;;;    2010-10-31 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
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

(asdf:defsystem :com.informatimago.common-lisp.data-encoding
    :name "This library contains packages to encode and decode data in various ways."
    :description  "This ASDF system gathers various Common Lisp library packages."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.data-encoding/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.common-lisp.cesarum
                 :com.informatimago.common-lisp.arithmetic)
    :components ((:file "data-encoding" :depends-on ())
                 ;; (:file "data-encoding-test" :depends-on ())
                 (:file "bencode" :depends-on ())
                 (:file "ecp"     :depends-on ())))


;;;; THE END ;;;;
