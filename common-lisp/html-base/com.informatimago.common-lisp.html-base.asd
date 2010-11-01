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

(asdf:defsystem :com.informatimago.common-lisp.html-base
    :name "com.informatimago.common-lisp.html-base"
    :description  "This ASDF system gathers various Common Lisp library packages."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.0"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Automn 2010")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.common-lisp.html-base/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.common-lisp.cesarum
                 :com.informatimago.common-lisp.lisp-sexp)
    :components ((:file "html-entities"  :depends-on ())
                 ;; The html401.lisp file is designed to be loaded in various packages
                 ;; where different definitions of the DEFELEMENT and DEFATTRIBUTE macros
                 ;; will process it.
                 ;; (:file "html401"        :depends-on ())
                 ))

;;;; THE END ;;;;
