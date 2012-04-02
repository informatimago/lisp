;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.lisp-reader.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.lisp-reader library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-03 <PJB> Added package.lisp (improved version of Xach Beane's zpack).
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

(asdf:defsystem :com.informatimago.common-lisp.lisp-reader
    :name "com.informatimago.common-lisp.lisp-reader"
    :description  "This package implements a standard Common Lisp reader."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.lisp-reader/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.common-lisp.lisp-sexp)
    :components ((:file "reader"          :depends-on ())
                 (:file "package-fun"     :depends-on ())
                 (:file "package-mac"     :depends-on ("package-fun"))
                 (:file "package-def"     :depends-on ("package-fun" "package-mac"))))

;;;; THE END ;;;;
