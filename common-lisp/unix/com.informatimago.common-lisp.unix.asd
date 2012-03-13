;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.unix.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.common-lisp.unix library.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-13 <PJB> Added "option".
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

(asdf:defsystem :com.informatimago.common-lisp.unix
    :name "com.informatimago.common-lisp.unix"
    :description  "This library implements access to a few unix administrative files (passwd, group, etc)."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.unix/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:com.informatimago.common-lisp.cesarum)
    :components ((:file "aliases" :depends-on ())
                 (:file "group"   :depends-on ())
                 (:file "passwd"  :depends-on ())
                 (:file "option"  :depends-on ())))

;;;; THE END ;;;;
