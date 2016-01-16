;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.objcl.cocoa-playground.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Playground for Cocoa and ObjCL.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-22 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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

#+not-yet
(asdf:defsystem "com.informatimago.objcl.cocoa-playground"
    ;; system attribtues:
    :description  "Playground for Cocoa and ObjCL."
    :long-description "

Playground for Cocoa and ObjCL.

"
    :author    "Pascal J. Bourguignon <pjb@informatimago.com>"
    :maitainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    :licence "AGPL3"
    ;; component attributes:
    :version "1.9.0"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.cocoa-playground/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :depends-on (:com.informatimago.objcl)
    :components ((:file "layout"             :depends-on ())
                 (:file "cocoa"              :depends-on ("layout"))))

;;;; THE END ;;;;
