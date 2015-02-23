;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               linc.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file for the Linc project.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(asdf:defsystem :com.informatimago.linc
    ;; system attributes:
    :description "LINC Is Not C, but almost.  It allows writing C code as S-exps."
    :long-description "

LINC Is Not C, but almost.

The purpose is to be able to use Common Lisp
at the meta-programming level to generate C sources.
Linc programs can also be executed and debugged in the
Common Lisp environment.

A linc file contains normal Common Lisp expressions,
and linc expressions.  When compiling the linc file,
the Common Lisp expressions are executed, which will  
generate a corresponding C source.

"
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    :licence "AGPL3"
    ;; component attributes:
    :version "0.0.0"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "2007")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.linc/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :depends-on ("split-sequence"
                 "closer-mop"
                 "com.informatimago.common-lisp.cesarum")
    :components ((:file "packages")
                 (:file "c-syntax"           :depends-on ("packages"))
                 (:file "c-operators"        :depends-on ("packages" "c-syntax"))
                 ;; Not yet (:file "c++-syntax"         :depends-on ("packages"))
                 (:file "linc"               :depends-on ("packages" "c-syntax" "c-operators"))))

;;;; THE END ;;;;
