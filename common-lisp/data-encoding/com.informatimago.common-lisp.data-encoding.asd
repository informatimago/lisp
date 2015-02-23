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

(asdf:defsystem :com.informatimago.common-lisp.data-encoding
    ;; system attributes:
    :description  "Informatimago Common Lisp Various Data Encoding/Decoding Algorithms."
    :long-description "

Various Data Encoding/Decoding Algorithms:

- data-encoding: a specific way to encode and decode data in a byte
  vector buffer.

- bencode: the encoding used by Torrent files;

- ecp: Minitel-1b Error Correction Procedure;

"
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    :licence "AGPL3"
    ;; component attributes:
    :version "1.2.1"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2010")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.common-lisp.data-encoding/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :depends-on ("com.informatimago.common-lisp.cesarum"
                 "com.informatimago.common-lisp.arithmetic")
    :components ((:file "data-encoding" :depends-on ())
                 ;; (:file "data-encoding-test" :depends-on ())
                 (:file "bencode"     :depends-on ())
                 (:file "hexadecimal" :depends-on ())
                 (:file "ecp"         :depends-on ())))


;;;; THE END ;;;;
