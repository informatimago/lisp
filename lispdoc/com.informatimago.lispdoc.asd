;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.lispdoc.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to generate the documentation of the com.informatimago lisp packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-31 <PJB> Created this .asd file.
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

#+mocl
(asdf:defsystem "com.informatimago.lispdoc"
    ;; system attributes:
    :description "Dummy Informatimago Common Lisp Documentation Generator"
  :long-description "

This system would use closer-mop which is not available for MOCL.

"
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    :licence "LLGPL"
    ;; component attributes:
    :version "1.2.0"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lispdoc/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :depends-on ()
    :components ())

#-mocl
(asdf:defsystem "com.informatimago.lispdoc"
  ;; system attributes:
  :description "Informatimago Common Lisp Documentation Generator"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "LLGPL"
  ;; component attributes:
  :version "1.2.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Spring 2012")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lispdoc/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on (
               ;; Dependencies:
               "cl-ppcre"
               "closer-mop"
               "split-sequence"
               "com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.html-generator"
               ;; The documented systems: TODO: we shouldn't depend on them.
               "com.informatimago.common-lisp"
               "com.informatimago.clext"
               "com.informatimago.clmisc"
               "com.informatimago.rdp"
               #+(and ccl darwin) "com.informatimago.objcl")
  :components ((:file "package")
               (:file "utility"     :depends-on ("package"))
               (:file "uri"         :depends-on ("package"))
               (:file "doc"         :depends-on ("package"))
               (:file "tree"        :depends-on ("package" "utility"))
               (:file "lispdoc"     :depends-on ("package" "doc"))
               (:file "generate"    :depends-on ("package" "doc" "lispdoc" "utility" "tree"))
               (:file "gentext"     :depends-on ("package" "doc" "generate" "utility" "tree" "uri"))
               (:file "genrst"      :depends-on ("package" "doc" "generate" "utility" "tree" "uri"))
               (:file "genhtml"     :depends-on ("package" "doc" "generate" "utility" "tree" "uri"))
               (:file "lispdoc-run" :depends-on ("package" "lispdoc" "genhtml" "genrst" "gentext")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


;;;; THE END ;;;;
