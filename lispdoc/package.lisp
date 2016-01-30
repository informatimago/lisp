;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-04 <PJB> Extracted from lispdoc.lisp
;;;;BUGS
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.LISPDOC.UTILITY"
  (:use "COMMON-LISP")
  (:export "APPENDF")
  (:documentation "

A few utility operators.

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.URI"
  (:use "COMMON-LISP" "CL-PPCRE")
  (:export "URI" "URI-REFERENCE")
  (:documentation "

Exports cl-ppcre scanners for URI and URI-REFERENCE.

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.DOC"
  (:use "COMMON-LISP")
  (:export "CLASSDOC" "CLASSDOC-INITARGS" "CLASSDOC-P"
           "CLASSDOC-PRECEDENCE-LIST" "COPY-CLASSDOC" "COPY-DOC"
           "COPY-FUNDOC" "COPY-PACKDOC" "COPY-VARDOC" "DOC" "DOC-KIND"
           "DOC-NAME" "DOC-P" "DOC-STRING" "DOC-SYMBOL" "FUNDOC"
           "FUNDOC-LAMBDA-LIST" "FUNDOC-P" "MAKE-CLASSDOC" "MAKE-DOC"
           "MAKE-FUNDOC" "MAKE-PACKDOC" "MAKE-VARDOC" "PACKDOC"
           "PACKDOC-EXTERNAL-SYMBOL-DOCS" "PACKDOC-NICKNAMES"
           "PACKDOC-P" "VARDOC" "VARDOC-INITIAL-VALUE" "VARDOC-P")
  (:documentation "

The documentation is stored in DOC objects (various subclasses).

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015
    Copyright (C) 2003 Sven Van Caekenberghe.

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LISPDOC.DOC")
  (:export "LISPDOC")
  (:documentation "

LISPDOC scans in-image packages and return a list of filled PACKDOC
structures.


License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015
    Copyright (C) 2003 Sven Van Caekenberghe.

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.TREE"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.LISPDOC.UTILITY")
  (:export "*HIERARCHICAL-PACKAGE-SEPARATOR*" "PACKAGE-PATH" "TREE"
           "MAKE-TREE" "TREE-P" "TREE-COPY" "TREE-PARENT" "TREE-NODE"
           "TREE-PACKAGE" "TREE-CHILDREN" "TREE-CHILDREN-NAMED"
           "TREE-ADD-NODE-AT-PATH" "MAKE-INDEX-TREE"
           "TREE-NODE-AT-PATH" "TREE-PATH")
  (:documentation "

A package tree index, for hierarchical packages index.

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.GENERATE"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.LISPDOC"
        "COM.INFORMATIMAGO.LISPDOC.DOC"
        "COM.INFORMATIMAGO.LISPDOC.TREE"
        "COM.INFORMATIMAGO.LISPDOC.UTILITY")

  (:export "DOCUMENTATION-GENERATOR" "DOCUMENTATION-TITLE" "COPYRIGHT"
           "AUTHOR" "EMAIL" "KEYWORDS" "PACKDOCS" "PAGES" "INDEX-TREE"
           "RENDER" "NAVIGATION"
           
           "GENERATE-LISPDOC"
           "GENERATE-BEGIN" "GENERATE-END"
           "GENERATE-INTRODUCTION" 
           "GENERATE-SYMBOL-INDEX"
           "GENERATE-PERMUTED-SYMBOL-INDEX"
           "GENERATE-FLAT-SYMBOL-INDEX"
           "GENERATE-FLAT-PACKAGE-INDEX"
           "GENERATE-HIERARCHICAL-PACKAGE-INDEX"
           "GENERATE-PACKAGE-DOCUMENTATION-PAGES"
           "GENERATE-NAVIGATION-MENU"
           "PACKAGE-NAVIGATION-MENU"
           "BUILD-PERMUTED-SYMBOL-INDEX-GROUPS"
           "BUILD-FLAT-SYMBOL-INDEX-GROUPS"
           "FIRST-LETTER" "RIGHT-CASE"
           "COLLECT-ALL-SYMBOLS")

  (:documentation "

A generator class shall have methods defined for all the generic
functions exported from this package.

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.GENERATE.HTML"
  (:use "COMMON-LISP"
        "CL-PPCRE"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"
        "COM.INFORMATIMAGO.LISPDOC.URI"
        "COM.INFORMATIMAGO.LISPDOC.DOC"
        "COM.INFORMATIMAGO.LISPDOC.TREE"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE"
        "COM.INFORMATIMAGO.LISPDOC.UTILITY")
  (:shadowing-import-from "COMMON-LISP" "MAP") ; in html-generator too.
  (:shadowing-import-from "CL-PPCRE" "SCAN")  ; in html-generator too.
  (:export "HTML-DOCUMENTATION")
  (:documentation "

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015
    Copyright (C) 2003 Sven Van Caekenberghe.

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.GENERATE.RST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LISPDOC.URI"
        "COM.INFORMATIMAGO.LISPDOC.DOC"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE"
        "COM.INFORMATIMAGO.LISPDOC.UTILITY")
  (:export "RST-DOCUMENTATION")
  (:documentation "

Generate a single-file reStructuredText documentation.

The docstrings may contain reStructuredText markups, which is taken
as-is, but title and subtitles need to marked up consistently across
all the packages documented in a single document.

The RST-DOCUMENTATION generator will generate only double-line (above
and below) document and chapter title markups, reserving single-line
(below) title markups to the docstrings.

It would be advised to use the following title markups in docstrings:

    docstring section
    ********************

    docstring subsection
    ====================

    sub-subsection
    --------------------

    sub-sub-subsection
    ....................

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.GENERATE.TEXT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LISPDOC.URI"
        "COM.INFORMATIMAGO.LISPDOC.DOC"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE"
        "COM.INFORMATIMAGO.LISPDOC.UTILITY")
  (:export "TEXT-DOCUMENTATION")
  (:documentation "

Generate a single-file plain-text documentation.

The docstrings are copied as-is, surrounded by some formatted titles.

License:

    LLGPL

    Copyright Pascal J. Bourguignon 2015 - 2015

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


(defpackage "COM.INFORMATIMAGO.LISPDOC.RUN"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LISPDOC.DOC"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE.RST"
        "COM.INFORMATIMAGO.LISPDOC.GENERATE.HTML")
  (:export "DOC")
 (:documentation "

Automatically generate documentation for properly documented symbols
exported from packages.

This is tool automatically generates documentation for Common Lisp code
based on symbols that exported from packages and properly documented.
This code was written for OpenMCL <http://openmcl.clozure.com>

There are generators for HTML and reStructuredText formats.
Other generators classes can be written by the user.


License:

    LLGPL

    Copyright Pascal J. Bourguignon 2012 - 2015
    Copyright (C) 2003 Sven Van Caekenberghe.

    You are granted the rights to distribute and use this software
    as governed by the terms of the Lisp Lesser GNU Public License
    <http://opensource.franz.com/preamble.html> also known as the LLGPL.
"))


;;;; THE END ;;;;

