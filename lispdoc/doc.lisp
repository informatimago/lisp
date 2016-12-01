;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               doc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the doc classes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-03 <PJB> Extracted from lispdoc.
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
(in-package "COM.INFORMATIMAGO.LISPDOC.DOC")


(defstruct doc
  symbol
  kind
  string)

(defstruct (packdoc (:include doc))
  nicknames
  external-symbol-docs)

(defstruct (vardoc (:include doc))
  initial-value)

(defstruct (fundoc (:include doc))
  lambda-list)

(defstruct (classdoc (:include doc))
  precedence-list
  initargs)


(defun doc-name (doc)
  (let ((name (doc-symbol doc)))
    (etypecase name
      (package (package-name name))
      (symbol  name)
      (cons    (second name)))))


;;;; THE END ;;;;
