;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dummy-asdf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines a dummy ASDF package to be able to compile packages
;;;;    depending on ASDF even when ASDF is missing.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-07 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COMMON-LISP-USER")

#-asdf
(defpackage "ASDF"
  (:use "COMMON-LISP")
  (:export "DEFSYSTEM" "LOAD-OP" "TEST-OP" "OOS" "OPERATE"
           "*COMPILE-FILE-WARNINGS-BEHAVIOUR*"
           "*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*"
           "*CENTRAL-REGISTRY*"
           "FIND-SYSTEM" "SYSTEM"
           "COMPONENT-NAME" "COMPONENT-DEPENDS-ON"
           "RUN-SHELL-COMMAND")
  (:intern "*VERBOSE-OUT*"
           "NAME" "LICENCE"))

#-asdf
(progn
  (defvar asdf:*compile-file-warnings-behaviour*    nil)
  (defvar asdf:*system-definition-search-functions* nil)
  (defvar asdf:*verbose-out*                        nil)
  (defvar asdf:*central-registry*                   nil))


;;;; THE END ;;;;
