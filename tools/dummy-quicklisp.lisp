;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dummy-quicklisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines a dummy QUICKLISP package to be able to compile
;;;;    packages depending on QUICKLISP even when it is not present.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-07 <PJB> Extracted from quicklisp.lisp.
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

#-quicklisp
(defpackage "QUICKLISP"
  (:nicknames "QL-DIST" "QL" "QUICKLISP-CLIENT")
  (:export "CLEAN" "DIST" "ENABLED-DISTS" "ENSURE-INSTALLED"
           "INSTALLED-RELEASES" "NAME" "PROJECT-NAME"
           "PROVIDED-RELEASES" "PROVIDED-SYSTEMS" "QUICKLOAD"
           "REGISTER-LOCAL-PROJECTS" "RELEASE" "UNINSTALL"
           "UPDATE-ALL-DISTS" "UPDATE-CLIENT" "WHERE-IS-SYSTEM"))

#-quicklisp
(defmacro quicklisp::generate-dummy-functions (&rest funames)
  `(progn ,@(mapcar (lambda (funame)
                      `(defun ,funame (&rest args) (declare (ignore args)) nil))
                    funames)))

#-quicklisp
(quicklisp::generate-dummy-functions
 QUICKLISP:DIST
 QUICKLISP:ENABLED-DISTS
 QUICKLISP:ENSURE-INSTALLED
 QUICKLISP:INSTALLED-RELEASES
 QUICKLISP:PROVIDED-RELEASES
 QUICKLISP:PROVIDED-SYSTEMS
 QUICKLISP:QUICKLOAD
 QUICKLISP:REGISTER-LOCAL-PROJECTS
 QUICKLISP:RELEASE
 QUICKLISP:UNINSTALL
 QUICKLISP:UPDATE-ALL-DISTS
 QUICKLISP:UPDATE-CLIENT
 QUICKLISP:WHERE-IS-SYSTEM)

;;;; THE END ;;;;
