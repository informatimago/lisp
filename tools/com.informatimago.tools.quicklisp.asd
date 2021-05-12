;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               quicklisp.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Quicklisp tools.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-06 <PJB> Extracted from ~/rc/common.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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

(asdf:defsystem "com.informatimago.tools.quicklisp"
  :description "Quicklisp tools."
  :author "Pascal J. Bourguignon"
  :version "1.3.0"
  :license "GPL3"
  :depends-on ( ;; assumed ;; "quicklisp"
               "com.informatimago.tools.pathname"
               "com.informatimago.common-lisp.cesarum")
  :components ((:file "dummy-quicklisp" :depends-on ())
               (:file "dummy-asdf"      :depends-on ())
               (:file "asdf-tools"      :depends-on ("dummy-asdf"))
               (:file "quicklisp"       :depends-on ("dummy-quicklisp"
                                                     "asdf-tools")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

;;;; THE END ;;;;
