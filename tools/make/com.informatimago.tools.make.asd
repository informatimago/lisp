;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.tools.make.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Make & makefile tools.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-04-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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
  (if (find-package "UIOP")
      (push :uiop *features*)
      (setf *features* (remove :uiop *features*))))

(asdf:defsystem "com.informatimago.tools.make"
  :description "Make and makefile tools."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("com.informatimago.common-lisp.cesarum"
               "split-sequence")
  :components ((:file "package")
               (:file "parser" :depends-on ("package")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)


;;;; THE END ;;;;
