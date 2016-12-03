;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               c-operators.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-02 <PJB>
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(make-declare com.informatimago.languages.linc.c::variable  "A variable.")
(make-declare com.informatimago.languages.linc.c::class     "A class.")
(make-declare com.informatimago.languages.linc.c::struct    "A struct.")
(make-declare com.informatimago.languages.linc.c::union     "A union.")
(make-declare com.informatimago.languages.linc.c::type      "A type.")
(make-declare com.informatimago.languages.linc.c::enum      "An enum.")
(make-declare com.informatimago.languages.linc.c::function  "A function.")
(make-declare com.informatimago.languages.linc.c::macro     "A preprocessor macro.")

(gen-operators)


;;;; THE END ;;;;
