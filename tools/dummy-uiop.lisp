;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dummy-uiop.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines a dummy UIOP package to be able to compile packages
;;;;    depending on UIOP even when it is not available.
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

#-(and)
(progn
  (eval-when (:compile-toplevel)
    (print (list :compile-toplevel
                 :asdf-version asdf::*asdf-version*
                 :features (find :uiop *features*)
                 :uiop-size (when (find-package "UIOP")
                              (let ((n 0)) (do-external-symbols (s "UIOP") (incf n)) n))
                 :packages (sort (mapcar (function package-name) (list-all-packages))
                                 (function string<))))
    (terpri))
  (eval-when (:load-toplevel)
    (print (list :load-toplevel
                 :asdf-version asdf::*asdf-version*
                 :features (find :uiop *features*)
                 :uiop-size (when (find-package "UIOP")
                              (let ((n 0)) (do-external-symbols (s "UIOP") (incf n)) n))
                 :packages (sort (mapcar (function package-name) (list-all-packages))
                                 (function string<))))
    (terpri))
  (eval-when (:execute)
    (print (list :execute
                 :asdf-version asdf::*asdf-version*
                 :features (find :uiop *features*)
                 :uiop-size (when (find-package "UIOP")
                              (let ((n 0)) (do-external-symbols (s "UIOP") (incf n)) n))
                 :packages (sort (mapcar (function package-name) (list-all-packages))
                                 (function string<))))
    (terpri)))

#-uiop
(defpackage "UIOP"
  (:use "COMMON-LISP")
  (:export "SYMBOL-CALL" "RUN-PROGRAM" "GETENV"))


#-uiop
(defmacro uiop::generate-dummy-functions (&rest funames)
  `(progn ,@(mapcar (lambda (funame)
                      `(defun ,funame (&rest args) (declare (ignore args)) nil))
                    funames)))

#-uiop
(uiop::generate-dummy-functions
 UIOP:SYMBOL-CALL
 UIOP:RUN-PROGRAM
 uiop:getenv)

;;;; THE END ;;;;


