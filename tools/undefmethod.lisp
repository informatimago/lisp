;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               undefmethod.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;     A macro to undefine methods.
;;;;
;;;;AUTHORS
;;;;    <PJB> irc://Shinmera@irc.freenode.org <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-12-26 <PJB> Imported Shinmera's undefmethod from his paste
;;;;                     http://plaster.tymoon.eu/view/3N
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Copyright irc://Shinmera@irc.freenode.org 2014 - 2014
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.TOOLS.UNDEFMETHOD"
  (:use "COMMON-LISP")
  (:export "UNDEFMETHOD")
  (:documentation "

;; Find your defmethod
(defmethod find-place ((switch string) place)
  (gethash string place))

;; Change to undefmethod
(undefmethod find-place ((switch string) place)
  (gethash string place))

;; Hit C-c C-c again and gone it is!

Copyright irc://Shinmera@irc.freenode.org

"))
(in-package "COM.INFORMATIMAGO.TOOLS.UNDEFMETHOD")

(defmacro undefmethod (name &rest args)
  (flet ((lambda-keyword-p (symbol)
           (find symbol lambda-list-keywords)))
    (destructuring-bind (qualifiers args) (loop for thing = (pop args)
                                                until (listp thing)
                                                collect thing into qualifiers
                                                finally (return (list qualifiers thing)))
      `(remove-method
        #',name
        (find-method
         #',name
         ',qualifiers
         (mapcar #'find-class
                 ',(loop for arg in args
                         until (lambda-keyword-p arg)
                         collect (if (listp arg) (second arg) T))))))))


;;;; THE END ;;;;
