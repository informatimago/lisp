;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               macros.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines DEFUN and LAMBDA, to deal with interactive declarations.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-11 <PJB> Extracted from editor.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(in-package "COM.INFORMATIMAGO.EDITOR")

;;;---------------------------------------------------------------------
;;; Commands: interactive functions
;;;---------------------------------------------------------------------
;;;
;;; We want to define commands, with a special INTERACTIVE
;;; declaration.  So we need to use our own DEFUN (and LAMBDA) macros.

(declaim (declaration interactive))


(defvar *interactive-decls* (make-hash-table #+clisp :weak #+clisp :key)
  "A map of commands name or functions to INTERACTIVE declarations.")


(defmacro defun (name arguments &body body)
  "Do additionnal book-keeping over CL:DEFUN, for INTERACTIVE commands."
  (let* ((decls (mapcan (function rest) (extract-declarations body)))
         (inter (find 'interactive decls :key (function first))))
    (if inter
        `(progn
           (cl:defun ,name ,arguments ,@body)
           (setf (gethash ',name           *interactive-decls*) ',inter
                 (gethash (function ,name) *interactive-decls*) ',inter)
           ',name)
        `(progn
           (cl:defun ,name ,arguments ,@body)
           (remhash ',name           *interactive-decls*)
           (remhash (function ,name) *interactive-decls*)
           ',name))))


(defmacro lambda (arguments &body body)
  "Do additionnal bookkeeping over CL:LAMBDA, for INTERACTIVE commands."
  (let* ((decls (mapcan (function rest) (extract-declarations body)))
         (inter (find 'interactive decls :key (function first))))
    (if inter
        `(flet ((anonymous-function ,arguments ,@body))
           (setf (gethash (function anonymous-function) *interactive-decls*) ',inter)
           (function anonymous-function))
        `(cl:lambda  ,arguments ,@body))))


(defun interactivep (fundesc)
  "Whether the function FUNCDESC is INTERACTIVE."
  (gethash fundesc *interactive-decls*))


(defun getenv (var)
  #+asdf3 (uiop/os:getenv var)
  #-asdf3 (asdf:getenv    var))

(defun (setf getenv) (new-val var)
  #+asdf3 (setf (uiop/os:getenv var) new-val)
  #-asdf3 (setf (asdf:getenv    var) new-val))


;;;; THE END ;;;;
