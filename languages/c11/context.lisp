;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               context.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    C11 compilation context.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-12 <PJB> Created.
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defclass context ()
  ((c-identifiers-package :initarg :c-identifiers-package
                          :initform (load-time-value (find-package "COM.INFORMATIMAGO.LANGUAGES.C11.C"))
                          :accessor context-c-identifiers-package)
   (typedefs              :initarg :typedefs
                          :initform (make-hash-table :test (function equal))
                          :documentation "Maps identifier symbols to typedef declarations.
The scanner uses it to detect typedef_name tokens."
                          :reader context-typedefs)
   (functions             :initarg :functions
                          :initform (make-hash-table :test (function equal))
                          :documentation "Maps identifier symbols to function declarations.
The scanner uses it to detect func_name tokens."
                          :reader context-functions)
   (enumeration-constants :initarg :enum-constants
                          :initform (make-hash-table :test (function equal))
                          :documentation "Maps identifier symbols to enumeration constant declarations.
The scanner uses it to detect enumeration_constant tokens."
                          :reader context-enumeration-constants)))

(defvar *context* nil
  "The C11 parser context.")

(defgeneric typedef-name-p (context token)
  (:method ((context null) token)
    nil))

(defgeneric function-name-p (context token)
  (:method ((context null) token)
    nil))

(defgeneric enumeration-constant-name-p (context token)
  (:method ((context null) token)
    nil))

(defun identifier-in-table-p (context table name)
  (declare (ignore context))
  (and (eq '|identifier| (token-kind name))
       (gethash (token-symbol name) table)))

(defun enter-into-table (context table kind name definition)
  (declare (ignore context))
  (assert (eq kind (token-kind name)) (name))
  (setf (gethash (token-symbol name) table) definition))

(defmethod typedef-name-p ((context context) token)
  (identifier-in-table-p context (context-typedefs context) token))

(defmethod function-name-p ((context context) token)
  (identifier-in-table-p context (context-functions context) token))

(defmethod enumeration-constant-name-p ((context context) token)
  (identifier-in-table-p context (context-enumeration-constants context) token))

(defgeneric enter-typedef (context name &optional definition)
  (:method ((context context) name &optional (definition t))
    (enter-into-table context (context-typedefs context) '|typedef_name| name definition)))

(defgeneric enter-function (context name &optional definition)
  (:method ((context context) name &optional (definition t))
    (enter-into-table context (context-functions context) '|func_name| name definition)))

(defgeneric enter-enumeration-constant (context name &optional definition)
  (:method ((context context) name &optional (definition t))
    (enter-into-table context (context-enumeration-constants context) '|enumeration_constant| name definition)))

;;;; THE END ;;;;
