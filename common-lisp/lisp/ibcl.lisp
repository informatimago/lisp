;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ibcl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See :documentation of package below.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-08-07 <PJB> Restructured.
;;;;    2006-07-01 <PJB> Added deftype, defclass.
;;;;    2006-05-04 <PJB> Added this header. Augmented.
;;;;BUGS
;;;;    Missing some def* macros, like define-symbol-macro,
;;;;    defconditions, defmethod, defgeneric, etc.
;;;;    Missing some functions, like make-package, rename-package, etc.
;;;;    See also MOP functions.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************


(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP"

  (:nicknames  "COM.INFORMATIMAGO.COMMON-LISP.LISP.IBCL"
               "IMAGE-BASED-COMMON-LISP"
               "IBCL")
  
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE")
  
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-SAVING-DEFINES"
                          ;; Not defpackage we provide our own wrapping.
                          "DEFINE-COMPILER-MACRO"          "DEFINE-MODIFY-MACRO"
                          "DEFINE-SETF-EXPANDER"           "DEFMACRO" "DEFSETF"
                          "DEFVAR"                         "DEFPARAMETER"
                          "DEFCONSTANT"                    "DEFINE-SYMBOL-MACRO"
                          "DEFSTRUCT"                      "DEFCLASS"
                          "DEFTYPE"                        "DEFINE-CONDITION"
                          "DEFINE-METHOD-COMBINATION"      "DEFGENERIC"
                          "DEFMETHOD"                      "DEFUN")

  (:shadow "FIND-SYMBOL" "FIND-PACKAGE" "IMPORT" "SHADOW"
           "SHADOWING-IMPORT" "DELETE-PACKAGE" "MAKE-PACKAGE"
           "WITH-PACKAGE-ITERATOR" "UNEXPORT" "IN-PACKAGE"
           "UNUSE-PACKAGE" "USE-PACKAGE" "DEFPACKAGE" "DO-SYMBOLS"
           "DO-EXTERNAL-SYMBOLS" "FIND-SYMBOL")
  
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym "COMMON-LISP")
                   (push (string sym) symbols))
                 (do-external-symbols (sym "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE")
                   (push (string sym) symbols))
                 symbols))
  
  (:documentation "

The package IBCL exports the same symbols as COMMON-LISP, but for
some of the functions of macros modified to track of the source of the
definitions and to be able to edit them from the image, and to save
them in files.

The package IBCL-USER is a virgin package using IBCL instead of CL.

One can work at the REPL, define variables with DEFCONSTANT, DEFVAR,
DEFPARAMETER, macros with DEFMACRO, and functions with DEFUN, edit
macro and function definitions with ED.

The function LIST-PACKAGES-WITH-SOURCES returns a list of packages
where some of these variables or functions are defined.

The function SOURCE returns the source form of the given symbol or
package.

The function SAVE-SOURCES saves the definitions in a package, or all
the definitions to a file or stream.


The IBCL package provides some shadowed CL functions to substittute
sneakily IBCL for CL.  When defpackage is ibcl:defpackage, as long as
CL package operators are not qualified, (cl:defpackage …)
vs. (defpackage …), the IBCL package will be used instead of the CL
package.  This should allow loading libraries using IBCL instead of
CL.

 (in-package :ibcl-user)
 (asdf-load-source :some-system)
 (source 'some-system:some-function :function)
 --> (defun some-system:some-function …)

Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))


(cl:defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP-USER"
  (:nicknames  "COM.INFORMATIMAGO.COMMON-LISP.LISP.IBCL-USER"
               "IMAGE-BASED-COMMON-LISP-USER"
               "IBCL-USER")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP")
  (:documentation "

The package IBCL-USER is a virgin package using IBCL instead of CL.

Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))


(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.IMAGE-BASED-COMMON-LISP")


(define-condition simple-package-error (package-error simple-error)
  ())

(defun normalize-package-designator (package)
  (let ((pack (cl:find-package package)))
    (if pack
      (package-name pack)
      (error 'simple-package-error
             :package package
             :format-control "No such package ~S"
             :format-arguments (list package)))))


(defun substitute-package-designator (new old sequence)
  (substitute (normalize-package-designator new)
              (normalize-package-designator old)
              sequence
              :key (function normalize-package-designator)
              :test (function equal)))

(defun substitute-packages (new-old-a-list sequence)
  (loop
    :for (new . old) :in new-old-a-list
    :do (setf sequence (substitute-package-designator new old sequence))
    :finally (return sequence)))

(defun substitute-one-package (new-old-a-list package)
  (let ((package  (normalize-package-designator package)))
    (or (car (rassoc package new-old-a-list
                     :key (function normalize-package-designator)
                     :test (function equal)))
        package)))

(defvar *package-map* '(("IBCL-USER" . "COMMON-LISP-USER")
                        ("IBCL"      . "COMMON-LISP"))
  "An A-list mapping new packages for old packages.")





(defun ensure-list (object)
  (if (listp object)
    object
    (list object)))


(defun find-symbol (string &optional (package *package*))
  (cl:find-symbol string (substitute-one-package *package-map* package)))

(defun find-package (designator)
  (let ((found  (cl:find-package designator)))
    (when found
      (cl:find-package (substitute-one-package *package-map* found)))))

(defun import (symbols &optional (package *package*))
  (cl:import symbols (substitute-one-package *package-map* package)))

(defun shadow (symbol-names &optional (package *package*))
  (cl:shadow symbol-names (substitute-one-package *package-map* package)))

(defun shadowing-import (symbols &optional (package *package*))
  (cl:shadowing-import symbols (substitute-one-package *package-map* package)))

(defun delete-package (package-designator)
  (prog1 (cl:delete-package package-designator)
    (remhash (find-package package-designator)
             com.informatimago.common-lisp.lisp.source::*sources*)))

(defun make-package (package &key nicknames uses)
  (cl:make-package package
                   :nicknames nicknames
                   :use (substitute-packages *package-map* uses)))

(defmacro with-package-iterator ((name package-list-form &rest symbol-types) &body body)
  `(cl:with-package-iterator (,name
                              (substitute-packages *package-map* (ensure-list ,package-list-form))
                              ,@symbol-types)
     ,@body))

(defun unexport (symbols &optional (package *package*))
  (cl:unexport symbols (substitute-one-package  *package-map* package)))

(defmacro in-package (designator)
  `(cl:in-package ,(substitute-one-package *package-map* designator)))


(defun unuse-package (packages-to-unuse &optional (designator *package*))
  (cl:unuse-package (substitute-packages *package-map* (ensure-list packages-to-unuse))
                    (substitute-one-package *package-map* designator)))

(defun use-package (packages-to-use &optional (designator *package*))
  (cl:use-package (substitute-packages *package-map* (ensure-list packages-to-use))
                  (substitute-one-package *package-map* designator)))

(defmacro defpackage (name &rest options)
  "
DO:             Same as CL:DEFPACKAGE, but substitute the packages
                specified by *PACKAGE-MAP*, and record the defpackage
                form in the sources.

RETURN:         The package name.
"
  `(com.informatimago.common-lisp.lisp.cl-saving-defines:defpackage ,name
     ,@(mapcar
        (lambda (option)
            (if (consp option)
              (case (first option)
                ((:use)
                 (cons :use (substitute-packages *package-map* (rest option))))
                ((:shadowing-import-from :import-from)
                 (list* (first option)
                        (substitute-one-package *package-map* (second option))
                        (cddr  option)))
                (otherwise
                 option))
              option))
        options)))


(cl:defmacro do-symbols ((var &optional (package *package*) result-form) &body body)
  `(cl:do-symbols (,var 
                    (substitute-one-package *package-map* ,package)
                    ,result-form)
     ,@body))

(defmacro do-external-symbols ((var &optional (package *package*) result-form) &body body)
  `(cl:do-external-symbols (,var 
                             (substitute-one-package *package-map* ,package)
                             ,result-form)
     ,@body))


;;;; THE END ;;;;










