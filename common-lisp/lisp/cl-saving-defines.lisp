;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cl-saving-defines.lisp
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
;;;;    2012-08-07 <PJB> Extracted from ibcl.lisp and restructured.
;;;;    2006-07-01 <PJB> Added deftype, defclass.
;;;;    2006-05-04 <PJB> Added this header. Augmented.
;;;;BUGS
;;;;
;;;;    Since we collect only the whole forms of some specific macros,
;;;;    we don't save closure, or eval-when, progn, etc.
;;;;
;;;;    (let ((x 42))
;;;;      (defun g () x)
;;;;      (defun s (z) (setf x z)))
;;;;    (source 'g 'function)
;;;;    --> (defun g nil x)             ; instead of (let …)
;;;;    (source 's 'function)
;;;;    --> (defun s (z) (setf x z))    ; instead of (let …)
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-SAVING-DEFINES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:shadow "DEFPACKAGE"
           "DEFINE-COMPILER-MACRO"          "DEFINE-MODIFY-MACRO"
           "DEFINE-SETF-EXPANDER"           "DEFMACRO" "DEFSETF"
           "DEFVAR"                         "DEFPARAMETER"
           "DEFCONSTANT"                    "DEFINE-SYMBOL-MACRO"
           "DEFSTRUCT"                      "DEFCLASS"
           "DEFTYPE"                        "DEFINE-CONDITION"
           "DEFINE-METHOD-COMBINATION"      "DEFGENERIC"
           "DEFMETHOD"                      "DEFUN")
  (:export "DEFPACKAGE"
           "DEFINE-COMPILER-MACRO"          "DEFINE-MODIFY-MACRO"
           "DEFINE-SETF-EXPANDER"           "DEFMACRO" "DEFSETF"
           "DEFVAR"                         "DEFPARAMETER"
           "DEFCONSTANT"                    "DEFINE-SYMBOL-MACRO"
           "DEFSTRUCT"                      "DEFCLASS"
           "DEFTYPE"                        "DEFINE-CONDITION"
           "DEFINE-METHOD-COMBINATION"      "DEFGENERIC"
           "DEFMETHOD"                      "DEFUN")
  (:documentation "

This package exports macros wrapping the CL def* macros that save the
source form.

Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.CL-SAVING-DEFINES")


(cl:defmacro define-wrapper-macro (defname (name &rest lambda-list) source-type)
  (let ((source (gensym)))
    `(cl:defmacro ,defname ,(list* '&whole source (cons name lambda-list))
       (declare (ignorable ,@(mapcar (function parameter-name)
                                     (lambda-list-parameters
                                      (parse-lambda-list (cons name lambda-list) :macro)))))
       `(progn
          (setf (source ',,name ',,source-type) ',,source)
          ,(cons (intern (string (first ,source)) "COMMON-LISP") (rest ,source))))))



(define-wrapper-macro defpackage          (name &rest options)               :package)

(define-wrapper-macro defvar              (name &optional value docstring)   :variable)
(define-wrapper-macro defparameter        (name value &optional docstring)   :variable)
(define-wrapper-macro defconstant         (name value &optional docstring)   :variable)
(define-wrapper-macro define-symbol-macro (name expansion)                   :variable)


(cl:defmacro defstruct (&whole form name-and-options &rest fields)
  (declare (ignorable fields))
  `(progn
     (setf (source ',(if (consp name-and-options)
                         (car name-and-options)
                         name-and-options)
                   :type) ',form)
     ,(cons 'cl:defstruct (rest form))))

(define-wrapper-macro deftype          (name lambda-list &body body)         :type)
(define-wrapper-macro defclass         (name parents slots &rest options)    :type)
(define-wrapper-macro define-condition (name parents slots &rest options)    :type)


(define-wrapper-macro define-compiler-macro (name lambda-list &body body)    :compiler-macro)

(define-wrapper-macro define-modify-macro   (name lambda-list fun &optional docstring) :function)
(define-wrapper-macro define-setf-expander  (name lambda-list &body body)    :function)
(define-wrapper-macro defsetf               (name lambda-list &body body)    :function)
(define-wrapper-macro defmacro              (name lambda-list &body body)    :function)
(define-wrapper-macro defun                 (name lambda-list &body body)    :function)
(define-wrapper-macro defgeneric            (name lambda-list &rest options) :function)

(define-wrapper-macro define-method-combination (name &rest options) :method-combination)

(cl:defmacro defmethod (&whole form name &rest arguments)
  (let ((qualifiers (loop
                      :while (atom (first arguments))
                      :collect (pop arguments))))
    (destructuring-bind (lambda-list &body body) arguments
      (declare (ignorable body))
      (let ((specializers (mapcar
                           (lambda (parameter)
                               (if (parameter-specializer-p parameter)
                                 (parameter-specializer parameter)
                                 't))
                           (lambda-list-mandatory-parameters
                            (parse-lambda-list lambda-list :specialized)))))
        `(progn
           (setf (source '(,name ,qualifiers ,specializers) :method) ',form)
           ,(cons 'cl:defmethod (rest form)))))))


;;;; THE END ;;;;










