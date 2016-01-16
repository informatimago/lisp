;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lispdoc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generate HTML documentation of a set of CL packages.
;;;;
;;;;    Originally:
;;;;    Id: lispdoc.lisp,v 1.8 2004/01/13 14:03:41 sven Exp 
;;;;AUTHORS
;;;;    Sven Van Caekenberghe.
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-29 <PJB> 
;;;;BUGS/TODO
;;;;
;;;;    - improve class documentation (slots, accessors).
;;;;
;;;;    - improve navigation menu (symbol lists, tree).
;;;;
;;;;    - make it run on clisp, sbcl, etc.
;;;;
;;;;    - deal with re-exported symbol, whose home is not one of the
;;;;      documented packages.
;;;;
;;;;    - make it merge documentations (tree, navigation), since some
;;;;      packages can only be loaded in a specific implementation.
;;;;
;;;;    - make links from symbols and packages to source files (eg. gitorious).
;;;;
;;;;    - It would be nice to have a reST parser for the docstrings.
;;;;    Check cl-docutils for its reST parser?
;;;;    http://www.jarw.org.uk/lisp/cl-docutils.html
;;;;
;;;;LEGAL
;;;;    LLGPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
;;;;    Copyright (C) 2003 Sven Van Caekenberghe.
;;;;    
;;;;    This library is licenced under the Lisp Lesser General Public
;;;;    License.
;;;;    
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;    
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;    
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(in-package "COM.INFORMATIMAGO.LISPDOC")



;;;----------------------------------------------------------------------
;;;
;;; documentation portability layer
;;;

(defun function-lambda-list (funame)
  "
FUNAME:  A function name.
RETURN:  The function lambda list.
"
  (let ((le (function-lambda-expression (if (consp funame)
                                            (fdefinition funame)
                                            (or (macro-function funame)
                                                (symbol-function funame))))))
    (if le
        (second le)
        (or
         #+openmcl   (ccl:arglist funame)
         #+lispworks (lw:function-lambda-list funame)
         '()))))


(defun class-precedence-list (class-name)
  "
CLASS-NAME: A class name.
RETURN:     The class precedence list.
"
  (closer-mop:class-precedence-list (find-class class-name)))


(defun class-slot-initargs (class-name)
  "
CLASS-NAME: A class name.
RETURN:     The initargs of the class slots.
"
  (let ((class (find-class class-name)))
    (mapcan (lambda (slot) (copy-seq (closer-mop:slot-definition-initargs slot)))
            (closer-mop:class-slots class))))


(defun has-meaning (symbol)
  (or (boundp symbol)
      (fboundp symbol)
      (ignore-errors (fdefinition `(setf ,symbol)))
      (ignore-errors (find-class symbol))))



;;;----------------------------------------------------------------------
;;;
;;; lispdoc
;;;

(defun lispdoc-symbol (symbol)
  "
RETURN: A list of doc structures for the SYMBOL.
"
  (let ((doc '()))
    
    (when (documentation symbol 'variable)
      (push (make-vardoc :kind          (if (constantp symbol)
                                            :constant
                                            :variable)
                         :symbol         symbol
                         :string        (documentation symbol 'variable)
                         :initial-value (if (boundp symbol)
                                            (symbol-value symbol)
                                            :unbound))
            doc))
    
    (let ((spec `(setf ,symbol)))
      (when (and (documentation spec 'function)
                 (fboundp spec))
        (push (make-fundoc :kind        (cond
                                          ((typep (fdefinition spec) 'standard-generic-function)
                                           :generic-function)
                                          (t
                                           :function))
                           :symbol      `(setf ,symbol)
                           :string      (documentation spec 'function)
                           :lambda-list (function-lambda-list spec))
              doc)))
    
    (when (and (documentation symbol 'function)
               (fboundp symbol))
      (push (make-fundoc :kind        (cond
                                        ((macro-function symbol)
                                         :macro)
                                        ((typep (fdefinition symbol) 'standard-generic-function)
                                         :generic-function)
                                        (t
                                         :function))
                         :symbol      symbol
                         :string      (documentation symbol 'function)
                         :lambda-list (function-lambda-list symbol))
            doc))
    
    (when (documentation symbol 'type)
      (cond
        ((not (ignore-errors (find-class symbol)))
         (push (make-doc :kind :type
                         :symbol symbol
                         :string (documentation symbol 'type))
               doc))
        ((subtypep (find-class symbol) (find-class 'structure-object))
         (push  (make-classdoc :kind :structure
                               :symbol symbol
                               :string (documentation symbol 'type))
                doc))
        (t
         (block :ignore
           (push (make-classdoc :kind            (cond
                                                   ((subtypep (find-class symbol) (find-class 'condition))
                                                    :condition)
                                                   ((subtypep (find-class symbol) (find-class 'standard-object))
                                                    :class)
                                                   (t (return-from :ignore)))
                                :symbol          symbol
                                :string          (documentation symbol 'type)
                                :precedence-list (mapcar (function class-name) (class-precedence-list symbol))
                                :initargs        (class-slot-initargs symbol))
                 doc)))))
    
    (unless  doc
      (push (make-doc :kind (if (has-meaning symbol)
                                :undocumented
                                :skip)
                      :symbol symbol)
            doc))
    
    doc))


(defun lispdoc-package (package)
  "
RETURN:  packdoc structure for the package.
"
  (make-packdoc :kind :package
                :symbol package
                :string (or (documentation package t) :undocumented)
                :nicknames (package-nicknames package)
                :external-symbol-docs
                (mapcan (function lispdoc-symbol)
                        (let ((symbols '()))
                          (do-external-symbols (x package) (push x symbols))
                          (sort symbols (function string-lessp))))))


(defun lispdoc (packages)
  "Generate a lispdoc sexp documenting the exported symbols of each package"
  (mapcar (lambda (package)
            (lispdoc-package (if (packagep package)
                                 package
                                 (find-package package))))
          packages))



;;;; THE END ;;;;


