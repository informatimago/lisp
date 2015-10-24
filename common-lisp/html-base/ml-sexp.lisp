;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ml-sexp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a functional abstraction
;;;;    to manage a sexp representing a structured document (XML, HTML, SGML).
;;;;    It is basically a DOM working on sexp of a specific form.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-22 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.HTML-BASE.ML-SEXP"
  (:use "COMMON-LISP")
  (:export

   "MAKE-ELEMENT" "ELEMENT-TAG" "ELEMENT-ATTRIBUTES" "ELEMENT-CHILDREN"
   "ELEMENT-TAG-EQUAL-P" "ATTRIBUTE-NAMED" "VALUE-OF-ATTRIBUTE-NAMED"

   "MAKE-ATTRIBUTE" "ATTRIBUTE-NAME" "ATTRIBUTE-VALUE"
   "ATTRIBUTE-NAME-EQUAL-P"
   
   "ELEMENT-CHILD"
   "STRING-SINGLE-CHILD-P"
   
   "CHILD-TAGGED"            "CHILDREN-TAGGED"            "GRANDCHILDREN-TAGGED"
   "CHILD-VALUED"            "CHILDREN-VALUED"            "GRANDCHILDREN-VALUED"
   "CHILD-TAGGED-AND-VALUED" "CHILDREN-TAGGED-AND-VALUED" "GRANDCHILDREN-TAGGED-AND-VALUED"
   
   "ELEMENT-AT-PATH"

   "VALUE-TO-BOOLEAN")
  (:documentation "

This package exports a functional abstraction
to manage a sexp representing a structured document (XML, HTML, SGML).
It is basically a DOM working on sexp of a specific form.

AGPL3
Copyright Pascal J. Bourguignon 2015 - 2015
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.HTML-BASE.ML-SEXP")


#|

Principles:

A SGML element is represented by a single list containing:

- the tag
- a list of attributes
- the contents of the elements, as the rest of the element list.

The tag is represented as a string designator.  It can be a keyword,
symbol or string (or possibly a character, but it would be
unexpected).  Symbols whose letters are all uppercase are considered
case insensitive: when generating the default case for the DTD is used
(lowercase for XML, uppercase for HTML).  Mixed case symbols or
strings are case sensitive: the case is transmitted as is.

When xml name spaces are used, the tag can be qualified by the
namespace, by including it in the designated string: ::

    <ns:tag />

is represented as: ::

    ("ns:tag" ())

or by: ::

    (:ns\:tag ())


Note: we may also accept a xmls sexp such as: ::

    ((tag . "ns") ())

to represent the tag in this namespace, but notice that xmls doesn't
handle properly qualified attribute names, the namespace is lost for
attributes.  This xmls representation is discouraged.


In addition to normal elements, there are sgml directives
(eg. ``<!DOCTYPE>``) and xml declarations (eg. ``<?xml>``).

(:?|xml| (:version "1.0" :encoding "utf-8"))

(:!doctype :|html| :PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")

(:!-- " comment ")

::

    <?xml version="1.0" encoding="utf-8" ?>
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <!-- comment -->
    <tag attribute="value" att2="val2"><content1 /><content2 /></tag>


::

    (:tag (:attribute "value" :att2 "val2")
       (:content1 ())
       (:content2 ()))

|#


(defgeneric element-name       (element)   (:method ((element   t)) nil))
(defgeneric element-attributes (element)   (:method ((element   t)) nil))
(defgeneric element-children   (element)   (:method ((element   t)) nil))
(defgeneric attribute-name     (attribute) (:method ((attribute t)) nil))
(defgeneric attribute-value    (attribute) (:method ((attribute t)) nil))

(defun make-element (tag attributes children)
  (list* tag attributes children))

(defmethod element-name       ((entity cons))
  (car entity))

(defmethod element-attributes ((entity cons))
  (cadr entity))

(defmethod element-children   ((entity cons))
  (cddr entity))


(defun make-attribute (name value)
  (list name value))

(defmethod attribute-name  ((attribute cons))
  (first attribute))

(defmethod attribute-value ((attribute cons))
  (second attribute))



;;; xmls entity name may go in namespaces in which case they're lists: (name namespace).

(defgeneric element-tag       (element)
  (:method ((element t))
    "")
  (:method ((element cons))
    (let ((tag (element-name element)))
      (if (listp tag)
          (first tag)
          tag))))

(defgeneric element-tag-equal-p (a b)
  (:method ((a string) (b string)) (string-equal a b))
  (:method ((a string) (b symbol)) (string-equal a b))
  (:method ((a symbol) (b string)) (string-equal a b))
  (:method ((a symbol) (b symbol)) (string-equal a b))
  (:method ((a cons)   (b cons))   (element-tag-equal-p (element-tag a) (element-tag b)))
  (:method ((a cons)   (b string)) (element-tag-equal-p (element-tag a) b))
  (:method ((a cons)   (b symbol)) (element-tag-equal-p (element-tag a) b))
  (:method ((a string) (b cons))   (element-tag-equal-p a (element-tag b)))
  (:method ((a symbol) (b cons))   (element-tag-equal-p a (element-tag b))))

(defgeneric attribute-name-equal-p (a b)
  (:method ((a string) (b string)) (string-equal a b))
  (:method ((a string) (b symbol)) (string-equal a b))
  (:method ((a symbol) (b string)) (string-equal a b))
  (:method ((a symbol) (b symbol)) (string-equal a b))
  (:method ((a cons)   (b cons))   (attribute-name-equal-p (attribute-name a) (attribute-name b)))
  (:method ((a cons)   (b string)) (attribute-name-equal-p (attribute-name a) b))
  (:method ((a cons)   (b symbol)) (attribute-name-equal-p (attribute-name a) b))
  (:method ((a string) (b cons))   (attribute-name-equal-p a (attribute-name b)))
  (:method ((a symbol) (b cons))   (attribute-name-equal-p a (attribute-name b))))



(defgeneric attribute-named (element attribute))
(defgeneric value-of-attribute-named (element attribute))

(defgeneric element-child (element))
(defgeneric string-single-child-p (element))

(defgeneric child-tagged (element tag))
(defgeneric children-tagged (element tag))
(defgeneric grandchildren-tagged (element tag))

(defgeneric child-valued (element attribute value))
(defgeneric children-valued (element attribute value))
(defgeneric grandchildren-valued (element attribute value))

(defgeneric child-tagged-and-valued (element tag attribute value))
(defgeneric children-tagged-and-valued (element tag attribute value))
(defgeneric grandchild-tagged-and-valued (element tag attribute value))

(defgeneric element-at-path (element tag-path))


(defmethod attribute-named (element attribute-name)
  (loop
    :for (k v) :on (element-attributes element) :by (function cddr)
    :until (attribute-name-equal-p attribute-name k)
    :finally (return (list k v))))

(defmethod value-of-attribute-named (element attribute-name)
  (attribute-value (attribute-named element attribute-name)))

(defmethod element-child (element)
  (first (element-children element)))

(defmethod string-single-child-p (element)
  (and (= 1 (length (element-children element)))
       (stringp (element-child element))))



(defmethod child-tagged (element tag)
  (find tag (element-children element)
        :test (function element-tag-equal-p)
        :key (function element-tag)))

(defmethod children-tagged (element tag)
  (remove tag (element-children element)
          :test-not (function element-tag-equal-p)
          :key (function element-tag)))

(defmethod grandchildren-tagged (element tag)
  (append (children-tagged element tag)
          (mapcan (lambda (child) (grandchildren-tagged child tag))
                  (element-children element))))


(defmethod child-valued (element attribute value)
  (find-if (lambda (child) (string-equal value (value-of-attribute-named child attribute)))
           (element-children element)))

(defmethod children-valued (element attribute value)
  (remove-if-not (lambda (child) (string-equal value (value-of-attribute-named child attribute)))
                 (element-children element)))

(defmethod grandchildren-valued (element attribute value)
  (append (children-valued element attribute value)
          (mapcan (lambda (child) (grandchildren-valued child attribute value))
                  (element-children element))))


(defmethod child-tagged-and-valued (element tag attribute value)
  (find-if (lambda (child)
             (and (consp child)
                  (element-tag-equal-p (element-tag child) tag)
                  (string-equal (value-of-attribute-named child attribute) value)))
           (element-children element)))

(defmethod children-tagged-and-valued (element tag attribute value)
  (remove-if-not (lambda (child)
                   (and (consp child)
                        (element-tag-equal-p (element-tag child) tag)
                        (string-equal (value-of-attribute-named child attribute) value)))
                 (element-children element)))

(defmethod grandchildren-tagged-and-valued (element tag attribute value)
  (append (children-tagged-and-valued element tag attribute value)
          (mapcan (lambda (child) (grandchildren-tagged-and-valued child tag attribute value))
                  (element-children element))))



(defmethod element-at-path (element tag-path)
  (if (null tag-path)
      element
      (element-at-path (child-tagged element (first tag-path))
                       (rest tag-path))))


(defun value-to-boolean (value)
  (string-equal "true" value))

;;;; THE END ;;;;


