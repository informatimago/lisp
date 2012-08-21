;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ibcl-source.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    See :documentation of package below.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-08-07 <PJB> Extracted from ibcl.lisp and restructured.
;;;;    2006-07-01 <PJB> Added deftype, defclass.
;;;;    2006-05-04 <PJB> Added this header. Augmented.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE"
  (:use "COMMON-LISP")
  (:export "SOURCE" ; accessor
           
           "LIST-PACKAGES-WITH-SOURCES"
           "SYMBOLS-WITH-SOURCES"
           "LIST-SOURCES"
           "SAVE-SOURCES"
           ;; --
           "*SOURCE-TYPES*")
  (:documentation "

This package manages the book-keeping of source forms for the CL
defining macros.

The source forms are kept in the property list of the symbol naming
the object defined by it.  On defpackage source forms are kept in a
separate hash-table.

The function SOURCE returns the source form of the given symbol for
the given source-type.

The function (setf source) sets the source form of the given symbol for
the given source-type..

The function LIST-PACKAGES-WITH-SOURCES returns a list of packages
where some of these variables or functions are defined.

The function SYMBOLS-WITH-SOURCES returns a list of symbols having
some source attached.

The function LIST-SOURCES returns a list of sources.

The function SAVE-SOURCES saves the definitions in a package, or all
the definitions to a file or stream.


Copyright Pascal J. Bourguignon 2006 - 2012
This package is provided under the Afero General Public License 3.
See the source file for details.

"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE")

;; (source x :setf) == (source '(setf x) :function)

(defvar *source-types*
  '(:type               ; deftype defstruct defclass define-condition
    :variable           ; defvar defparameter defconstant define-symbol-macro 
    :function           ; defun f        defgeneric f   defmacro define-modify-macro
    :setf-function      ; defun (setf f) defgeneric (setf f)
    :setf-macro         ; defsetf define-setf-expander
    :method             ; defmethod m
    :setf-method        ; defmethod (setf m) 
    :method-combination ; define-method-combination
    :compiler-macro     ; define-compiler-macro
    :package            ; defpackage
    ))


(defun source-type-designator (object)
  "
OBJECT:         A keyword, a symbol or string that is string-equal to one of
                the keywords member of *SOURCE-TYPES*.

RETURN:         The keywords member of *SOURCE-TYPES* designated by the OBJECT.

SIGNAL:         A TYPE-ERROR if OBJECT is not a source-type designator.
"
  (cond
    ((member object *source-types*)
     object)
    ((member object *source-types* :test (function string-equal))
     (intern (string-upcase object) "KEYWORD"))
    (t
     (error 'simple-type-error
            :datum object
            :expected-type 'source-type
            :format-control "~S is not a source-type designator (~{~S~^ ~})."
            :format-arguments (list object *source-types*)))))


(defun setf-name-p (name)
  "
RETURN:  If NAME is of the form (SETF REAL-NAME) then REAL-NAME else NIL.
"
  (and (consp name)
       (consp (cdr name))
       (null (cddr name))
       (eq 'setf (car name))
       (symbolp (cadr name))
       (cadr name)))


(defun normalize-object-and-source-type (object source-type)
  "
OBJECT:         A symbol or a (setf symbol);  or for methods, a list:
                (name qualifiers specializers);  or for packages, a
                package designator.

SOURCE-TYPE:    A source type designator.

RETURN:         If source-type is :method or :setf-method,
                then 3 values: the symbol of the method name,
                               the source-type :method or :setf-method
                               a list containing the method-qualifiers and the specializers.
                else 2 values: the symbol of the object, (or for packages, the package name string),
                               the source-type.

NOTE:           The returned source type can be changed to
                :setf-method, :setf-function or :setf-compiler-macro
                when it's :method, :function or :compiler-macro and
                the object is (setf real-name).
"
  (let ((source-type (source-type-designator source-type)))
    (flet ((err () (error "Invalid object ~S for source-type ~S" object source-type)))
      (case source-type

        ((:package)
         (values (find-package object) source-type))
        
        ((:method :setf-method)
         (if (and (listp object)
                  (= 3 (length object))
                  (listp (second object))
                  (listp (third object)))
           (cond
             ((symbolp (first object))
              (values (first object) source-type (rest object)))
             ((eq source-type :method)
              (let ((real-name (setf-name-p (first object))))
                (if real-name
                  (values real-name :setf-method (rest object))
                  (err))))
             (t
              (err)))
           (err)))
        
        ((:function :compiler-macro)
         (cond
           ((symbolp object)
            (values object source-type))
           (t
            (let ((real-name (setf-name-p object)))
              (if real-name
                (values real-name
                        (if (eq source-type :function)
                          :setf-function
                          :setf-compiler-macro))
                (err))))))

        (otherwise
         (if (symbolp object)
           (values object source-type)
           (err)))))))


(defun test/normalize-object-and-source-type ()
  (assert (equal (list
                 (multiple-value-list
                  (normalize-object-and-source-type 'f 'function))
                 (multiple-value-list
                  (normalize-object-and-source-type '(setf f) 'function))
                 (multiple-value-list
                  (normalize-object-and-source-type 'f 'setf-function))
                 (ignore-errors
                   (multiple-value-list
                    (normalize-object-and-source-type '(setf f) 'setf-function))))
                '((f :function)
                  (f :setf-function)
                  (f :setf-function)
                  nil)))
  (assert (equal (list
                 (multiple-value-list
                  (normalize-object-and-source-type '(m () (t t)) :method))
                 (multiple-value-list
                  (normalize-object-and-source-type '((setf m) () (t t)) ':method))
                 (multiple-value-list
                  (normalize-object-and-source-type '(m () (t t)) 'setf-method))
                 (ignore-errors
                   (multiple-value-list
                    (normalize-object-and-source-type '((setf m) () (t t)) :setf-method))))
                '((m :method (() (t t)))
                  (m :setf-method (() (t t)))
                  (m :setf-method (() (t t)))
                  nil)))
 :success)

(test/normalize-object-and-source-type)



;;; Sources for symbols are stored in a symbol property.  The property
;;; key is SOURCES.  The property value is an a-list mapping
;;; source-type to the source form.
;;;
;;; Sources for other objects are stored in the *SOURCES* hash-table.
;;; For now, only packages.  It should probably be a weak hash-table.

(defvar *sources* (make-hash-table :test (function equal))
  "A table mapping packages to their source forms.")

(defvar *order* '()
  "A big list of all the normalized (object source-type [other-keys])
that have been assigned some source, in the reverse order.  This list
allows to write the sources in a consistent order.")


(defgeneric operate-source (name source-type &key other-keys new-source package)
  (:documentation
   "
NEW-SOURCE:     A sexp.

PACKAGE:        The package that was *PACKAGE* when the NEW-SOURCE sexp was read.
                Used only when NEW-SOURCE is not NIL.

OBJECT:         A symbol, a (setf symbol), or for methods, a list:
                (name qualifiers specializers).

SOURCE-TYPE:    A source type designator.

OTHER-KEYS:     When SOURCE-TYPE is :method or :setf-method, the
                method-qualifiers and specializers.  In the future,
                other source-type may also require additionnal keys.

DO:             Set or get the source specified OBJECT and SOURCE-TYPE.
                If NEW-SOURCE is NIL, then get it, otherwise set it.

RETURN:         The source of the specified object; the package.
"))



;;; Register the object/source-type/other-keys in the *order* list.

(defmethod operate-source :after (name source-type &key other-keys new-source package)
  (declare (ignore package other-keys))
  (when new-source
    (pushnew (case source-type
               ((:method :setf-method) (list (cons name other-keys) source-type))
               (otherwise              (list name source-type)))
             *order* :test (function equal))))


;;; Operate packages:

(defmethod operate-source ((name t) (source-type (eql :package))
                              &key other-keys new-source package)
  (operate-source (find-package name) source-type
                  :other-keys other-keys
                  :new-source new-source
                  :package package))

(defmethod operate-source ((name package) (source-type (eql :package))
                              &key other-keys new-source package)
   (declare (ignore other-keys))
  (values-list (if new-source
                 (setf (gethash name *sources*) (list new-source package))
                 (gethash name *sources*))))


;;; Operate symbols:

(defmethod operate-source ((name symbol) (source-type symbol)
                              &key other-keys new-source package)
  (declare (ignore other-keys))
  (values-list (if new-source
                 (let ((entry (assoc source-type (get name 'sources))))
                   (if entry
                     (setf (cdr entry) (list new-source package))
                     (push (cons source-type (list new-source package)) (get name 'sources '())))
                   (list new-source package))
                 (cdr (assoc source-type (get name 'sources))))))


;;; Operate methods:

(defun operate-keyed (name source-type other-keys new-source package)
  (values-list (if new-source
                 (let ((entry (assoc source-type (get name 'sources))))
                   (if entry
                     (let ((mentry (assoc other-keys (cdr entry))))
                       (if mentry
                         (setf (cdr mentry) (list new-source package))
                         (push (cons other-keys (list new-source package)) (cdr entry))))
                     (push (cons source-type (acons other-keys (list new-source package) nil))
                           (get name 'sources '())))
                   (list new-source package))
                 (cdr (assoc other-keys (cdr (assoc source-type (get name 'sources)))
                             :test (function equal))))))

(defmethod operate-source ((name symbol) (source-type (eql :method))
                              &key other-keys new-source package)
  (operate-keyed name source-type other-keys new-source package))

(defmethod operate-source ((name symbol) (source-type (eql :setf-method))
                              &key other-keys new-source package)
  (operate-keyed name source-type other-keys new-source package))


;;; Public API:

(defun source (object source-type &optional (package *package*))
  "
OBJECT:         A symbol, a (setf symbol), or for methods, a list:
                (name qualifiers specializers).

SOURCE-TYPE:    A source type designator.

PACKAGE         Ignored.

RETURN:         The source form corresponding to the OBJECT (usually a
                symbol) interpreted as KIND.
"
  (multiple-value-bind (name source-type keys)
      (normalize-object-and-source-type object source-type)
    (operate-source name source-type
                    :other-keys keys
                    :package package)))


(defun (setf source) (new-source object source-type &optional (package *package*))
  "
NEW-SOURCE:     A sexp.

PACKAGE:        The package that was *PACKAGE* when the NEW-SOURCE sexp was read.
                Used only when NEW-SOURCE is not NIL.

OBJECT:         A symbol, a (setf symbol), or for methods, a list:
                (name qualifiers specializers).

SOURCE-TYPE:    A source type designator.

DO:             Registers the NEW-SOURCE as source for the specified
                OBJECT and SOURCE-TYPE.  The *PACKAGE* is recorded
                along with the source sexp, so that saving it can be
                done by printing it in the same package as when it was
                read.

RETURN:         NEW-SOURCE.
"
  (multiple-value-bind (name source-type keys)
      (normalize-object-and-source-type object source-type)
    (operate-source name source-type
                    :other-keys keys
                    :new-source new-source
                    :package package)))


;;; Tests (TBW):
;; (setf (source '(m () (t t)) :method) '(defmethod m (x y) (list x y)))
;; (setf (source 'm 'variable) '(defvar m 42))
;; (setf (source '*x* 'variable) '(defparameter *x* '(Hello world)))
;; (source '(m () (t t)) :method)
;; (defmethod m (x y) (list x y))
;; #<Package "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE">
;; (source 'm :variable)
;; (defvar m 42)
;; #<Package "COM.INFORMATIMAGO.COMMON-LISP.LISP.SOURCE">




(defun list-packages-with-sources ()
  "
RETURN:         A list of packages that have some sources saved.
"
  (loop
    :for package :in (list-all-packages)
    :when (do-symbols (sym package nil)
            (when (get sym 'sources)
              (return t)))
    :collect package))


(defun symbols-with-sources (&optional (package nil))
  "
RETURN:        A list of symbols in the package PACKAGE that have some
               sources.
"
  (let ((result '()))
    (do-symbols (sym package result)
      (when (get sym 'sources)
        (push sym result)))))


;; (symbols-with-sources (first (list-packages-with-sources)))
;; (m *x*)


(defun list-sources (&optional (package nil))
  "
PACKAGE:        A package designator to filter out the definitions
                made in this package, or NIL to get all of them.

RETURN:         A list of ((name source-type other-keys) source package) pairs.
"
  (mapcan (lambda (item)
              (destructuring-bind (object source-type) item
                (multiple-value-bind (source-form source-package) (source object source-type)
                  (when (or (null package) (eql package source-package))
                    (list (list (list object source-type) source-form source-package))))))
          (reverse *order*)))

;; (list-sources)


(defun save-sources (path-or-stream &key (package *package*) (line-spacing 0))
  "
PATH-OR-STREAM: A pathname designator, or a stream, where the sources will be saved.

PACKAGE:        A package designator indicating the package whose
                sources will be saved.  The default is *PACKAGE*.
                NIL may be passed to specify to save all packages.
"
  (labels ((do-it (out sources)
             (format out ";;;; -*- mode:lisp -*-")
             (format out "~%;;;; Generated from sources saved by ~A~V%" 
                     (package-name '#.*package*) line-spacing)
             (let ((*print-readably* nil)
                   (*print-case*     :downcase))
               (loop
                 :with current-package = (find-package "COMMON-LISP-USER")
                 :for (nil source package) :in sources
                 :do (progn
                       (unless (eql current-package package)
                         (let ((*package* current-package))
                           (pprint `(in-package ,(package-name package)) out)
                           (format out "~V%" line-spacing))
                         (setf current-package package))
                       (let ((*package* current-package))
                         (pprint source out)
                         (format out "~V%" line-spacing)))))
             (format out "~%;;;; THE END ;;;;~%")))
    (if (streamp path-or-stream)
      (do-it path-or-stream (list-sources package))
      (with-open-file (out path-or-stream
                           :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
        (do-it out (list-sources package)))))
  (values))

;; (save-sources *standard-output* :line-spacing 0)


;;;; THE END ;;;;
