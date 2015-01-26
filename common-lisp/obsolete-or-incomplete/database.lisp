;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               database.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A Lisp Database access layer.
;;;;
;;;;    This package defines a generic API to access databases.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-09-22 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DATABASE"
  (:use "COMMON-LISP")
  (:export "INITIALIZE-INSTANCE" "OBJECT-FILTER" "SET-OBJECT-FILTER"
           "ORDER-LIST" "SET-ORDER-LIST" "FETCH-OBJECTS" "COUNT-OBJECTS" "DELETE-OBJECT"
           "INSERT-OBJECT" "CREATE-OBJECT" "DATASOURCE" "DEF-ENTITY"
           "INTERNAL-ORDER-LIST" "DEFAULT-ORDER-LIST" "USER-SORT-ATTRIBUTES"
           "LIST-ATTRIBUTES" "ATTRIBUTES" "CONSTRUCTOR" "PLURAL" "ENTITY" "INPUT"
           "COMMENT" "EDIT-CONTROL" "SHOW-CONTROL" "OUTPUT" "ACCESSOR" "NAME" "REF"
           "ATTRIBUTE" "MESSAGE" "VALUE" "PLACE" "DATA-ERROR")
  (:documentation
   "This package defines a generic API to access databases.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.DATABASE")




(defgeneric attribute-with-ref (self ref))
(defgeneric create-object (self &rest args))
(defgeneric count-objects (self))
(defgeneric set-order-list (self order-list))
(defgeneric set-object-filter (self object-filter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENTITIES -- meta information about database entities
;;;
;;; This meta information describes the entities and their attributes, 
;;; with human readable names, input and output functions, etc.
;;;


(define-condition data-error (error)
  ((place
    :accessor place
    :initarg :place) ;; The variable name that contains a bad value.
   (value
    :accessor value
    :initarg :value) ;; The bad value.
   (message
    :accessor message
    :initarg :message) ;; The message explaining the problem.
   )
  (:documentation "A condition occuring when bad input data is detected.")
  (:report (lambda (condition stream)
             (format stream "~&A ~a attribute cannot contain ~A.~%~A~%"
                     (place condition) (value condition) (message condition))))
  ) ;;DATA-ERROR


(defclass attribute ()
  (
   (ref
    :accessor ref
    :initarg :ref
    :type symbol
    :documentation "A reference to the attribute.")
   (name
    :accessor name
    :initarg :name
    :type string
    :documentation "A human readable name for the attribute.")
   (accessor
    :accessor accessor
    :initarg :accessor
    :type symbol
    :documentation "The accessor function name.")
   (output
    :accessor output
    :initarg :output
    :documentation
    ":HIDDEN, or a (lambda (attribute) ...) returning a human readble
     string representation of the value to be displayed.")
   (show-control
    :accessor show-control
    :initarg :show-control
    :documentation
    "Describes to the UI layer how to show the attribute (read-only).")
   (edit-control
    :accessor edit-control
    :initarg :edit-control
    :documentation
    "Describes to the UI layer how to edit the attribute (read-write).")
   (comment
    :accessor comment
    :initarg :comment
    :type string
    :documentation "A comment/help to be displayed along with the attribute.")
   (input
     :accessor input
     :initarg :input
     :documentation
     "A function used to check and convert the input value from the form
     attribute returning the input value, or signaling a DATA-ERROR.")
   )
  (:documentation "Description of a entity attribute.")
  ) ;;ATTRIBUTE
   

(defclass entity ()
  (
   (ref
    :accessor ref
    :initarg :ref
    :type     symbol
    :documentation
    "A reference to the entity description.
     The value slot of this symbol stores the ENTITY instance.")
   (name
    :accessor name
    :initarg :name
    :type     string
    :documentation
    "A human readable name for the entity represented by this entity.")
   (plural
    :accessor plural
    :initarg :plural
    :type     string
    :documentation "The plural form of the NAME.")
   (constructor
    :accessor constructor
    :initarg :constructor
    :type     symbol
    :documentation
    "The constructor function symbol, used to make a new entity." )
   (attributes
    :accessor attributes
    :initarg :attributes
    :type     list
    :documentation "The list of ATTRIBUTE descriptions.")
   (list-attributes
    :accessor list-attributes
    :initarg :list-attributes
    :type     list
    :documentation
    "The list of ATTRIBUTE references that must be used for lists, in order.")
   (user-sort-attributes
    :accessor user-sort-attributes
    :initarg :user-sort-attributes
    :type list
    :documentation
    "The list of ATTRIBUTE references available as sort criteria to the user.
     Note: this ought to be a sublist of list-attributes.")
   (default-order-list
       :accessor default-order-list
     :initarg :default-order-list
     :type     list
     :documentation
     "The order list used as default sort criteria.
      A list of (ATTRIBUTE-reference  (OR :ASCEND :DESCENT)).")
   (internal-order-list
    :accessor internal-order-list
    :initarg :internal-order-list
    :type     list
    :documentation
    "An addition order list appended to the user order lists,
      to get a consistent sort order.
       A list of (ATTRIBUTE-reference  (OR :ASCEND :DESCENT)).")
   )
  (:documentation "Description of a entity.")
  ) ;;ENTITY



(defmethod attribute-with-ref ((self entity) (ref symbol))
  (car (member ref (attributes self) :key (function ref)))
  ) ;;attribute-with-ref


;;; ------------------------------------------------------------------------
;;; Defining entities and their attributes easily.
;;; ----------------------------------------------
;;;

(defun gather-attributes (arguments expected)
  ;; This is the attributes of the CLOS object ENTITY and ATTRIBUTE!
  (do ((attributes '())
       (arg arguments (cddr arg)))
      ((null arg) (nreverse attributes))
    (let ((tok-process (assoc (first arg) expected)))
      (unless tok-process
        (error "Unexpected token at: ~S." arg))
      (push (first arg) attributes)
      (push (funcall (coerce (cdr tok-process) 'function)
                     (eval (second arg))) attributes)))
  ) ;;GATHER-ATTRIBUTES


(defun make-type-check (type attribute)
  (lambda (attrib)
    (unless (typep attrib type)
      (error "Expected a ~A as ~A instead of ~S." type attribute attrib))
    attrib)
  ) ;;MAKE-TYPE-CHECK


(defmacro def-entity (instvar &rest args)
  "
DO:    Generate an instruction to make an instance of a ENTITY description.
       This instance is assigned to the variable INSTVAR.
"
  `(defparameter
       ,instvar
     (apply
      (function make-instance) 'entity 
      (gather-attributes
       (cons :ref '(',instvar ,@args))
       `((:ref         . ,(make-type-check 'symbol "entity reference"))
         (:name        . ,(make-type-check 'string "entity name"))
         (:plural      . ,(make-type-check 'string "entity plural"))
         (:constructor . ,(make-type-check 'symbol "entity constructor"))
         (:list-attributes . ,(make-type-check 'list   "entity list attributes"))
         (:user-sort-attribute
          . ,(make-type-check 'list "entity user sort list"))
         (:default-order-list
             . ,(make-type-check 'list "entity default order  list"))
         (:internal-order-list
          . ,(make-type-check 'list "entity internal order  list"))
         (:attributes
          . ,(lambda (attributes)
                     (mapcar
                      (lambda (attrib)
                        (gather-attributes
                         (cons :ref (cons (list 'quote (car attrib)) (cdr attrib)))
                         '((:ref    . ,(make-type-check 'symbol "attribute reference"))
                           (:name     . ,(make-type-check 'string "attribute name"))
                           (:accessor . ,(make-type-check 'symbol "attribute accessor"))
                           (:output
                            . (lambda (attrib)
                                (cond
                                  ((eq attrib :hidden) attrib)
                                  ((functionp attrib) attrib)
                                  ((and (listp attrib) (eq 'lambda (first attrib)))
                                   (eval attrib))
                                  ((and (listp attrib) (eq 'function (first attrib)))
                                   (eval attrib))
                                  ((and (symbolp attrib) (fboundp attrib))
                                   (eval `(function ,attrib)))
                                  (t (error "Expected :HIDDEN or a function as attribute output instead of ~S."  attrib)))))
                           (:show-control . ,(function identity))
                           ;;(MAKE-TYPE-CHECK 'LIST "attribute SHOW-control widget"))
                           (:edit-control . ,(function identity))
                           ;;(MAKE-TYPE-CHECK 'LIST "attribute EDIT-control widget"))
                           (:comment . ,(make-type-check '(or null string) "attribute comment"))
                           (:input
                             . (lambda (attrib)
                                 (cond
                                   ((functionp attrib) attrib)
                                   ((and (listp attrib) (eq 'lambda (first attrib)))
                                    (eval attrib))
                                   ((and (listp attrib) (eq 'function (first attrib)))
                                    (eval attrib))
                                   ((and (symbolp attrib) (fboundp attrib))
                                    (eval `(function ,attrib)))
                                   (t (error "Expected a function as attribute input instead of ~S."  attrib)))))
                           )
                         ))
                      attributes))))
       )))
  ) ;;DEF-ENTITY


;;; ------------------------------------------------------------------------
;;; Editing entities
;;; ---------------
;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DATASOURCE abstraction
;;;


(defclass datasource ()
  (
   (entity
    :accessor entity
    :initarg :entity
    :type     entity
    :documentation
    "The ENTITY describing the objects contained in this DATASOURCE.")
   (order-list
    :accessor order-list
    :initarg :order-list
    :type     list
    :documentation "The order list used to sort the data.")
   (object-filter
    :accessor object-filter
    :initarg :object-filter-list
    :type     list
    :documentation "The filter used to select the data from the source.")
   )
  (:documentation "An abstract data source.")
  ) ;;DATASOURCE


(defmethod initialize-instance :after ((self datasource)
                                       &rest initargs
                                       &key &allow-other-keys)
  "
DO:     Initalize the instance.
"
  (declare (ignore initargs))
  (setf (object-filter self) t) ;; return all objects.
  self)                         ;;INITIALIZE-INSTANCE


(defmethod create-object ((self datasource) &rest args)
  "Creates a new instance of the ENTITY.
This instance is not inserted into the DATASOURCE: use INSERT-OBJECT.
The ARGS are passed to the entity constructor function."
  (apply (constructor (entity self)) args)
  ) ;;CREATE-OBJECT


(defgeneric insert-object (datasource object)
  (:documentation "Inserts the OBJECT into the DATASOURCE.")
  ) ;;INSERT-OBJECT


(defgeneric delete-object (datasource object)
  (:documentation "Deletes the OBJECT from the DATASOURCE.")
  ) ;;DELETE-OBJECT


(defmethod count-objects ((self datasource))
  "Returns the number of OBJECTS present in the DATASOURCE.
This default implementation just fetches all the objects and count them.
It should rather be overriden by subclasses!"
  (length (fetch-objects self))
  ) ;;COUNT-OBJECTS


(defgeneric fetch-objects (datasource &key start end)
  (:documentation "Returns a list of objects found in the DATASOURCE.
   When the key START and END are present, returns only this range of objects.")
  ) ;;FETCH-OBJECTS


(defmethod set-order-list ((self datasource) (order-list list))
  "Set the ORDER-LIST of the DATASOURCE.
Next time FETCH-OBJECTS will be called, the objects will be returned in
following this ORDER-LIST.
An ORDER-LIST has the following syntax: ( ( FIELD-REF :ASCEND|:DESCENT )* )
When NIL, the default-user-order-list from the entity is used."
  (setf (order-list self) order-list)
  ) ;;SET-ORDER-LIST


(defmethod set-object-filter ((self datasource) (object-filter t))
  "Set the OBJECT-FILTER of the DATASOURCE.
   Next time FETCH-OBJECTS or COUNT-OBJECTS will be called, only objects
   passing this OBJECT-FILTER will be returned or counted.
   An OBJECT-FILTER has the following syntax:
      OBJECT-FILTER ::=  NIL       ;; no object is selected.
                      |  T         ;; all objects are selected.
                      |  (NOT OBJECT-FILTER)
                      |  (AND OBJECT-FILTER*)
                      |  (OR  OBJECT-FILTER*)
                      |  (MATCHES    FIELD-REF PATTERN)
                      |  (=          EXPRESSION EXPRESSION)
                      |  (/=         EXPRESSION EXPRESSION)
                      |  (<          EXPRESSION EXPRESSION)
                      |  (<=         EXPRESSION EXPRESSION)
                      |  (>          EXPRESSION EXPRESSION)
                      |  (>=         EXPRESSION EXPRESSION)
                      |  (MEMBER     FIELD-REF  (EXPRESSION*))
                      .
      EXPRESSION    ::= (* EXPRESSION*)
                      | (/ EXPRESSION EXPRESSION)
                      | (+ EXPRESSION*)
                      | (- EXPRESSION EXPRESSION)
                      | (- EXPRESSION)
                      | FIELD-REF | STRING | NUMBER .
      PATTERN       ::= STRING . ;; with zero, one or more '_' or '%' wildcards.
      FIELD-REF     ::= SYMBOL .
   "
  (setf (object-filter self) object-filter)
  ) ;;SET-OBJECT-FILTER





;;;; database.lisp                    --                     --          ;;;;
