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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DATABASE"
  (:USE "COMMON-LISP")
  (:EXPORT "INITIALIZE-INSTANCE" "OBJECT-FILTER" "SET-OBJECT-FILTER"
           "ORDER-LIST" "SET-ORDER-LIST" "FETCH-OBJECTS" "COUNT-OBJECTS" "DELETE-OBJECT"
           "INSERT-OBJECT" "CREATE-OBJECT" "DATASOURCE" "DEF-ENTITY"
           "INTERNAL-ORDER-LIST" "DEFAULT-ORDER-LIST" "USER-SORT-ATTRIBUTES"
           "LIST-ATTRIBUTES" "ATTRIBUTES" "CONSTRUCTOR" "PLURAL" "ENTITY" "INPUT"
           "COMMENT" "EDIT-CONTROL" "SHOW-CONTROL" "OUTPUT" "ACCESSOR" "NAME" "REF"
           "ATTRIBUTE" "MESSAGE" "VALUE" "PLACE" "DATA-ERROR")
  (:DOCUMENTATION
   "This package defines a generic API to access databases.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.DATABASE")




(DEFGENERIC attribute-with-ref (self ref))
(DEFGENERIC CREATE-OBJECT (SELF &REST ARGS))
(DEFGENERIC COUNT-OBJECTS (SELF))
(DEFGENERIC SET-ORDER-LIST (SELF ORDER-LIST))
(DEFGENERIC SET-OBJECT-FILTER (SELF OBJECT-FILTER))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENTITIES -- meta information about database entities
;;;
;;; This meta information describes the entities and their attributes, 
;;; with human readable names, input and output functions, etc.
;;;


(DEFINE-CONDITION DATA-ERROR (ERROR)
  ((PLACE
    :ACCESSOR PLACE
    :INITARG :PLACE) ;; The variable name that contains a bad value.
   (VALUE
    :ACCESSOR VALUE
    :INITARG :VALUE) ;; The bad value.
   (MESSAGE
    :ACCESSOR MESSAGE
    :INITARG :MESSAGE) ;; The message explaining the problem.
   )
  (:DOCUMENTATION "A condition occuring when bad input data is detected.")
  (:REPORT (LAMBDA (CONDITION STREAM)
             (FORMAT STREAM "~&A ~a attribute cannot contain ~A.~%~A~%"
                     (PLACE CONDITION) (VALUE CONDITION) (MESSAGE CONDITION))))
  ) ;;DATA-ERROR


(DEFCLASS ATTRIBUTE ()
  (
   (REF
    :ACCESSOR REF
    :INITARG :REF
    :TYPE SYMBOL
    :DOCUMENTATION "A reference to the attribute.")
   (NAME
    :ACCESSOR NAME
    :INITARG :NAME
    :TYPE STRING
    :DOCUMENTATION "A human readable name for the attribute.")
   (ACCESSOR
    :ACCESSOR ACCESSOR
    :INITARG :ACCESSOR
    :TYPE SYMBOL
    :DOCUMENTATION "The accessor function name.")
   (OUTPUT
    :ACCESSOR OUTPUT
    :INITARG :OUTPUT
    :DOCUMENTATION
    ":HIDDEN, or a (lambda (attribute) ...) returning a human readble
     string representation of the value to be displayed.")
   (SHOW-CONTROL
    :ACCESSOR SHOW-CONTROL
    :INITARG :SHOW-CONTROL
    :DOCUMENTATION
    "Describes to the UI layer how to show the attribute (read-only).")
   (EDIT-CONTROL
    :ACCESSOR EDIT-CONTROL
    :INITARG :EDIT-CONTROL
    :DOCUMENTATION
    "Describes to the UI layer how to edit the attribute (read-write).")
   (COMMENT
    :ACCESSOR COMMENT
    :INITARG :COMMENT
    :TYPE STRING
    :DOCUMENTATION "A comment/help to be displayed along with the attribute.")
   (INPUT
     :ACCESSOR INPUT
     :INITARG :INPUT
     :DOCUMENTATION
     "A function used to check and convert the input value from the form
     attribute returning the input value, or signaling a DATA-ERROR.")
   )
  (:DOCUMENTATION "Description of a entity attribute.")
  ) ;;ATTRIBUTE
   

(DEFCLASS ENTITY ()
  (
   (REF
    :ACCESSOR REF
    :INITARG :REF
    :TYPE     SYMBOL
    :DOCUMENTATION
    "A reference to the entity description.
     The value slot of this symbol stores the ENTITY instance.")
   (NAME
    :ACCESSOR NAME
    :INITARG :NAME
    :TYPE     STRING
    :DOCUMENTATION
    "A human readable name for the entity represented by this entity.")
   (PLURAL
    :ACCESSOR PLURAL
    :INITARG :PLURAL
    :TYPE     STRING
    :DOCUMENTATION "The plural form of the NAME.")
   (CONSTRUCTOR
    :ACCESSOR CONSTRUCTOR
    :INITARG :CONSTRUCTOR
    :TYPE     SYMBOL
    :DOCUMENTATION
    "The constructor function symbol, used to make a new entity." )
   (ATTRIBUTES
    :ACCESSOR ATTRIBUTES
    :INITARG :ATTRIBUTES
    :TYPE     LIST
    :DOCUMENTATION "The list of ATTRIBUTE descriptions.")
   (LIST-ATTRIBUTES
    :ACCESSOR LIST-ATTRIBUTES
    :INITARG :LIST-ATTRIBUTES
    :TYPE     LIST
    :DOCUMENTATION
    "The list of ATTRIBUTE references that must be used for lists, in order.")
   (USER-SORT-ATTRIBUTES
    :ACCESSOR USER-SORT-ATTRIBUTES
    :INITARG :USER-SORT-ATTRIBUTES
    :TYPE LIST
    :DOCUMENTATION
    "The list of ATTRIBUTE references available as sort criteria to the user.
     Note: this ought to be a sublist of list-attributes.")
   (DEFAULT-ORDER-LIST
       :ACCESSOR DEFAULT-ORDER-LIST
     :INITARG :DEFAULT-ORDER-LIST
     :TYPE     LIST
     :DOCUMENTATION
     "The order list used as default sort criteria.
      A list of (ATTRIBUTE-reference  (OR :ASCEND :DESCENT)).")
   (INTERNAL-ORDER-LIST
    :ACCESSOR INTERNAL-ORDER-LIST
    :INITARG :INTERNAL-ORDER-LIST
    :TYPE     LIST
    :DOCUMENTATION
    "An addition order list appended to the user order lists,
      to get a consistent sort order.
       A list of (ATTRIBUTE-reference  (OR :ASCEND :DESCENT)).")
   )
  (:DOCUMENTATION "Description of a entity.")
  ) ;;ENTITY



(defmethod attribute-with-ref ((self entity) (ref symbol))
  (car (member ref (attributes self) :key (function ref)))
  ) ;;attribute-with-ref


;;; ------------------------------------------------------------------------
;;; Defining entities and their attributes easily.
;;; ----------------------------------------------
;;;

(DEFUN GATHER-ATTRIBUTES (ARGUMENTS EXPECTED)
  ;; This is the attributes of the CLOS object ENTITY and ATTRIBUTE!
  (DO ((ATTRIBUTES '())
       (ARG ARGUMENTS (CDDR ARG)))
      ((NULL ARG) (NREVERSE ATTRIBUTES))
    (LET ((TOK-PROCESS (ASSOC (FIRST ARG) EXPECTED)))
      (UNLESS TOK-PROCESS
        (ERROR "Unexpected token at: ~S." ARG))
      (PUSH (FIRST ARG) ATTRIBUTES)
      (PUSH (FUNCALL (COERCE (CDR TOK-PROCESS) 'FUNCTION)
                     (EVAL (SECOND ARG))) ATTRIBUTES)))
  ) ;;GATHER-ATTRIBUTES


(DEFUN MAKE-TYPE-CHECK (TYPE ATTRIBUTE)
  (LAMBDA (ATTRIB)
    (UNLESS (TYPEP ATTRIB TYPE)
      (ERROR "Expected a ~A as ~A instead of ~S." TYPE ATTRIBUTE ATTRIB))
    ATTRIB)
  ) ;;MAKE-TYPE-CHECK


(DEFMACRO DEF-ENTITY (INSTVAR &REST ARGS)
  "
DO:    Generate an instruction to make an instance of a ENTITY description.
       This instance is assigned to the variable INSTVAR.
"
  `(DEFPARAMETER
       ,INSTVAR
     (APPLY
      (FUNCTION MAKE-INSTANCE) 'ENTITY 
      (GATHER-ATTRIBUTES
       (CONS :REF '(',INSTVAR ,@ARGS))
       `((:REF         . ,(MAKE-TYPE-CHECK 'SYMBOL "entity reference"))
         (:NAME        . ,(MAKE-TYPE-CHECK 'STRING "entity name"))
         (:PLURAL      . ,(MAKE-TYPE-CHECK 'STRING "entity plural"))
         (:CONSTRUCTOR . ,(MAKE-TYPE-CHECK 'SYMBOL "entity constructor"))
         (:LIST-ATTRIBUTES . ,(MAKE-TYPE-CHECK 'LIST   "entity list attributes"))
         (:USER-SORT-ATTRIBUTE
          . ,(MAKE-TYPE-CHECK 'LIST "entity user sort list"))
         (:DEFAULT-ORDER-LIST
             . ,(MAKE-TYPE-CHECK 'LIST "entity default order  list"))
         (:INTERNAL-ORDER-LIST
          . ,(MAKE-TYPE-CHECK 'LIST "entity internal order  list"))
         (:ATTRIBUTES
          . ,(LAMBDA (ATTRIBUTES)
                     (MAPCAR
                      (LAMBDA (ATTRIB)
                        (GATHER-ATTRIBUTES
                         (CONS :REF (CONS (LIST 'QUOTE (CAR ATTRIB)) (CDR ATTRIB)))
                         '((:REF    . ,(MAKE-TYPE-CHECK 'SYMBOL "attribute reference"))
                           (:NAME     . ,(MAKE-TYPE-CHECK 'STRING "attribute name"))
                           (:ACCESSOR . ,(MAKE-TYPE-CHECK 'SYMBOL "attribute accessor"))
                           (:OUTPUT
                            . (LAMBDA (ATTRIB)
                                (COND
                                  ((EQ ATTRIB :HIDDEN) ATTRIB)
                                  ((FUNCTIONP ATTRIB) ATTRIB)
                                  ((AND (LISTP ATTRIB) (EQ 'LAMBDA (FIRST ATTRIB)))
                                   (EVAL ATTRIB))
                                  ((AND (LISTP ATTRIB) (EQ 'FUNCTION (FIRST ATTRIB)))
                                   (EVAL ATTRIB))
                                  ((AND (SYMBOLP ATTRIB) (FBOUNDP ATTRIB))
                                   (EVAL `(FUNCTION ,ATTRIB)))
                                  (T (ERROR "Expected :HIDDEN or a function as attribute output instead of ~S."  ATTRIB)))))
                           (:SHOW-CONTROL . ,(FUNCTION IDENTITY))
                           ;;(MAKE-TYPE-CHECK 'LIST "attribute SHOW-control widget"))
                           (:EDIT-CONTROL . ,(FUNCTION IDENTITY))
                           ;;(MAKE-TYPE-CHECK 'LIST "attribute EDIT-control widget"))
                           (:COMMENT . ,(MAKE-TYPE-CHECK '(OR NULL STRING) "attribute comment"))
                           (:INPUT
                             . (LAMBDA (ATTRIB)
                                 (COND
                                   ((FUNCTIONP ATTRIB) ATTRIB)
                                   ((AND (LISTP ATTRIB) (EQ 'LAMBDA (FIRST ATTRIB)))
                                    (EVAL ATTRIB))
                                   ((AND (LISTP ATTRIB) (EQ 'FUNCTION (FIRST ATTRIB)))
                                    (EVAL ATTRIB))
                                   ((AND (SYMBOLP ATTRIB) (FBOUNDP ATTRIB))
                                    (EVAL `(FUNCTION ,ATTRIB)))
                                   (T (ERROR "Expected a function as attribute input instead of ~S."  ATTRIB)))))
                           )
                         ))
                      ATTRIBUTES))))
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


(DEFCLASS DATASOURCE ()
  (
   (ENTITY
    :ACCESSOR ENTITY
    :INITARG :ENTITY
    :TYPE     ENTITY
    :DOCUMENTATION
    "The ENTITY describing the objects contained in this DATASOURCE.")
   (ORDER-LIST
    :ACCESSOR ORDER-LIST
    :INITARG :ORDER-LIST
    :TYPE     LIST
    :DOCUMENTATION "The order list used to sort the data.")
   (OBJECT-FILTER
    :ACCESSOR OBJECT-FILTER
    :INITARG :OBJECT-FILTER-LIST
    :TYPE     LIST
    :DOCUMENTATION "The filter used to select the data from the source.")
   )
  (:DOCUMENTATION "An abstract data source.")
  ) ;;DATASOURCE


(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((SELF DATASOURCE)
                                       &REST INITARGS
                                       &KEY &ALLOW-OTHER-KEYS)
  "
DO:     Initalize the instance.
"
  (DECLARE (IGNORE INITARGS))
  (SETF (OBJECT-FILTER SELF) T) ;; return all objects.
  SELF)                         ;;INITIALIZE-INSTANCE


(DEFMETHOD CREATE-OBJECT ((SELF DATASOURCE) &REST ARGS)
  "Creates a new instance of the ENTITY.
This instance is not inserted into the DATASOURCE: use INSERT-OBJECT.
The ARGS are passed to the entity constructor function."
  (APPLY (CONSTRUCTOR (ENTITY SELF)) ARGS)
  ) ;;CREATE-OBJECT


(DEFGENERIC INSERT-OBJECT (DATASOURCE OBJECT)
  (:DOCUMENTATION "Inserts the OBJECT into the DATASOURCE.")
  ) ;;INSERT-OBJECT


(DEFGENERIC DELETE-OBJECT (DATASOURCE OBJECT)
  (:DOCUMENTATION "Deletes the OBJECT from the DATASOURCE.")
  ) ;;DELETE-OBJECT


(DEFMETHOD COUNT-OBJECTS ((SELF DATASOURCE))
  "Returns the number of OBJECTS present in the DATASOURCE.
This default implementation just fetches all the objects and count them.
It should rather be overriden by subclasses!"
  (LENGTH (FETCH-OBJECTS SELF))
  ) ;;COUNT-OBJECTS


(DEFGENERIC FETCH-OBJECTS (DATASOURCE &KEY START END)
  (:DOCUMENTATION "Returns a list of objects found in the DATASOURCE.
   When the key START and END are present, returns only this range of objects.")
  ) ;;FETCH-OBJECTS


(DEFMETHOD SET-ORDER-LIST ((SELF DATASOURCE) (ORDER-LIST LIST))
  "Set the ORDER-LIST of the DATASOURCE.
Next time FETCH-OBJECTS will be called, the objects will be returned in
following this ORDER-LIST.
An ORDER-LIST has the following syntax: ( ( FIELD-REF :ASCEND|:DESCENT )* )
When NIL, the default-user-order-list from the entity is used."
  (SETF (ORDER-LIST SELF) ORDER-LIST)
  ) ;;SET-ORDER-LIST


(DEFMETHOD SET-OBJECT-FILTER ((SELF DATASOURCE) (OBJECT-FILTER T))
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
  (SETF (OBJECT-FILTER SELF) OBJECT-FILTER)
  ) ;;SET-OBJECT-FILTER





;;;; database.lisp                    --                     --          ;;;;
