;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               graph.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;    
;;;;    Graph class.
;;;;
;;;;    This is a CLOS based implementation of graphs.
;;;;    It comes from an emacs/eieio implementation used to analyze
;;;;    CVS versioning graph.
;;;;
;;;;    Subclasses exist to generate dot files, and Diagram! files.
;;;;
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-06 <PJB> Added defgeneric.
;;;;    2003-05-16 <PJB> Converted to Common-Lisp.
;;;;    2003-05-14 <PJB> Extracted from pjb-cvs.el
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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
;;;;    You should have received a COPY of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.LIST" "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT "EDGE-CLASS" "EDGES" "NODES" "TO" "FROM" "NODES" "WEIGHT" "INDEX"
           "ELEMENTS" "PROPERTIES" "IDENT" "SHOW-GRAPH" "FIND-NODES-WITH-PROPERTY"
           "COPY" "WALK-EDGES-FROM-NODE" "WALK-FROM-NODE" "FLOW-DISTANCE-FROM-NODE"
           "ADJACENT-NODES" "SUCCESSOR-NODES" "DIRECTED-EDGES-FROM-NODE"
           "DIRECTED-EDGES-BETWEEN-NODES" "EDGES-BETWEEN-NODES"
           "REMOVE-EDGES-BETWEEN-NODES" "REMOVE-EDGES" "REMOVE-EDGE"
           "ADD-EDGE-BETWEEN-NODES" "ADD-EDGE" "REMOVE-NODES" "REMOVE-NODE" "ADD-NODES"
           "ADD-NODE" "DESCRIPTION" "GRAPH-CLASS" "SUBCLASS-OF-EDGE-P"
           "WEIGHTED-DIRECTED-EDGE-CLASS" "SET-NODES" "SUCCESSOR-OF" "IS-BETWEEN-NODES"
           "NODES" "DIRECTED-EDGE-CLASS" "WEIGHTED-UNDIRECTED-EDGE-CLASS"
           "IDENTICAL-NODES" "UNDIRECTED-EDGE-CLASS" "NODE-CONS-P" "SET-WEIGHT"
           "WEIGHT-MIXIN-CLASS" "EDGE-CLASS" "ADD-ELEMENT" "CONTAINS-ELEMENT"
           "HASHED-SET-CLASS" "FIND-ELEMENTS-WITH-PROPERTY" "ELEMENT-LIST"
           "SELECT-ELEMENTS" "MAP-ELEMENTS" "PERFORM-WITH-ELEMENTS" "REMOVE-ELEMENT"
           "ADD-ELEMENTS" "CARDINAL" "SET-CLASS" "DELETE-PROPERTY" "GET-PROPERTY"
           "SET-PROPERTY" "PROPERTY-NAMES" "INITIALIZE-INSTANCE" "ELEMENT-CLASS")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "WHILE")
  (:DOCUMENTATION
   "This package exports classes for elements, sets and graphs.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH")



;;; cat graph.lisp | sed -n -e '/^(DEF/s/^(DEF/;; /p' -e 's/;; UN/;; FUN/'

;; CLASS ELEMENT-CLASS ()
;; METHOD INITIALIZE-INSTANCE ((SELF ELEMENT-CLASS) FIELDS)
;; METHOD DESCRIPTION ((SELF ELEMENT-CLASS))
;; METHOD PROPERTY-NAMES ((SELF ELEMENT-CLASS))
;; METHOD SET-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL) PROP-VALUE)
;; METHOD GET-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL))
;; METHOD DELETE-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL))

;; CLASS SET-CLASS (ELEMENT-CLASS)
;; METHOD DESCRIPTION ((SELF SET-CLASS))
;; METHOD CARDINAL ((SELF SET-CLASS))
;; METHOD CONTAINS-ELEMENT ((SELF SET-CLASS) (ANELEMENT ELEMENT-CLASS))
;; METHOD ADD-ELEMENT ((SELF SET-CLASS) (NEWELEMENT ELEMENT-CLASS))
;; METHOD ADD-ELEMENTS ((SELF SET-CLASS) NEWELEMENTLIST)
;; METHOD REMOVE-ELEMENT ((SELF SET-CLASS) (OLDELEMENT ELEMENT-CLASS))
;; METHOD PERFORM-WITH-ELEMENTS ((SELF SET-CLASS) LAMBDA-BODY)
;; METHOD MAP-ELEMENTS ((SELF SET-CLASS) LAMBDA-BODY)
;; METHOD SELECT-ELEMENTS ((SELF SET-CLASS) SELECT-LAMBDA)
;; METHOD ELEMENT-LIST ((SELF SET-CLASS))
;; METHOD FIND-ELEMENTS-WITH-PROPERTY

;; CLASS HASHED-SET-CLASS (SET-CLASS)
;; METHOD CONTAINS-ELEMENT ((SELF HASHED-SET-CLASS) (ANELEMENT ELEMENT-CLASS))
;; METHOD ADD-ELEMENT ((SELF HASHED-SET-CLASS) (NEWELEMENT ELEMENT-CLASS))

;; CLASS EDGE-CLASS (ELEMENT-CLASS)
;; METHOD DESCRIPTION ((SELF EDGE-CLASS))
;; GENERIC COPY ((SELF EDGE-CLASS))
;; GENERIC NODES ((SELF EDGE-CLASS))
;; GENERIC IS-BETWEEN-NODES ((SELF EDGE-CLASS) 
;; GENERIC SUCCESSOR-OF ((SELF EDGE-CLASS) (NODE ELEMENT-CLASS))

;; CLASS WEIGHT-MIXIN-CLASS ()
;; METHOD SET-WEIGHT ((SELF WEIGHT-MIXIN-CLASS) (NEWWEIGHT INTEGER))
;; FUN NODE-CONS-P (ITEM)

;; CLASS UNDIRECTED-EDGE-CLASS (EDGE-CLASS)
;; METHOD COPY ((SELF UNDIRECTED-EDGE-CLASS))
;; FUN IDENTICAL-NODES (NODES-CONS-A NODES-CONS-B)
;; METHOD IS-BETWEEN-NODES ((SELF EDGE-CLASS) 
;; METHOD SUCCESSOR-OF ((SELF UNDIRECTED-EDGE-CLASS) (NODE ELEMENT-CLASS))
;; METHOD SET-NODES ((SELF UNDIRECTED-EDGE-CLASS) 

;; CLASS WEIGHTED-UNDIRECTED-EDGE-CLASS
;; METHOD DESCRIPTION ((SELF WEIGHTED-UNDIRECTED-EDGE-CLASS))
;; CLASS DIRECTED-EDGE-CLASS (EDGE-CLASS)
;; METHOD COPY ((SELF DIRECTED-EDGE-CLASS))
;; METHOD NODES ((SELF DIRECTED-EDGE-CLASS))
;; METHOD IS-BETWEEN-NODES ((SELF EDGE-CLASS) 
;; METHOD SUCCESSOR-OF ((SELF DIRECTED-EDGE-CLASS) (NODE ELEMENT-CLASS))
;; METHOD SET-NODES ((SELF DIRECTED-EDGE-CLASS)
;; CLASS WEIGHTED-DIRECTED-EDGE-CLASS (DIRECTED-EDGE-CLASS WEIGHT-MIXIN-CLASS)
;; METHOD DESCRIPTION ((SELF WEIGHTED-DIRECTED-EDGE-CLASS))
;; METHOD COPY ((SELF WEIGHTED-DIRECTED-EDGE-CLASS))
;; FUN SUBCLASS-OF-EDGE-P (ITEM)

;; CLASS GRAPH-CLASS (ELEMENT-CLASS)
;; METHOD DESCRIPTION ((SELF GRAPH-CLASS))
;; METHOD ADD-NODE ((SELF GRAPH-CLASS) (NEWNODE ELEMENT-CLASS))
;; METHOD ADD-NODES ((SELF GRAPH-CLASS) NEWNODELIST)
;; METHOD REMOVE-NODE ((SELF GRAPH-CLASS) (OLDNODE ELEMENT-CLASS))
;; METHOD REMOVE-NODES ((SELF GRAPH-CLASS) OLDNODELIST)
;; METHOD ADD-EDGE ((SELF GRAPH-CLASS) (NEWEDGE EDGE-CLASS))
;; METHOD ADD-EDGE-BETWEEN-NODES ((SELF GRAPH-CLASS) 
;; METHOD REMOVE-EDGE ((SELF GRAPH-CLASS) (OLDEDGE EDGE-CLASS))
;; METHOD REMOVE-EDGES ((SELF GRAPH-CLASS) EDGE-LIST)
;; METHOD REMOVE-EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS)
;; METHOD EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS)
;; METHOD DIRECTED-EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS) 
;; METHOD DIRECTED-EDGES-FROM-NODE ((SELF GRAPH-CLASS)
;; METHOD SUCCESSOR-NODES ((SELF GRAPH-CLASS) (NODE ELEMENT-CLASS))
;; METHOD ADJACENT-NODES ((SELF GRAPH-CLASS) (NODE ELEMENT-CLASS))
;; METHOD FLOW-DISTANCE-FROM-NODE ((SELF GRAPH-CLASS) 
;; METHOD WALK-FROM-NODE ((SELF GRAPH-CLASS) (STARTNODE ELEMENT-CLASS)
;; METHOD WALK-EDGES-FROM-NODE ((SELF GRAPH-CLASS)
;; METHOD COPY ((SELF GRAPH-CLASS) &REST KEYS)
;; METHOD FIND-NODES-WITH-PROPERTY ((SELF GRAPH-CLASS)
;; METHOD SHOW-GRAPH ((SELF GRAPH-CLASS))

(DEFGENERIC PROPERTY-NAMES (SELF))
(DEFGENERIC SET-PROPERTY (SELF PROP-NAME PROP-VALUE))
(DEFGENERIC GET-PROPERTY (SELF PROP-NAME))
(DEFGENERIC DELETE-PROPERTY (SELF PROP-NAME))
(DEFGENERIC CARDINAL (SELF))
(DEFGENERIC ADD-ELEMENTS (SELF NEWELEMENTLIST))
(DEFGENERIC REMOVE-ELEMENT (SELF OLDELEMENT))
(DEFGENERIC PERFORM-WITH-ELEMENTS (SELF LAMBDA-BODY))
(DEFGENERIC MAP-ELEMENTS (SELF LAMBDA-BODY))
(DEFGENERIC SELECT-ELEMENTS (SELF SELECT-LAMBDA))
(DEFGENERIC ELEMENT-LIST (SELF))
(DEFGENERIC FIND-ELEMENTS-WITH-PROPERTY (SELF PROPERTY VALUE))
(DEFGENERIC CONTAINS-ELEMENT (SELF ANELEMENT))
(DEFGENERIC ADD-ELEMENT (SELF NEWELEMENT))
(DEFGENERIC SET-WEIGHT (SELF NEWWEIGHT))
(DEFGENERIC SET-NODES (SELF NEWFROM NEWTO))
(DEFGENERIC DESCRIPTION (SELF))
(DEFGENERIC ADD-NODE (SELF NEWNODE))
(DEFGENERIC ADD-NODES (SELF NEWNODELIST))
(DEFGENERIC REMOVE-NODE (SELF OLDNODE))
(DEFGENERIC REMOVE-NODES (SELF OLDNODELIST))
(DEFGENERIC ADD-EDGE (SELF NEWEDGE))
(DEFGENERIC ADD-EDGE-BETWEEN-NODES (SELF NODEA NODEB))
(DEFGENERIC REMOVE-EDGE (SELF OLDEDGE))
(DEFGENERIC REMOVE-EDGES (SELF EDGE-LIST))
(DEFGENERIC REMOVE-EDGES-BETWEEN-NODES (SELF NODEA NODEB))
(DEFGENERIC EDGES-BETWEEN-NODES (SELF NODEA NODEB))
(DEFGENERIC DIRECTED-EDGES-BETWEEN-NODES (SELF FROMNODE TONODE))
(DEFGENERIC DIRECTED-EDGES-FROM-NODE (SELF FROMNODE))
(DEFGENERIC SUCCESSOR-NODES (SELF NODE))
(DEFGENERIC ADJACENT-NODES (SELF NODE))
(DEFGENERIC FLOW-DISTANCE-FROM-NODE (SELF STARTNODE PROP-NAME))
(DEFGENERIC WALK-FROM-NODE (SELF STARTNODE LAMBDA-BODY))
(DEFGENERIC WALK-EDGES-FROM-NODE (SELF STARTNODE LAMBDA-BODY))
(DEFGENERIC FIND-NODES-WITH-PROPERTY (SELF PROPERTY VALUE))
(DEFGENERIC SHOW-GRAPH (SELF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS ELEMENT-CLASS ()
  ((IDENT
    :READER   IDENT
    :TYPE     SYMBOL
    :DOCUMENTATION "A unique symbol identifying this element.")
   (PROPERTIES 
    :INITFORM NIL
    :INITARG  :PROPERTIES
    :ACCESSOR PROPERTIES
    :TYPE     LIST
    :DOCUMENTATION
    "A plist of properties for this elements.
It can be used to store markers while walking sets or graphs containing them."))
  (:DOCUMENTATION
   "An element of a SET-CLASS."))


(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((SELF ELEMENT-CLASS) &REST INITARGS
                                       &KEY &ALLOW-OTHER-KEYS)
  "
DO:     Initalize the instance id.
"
  (DECLARE (IGNORE INITARGS))
  (SETF (SLOT-VALUE SELF 'IDENT) 
        (GENSYM (FORMAT NIL "~A-"
                        (STRING-UPCASE (CLASS-NAME (CLASS-OF SELF))))))
  SELF)


(DEFMETHOD DESCRIPTION ((SELF ELEMENT-CLASS))
  "
RETURN: A string describing this element.
"
  (FORMAT NIL "<An instance of ~A>" (CLASS-NAME (CLASS-OF SELF))))


;; (defmethod identicalTo ((self ELEMENT-CLASS) (other ELEMENT-CLASS))
;;   "
;; RETURN: Whether self and other are the same element. (eq self other)
;; "
;;   (eq self other)
;;   )

(DEFMETHOD PROPERTY-NAMES ((SELF ELEMENT-CLASS))
  "
RETURN: The list of property names (keys) of properties of this element.
"
  (DO ((PS (PROPERTIES SELF) (CDDR PS))
       (RES '()))
      ((NULL PS) RES)
    (PUSH (CAR PS) RES)))


(DEFMETHOD SET-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL) PROP-VALUE)
  "
POST:  (eq (GET-PROPERTY self prop-name) prop-value)
"
  (SETF (SLOT-VALUE SELF 'PROPERTIES)
        (PLIST-PUT (PROPERTIES SELF) PROP-NAME PROP-VALUE)))


(DEFMETHOD GET-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL))
  "
RETURN: the property `prop-name' of this element.
"
  (PLIST-GET (PROPERTIES SELF) PROP-NAME))


(DEFMETHOD DELETE-PROPERTY ((SELF ELEMENT-CLASS) (PROP-NAME SYMBOL))
  "
DO:     Remove the property named `prop-name' from the property list of
        this element.
"
  (SETF (SLOT-VALUE SELF 'PROPERTIES)
        (PLIST-REMOVE (PROPERTIES SELF) PROP-NAME)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS SET-CLASS (ELEMENT-CLASS)
  ((ELEMENTS
    :INITFORM NIL
    :INITARG  :ELEMENTS
    :ACCESSOR ELEMENTS
    :TYPE     LIST
    :DOCUMENTATION
    "The list of elements in this set."))
  (:DOCUMENTATION
   "A set of elements."))


(DEFMETHOD DESCRIPTION ((SELF SET-CLASS))
  "
RETURN: A string describing this element.
"
  (FORMAT NIL "<An instance of ~A with ~D elements>" 
          (CLASS-NAME (CLASS-OF SELF)) (CARDINAL SELF)))


(DEFMETHOD CARDINAL ((SELF SET-CLASS))
  "
RETURN: The number of elements in this set.
"
  (LENGTH (ELEMENTS SELF)))


(DEFMETHOD CONTAINS-ELEMENT ((SELF SET-CLASS) (ANELEMENT ELEMENT-CLASS))
  "
RETURN: Whether this set contains anElement.
"
  ;;   (let ((elem-list (elements self))
  ;;         (result nil))
  ;;     (while elem-list
  ;;       (when (eq anElement (car elem-list))
  ;;         (setq result t)
  ;;         (setq elem-list nil))
  ;;       (setq elem-list (cdr elem-list)))
  ;;     result)
  (MEMBER ANELEMENT (ELEMENTS SELF)))


(DEFMETHOD ADD-ELEMENT ((SELF SET-CLASS) (NEWELEMENT ELEMENT-CLASS))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == old_CARDINAL
        (not already_in) ==> (CARDINAL self) == (1+ old_CARDINAL)
                             (CONTAINS-ELEMENT self newElement)
"
  (WHEN (NOT (CONTAINS-ELEMENT SELF NEWELEMENT))
    (SETF (SLOT-VALUE SELF 'ELEMENTS) (CONS NEWELEMENT (ELEMENTS SELF)))))


(DEFMETHOD ADD-ELEMENTS ((SELF SET-CLASS) NEWELEMENTLIST)
  "
DO:     Add each element of the newElementList to this set.
"
  (DOLIST (NEWELEMENT NEWELEMENTLIST)
    (ADD-ELEMENT SELF NEWELEMENT)))


(DEFMETHOD REMOVE-ELEMENT ((SELF SET-CLASS) (OLDELEMENT ELEMENT-CLASS))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == (1- old_CARDINAL),
                             (not (CONTAINS-ELEMENT self oldElement))
        (not already_in) ==> (CARDINAL self) == old_CARDINAL
"
  (SETF (SLOT-VALUE SELF 'ELEMENTS) (DELETE OLDELEMENT (ELEMENTS SELF))))


(DEFMETHOD PERFORM-WITH-ELEMENTS ((SELF SET-CLASS) LAMBDA-BODY)
  "
DO:     calls lambda-body with each element in the set.
NOTE:   lambda-body must not change this set.
"
  (MAPC LAMBDA-BODY (ELEMENTS SELF)))


(DEFMETHOD MAP-ELEMENTS ((SELF SET-CLASS) LAMBDA-BODY)
  "
RETURN: the list of results returned by lambda-body called with each element.
NOTE:   lambda-body must not change this set.
"
  (MAPCAR LAMBDA-BODY (ELEMENTS SELF)))


(DEFMETHOD SELECT-ELEMENTS ((SELF SET-CLASS) SELECT-LAMBDA)
  "
RETURN: A list of elements for which select-lambda returned true.
"
  (LET ((RESULT NIL))
    (MAPC (LAMBDA (ELEM)
            (WHEN (FUNCALL SELECT-LAMBDA ELEM)
              (PUSH ELEM RESULT)))
          (ELEMENTS SELF))
    RESULT))


(DEFMETHOD ELEMENT-LIST ((SELF SET-CLASS))
  "
RETURN: A new list of the elements in self.
"
  (MAP 'LIST (FUNCTION IDENTITY) (ELEMENTS SELF)))


(DEFMETHOD FIND-ELEMENTS-WITH-PROPERTY
    ((SELF SET-CLASS) (PROPERTY SYMBOL) VALUE)
  "
RETURN: A list of elements that have as property PROPERTY the value VALUE.
"
  (SELECT-ELEMENTS SELF (LAMBDA (ELEM)
                          (LET ((PVAL (GET-PROPERTY ELEM PROPERTY)))
                            (AND PVAL (EQUAL PVAL VALUE))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS HASHED-SET-CLASS (SET-CLASS)
  ((INDEX
    :INITFORM (LAMBDA () (MAKE-HASH-TABLE :TEST 'EQ))
    :INITARG :INDEX
    :ACCESSOR INDEX
    :TYPE     HASH-TABLE
    :DOCUMENTATION "A hashtable used to index the elements in this set."))
  (:DOCUMENTATION "This is a specialized kind of set that maintains a hashtable
index of its elements to be able to retrieve them rapidly."))
               

(DEFMETHOD CONTAINS-ELEMENT ((SELF HASHED-SET-CLASS) (ANELEMENT ELEMENT-CLASS))
  "
RETURN: Whether this set contains anElement.
"
  (GETHASH ANELEMENT (INDEX SELF)))


(DEFMETHOD ADD-ELEMENT ((SELF HASHED-SET-CLASS) (NEWELEMENT ELEMENT-CLASS))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == old_CARDINAL
        (not already_in) ==> (CARDINAL self) == (1+ old_CARDINAL)
                             (CONTAINS-ELEMENT self newElement)
"
  (CALL-NEXT-METHOD)
  (SETF (GETHASH NEWELEMENT (INDEX SELF)) NEWELEMENT))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS EDGE-CLASS (ELEMENT-CLASS)
  ((tag :initform nil :initarg :tag
        :documentation "Reserved for the client."))
  (:DOCUMENTATION
   "An abstract  edge."))


(DEFMETHOD DESCRIPTION ((SELF EDGE-CLASS))
  "
RETURN: A string describing this element.
"
  (LET ((NODES (NODES SELF)))
    (FORMAT NIL "<A ~A between { ~A and ~A }>" 
            (CLASS-NAME (CLASS-OF SELF)) 
            (DESCRIPTION (CAR  NODES)) 
            (DESCRIPTION (CDR NODES)))))


(DEFGENERIC COPY (SELF &KEY &ALLOW-OTHER-KEYS)
  (:DOCUMENTATION "
RETURN: A COPY of this edge.
        The COPY has the same  NODES than this edge.
        Other attributes are normally copied."))


(DEFGENERIC NODES (SELF)
  (:DOCUMENTATION "
RETURN: A cons containing the two NODES of the edge, in no specific order.
        (Subclasses implementing directed edges should add specific methods
         to get the `from' and the `to' NODES)."))


(DEFGENERIC IS-BETWEEN-NODES (SELF NODEA NODEB)
  (:DOCUMENTATION "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        If this edge is directed then `nodeA' is compared to the from node
                                  and `nodeB' is compared to the  to  node,
        otherwise, the node order is not important."))


(DEFGENERIC SUCCESSOR-OF (SELF NODE)
  (:DOCUMENTATION "
RETURN: If node is a node of the edge, then return its successor or nil.
        That is, for an undirected edge e, 
             (and (eq (SUCCESSOR-OF e (car (NODES e))) (cdr (NODES e)))
                  (eq (SUCCESSOR-OF e (cdr (NODES e))) (car (NODES e))) )
        while for a directed edge d:
             (xor (eq (SUCCESSOR-OF e (car (NODES e))) (cdr (NODES e)))
                  (eq (SUCCESSOR-OF e (cdr (NODES e))) (car (NODES e))) )"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS WEIGHT-MIXIN-CLASS ()
  ((WEIGHT
    :INITFORM 1
    :INITARG  :WEIGHT
    :ACCESSOR WEIGHT
    :TYPE     NUMBER
    :DOCUMENTATION "The weight of the edge."))
  (:DOCUMENTATION
   "This is a mixin for the subclasses of EDGE-CLASS
    to add a weight to the edge."))


(DEFMETHOD SET-WEIGHT ((SELF WEIGHT-MIXIN-CLASS) (NEWWEIGHT INTEGER))
  "
POST:   (equal (weight self) newWeight)
"
  (SETF (SLOT-VALUE SELF 'WEIGHT) NEWWEIGHT))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN NODE-CONS-P (ITEM)
  "
RETURN: Whether `item' is a cons of two objects kind of ELEMENT-CLASS.
"
  (AND (CONSP ITEM)
       (TYPEP (CAR ITEM) 'ELEMENT-CLASS)
       (TYPEP (CDR ITEM) 'ELEMENT-CLASS)))


(DEFCLASS UNDIRECTED-EDGE-CLASS (EDGE-CLASS)
  ((NODES
    :INITARG :NODES
    :ACCESSOR NODES
    :TYPE     (SATISFIES NODE-CONS-P)
    :DOCUMENTATION
    "A cons containing the two unordered NODES of the edge." ))
  (:DOCUMENTATION "An undirected edge."))


(DEFMETHOD COPY ((SELF UNDIRECTED-EDGE-CLASS) &KEY &ALLOW-OTHER-KEYS)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (MAKE-INSTANCE (CLASS-OF SELF)
    :NODES  (NODES SELF)))


(DEFUN IDENTICAL-NODES (NODES-CONS-A NODES-CONS-B)
  "
RETURN: Whether NODES-cons-a and NODES-cons-b contain the same NODES.
"
  (OR (AND (EQ (CAR NODES-CONS-A) (CAR NODES-CONS-B))
           (EQ (CDR NODES-CONS-A) (CDR NODES-CONS-B)))
      (AND (EQ (CAR NODES-CONS-A) (CDR NODES-CONS-B))
           (EQ (CDR NODES-CONS-A) (CAR NODES-CONS-B)))))


;; (defmethod identicalTo ((self UNDIRECTED-EDGE-CLASS) (other PjBElement))
;;   "
;; RETURN: Whether self and other are the same undirected edge.
;; NOTE:   We only compare the NODES linked by this edge,
;;         not any other attribute
;;         that could be added by a subclass...
;; "
;;   (and (is-kind-of other UNDIRECTED-EDGE-CLASS)
;;        (IDENTICAL-NODES (NODES self) (NODES other)))
;;   )

(DEFMETHOD IS-BETWEEN-NODES ((SELF EDGE-CLASS) 
                             (NODEA ELEMENT-CLASS)  (NODEB ELEMENT-CLASS))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        The node order is not important.
"
  (IDENTICAL-NODES (NODES SELF) (CONS NODEA NODEB)))


(DEFMETHOD SUCCESSOR-OF ((SELF UNDIRECTED-EDGE-CLASS) (NODE ELEMENT-CLASS))
  "
RETURN: If node is a node of this edge, then the other node, else nil.
"
  (LET ( (NODES (NODES SELF)) )
    (COND
      ((EQ NODE (CAR NODES)) (CDR NODES))
      ((EQ NODE (CDR NODES)) (CAR NODES))
      (T NIL))))


(DEFMETHOD SET-NODES ((SELF UNDIRECTED-EDGE-CLASS) 
                      (NODEA ELEMENT-CLASS) (NODEB ELEMENT-CLASS))
  "
DO:     set the NODES of this edge.
"
  (SETF (SLOT-VALUE SELF 'NODES) (CONS NODEA NODEB)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS WEIGHTED-UNDIRECTED-EDGE-CLASS 
    (UNDIRECTED-EDGE-CLASS WEIGHT-MIXIN-CLASS)
  ()
  (:DOCUMENTATION
   "A weighted, undirected edge."))


(DEFMETHOD DESCRIPTION ((SELF WEIGHTED-UNDIRECTED-EDGE-CLASS))
  "
RETURN: A string describing this element.
"
  (LET ((NODES (NODES SELF)))
    (FORMAT NIL "<A ~A between { ~A and ~A } weighting ~A>" 
            (CLASS-NAME (CLASS-OF SELF)) 
            (DESCRIPTION (CAR  NODES))
            (DESCRIPTION (CDR NODES))
            (WEIGHT SELF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS DIRECTED-EDGE-CLASS (EDGE-CLASS)
  ((FROM
    :INITARG  :FROM
    :ACCESSOR  FROM
    :TYPE      ELEMENT-CLASS
    :DOCUMENTATION
    "The `from' node of this edge." )
   (TO
    :INITARG  :TO
    :ACCESSOR  TO
    :TYPE      ELEMENT-CLASS
    :DOCUMENTATION
    "The `to' node of this edge." ))
  (:DOCUMENTATION "An directed edge."))


(DEFMETHOD COPY ((SELF DIRECTED-EDGE-CLASS) &KEY &ALLOW-OTHER-KEYS)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (MAKE-INSTANCE (CLASS-OF SELF)
    :FROM   (FROM   SELF)
    :TO     (TO     SELF)))


(DEFMETHOD NODES ((SELF DIRECTED-EDGE-CLASS))
  "
RETURN: A cons containing the two NODES of the edge in no particular order.
NOTE:   Use the accessor methods `from' and `to' to get the wanted node.
"
  (CONS (FROM SELF) (TO SELF)))


;; (defmethod identicalTo ((self DIRECTED-EDGE-CLASS) (other PjBElement))
;;   "
;; RETURN: Whether self and other are the same directed edge.
;; NOTE:   We only compare the NODES linked by this edge,
;;         not any other attribute
;;         that could be added by a subclass...
;; "
;;   (and (is-kind-of other DIRECTED-EDGE-CLASS)
;;        (identicalTo (from self) (from other))
;;        (identicalTo (to   self) (to   other)) )
;;   )


(DEFMETHOD IS-BETWEEN-NODES ((SELF DIRECTED-EDGE-CLASS) 
                             (NODEA ELEMENT-CLASS)  (NODEB ELEMENT-CLASS))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        `nodeA' is compared to the from node
        and `nodeB' is compared to the  to  node.

"
  (AND (EQ (FROM SELF) NODEA) (EQ (TO SELF) NODEB)))


(DEFMETHOD SUCCESSOR-OF ((SELF DIRECTED-EDGE-CLASS) (NODE ELEMENT-CLASS))
  "
RETURN: If node is the `from'  node of this edge, then the `to' node, else nil.
"
  (IF (EQ NODE (FROM SELF))  (TO SELF)  NIL))


(DEFMETHOD SET-NODES ((SELF DIRECTED-EDGE-CLASS)
                      (NEWFROM ELEMENT-CLASS) (NEWTO ELEMENT-CLASS))
  "
DO:     set the NODES of this edge.
"
  (SETF (SLOT-VALUE SELF 'FROM) NEWFROM)
  (SETF (SLOT-VALUE SELF 'TO)   NEWTO))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFCLASS WEIGHTED-DIRECTED-EDGE-CLASS (DIRECTED-EDGE-CLASS WEIGHT-MIXIN-CLASS)
  ()
  (:DOCUMENTATION "A weighted, directed edge."))


(DEFMETHOD DESCRIPTION ((SELF WEIGHTED-DIRECTED-EDGE-CLASS))
  "
RETURN: A string describing this element.
"
  (LET ((NODES (NODES SELF)))
    (FORMAT NIL "<A ~A between { ~A and ~A } weighting ~A>" 
            (CLASS-NAME (CLASS-OF SELF))
            (DESCRIPTION (CAR  NODES))
            (DESCRIPTION (CDR NODES))
            (WEIGHT SELF))))


(DEFMETHOD COPY ((SELF WEIGHTED-DIRECTED-EDGE-CLASS) &KEY &ALLOW-OTHER-KEYS)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (MAKE-INSTANCE (CLASS-OF SELF)
    :WEIGHT (WEIGHT SELF)
    :FROM   (FROM   SELF)
    :TO     (TO     SELF)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN SUBCLASS-OF-EDGE-P (ITEM)
  "
RETURN: Whether `item' is a subclass of EDGE-CLASS (not EDGE-CLASS itself).
"
  (SUBTYPEP ITEM 'EDGE-CLASS))


(DEFCLASS GRAPH-CLASS (ELEMENT-CLASS)
  ((NODES
    :INITARG  :NODES
    :ACCESSOR NODES
    :TYPE     SET-CLASS
    :DOCUMENTATION
    "The set of NODES in this graph.")
   (EDGES
    :INITARG :EDGES
    :ACCESSOR EDGES
    :TYPE     SET-CLASS
    :DOCUMENTATION
    "The set of edges in this graph.")
   (EDGE-CLASS
    :INITFORM 'UNDIRECTED-EDGE-CLASS
    :INITARG  :EDGE-CLASS
    :ACCESSOR EDGE-CLASS
    ;;:type     (satisfies 'SUBCLASS-OF-EDGE-P)
    :DOCUMENTATION
    "The class used to make new edges in this graph. 
Default is UNDIRECTED-EDGE-CLASS."))
  (:DOCUMENTATION 
   "A graph of elements. By default, it's a undirected graph."))


(DEFMETHOD INITIALIZE-INSTANCE :AFTER ((SELF GRAPH-CLASS) &REST INITARGS
                                       &KEY &ALLOW-OTHER-KEYS)
  "
DO:     Initalize the instance nodes and edges.
"
  (DECLARE (IGNORE INITARGS))
  (SETF (NODES SELF) (MAKE-INSTANCE 'SET-CLASS))
  (SETF (EDGES SELF) (MAKE-INSTANCE 'SET-CLASS))
  SELF)



(DEFMETHOD DESCRIPTION ((SELF GRAPH-CLASS))
  "
RETURN: A string describing this element.
"
  (FORMAT NIL "<A ~A with ~D NODES and ~D edges>" 
          (CLASS-NAME (CLASS-OF SELF)) 
          (CARDINAL (NODES SELF))
          (CARDINAL (EDGES SELF))))


(DEFMETHOD ADD-NODE ((SELF GRAPH-CLASS) (NEWNODE ELEMENT-CLASS))
  "
DO:     Add newNode to the set of NODES of this graph.
"
  (ADD-ELEMENT (NODES SELF) NEWNODE))


(DEFMETHOD ADD-NODES ((SELF GRAPH-CLASS) NEWNODELIST)
  "
DO:     Add a list of new NODES to the set of NODES of this graph.
"
  (ADD-ELEMENTS (NODES SELF) NEWNODELIST))



(DEFMETHOD REMOVE-NODE ((SELF GRAPH-CLASS) (OLDNODE ELEMENT-CLASS))
  "
DO:      Remove the oldNode from the graph. 
         This implies removing all the edges adjacent to the node too.
"
  (WHEN (CONTAINS-ELEMENT (NODES SELF) OLDNODE)
    (REMOVE-EDGES SELF (SELECT-ELEMENTS (EDGES SELF)
                                        (LAMBDA (EDGE)
                                          (OR (EQ OLDNODE (FROM EDGE))
                                              (EQ OLDNODE (TO   EDGE))))))))


(DEFMETHOD REMOVE-NODES ((SELF GRAPH-CLASS) OLDNODELIST)
  "
DO:      Remove all the NODES of the oldNodeList from this graph.
"
  (DOLIST (NODE OLDNODELIST) (REMOVE-NODE SELF NODE)))


(DEFMETHOD ADD-EDGE ((SELF GRAPH-CLASS) (NEWEDGE EDGE-CLASS))
  "
PRE:    (and (CONTAINS-ELEMENT (NODES self) (nth 0 (NODES newEdge)))
             (CONTAINS-ELEMENT (NODES self) (nth 1 (NODES newEdge))))
DO:     Add a new edge to this graph.
"
  (WHEN (AND (CONTAINS-ELEMENT (NODES SELF) (CAR (NODES NEWEDGE)))
             (CONTAINS-ELEMENT (NODES SELF) (CDR (NODES NEWEDGE))))
    (ADD-ELEMENT (EDGES SELF) NEWEDGE)))


(DEFMETHOD ADD-EDGE-BETWEEN-NODES ((SELF GRAPH-CLASS) 
                                   (NODEA ELEMENT-CLASS) (NODEB ELEMENT-CLASS))
  "
DO:     Create a new edge (of class edge-class) between `nodeA' and `nodeB'.
        and add it to this graph.
        If the edge is directed, 
        then `nodeA' is the `from' node and `nodeB' the `to' node.
"
  (LET ((EDGE (MAKE-INSTANCE (EDGE-CLASS SELF))))
    (SET-NODES EDGE NODEA NODEB)
    (ADD-EDGE SELF EDGE)))


(DEFMETHOD REMOVE-EDGE ((SELF GRAPH-CLASS) (OLDEDGE EDGE-CLASS))
  "
DO:     Remove the `oldEdge' from this graph.
"
  (REMOVE-ELEMENT (EDGES SELF) OLDEDGE))


(DEFMETHOD REMOVE-EDGES ((SELF GRAPH-CLASS) EDGE-LIST)
  "
DO:     Remove all the edges in edge-list from this graph.
"
  (DOLIST (EDGE EDGE-LIST)  (REMOVE-EDGE SELF EDGE)))


(DEFMETHOD REMOVE-EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS)
                                       (NODEA ELEMENT-CLASS)
                                       (NODEB ELEMENT-CLASS))
  "
DO:     Remove all edges between `nodeA' and `nodeB'.
"
  (MAPC (LAMBDA (EDGE)
          (REMOVE-EDGE SELF EDGE))
        (EDGES-BETWEEN-NODES SELF NODEA NODEB)))


(DEFMETHOD EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS)
                                (NODEA ELEMENT-CLASS) (NODEB ELEMENT-CLASS))
  "
RETURN: A list of edges existing between the `nodeA' and `nodeB'.
        If the graph is directed then `nodeA' corresponds to the from node
                                  and `nodeB' corresponds to the  to  node.
"
  (SELECT-ELEMENTS (EDGES SELF)
                   (LAMBDA (EDGE)
                     (IS-BETWEEN-NODES EDGE NODEA NODEB))))


(DEFMETHOD DIRECTED-EDGES-BETWEEN-NODES ((SELF GRAPH-CLASS) 
                                         (FROMNODE ELEMENT-CLASS)
                                         (TONODE ELEMENT-CLASS))
  "
RETURN: A list of edges existing from the `fromNode' and to the `toNode'.
"
  (SELECT-ELEMENTS (EDGES SELF)
                   (LAMBDA (EDGE)
                     (EQ (SUCCESSOR-OF EDGE FROMNODE) TONODE))))


(DEFMETHOD DIRECTED-EDGES-FROM-NODE ((SELF GRAPH-CLASS)
                                     (FROMNODE ELEMENT-CLASS))
  "
PRE:    edge-class is-subclass-of DIRECTED-EDGE-CLASS
        or edge-class eq DIRECTED-EDGE-CLASS.
RETURN: A list of edges existing from the `fromNode'.
"
  (UNLESS (SUBTYPEP (EDGE-CLASS SELF) 'DIRECTED-EDGE-CLASS)
    (ERROR "This graph is not a directed graph. Can't apply ~
            DIRECTED-EDGES-FROM-NODE."))
  (SELECT-ELEMENTS (EDGES SELF)
                   (LAMBDA (EDGE) (EQ (FROM EDGE) FROMNODE))))


(DEFMETHOD SUCCESSOR-NODES ((SELF GRAPH-CLASS) (NODE ELEMENT-CLASS))
  "
RETURN: The list of successors NODES of the given node in this graph.
NOTE:   For undirected graphs, it's the same as ADJACENT-NODES.
"
  (LET ((RESULT NIL))
    (PERFORM-WITH-ELEMENTS
     (EDGES SELF)
     (LAMBDA (EDGE)
       (LET ( (SUCC (SUCCESSOR-OF EDGE NODE)) )
         (WHEN SUCC
           (UNLESS (MEMBER SUCC RESULT) (PUSH SUCC RESULT))))))
    RESULT))


(DEFMETHOD ADJACENT-NODES ((SELF GRAPH-CLASS) (NODE ELEMENT-CLASS))
  "
RETURN: The list of NODES adjacent to the given node in this graph.
NOTE:   For directed graphs, an adjacent node is either a predecessor
        or a successors of the node.
"
  (LET ((RESULT NIL))
    (PERFORM-WITH-ELEMENTS
     (EDGES SELF)
     (LAMBDA (EDGE)
       (LET ((NS (NODES EDGE)))
         (COND 
           ((EQ NODE (CAR NS)) 
            (UNLESS (MEMBER (CDR NS) RESULT) (PUSH (CDR NS) RESULT)))
           ((EQ NODE (CDR NS)) 
            (UNLESS (MEMBER (CAR NS) RESULT) (PUSH (CAR NS) RESULT)))))))
    RESULT))


(DEFMETHOD FLOW-DISTANCE-FROM-NODE ((SELF GRAPH-CLASS) 
                                    (STARTNODE ELEMENT-CLASS)
                                    (PROP-NAME SYMBOL))
  "
DO:     Compute for each node in this graph the distance from the startNode,
        and store it as a property named prop-name.
NOTE:   If the graph is not connex, then some distances will be nil, 
        meaning infinity.
"
  (PERFORM-WITH-ELEMENTS (NODES SELF) (LAMBDA (NODE) 
                                        (SET-PROPERTY NODE PROP-NAME NIL)))
  (WHEN (CONTAINS-ELEMENT (NODES SELF) STARTNODE)
    (SET-PROPERTY STARTNODE PROP-NAME 0)
    (LET ( (CUR-NODES (LIST STARTNODE))
          CUR-NODE DISTANCE SUC-NODES SUC-DIST )
      (WHILE CUR-NODES
        (setf CUR-NODE (CAR CUR-NODES)
              CUR-NODES (CDR CUR-NODES)
              DISTANCE (1+ (GET-PROPERTY CUR-NODE PROP-NAME))
              SUC-NODES (SUCCESSOR-NODES SELF CUR-NODE))
        ;; (not (null distance))
        (DOLIST (SUC-NODE SUC-NODES)
          (setf SUC-DIST (GET-PROPERTY SUC-NODE PROP-NAME))
          (WHEN (OR (NULL SUC-DIST) (< DISTANCE SUC-DIST))
            (SET-PROPERTY SUC-NODE PROP-NAME DISTANCE)
            (UNLESS (MEMBER SUC-NODE CUR-NODES)
              (PUSH SUC-NODE CUR-NODES))))))))


(DEFMETHOD WALK-FROM-NODE ((SELF GRAPH-CLASS) (STARTNODE ELEMENT-CLASS)
                           LAMBDA-BODY)
  "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each node as argument. 
"
  (LET ((STAMP (GENSYM "walked-")))
    (WHEN (CONTAINS-ELEMENT (NODES SELF) STARTNODE)
      (PERFORM-WITH-ELEMENTS (NODES SELF) 
                             (LAMBDA (NODE) (SET-PROPERTY NODE STAMP NIL)))
      (LET ( (CUR-NODES (LIST STARTNODE))
            CUR-NODE  SUC-NODES  )
        (WHILE CUR-NODES
          (SETQ CUR-NODE  (CAR CUR-NODES)
                CUR-NODES (CDR CUR-NODES))

          (SET-PROPERTY CUR-NODE STAMP T)
          (FUNCALL LAMBDA-BODY CUR-NODE)

          (SETQ SUC-NODES (SUCCESSOR-NODES SELF CUR-NODE))
          (DOLIST (SUC-NODE SUC-NODES)
            (UNLESS (GET-PROPERTY SUC-NODE STAMP)
              (PUSH SUC-NODE CUR-NODES)) )))
      (PERFORM-WITH-ELEMENTS (NODES SELF) 
                             (LAMBDA (NODE) (DELETE-PROPERTY NODE STAMP))))))


(DEFMETHOD WALK-EDGES-FROM-NODE ((SELF GRAPH-CLASS)
                                 (STARTNODE ELEMENT-CLASS) LAMBDA-BODY)
  "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each edges as argument. Since it's the edges that are passed
        to lambda-body, one node can be \"walked\" several times either as
        `from' or `to' node or different edges.
"
  (LET ((STAMP (GENSYM "walked-")))
    (WHEN (CONTAINS-ELEMENT (NODES SELF) STARTNODE)
      (PERFORM-WITH-ELEMENTS (EDGES SELF) 
                             (LAMBDA (ITEM) (SET-PROPERTY ITEM STAMP NIL)))
      (PERFORM-WITH-ELEMENTS (NODES SELF) 
                             (LAMBDA (ITEM) (SET-PROPERTY ITEM STAMP NIL)))
      (SET-PROPERTY STARTNODE STAMP T)
      (LET ((CUR-NODES (LIST STARTNODE))
            CUR-NODE)
        (WHILE CUR-NODES
          (SETQ CUR-NODE  (CAR CUR-NODES)
                CUR-NODES (CDR CUR-NODES))
          (DOLIST (EDGE (DIRECTED-EDGES-FROM-NODE SELF CUR-NODE))
            (UNLESS (GET-PROPERTY EDGE STAMP)
              (SET-PROPERTY EDGE STAMP T)
              (FUNCALL LAMBDA-BODY EDGE)
              (UNLESS (GET-PROPERTY (TO EDGE) STAMP)
                (SET-PROPERTY (TO EDGE) STAMP T)
                (PUSH (TO EDGE) CUR-NODES))))))
      (PERFORM-WITH-ELEMENTS (EDGES SELF) 
                             (LAMBDA (ITEM) (DELETE-PROPERTY ITEM STAMP)))
      (PERFORM-WITH-ELEMENTS (NODES SELF) 
                             (LAMBDA (ITEM) (DELETE-PROPERTY ITEM STAMP))))))



(DEFMETHOD COPY ((SELF GRAPH-CLASS)
                 &KEY (COPY-NODES NIL) (COPY-EDGES T) &ALLOW-OTHER-KEYS)
  "
RETURN: A COPY of this graph.
NOTE:   By default, the NODES are the same, but the edges are duplicated.
        The following keys are recognized:
            :COPY-NODES       default: NIL; T ==>   COPY-EDGE
            :COPY-EDGES       default: T;   NIL ==> (NOT COPY-NODE)
        The following combination are valid:
        COPY-NODES      COPY-EDGE        You get a deep COPY of the graph,
                                         where you can change anything 
                                         independtly from the orginal.
        (NOT COPY-NODES) COPY-EDGE       You get a new graph with new edges,
                                         but the same NODES.
        (NOT COPY-NODES) (NOT COPY-EDGE) You get a new graph with the same
                                         edges and the same NODES. But you 
                                         still can add or remove NODES or
                                         edges to make it different from the
                                         original graph.
"
  (LET (NEW-NODES
        NEW-EDGES
        NODE-HASH)
    (WHEN (AND COPY-NODES (NOT COPY-EDGES))
      (ERROR "Can't have both COPY-NODES and (NOT COPY-EDGES)."))
    (UNLESS COPY-NODES       (SETf COPY-NODES NIL))
    (UNLESS (NOT COPY-EDGES) (SETf COPY-EDGES T))
    (WHEN   (NOT COPY-EDGES) (SETf COPY-NODES NIL))
    (WHEN   COPY-NODES       (SETf COPY-EDGES T))
    (IF COPY-NODES 
        (setf NODE-HASH (MAKE-HASH-TABLE :TEST 'EQ 
                                         :SIZE (CARDINAL (NODES SELF)))
            
              NEW-NODES (MAKE-INSTANCE 'SET-CLASS 
                          :ELEMENTS (MAP-ELEMENTS 
                                     (NODES SELF)
                                     (LAMBDA (NODE)
                                       (LET ((NEW-NODE (COPY NODE)))
                                         (SETF (GETHASH NODE NODE-HASH) NEW-NODE)
                                         NEW-NODE)))))
        (setf NEW-NODES  (NODES SELF)))
    (IF COPY-EDGES
        (setf NEW-EDGES 
              (MAKE-INSTANCE 'SET-CLASS 
                :ELEMENTS (MAP-ELEMENTS
                           (EDGES SELF)
                           (LAMBDA (EDGE)
                             (LET ((NEW-EDGE (COPY EDGE))
                                   NODES)
                               (WHEN COPY-NODES
                                 (SETQ NODES (NODES NEW-EDGE))
                                 (SET-NODES NEW-EDGE 
                                            (GETHASH (CAR NODES) NODE-HASH)
                                            (GETHASH (CDR NODES) NODE-HASH)))
                               NEW-EDGE)))))
        (setf NEW-EDGES (EDGES SELF)))
    (MAKE-INSTANCE (CLASS-OF SELF) 
      :NODES NEW-NODES
      :EDGES NEW-EDGES
      :EDGE-CLASS (EDGE-CLASS SELF))))


(DEFMETHOD FIND-NODES-WITH-PROPERTY ((SELF GRAPH-CLASS)
                                     (PROPERTY SYMBOL) VALUE)
  "
RETURN: A list of NODES that have as property PROPERTY the value VALUE.
"
  (FIND-ELEMENTS-WITH-PROPERTY (NODES SELF) PROPERTY VALUE))


(DEFMETHOD SHOW-GRAPH ((SELF GRAPH-CLASS))
  (FORMAT T "~A {~%" (DESCRIPTION SELF))
  (PERFORM-WITH-ELEMENTS 
   (NODES SELF)
   (LAMBDA (NODE) (FORMAT T "   node ~A~%"  (DESCRIPTION NODE))))
  (PERFORM-WITH-ELEMENTS
   (EDGES SELF)
   (LAMBDA (EDGE) (FORMAT T "   edge ~A~%" (DESCRIPTION EDGE))))
  (FORMAT T "}~%"))



;;;; graph.lisp                       --                     --          ;;;;
