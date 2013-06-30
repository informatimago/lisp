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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" 
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "EDGE-CLASS" "EDGES" "NODES" "TO" "FROM" "WEIGHT" "INDEX"
           "ELEMENTS" "PROPERTIES" "IDENT" "SHOW-GRAPH" "FIND-NODES-WITH-PROPERTY"
           "COPY" "WALK-EDGES-FROM-NODE" "WALK-FROM-NODE" "FLOW-DISTANCE-FROM-NODE"
           "ADJACENT-NODES" "SUCCESSOR-NODES" "DIRECTED-EDGES-FROM-NODE"
           "DIRECTED-EDGES-BETWEEN-NODES" "EDGES-BETWEEN-NODES"
           "REMOVE-EDGES-BETWEEN-NODES" "REMOVE-EDGES" "REMOVE-EDGE"
           "ADD-EDGE-BETWEEN-NODES" "ADD-EDGE" "REMOVE-NODES" "REMOVE-NODE" "ADD-NODES"
           "ADD-NODE" "DESCRIPTION" "GRAPH-CLASS" "SUBCLASS-OF-EDGE-P"
           "WEIGHTED-DIRECTED-EDGE-CLASS" "SET-NODES" "SUCCESSOR-OF" "IS-BETWEEN-NODES"
           "DIRECTED-EDGE-CLASS" "WEIGHTED-UNDIRECTED-EDGE-CLASS"
           "IDENTICAL-NODES" "UNDIRECTED-EDGE-CLASS" "NODE-CONS-P" "SET-WEIGHT"
           "WEIGHT-MIXIN-CLASS" "EDGE-CLASS" "ADD-ELEMENT" "CONTAINS-ELEMENT"
           "HASHED-SET-CLASS" "FIND-ELEMENTS-WITH-PROPERTY" "ELEMENT-LIST"
           "SELECT-ELEMENTS" "MAP-ELEMENTS" "PERFORM-WITH-ELEMENTS" "REMOVE-ELEMENT"
           "ADD-ELEMENTS" "CARDINAL" "SET-CLASS" "DELETE-PROPERTY" "GET-PROPERTY"
           "SET-PROPERTY" "PROPERTY-NAMES" "ELEMENT-CLASS")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WHILE")
  (:documentation
   "

This package exports classes for elements, sets and graphs.

Graph class.

This is a CLOS based implementation of graphs.
It comes from an emacs/eieio implementation used to analyze
CVS versioning graph.

Subclasses exist to generate dot files, and Diagram! files.


See also:


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH")



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


(defgeneric property-names (self)
  (:documentation   "
RETURN: The list of property names (keys) of properties of this element.
"))


(defgeneric set-property (self prop-name prop-value)
  (:documentation  "
POST:  (eql (GET-PROPERTY self prop-name) prop-value)
"))


(defgeneric get-property (self prop-name)
  (:documentation   "
RETURN: the property `prop-name' of this element.
"))


(defgeneric delete-property (self prop-name)
  (:documentation   "
DO:     Remove the property named `prop-name' from the property list of
        this element.
"))


(defgeneric cardinal (self)
  (:documentation   "
RETURN: The number of elements in this set.
"))


(defgeneric add-elements (self newelementlist)
  (:documentation   "
DO:     Add each element of the newElementList to this set.
"))


(defgeneric remove-element (self oldelement)
  (:documentation   "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == (1- old_CARDINAL),
                             (not (CONTAINS-ELEMENT self oldElement))
        (not already_in) ==> (CARDINAL self) == old_CARDINAL
"))


(defgeneric perform-with-elements (self lambda-body)
  (:documentation   "
DO:     calls lambda-body with each element in the set.
NOTE:   lambda-body must not change this set.
"))


(defgeneric map-elements (self lambda-body)
  (:documentation   "
RETURN: the list of results returned by lambda-body called with each element.
NOTE:   lambda-body must not change this set.
"))


(defgeneric select-elements (self select-lambda)
  (:documentation   "
RETURN: A list of elements for which select-lambda returned true.
"))


(defgeneric element-list (self)
  (:documentation   "
RETURN: A new list of the elements in self.
"))

(defgeneric find-elements-with-property (self property value)
  (:documentation   "
RETURN: A list of elements that have as property PROPERTY the value VALUE.
"))


(defgeneric contains-element (self anelement)
  (:documentation   "
RETURN: Whether this set contains anElement.
"))


(defgeneric add-element (self newelement)
  (:documentation   "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == old_CARDINAL
        (not already_in) ==> (CARDINAL self) == (1+ old_CARDINAL)
                             (CONTAINS-ELEMENT self newElement)
"))


(defgeneric set-weight (self newweight)
  (:documentation   "
POST:   (equal (weight self) newWeight)
"))


(defgeneric set-nodes (self newfrom newto)
  (:documentation   "
DO:     set the NODES of this edge.
"))


(defgeneric description (self)
  (:documentation   "
RETURN: A string describing this element.
"))


(defgeneric add-node (self newnode)
  (:documentation   "
DO:     Add newNode to the set of NODES of this graph.
"))


(defgeneric add-nodes (self newnodelist)
  (:documentation   "
DO:     Add a list of new NODES to the set of NODES of this graph.
"))


(defgeneric remove-node (self oldnode)
  (:documentation   "
DO:      Remove the oldNode from the graph. 
         This implies removing all the edges adjacent to the node too.
"))


(defgeneric remove-nodes (self oldnodelist)
  (:documentation   "
DO:      Remove all the NODES of the oldNodeList from this graph.
"))


(defgeneric add-edge (self newedge)
  (:documentation   "
PRE:    (and (CONTAINS-ELEMENT (NODES self) (nth 0 (NODES newEdge)))
             (CONTAINS-ELEMENT (NODES self) (nth 1 (NODES newEdge))))
DO:     Add a new edge to this graph.
"))


(defgeneric add-edge-between-nodes (self nodea nodeb)
  (:documentation   "
DO:     Create a new edge (of class edge-class) between `nodeA' and `nodeB'.
        and add it to this graph.
        If the edge is directed, 
        then `nodeA' is the `from' node and `nodeB' the `to' node.
"))


(defgeneric remove-edge (self oldedge)
  (:documentation   "
DO:     Remove the `oldEdge' from this graph.
"))


(defgeneric remove-edges (self edge-list)
  (:documentation   "
DO:     Remove all the edges in edge-list from this graph.
"))


(defgeneric remove-edges-between-nodes (self nodea nodeb)
  (:documentation   "
DO:     Remove all edges between `nodeA' and `nodeB'.
"))


(defgeneric edges-between-nodes (self nodea nodeb)
  (:documentation "
RETURN: A list of edges existing between the `nodeA' and `nodeB'.
        If the graph is directed then `nodeA' corresponds to the from node
                                  and `nodeB' corresponds to the  to  node.
"))


(defgeneric directed-edges-between-nodes (self fromnode tonode)
  (:documentation   "
RETURN: A list of edges existing from the `fromNode' and to the `toNode'.
"))


(defgeneric directed-edges-from-node (self fromnode)
  (:documentation   "
PRE:    edge-class is-subclass-of DIRECTED-EDGE-CLASS
        or edge-class eq DIRECTED-EDGE-CLASS.
RETURN: A list of edges existing from the `fromNode'.
"))


(defgeneric successor-nodes (self node)
  (:documentation   "
RETURN: The list of successors NODES of the given node in this graph.
NOTE:   For undirected graphs, it's the same as ADJACENT-NODES.
"))


(defgeneric adjacent-nodes (self node)
  (:documentation   "
RETURN: The list of NODES adjacent to the given node in this graph.
NOTE:   For directed graphs, an adjacent node is either a predecessor
        or a successors of the node.
"))


(defgeneric flow-distance-from-node (self startnode prop-name)
  (:documentation   "
DO:     Compute for each node in this graph the distance from the startNode,
        and store it as a property named prop-name.
NOTE:   If the graph is not connex, then some distances will be nil, 
        meaning infinity.
"))


(defgeneric walk-from-node (self startnode lambda-body)
  (:documentation   "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each node as argument. 
"))


(defgeneric walk-edges-from-node (self startnode lambda-body)
  (:documentation   "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each edges as argument. Since it's the edges that are passed
        to lambda-body, one node can be \"walked\" several times either as
        `from' or `to' node or different edges.
"))


(defgeneric find-nodes-with-property (self property value)
  (:documentation  "
RETURN: A list of NODES that have as property PROPERTY the value VALUE.
"))


(defgeneric show-graph (self)
  (:documentation "
DO: Prints a description of the graph on the *STANDARD-OUTPUT*.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ident (element)
  (:documentation "A unique symbol identifying this element."))
(defgeneric properties (element)
  (:documentation     "A plist of properties for this elements.
It can be used to store markers while walking sets or graphs containing them."))

(defclass element-class ()
  ((ident
    :reader   ident
    :type     symbol
    :documentation "A unique symbol identifying this element.")
   (properties 
    :initform nil
    :initarg  :properties
    :accessor properties
    :type     list
    :documentation
    "A plist of properties for this elements.
It can be used to store markers while walking sets or graphs containing them."))
  (:documentation
   "An element of a SET-CLASS."))


(defmethod initialize-instance :after ((self element-class) &rest initargs
                                       &key &allow-other-keys)
  "
DO:     Initalize the instance id.
"
  (declare (ignore initargs))
  (setf (slot-value self 'ident) 
        (gensym (format nil "~A-"
                        (string-upcase (class-name (class-of self))))))
  self)


(defmethod description ((self element-class))
  "
RETURN: A string describing this element.
"
  (format nil "<An instance of ~A>" (class-name (class-of self))))


;; (defmethod identicalTo ((self ELEMENT-CLASS) (other ELEMENT-CLASS))
;;   "
;; RETURN: Whether self and other are the same element. (eq self other)
;; "
;;   (eq self other)
;;   )

(defmethod property-names ((self element-class))
  "
RETURN: The list of property names (keys) of properties of this element.
"
  (do ((ps (properties self) (cddr ps))
       (res '()))
      ((null ps) res)
    (push (car ps) res)))


(defmethod set-property ((self element-class) (prop-name symbol) prop-value)
  "
POST:  (eql (GET-PROPERTY self prop-name) prop-value)
"
  (setf (slot-value self 'properties)
        (plist-put (properties self) prop-name prop-value)))


(defmethod get-property ((self element-class) (prop-name symbol))
  "
RETURN: the property `prop-name' of this element.
"
  (plist-get (properties self) prop-name))


(defmethod delete-property ((self element-class) (prop-name symbol))
  "
DO:     Remove the property named `prop-name' from the property list of
        this element.
"
  (setf (slot-value self 'properties)
        (plist-remove (properties self) prop-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric elements (set)
  (:documentation "The elements in the set."))

(defclass set-class (element-class)
  ((elements
    :initform nil
    :initarg  :elements
    :accessor elements
    :type     list
    :documentation
    "The list of elements in this set."))
  (:documentation
   "A set of elements."))


(defmethod description ((self set-class))
  "
RETURN: A string describing this element.
"
  (format nil "<An instance of ~A with ~D elements>" 
          (class-name (class-of self)) (cardinal self)))


(defmethod cardinal ((self set-class))
  "
RETURN: The number of elements in this set.
"
  (length (elements self)))


(defmethod contains-element ((self set-class) (anelement element-class))
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
  (member anelement (elements self)))


(defmethod add-element ((self set-class) (newelement element-class))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == old_CARDINAL
        (not already_in) ==> (CARDINAL self) == (1+ old_CARDINAL)
                             (CONTAINS-ELEMENT self newElement)
"
  (when (not (contains-element self newelement))
    (setf (slot-value self 'elements) (cons newelement (elements self)))))


(defmethod add-elements ((self set-class) newelementlist)
  "
DO:     Add each element of the newElementList to this set.
"
  (dolist (newelement newelementlist)
    (add-element self newelement)))


(defmethod remove-element ((self set-class) (oldelement element-class))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == (1- old_CARDINAL),
                             (not (CONTAINS-ELEMENT self oldElement))
        (not already_in) ==> (CARDINAL self) == old_CARDINAL
"
  (setf (slot-value self 'elements) (delete oldelement (elements self))))


(defmethod perform-with-elements ((self set-class) lambda-body)
  "
DO:     calls lambda-body with each element in the set.
NOTE:   lambda-body must not change this set.
"
  (mapc lambda-body (elements self)))


(defmethod map-elements ((self set-class) lambda-body)
  "
RETURN: the list of results returned by lambda-body called with each element.
NOTE:   lambda-body must not change this set.
"
  (mapcar lambda-body (elements self)))


(defmethod select-elements ((self set-class) select-lambda)
  "
RETURN: A list of elements for which select-lambda returned true.
"
  (let ((result nil))
    (mapc (lambda (elem)
            (when (funcall select-lambda elem)
              (push elem result)))
          (elements self))
    result))


(defmethod element-list ((self set-class))
  "
RETURN: A new list of the elements in self.
"
  (map 'list (function identity) (elements self)))


(defmethod find-elements-with-property
    ((self set-class) (property symbol) value)
  "
RETURN: A list of elements that have as property PROPERTY the value VALUE.
"
  (select-elements self (lambda (elem)
                          (let ((pval (get-property elem property)))
                            (and pval (equal pval value))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric index (set)
  (:documentation "A hashtable used to index the elements in this set."))

(defclass hashed-set-class (set-class)
  ((index
    :initform (lambda () (make-hash-table :test 'eq))
    :initarg :index
    :accessor index
    :type     hash-table
    :documentation "A hashtable used to index the elements in this set."))
  (:documentation "This is a specialized kind of set that maintains a hashtable
index of its elements to be able to retrieve them rapidly."))
               

(defmethod contains-element ((self hashed-set-class) (anelement element-class))
  "
RETURN: Whether this set contains anElement.
"
  (gethash anelement (index self)))


(defmethod add-element ((self hashed-set-class) (newelement element-class))
  "
PRE:    already_in   = (CONTAINS-ELEMENT self newElement),
        old_CARDINAL = (CARDINAL self)
POST:   already_in       ==> (CARDINAL self) == old_CARDINAL
        (not already_in) ==> (CARDINAL self) == (1+ old_CARDINAL)
                             (CONTAINS-ELEMENT self newElement)
"
  (call-next-method)
  (setf (gethash newelement (index self)) newelement))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass edge-class (element-class)
  ((tag :initform nil :initarg :tag
        :documentation "Reserved for the client."))
  (:documentation
   "An abstract  edge."))


(defmethod description ((self edge-class))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format nil "<A ~A between { ~A and ~A }>" 
            (class-name (class-of self)) 
            (description (car  nodes)) 
            (description (cdr nodes)))))


(defgeneric copy (self &key &allow-other-keys)
  (:documentation "
RETURN: A COPY of this edge.
        The COPY has the same  NODES than this edge.
        Other attributes are normally copied."))


(defgeneric nodes (self)
  (:documentation "
RETURN: A list of NODES.
        For a GRAPH, it would be the nodes of the graph.
        For an edge, it would be the two nodes of the edge in no specific order.
        (Subclasses implementing directed edges should add specific methods
         to get the `from' and the `to' NODES)."))



(defgeneric is-between-nodes (self nodea nodeb)
  (:documentation "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        If this edge is directed then `nodeA' is compared to the from node
                                  and `nodeB' is compared to the  to  node,
        otherwise, the node order is not important."))


(defgeneric successor-of (self node)
  (:documentation "
RETURN: If node is a node of the edge, then return its successor or nil.
        That is, for an undirected edge e, 
             (and (eq (SUCCESSOR-OF e (car (NODES e))) (cdr (NODES e)))
                  (eq (SUCCESSOR-OF e (cdr (NODES e))) (car (NODES e))) )
        while for a directed edge d:
             (xor (eq (SUCCESSOR-OF e (car (NODES e))) (cdr (NODES e)))
                  (eq (SUCCESSOR-OF e (cdr (NODES e))) (car (NODES e))) )"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric weight (edge)
  (:documentation "The weight of the edge."))

(defclass weight-mixin-class ()
  ((weight
    :initform 1
    :initarg  :weight
    :accessor weight
    :type     number
    :documentation "The weight of the edge."))
  (:documentation
   "This is a mixin for the subclasses of EDGE-CLASS
    to add a weight to the edge."))


(defmethod set-weight ((self weight-mixin-class) (newweight integer))
  "
POST:   (equal (weight self) newWeight)
"
  (setf (slot-value self 'weight) newweight))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun node-list-p (item)
  "
RETURN: Whether `item' is a list of two objects kind of ELEMENT-CLASS.
"
  (and (listp item)
       (null (cddr item))
       (typep (first  item) 'element-class)
       (typep (second item) 'element-class)))



(defclass undirected-edge-class (edge-class)
  ((nodes
    :initarg :nodes
    :accessor nodes
    :type     (satisfies node-list-p)
    :documentation
    "A cons containing the two unordered NODES of the edge." ))
  (:documentation "An undirected edge."))


(defmethod copy ((self undirected-edge-class) &key &allow-other-keys)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (make-instance (class-of self)
    :nodes  (nodes self)))


(defun identical-nodes (nodes-cons-a nodes-cons-b)
  "
RETURN: Whether NODES-cons-a and NODES-cons-b contain the same NODES.
"
  (or (and (eq (car nodes-cons-a) (car nodes-cons-b))
           (eq (cdr nodes-cons-a) (cdr nodes-cons-b)))
      (and (eq (car nodes-cons-a) (cdr nodes-cons-b))
           (eq (cdr nodes-cons-a) (car nodes-cons-b)))))


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

(defmethod is-between-nodes ((self edge-class) 
                             (nodea element-class)  (nodeb element-class))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        The node order is not important.
"
  (identical-nodes (nodes self) (cons nodea nodeb)))


(defmethod successor-of ((self undirected-edge-class) (node element-class))
  "
RETURN: If node is a node of this edge, then the other node, else nil.
"
  (let ( (nodes (nodes self)) )
    (cond
      ((eq node (car nodes)) (cdr nodes))
      ((eq node (cdr nodes)) (car nodes))
      (t nil))))


(defmethod set-nodes ((self undirected-edge-class) 
                      (nodea element-class) (nodeb element-class))
  "
DO:     set the NODES of this edge.
"
  (setf (slot-value self 'nodes) (cons nodea nodeb)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass weighted-undirected-edge-class 
    (undirected-edge-class weight-mixin-class)
  ()
  (:documentation
   "A weighted, undirected edge."))


(defmethod description ((self weighted-undirected-edge-class))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format nil "<A ~A between { ~A and ~A } weighting ~A>" 
            (class-name (class-of self)) 
            (description (car  nodes))
            (description (cdr nodes))
            (weight self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric from (edge)
  (:documentation "The `from' node of this edge."))
(defgeneric to (edge)
  (:documentation "The `to' node of this edge."))
(defclass directed-edge-class (edge-class)
  ((from
    :initarg  :from
    :accessor  from
    :type      element-class
    :documentation
    "The `from' node of this edge." )
   (to
    :initarg  :to
    :accessor  to
    :type      element-class
    :documentation
    "The `to' node of this edge." ))
  (:documentation "An directed edge."))


(defmethod copy ((self directed-edge-class) &key &allow-other-keys)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (make-instance (class-of self)
    :from   (from   self)
    :to     (to     self)))


(defmethod nodes ((self directed-edge-class))
  "
RETURN: A list containing the two NODES of the edge in no particular order.
NOTE:   Use the accessor methods `from' and `to' to get the wanted node.
"
  (list (from self) (to self)))


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


(defmethod is-between-nodes ((self directed-edge-class) 
                             (nodea element-class)  (nodeb element-class))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        `nodeA' is compared to the from node
        and `nodeB' is compared to the  to  node.

"
  (and (eq (from self) nodea) (eq (to self) nodeb)))


(defmethod successor-of ((self directed-edge-class) (node element-class))
  "
RETURN: If node is the `from'  node of this edge, then the `to' node, else nil.
"
  (if (eq node (from self))  (to self)  nil))


(defmethod set-nodes ((self directed-edge-class)
                      (newfrom element-class) (newto element-class))
  "
DO:     set the NODES of this edge.
"
  (setf (slot-value self 'from) newfrom)
  (setf (slot-value self 'to)   newto))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass weighted-directed-edge-class (directed-edge-class weight-mixin-class)
  ()
  (:documentation "A weighted, directed edge."))


(defmethod description ((self weighted-directed-edge-class))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format nil "<A ~A between { ~A and ~A } weighting ~A>" 
            (class-name (class-of self))
            (description (car  nodes))
            (description (cdr nodes))
            (weight self))))


(defmethod copy ((self weighted-directed-edge-class) &key &allow-other-keys)
  "
RETURN: A COPY of this edge (only with same NODES).
"
  (make-instance (class-of self)
    :weight (weight self)
    :from   (from   self)
    :to     (to     self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subclass-of-edge-p (item)
  "
RETURN: Whether `item' is a subclass of EDGE-CLASS (not EDGE-CLASS itself).
"
  (subtypep item 'edge-class))

(defgeneric edges (graph)
  (:documentation "The edges of the graph."))

(defgeneric edge-class (graph)
  (:documentation "The class of edges of the graph."))

(defclass graph-class (element-class)
  ((nodes
    :initarg  :nodes
    :accessor nodes
    :type     set-class
    :documentation
    "The set of NODES in this graph.")
   (edges
    :initarg :edges
    :accessor edges
    :type     set-class
    :documentation
    "The set of edges in this graph.")
   (edge-class
    :initform 'undirected-edge-class
    :initarg  :edge-class
    :accessor edge-class
    ;;:type     (satisfies SUBCLASS-OF-EDGE-P)
    :documentation
    "The class used to make new edges in this graph. 
Default is UNDIRECTED-EDGE-CLASS."))
  (:documentation 
   "A graph of elements. By default, it's a undirected graph."))


(defmethod initialize-instance :after ((self graph-class) &rest initargs
                                       &key &allow-other-keys)
  "
DO:     Initalize the instance nodes and edges.
"
  (declare (ignore initargs))
  (unless (slot-boundp self 'nodes)
    (setf (nodes self) (make-instance 'set-class)))
  (if (slot-boundp self 'edges)
      (progn
        ;; todo: we should check that all edges are between two nodes of the graph.
        )
      (setf (edges self) (make-instance 'set-class)))
  self)



(defmethod description ((self graph-class))
  "
RETURN: A string describing this element.
"
  (format nil "<A ~A with ~D NODES and ~D edges>" 
          (class-name (class-of self)) 
          (cardinal (nodes self))
          (cardinal (edges self))))


(defmethod add-node ((self graph-class) (newnode element-class))
  "
DO:     Add newNode to the set of NODES of this graph.
"
  (add-element (nodes self) newnode))


(defmethod add-nodes ((self graph-class) newnodelist)
  "
DO:     Add a list of new NODES to the set of NODES of this graph.
"
  (add-elements (nodes self) newnodelist))



(defmethod remove-node ((self graph-class) (oldnode element-class))
  "
DO:      Remove the oldNode from the graph. 
         This implies removing all the edges adjacent to the node too.
"
  (when (contains-element (nodes self) oldnode)
    (remove-edges self (select-elements (edges self)
                                        (lambda (edge)
                                          (or (eq oldnode (from edge))
                                              (eq oldnode (to   edge))))))))


(defmethod remove-nodes ((self graph-class) oldnodelist)
  "
DO:      Remove all the NODES of the oldNodeList from this graph.
"
  (dolist (node oldnodelist) (remove-node self node)))


(defmethod add-edge ((self graph-class) (newedge edge-class))
  "
PRE:    (and (CONTAINS-ELEMENT (NODES self) (nth 0 (NODES newEdge)))
             (CONTAINS-ELEMENT (NODES self) (nth 1 (NODES newEdge))))
DO:     Add a new edge to this graph.
"
  (when (and (contains-element (nodes self) (car (nodes newedge)))
             (contains-element (nodes self) (cdr (nodes newedge))))
    (add-element (edges self) newedge)))


(defmethod add-edge-between-nodes ((self graph-class) 
                                   (nodea element-class) (nodeb element-class))
  "
DO:     Create a new edge (of class edge-class) between `nodeA' and `nodeB'.
        and add it to this graph.
        If the edge is directed, 
        then `nodeA' is the `from' node and `nodeB' the `to' node.
"
  (let ((edge (make-instance (edge-class self))))
    (set-nodes edge nodea nodeb)
    (add-edge self edge)))


(defmethod remove-edge ((self graph-class) (oldedge edge-class))
  "
DO:     Remove the `oldEdge' from this graph.
"
  (remove-element (edges self) oldedge))


(defmethod remove-edges ((self graph-class) edge-list)
  "
DO:     Remove all the edges in edge-list from this graph.
"
  (dolist (edge edge-list)  (remove-edge self edge)))


(defmethod remove-edges-between-nodes ((self graph-class)
                                       (nodea element-class)
                                       (nodeb element-class))
  "
DO:     Remove all edges between `nodeA' and `nodeB'.
"
  (mapc (lambda (edge)
          (remove-edge self edge))
        (edges-between-nodes self nodea nodeb)))


(defmethod edges-between-nodes ((self graph-class)
                                (nodea element-class) (nodeb element-class))
  "
RETURN: A list of edges existing between the `nodeA' and `nodeB'.
        If the graph is directed then `nodeA' corresponds to the from node
                                  and `nodeB' corresponds to the  to  node.
"
  (select-elements (edges self)
                   (lambda (edge)
                     (is-between-nodes edge nodea nodeb))))


(defmethod directed-edges-between-nodes ((self graph-class) 
                                         (fromnode element-class)
                                         (tonode element-class))
  "
RETURN: A list of edges existing from the `fromNode' and to the `toNode'.
"
  (select-elements (edges self)
                   (lambda (edge)
                     (eq (successor-of edge fromnode) tonode))))


(defmethod directed-edges-from-node ((self graph-class)
                                     (fromnode element-class))
  "
PRE:    edge-class is-subclass-of DIRECTED-EDGE-CLASS
        or edge-class eq DIRECTED-EDGE-CLASS.
RETURN: A list of edges existing from the `fromNode'.
"
  (unless (subtypep (edge-class self) 'directed-edge-class)
    (error "This graph is not a directed graph. Can't apply ~
            DIRECTED-EDGES-FROM-NODE."))
  (select-elements (edges self)
                   (lambda (edge) (eq (from edge) fromnode))))


(defmethod successor-nodes ((self graph-class) (node element-class))
  "
RETURN: The list of successors NODES of the given node in this graph.
NOTE:   For undirected graphs, it's the same as ADJACENT-NODES.
"
  (let ((result nil))
    (perform-with-elements
     (edges self)
     (lambda (edge)
       (let ( (succ (successor-of edge node)) )
         (when succ
           (unless (member succ result) (push succ result))))))
    result))


(defmethod adjacent-nodes ((self graph-class) (node element-class))
  "
RETURN: The list of NODES adjacent to the given node in this graph.
NOTE:   For directed graphs, an adjacent node is either a predecessor
        or a successors of the node.
"
  (let ((result nil))
    (perform-with-elements
     (edges self)
     (lambda (edge)
       (let ((ns (nodes edge)))
         (cond 
           ((eq node (car ns)) 
            (unless (member (cdr ns) result) (push (cdr ns) result)))
           ((eq node (cdr ns)) 
            (unless (member (car ns) result) (push (car ns) result)))))))
    result))


(defmethod flow-distance-from-node ((self graph-class) 
                                    (startnode element-class)
                                    (prop-name symbol))
  "
DO:     Compute for each node in this graph the distance from the startNode,
        and store it as a property named prop-name.
NOTE:   If the graph is not connex, then some distances will be nil, 
        meaning infinity.
"
  (perform-with-elements (nodes self) (lambda (node) 
                                        (set-property node prop-name nil)))
  (when (contains-element (nodes self) startnode)
    (set-property startnode prop-name 0)
    (let ( (cur-nodes (list startnode))
          cur-node distance suc-nodes suc-dist )
      (while cur-nodes
        (setf cur-node (car cur-nodes)
              cur-nodes (cdr cur-nodes)
              distance (1+ (get-property cur-node prop-name))
              suc-nodes (successor-nodes self cur-node))
        ;; (not (null distance))
        (dolist (suc-node suc-nodes)
          (setf suc-dist (get-property suc-node prop-name))
          (when (or (null suc-dist) (< distance suc-dist))
            (set-property suc-node prop-name distance)
            (unless (member suc-node cur-nodes)
              (push suc-node cur-nodes))))))))


(defmethod walk-from-node ((self graph-class) (startnode element-class)
                           lambda-body)
  "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each node as argument. 
"
  (let ((stamp (gensym "walked-")))
    (when (contains-element (nodes self) startnode)
      (perform-with-elements (nodes self) 
                             (lambda (node) (set-property node stamp nil)))
      (let ( (cur-nodes (list startnode))
            cur-node  suc-nodes  )
        (while cur-nodes
          (setq cur-node  (car cur-nodes)
                cur-nodes (cdr cur-nodes))

          (set-property cur-node stamp t)
          (funcall lambda-body cur-node)

          (setq suc-nodes (successor-nodes self cur-node))
          (dolist (suc-node suc-nodes)
            (unless (get-property suc-node stamp)
              (push suc-node cur-nodes)) )))
      (perform-with-elements (nodes self) 
                             (lambda (node) (delete-property node stamp))))))


(defmethod walk-edges-from-node ((self graph-class)
                                 (startnode element-class) lambda-body)
  "
DO:     Walk the graph starting form startNode, calling lambda-body 
        with each edges as argument. Since it's the edges that are passed
        to lambda-body, one node can be \"walked\" several times either as
        `from' or `to' node or different edges.
"
  (let ((stamp (gensym "walked-")))
    (when (contains-element (nodes self) startnode)
      (perform-with-elements (edges self) 
                             (lambda (item) (set-property item stamp nil)))
      (perform-with-elements (nodes self) 
                             (lambda (item) (set-property item stamp nil)))
      (set-property startnode stamp t)
      (let ((cur-nodes (list startnode))
            cur-node)
        (while cur-nodes
          (setq cur-node  (car cur-nodes)
                cur-nodes (cdr cur-nodes))
          (dolist (edge (directed-edges-from-node self cur-node))
            (unless (get-property edge stamp)
              (set-property edge stamp t)
              (funcall lambda-body edge)
              (unless (get-property (to edge) stamp)
                (set-property (to edge) stamp t)
                (push (to edge) cur-nodes))))))
      (perform-with-elements (edges self) 
                             (lambda (item) (delete-property item stamp)))
      (perform-with-elements (nodes self) 
                             (lambda (item) (delete-property item stamp))))))



(defmethod copy ((self graph-class)
                 &key (copy-nodes nil) (copy-edges t) &allow-other-keys)
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
  (let (new-nodes
        new-edges
        node-hash)
    (when (and copy-nodes (not copy-edges))
      (error "Can't have both COPY-NODES and (NOT COPY-EDGES)."))
    (unless copy-nodes       (setf copy-nodes nil))
    (unless (not copy-edges) (setf copy-edges t))
    (when   (not copy-edges) (setf copy-nodes nil))
    (when   copy-nodes       (setf copy-edges t))
    (if copy-nodes 
        (setf node-hash (make-hash-table :test 'eq 
                                         :size (cardinal (nodes self)))
            
              new-nodes (make-instance 'set-class 
                          :elements (map-elements 
                                     (nodes self)
                                     (lambda (node)
                                       (let ((new-node (copy node)))
                                         (setf (gethash node node-hash) new-node)
                                         new-node)))))
        (setf new-nodes  (nodes self)))
    (if copy-edges
        (setf new-edges 
              (make-instance 'set-class 
                :elements (map-elements
                           (edges self)
                           (lambda (edge)
                             (let ((new-edge (copy edge))
                                   nodes)
                               (when copy-nodes
                                 (setq nodes (nodes new-edge))
                                 (set-nodes new-edge 
                                            (gethash (car nodes) node-hash)
                                            (gethash (cdr nodes) node-hash)))
                               new-edge)))))
        (setf new-edges (edges self)))
    (make-instance (class-of self) 
      :nodes new-nodes
      :edges new-edges
      :edge-class (edge-class self))))


(defmethod find-nodes-with-property ((self graph-class)
                                     (property symbol) value)
  "
RETURN: A list of NODES that have as property PROPERTY the value VALUE.
"
  (find-elements-with-property (nodes self) property value))


(defmethod show-graph ((self graph-class))
  "
DO: Prints a description of the graph on the *STANDARD-OUTPUT*.
"
  (format t "~A {~%" (description self))
  (perform-with-elements 
   (nodes self)
   (lambda (node) (format t "   node ~A~%"  (description node))))
  (perform-with-elements
   (edges self)
   (lambda (edge) (format t "   edge ~A~%" (description edge))))
  (format t "}~%"))


;;;; THE END ;;;;
