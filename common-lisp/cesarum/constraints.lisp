;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               constraints.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-07-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2012
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CONSTRAINTS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DICTIONARY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "SOLVE-CONSTRAINTS")
  (:documentation "

A little constraint solver.

Given a graph of nodes, and a propagate function that propagates
constraints from node to nodes, the solver propagates the constraints
until no change occurs.

It computes the strongly connected components, and performs a
topological sort of the condensed graph to minimalize the number of
calls to propagate.



License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2011 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CONSTRAINTS")



;;;
;;; Tarjan's Strongly Connected Components Algorithm.
;;;

(defun make-graph (edges)
  ;; We represent the graph as a dictionnary mapping FROM nodes to
  ;; their adjacency list.
  (loop
     :with graph = (make-dictionary 'adaptating-dictionary)
     :for (from to) :in edges
     :do (progn
           (push to (dictionary-get graph from '()))
           (setf (dictionary-get graph to) (dictionary-get graph to '())))
     :finally (return graph)))

(defun graph-nodes (graph)
  "RETURN:   The list of nodes in the GRAPH"
  (let ((nodes '()))
    (dictionary-map (lambda (k v) (declare (ignore v)) (push k nodes)) graph)
    nodes))

(defun graph-adjacency-list (graph node)
  "RETURN:   The list of successors of NODE in the GRAPH."
  (dictionary-get graph node))




(defparameter *germany*
  (make-graph (mapcan (lambda (edge) (list edge (reverse edge)))
                     '((frankfurt mannheim)
                       (frankfurt wuerzburg)
                       (frankfurt kassel)
                       (stuttgart nuemberg)
                       (mannheim karlsruhe)
                       (wuerzburg erfurt)
                       (wuerzburg nuemberg)
                       (kassel muenchen)
                       (karlsruhe augsburg)
                       (augsburg muenchen)
                       (nuemberg muenchen)))))



(defun breadth-first-search (graph root goal &key (test 'eql) key)
  "
DO:     Implement the Breadth First Search algorithm on the given
        GRAPH, starting from the ROOT node, until the GOAL is reached.
        The GOAL is compared with the TEST function to the value of
        the KEY function applied to the nodes. (Default for KEY is
        IDENTITY).
RETURN: The goal node.
COMPLEXITY:  Time: O(|V|+|E|), Space: O(|V|)
"
  (breadth-first-search-if graph
                           root
                           (lambda (node) (funcall test node goal))
                           :key key))


(defun breadth-first-search-if (graph root predicate &key key)
  "
DO:     Implement the Breadth First Search algorithm on the given
        GRAPH, starting from the ROOT node, until the PREDICATE
        applied on the value of the KEY function applied to the node
        returns true.  (Default for KEY is IDENTITY).
RETURN: The goal node.
COMPLEXITY:  Time: O(|V|+|E|), Space: O(|V|)
"
  (let ((key (or key (function identity)))
        (head   '())
        (tail   '())
        (marks (make-hash-table)))
    (flet ((enqueue (node) (if (null head)
                               (setf head (list node)
                                     tail head)
                               (setf (cdr tail) (list node)
                                     tail (cdr tail))))
           (dequeue () (cond
                         ((null head)     nil)
                         ((eql head tail) (prog1 (car head)
                                            (setf head nil
                                                  tail nil)))
                         (t               (pop head))))
           (empty () (null head))
           (stop (node) (funcall predicate (funcall key node)))
           (mark (node) (setf (gethash node marks) t))
           (markedp (node) (gethash node marks)))
      (declare (inline enqueue dequeue empty stop mark markedp))
      (enqueue root)
      (mark root)
      (loop :until (empty) :do
         (let ((v (dequeue)))
           (when (stop v)
             (return-from breadth-first-search-if v))
           (loop :for w :in (graph-adjacency-list graph v) :do
              (unless (markedp w)
                (enqueue w)
                (mark w))))))))


(defun depth-first-search (graph root goal &key (test 'eql) key)
    "
DO:     Implement the Depth First Search algorithm on the given
        GRAPH, starting from the ROOT node, until the GOAL is reached.
        The GOAL is compared with the TEST function to the value of
        the KEY function applied to the nodes. (Default for KEY is
        IDENTITY).
RETURN: The goal node.
COMPLEXITY:  Time: O(|V|+|E|), Space: O(|V|+|E|)
"
  (depth-first-search-if graph
                           root
                           (lambda (node) (funcall test node goal))
                           :key key))


(defun depth-first-search-if (graph root predicate &key key)
    "
DO:     Implement the Depth First Search algorithm on the given
        GRAPH, starting from the ROOT node, until the PREDICATE
        applied on the value of the KEY function applied to the node
        returns true.  (Default for KEY is IDENTITY).
RETURN: The goal node.
COMPLEXITY:  Time: O(|V|+|E|), Space: O(|V|+|E|)
"
  (let ((key (or key (function identity)))
        (q     '())
        (marks (make-hash-table)))
    (flet ((stop (node) (funcall predicate (funcall key node)))
           (mark (node) (setf (gethash node marks) t))
           (markedp (node) (gethash node marks)))
      (declare (inline stop mark markedp))
      (push root q)
      (mark root)
      (loop :while q :do
         (let ((v (pop q)))
           (when (stop v)
             (return-from depth-first-search-if v))
           (loop :for w :in (graph-adjacency-list graph v) :do
              (unless (markedp w)
                (push w q)
                (mark w))))))))





(defstruct (node (:constructor make-node))
  label index lowlink)

(defun tarjan-strongly-connected-components (graph)
  "
DO:     Implement Tarjan's Strongly Connected Components Algorithm.
RETURN: A set of strongly connected components = sets of nodes.
"
  ;; Uses the NODE structure, and applies GRAPH-NODES and
  ;; GRAPH-ADJACENCY-LIST to GRAPH to get the list of vertices, and
  ;; the adjacency list of a vertex of the GRAPH.
  (let ((index 0)
        (nodes (make-dictionary 'adaptating-dictionary
                                :contents (mapcan (lambda (label)
                                                    (list label (make-node :label label)))
                                                  (graph-nodes graph))))
        (stack  '())
        (strongly-connected-components '()))
    (labels ((node (label) (dictionary-get nodes label))
             (strong-connect (node)
               ;; Set the depth index for v to the smallest unused index
               (setf (node-lowlink node) (setf (node-index node) index))
               (incf index)
               (push node stack)
               ;; Consider successors of v
               (loop
                  :for successor-label :in (graph-adjacency-list graph (node-label node))
                  :for successor = (node successor-label)
                  :do (cond
                        ((null (node-index successor))
                         ;; Successor w has not yet been visited; recurse on it
                         (strong-connect successor)
                         (setf (node-lowlink node) (min (node-lowlink node) (node-lowlink successor))))
                        ((member successor stack)
                         ;; Successor w is in stack S and hence in the current SCC
                         (setf (node-lowlink node) (min (node-lowlink node) (node-index successor))))))
               ;; If v is a root node, pop the stack and generate an SCC
               (when (= (node-lowlink node) (node-index node))
                 (push (loop
                          :for successor = (pop stack)
                          :collect (node-label successor)
                          :until (eql successor node))
                       strongly-connected-components))))
      (dictionary-map (lambda (label node)
                        (declare (ignore label))
                        (unless (node-index node)
                          (strong-connect node)))
                      nodes)
      strongly-connected-components)))



#-(and)"
algorithm tarjan is
  input: graph G = (V, E)
  output: set of strongly connected components (sets of vertices)

  index := 0
  S := empty
  for each v in V do
    if (v.index is undefined)
      strongconnect(v)
    end if
  repeat

  function strongconnect(v)
    // Set the depth index for v to the smallest unused index
    v.index := index
    v.lowlink := index
    index := index + 1
    S.push(v)

  // Consider successors of v
    for each (v, w) in E do
      if (w.index is undefined) then
        // Successor w has not yet been visited; recurse on it
        strongconnect(w)
        v.lowlink := min(v.lowlink, w.lowlink)
      else if (w is in S) then
        // Successor w is in stack S and hence in the current SCC
        v.lowlink := min(v.lowlink, w.index)
      end if
    end for

    // If v is a root node, pop the stack and generate an SCC
    if (v.lowlink = v.index) then
      start a new strongly connected component
      repeat
        w := S.pop()
        add w to current strongly connected component
      until (w = v)
      output the current strongly connected component
    end if
  end function

"



(defun condensate (graph)
  "
DO:      Given a GRAPH, find the strongly connected components in the
         graph, and replace them with single nodes to obtain a DAG.
RETURN:  The DAG, and an a-list mapping new names (uninterned symbols)
         to strongly connected subgraphs.
"
  ;; (condensate graph) --> dag; alist of (new-name . strongly-connected-component)
  ;; The DAG and the STRONGLY-CONNECTED-COMPONENT are given as list of edges (from to).
  (let* ((components (tarjan-strongly-connected-components graph))
         (old-new    (make-hash-table))
         (new-old    '()))
    (values
     (nconc
      (mapcan (lambda (from)
                (mapcar (lambda (to)
                          (list (gethash from old-new from)
                                (gethash to   old-new to)))
                        (graph-adjacency-list graph from)))
              (mapcar (lambda (component)
                        (if (null (rest component))
                            (first component)
                            (let ((new (make-symbol (format nil "~{~A~^/~}" component))))
                              (push (cons new component) new-old)
                              (dolist (old component)
                                (setf (gethash old old-new) new))
                              new)))
                      components))
      (delete-duplicates
       (mapcan (lambda (entry)
                 (let ((component (car entry)))
                   (mapcan (lambda (from)
                             (mapcan (lambda (to)
                                       (let ((new-to (gethash to old-new to)))
                                         (if (eql new-to component)
                                             '()
                                             (list (list component new-to)))))
                                     (graph-adjacency-list graph from)))
                           (cdr entry))))
               new-old)
       :test (function equal)))
     (mapcar (lambda (entry)
               (let ((component (car entry)))
                 (list component
                       (cdr entry)
                       (mapcan (lambda (from)
                                 (mapcar (lambda (to) (list from to))
                                         (graph-adjacency-list graph from)))
                               (cdr entry)))))
             new-old))))

#-(and)
(defun compute-closure (fun set)
  "
FUN:     set --> P(set)
          x |--> { y }
RETURN:  The closure of fun on the set.
NOTE:    Not a lisp closure!
EXAMPLE: (compute-closure (lambda (x) (list (mod (* x 2) 5))) '(1)) --> (2 4 3 1)
NOTE:    This version avoids calling FUN twice with the same argument.
"
  (loop
     :for follows = (delete-duplicates (mapcan fun set))
     :then (delete-duplicates (append (mapcan fun newbies) follows))
     :for newbies = (set-difference follows set)
     :while newbies
     :do (setf set (append newbies set))
     :finally (return set)))



(defun solve-constraints (edges propagate)
  "
DO:         Calls PROPAGATE on each edge until PROPAGATE returns NIL
            for all arcs.
EDGES:      A list of edges (from to).
            The nodes FROM and EDGE must be comparable with EQL.
PROPAGATE:  A function taking the nodes FROM and TO of an edge as argument,
            and returning whether changes occured.
"
  (let ((graph (make-graph edges)))
    (multiple-value-bind (dag-edges components) (condensate graph)
      (let ((plan (topological-sort
                   (delete-duplicates (mapcan (function copy-list) dag-edges))
                   (lambda (a b) (member (list a b) dag-edges :test (function equal))))))
        (flet ((close-constraint-cycle (component)
                 (loop
                    :with edges = (third component)
                    :for changed = nil
                    :do (loop
                           :for (from to) :in edges
                           :do (when (funcall propagate from to)
                                 (setf changed t)))
                    :while changed)))
          (dolist (from plan (values))
            (let ((component (assoc from components)))
              (if component
                  (close-constraint-cycle component)
                  (dolist (to (graph-adjacency-list graph from))
                    (funcall propagate from to))))))))))


;;;; THE END ;;;;
