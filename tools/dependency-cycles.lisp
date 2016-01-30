;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               dependency-cycles.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-09-06 <PJB> Updated for publication.
;;;;    2012-04-06 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPHVIZ.GRAPH-DOT"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS")
  (:export "ADJACENCY-LIST" "REACHABLE-LIST"
           "FIND-CYCLES" "FIND-SHORTEST-PATH"
           "REPORT-PROBLEMS" "PRINT-CYCLES"
           "DEPENDENCIES-GRAPH" "GENERATE-DEPENDENCIES-GRAPH")
  (:documentation "

Find cycles in a dependency graph.

The graph is defined by a list of nodes and two methods:

 (ADJACENCY-LIST node) -> list-of-nodes

 (REACHABLE-LIST node) -> list-of-nodes

             which is the recursive closure of ADJACENCY-LIST,
             but provided for efficiency.


The function FIND-CYCLES returns a list of cycles in the graph defined
by the given list of nodes and the previous methods.

 (FIND-CYCLES nodes) -> list-of-cycles

The function REPORT-PROBLEMS prints a report of the found cycles.

 (REPORT-PROBLEMS nodes)


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2012 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))
(in-package "COM.INFORMATIMAGO.TOOLS.DEPENDENCY-CYCLES")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (com.informatimago.clext.character-sets::fill-character-set-emacs-encoding)
;;   (com.informatimago.clext.character-sets::fill-character-set-lisp-encoding))

(defparameter *iso-8859-1* (make-external-format (find-character-set "ISO-8859-1") :unix))
(defparameter *utf-8*      (make-external-format (find-character-set "UTF-8")      :unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defgeneric adjacency-list (object)
  (:documentation
   "Return the list of vertices connected to vertex thru a single edge.")
  (:method ((vertex t)) '()))


(defgeneric reachable-list (vertex)
  (:documentation
   "Return the list of vertices rechable from vertex.")
  (:method ((vertex t)) '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;; dependency graphs should be trees, so we run topological sorts on
;; them.


(defun reachable (root successors)
  "
SUCCESSORS:  A function of one node returning a list of nodes.
RETURN:      A list of objects reachable from root traversing SUCCESSORS.
"
  (loop
     :with reachable = '()
     :with old = (funcall successors root)
     :while old
     :do (let ((current (pop old)))
           (pushnew current reachable)
           (dolist (successor (funcall successors current))
             (unless (member successor reachable)
               (pushnew  successor old))))
     :finally (return reachable)))


(defun closest-to-root (nodes ordered-nodes)
  "
RETURN: the node in NODES that is the closest to ROOT according to ordered-nodes
"
  (loop
     :with closest = (pop nodes)
     :with minimum = (position closest ordered-nodes)
     :for node :in nodes
     :for distance = (position node ordered-nodes)
     :initially (assert minimum)
     :when (and distance (< distance minimum))
     :do (setf closest node
               minimum distance)
     :finally (return (values closest minimum))))


;; (defun closer-to-root (a b ordered-nodes)
;;   (let ((p (position a ordered-nodes))
;;         (q (position b ordered-nodes)))
;;     (and p q (< p q))))



(defun find-shortest-path (from to successors)
  "
RETURN: The shortest path of length>0 from FROM to TO if it exists, or NIL.
"
  ;; breadth first search
  (loop
     :with processed = '()
     :for paths = (list (list from)) :then new-paths
     :for new-paths = (remove-if (lambda (head) (member head processed))
                                 (mapcan (lambda (path)
                                           (mapcar (lambda (new-node) (cons new-node path))
                                                   (funcall successors (first path))))
                                         paths)
                                 :key (function first))
     :for shortest-path = (find to new-paths :key (function first))
     :do (setf paths     (nconc paths new-paths)
               processed (nconc (delete-duplicates (mapcar (function first) new-paths)) processed))
     :until (or shortest-path (endp new-paths))
     :finally (return (reverse shortest-path))))


(defun print-cycle (path)
  (format t "~%There is a cycle going ~%from ~A" (first path))
  (dolist (node (rest path))
    (format t "~%  to ~A" node))
  (format t " !!!~%"))


(defun find-cycles (objects)
  (map 'list (lambda (cycle) (find-shortest-path cycle cycle (function adjacency-list)))
       (remove-if-not (lambda (x) (member x (reachable-list x))) objects)))


(defun report-problems (objects &key (report *error-output*))
  "
DO:     Find cycles in the graph defined by the nodes in the OBJECTS
        sequence and the ADJACENCY-LIST an REACHABLE-LIST methods.
"
  (let ((cycles (find-cycles objects)))
    (when cycles
      (format report "~&There are ~A cycles in the dependency relationship!~%"
              (length cycles))
      (dolist (path (sort cycles 
                          (function string<)
                          :key (function prin1-to-string)))
        (print-cycle path)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;


(defun dependencies-graph (objects)
  "
RETURN:  a graph as defined by the nodes in the OBJECTS sequence and
         the ADJACENCY-LIST an REACHABLE-LIST methods.
"
  (let ((graph (make-instance 'graph-class :edge-class 'directed-edge-class )))
    (add-nodes graph objects)
    (map nil (lambda (from)
               (dolist (to (adjacency-list from))
                 (when (find to objects)
                   (add-edge-between-nodes graph from to))))
         objects)
    graph))


(defun generate-dependencies-graph (objects output-dotfile-path)
  "
DO:     Generates a GraphViz dot file to draw the dependency-graph defined
        by the nodes in the OBJECTS sequence and the ADJACENCY-LIST an
        REACHABLE-LIST methods.
"
  (with-open-file (dot output-dotfile-path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :external-format *iso-8859-1*)
    (let ((graph  (dependencies-graph objects)))
      (set-property graph :nodesep 3)
      (set-property graph :ranksep 7)
      (set-property graph :page "64,48")
      (set-property graph :ratio :fill)
      (princ (generate-dot graph) dot))
    (values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;; (defun load-dependencies (path files)
;;   (with-open-file (stream path :external-format *iso-8859-1*)
;;     (loop
;;        :for line = (read-line stream nil stream)
;;        :for (includep included-file systemp) = (if (eq line stream)
;;                                                    '(nil nil nil)
;;                                                    (parse-include-line line))
;;        :until (eq line stream)
;;        :when includep
;;        :collect (or (gethash included-file files)
;;                     (make-instance 'header-file :path included-file)))))
;; 
;; 
;; (defparameter *root-path*       #p"/home/pjb/src/manager2/trunk/")
;; (defparameter *files*           (make-hash-table :test (function equal)))
;; (defparameter *headers*         (make-hash-table :test (function equal)))
;; (defparameter *sorted-headers*  '())
;; 
;; 
;; (defun process-sources (root-path)
;;   (let ((header-paths (directory (merge-pathnames "**/*.hxx" root-path nil)))
;;         (source-paths (directory (merge-pathnames "**/*.cxx" root-path nil))))
;;     (format t "~A headers; ~A sources~%" (length header-paths) (length source-paths))
;; 
;;     (dolist (path source-paths)
;;       (setf (gethash path *files*) (make-instance 'source-file :path (namestring path))))
;;     (dolist (path header-paths)
;;       (setf (gethash path *headers*)
;;             (setf (gethash path *files*) (make-instance 'header-file :path (namestring path))))))
;;   (setf *print-circle* t)
;;   (maphash (lambda (path file)
;;              (setf (dependencies file) (load-dependencies path *files*)))
;;            *files*)
;;   (maphash (lambda (path file)
;;              (declare (ignore path))
;;              (setf (dependencies-reachable file) (reachable file (function dependencies))))
;;            *files*)
;;   (progn (setf *sorted-headers*  (topological-sort (hash-table-values *headers*) (function dependencies)))
;;          (if (= (length *sorted-headers*) (hash-table-count *headers*))
;;              (format t "~&No #include cycle amongst headers. ~%")
;;              (format t "~&The #include relationship between headers contains cycles! It should be a tree.~%"))
;;          (report-problems (hash-table-values *headers*)))
;;   (progn (setf *sorted-files*  (topological-sort (hash-table-values *files*) (function dependencies)))
;;          (if (= (length *sorted-files*) (hash-table-count *files*))
;;              (format t "~&No #include cycle amongst sources. ~%")
;;              (format t "~&The #include relationship between sources contains cycles! It should be a tree.~%"))
;;          (report-problems (hash-table-values *files*))))

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; THE END ;;;;
