;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               graph-dot.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generate dot files from graphs (graph-class).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-06 <PJB> Added defgeneric.
;;;;    2003-05-16 <PJB> Converted from emacs lisp.
;;;;    2003-05-14 <PJB> Extracted from pjb-cvs.
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.GRAPHVIZ.GRAPH-DOT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH")
  (:export "GENERATE-DOT")
  (:documentation
   "
This package exports methods for GRAPH to generate dot(1) files.

    

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.GRAPHVIZ.GRAPH-DOT")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating dot files 

(defun dot-ident (ident)
  "
RETURN: A string containing the ident with the dash removed.
"
  (remove (character "-") (string ident)))


(defgeneric generate-dot (graph)
  (:documentation "RETURN: a string containing the dot file contents for the GRAPH."))

(defmethod generate-dot ((self element-class))
  "
RETURN: A string containing the dot file data for this ELEMENT-CLASS node.
"
  (let ((style     (or (get-property self :dot-style) "filled"))
        (color     (or (get-property self :dot-color) "black"))
        (fillcolor (or (get-property self :dot-fill-color) "LightYellow"))
        (label     (or (get-property self :dot-label)
                       (let ((pnames (property-names self)))
                         (cond
                           ((null pnames) nil)
                           ((= 1 (length pnames))
                            (format nil "~A" (get-property self (car pnames))))
                           (t (unsplit-string
                               (mapcar
                                (lambda (prop-name)
                                  (if (let ((name (symbol-name prop-name)))
                                        (and (< 4 (length name) )
                                             (string-equal
                                              "DOT-"
                                              (subseq
                                               (symbol-name prop-name) 0 4))))
                                      ""
                                      (format nil "~A = ~A" prop-name
                                              (get-property self prop-name))))
                                pnames)
                               #\Newline))))
                       (dot-ident (ident self)))))
    (format nil "~A [ style=~A color=~A fillcolor=~A label=\"~A\" ];~%"
            (dot-ident (ident self)) style color fillcolor label)))




(defmethod generate-dot ((self edge-class))
  "
RETURN: A string containing the dot file data for this edge.
"
  (format nil "~A -> ~A ;~%"
          (dot-ident (ident (car (nodes self))))
          (dot-ident (ident (cdr (nodes self))))))




(defmethod generate-dot ((self directed-edge-class))
  "
RETURN: A string containing the dot file data for this edge.
"
  (format nil "~A -> ~A ;~%"
          (dot-ident (ident (from self)))
          (dot-ident (ident (to self)))))



(defmethod generate-dot ((self weighted-directed-edge-class))
  "
RETURN: A string containing the dot file data for this edge.
"
  (format nil "~A -> ~A [ weight=~D, style=~A, color=~A ];~%"
          (dot-ident (ident (from self)))
          (dot-ident (ident (to   self)))
          (weight self)
          (cond
            ((< (weight self) 3)  "dotted")
            ((< (weight self) 10) "dashed")
            ((< (weight self) 15) "solid")
            (t                    "bold"))
          "black"))




;;; (DESCRIPTION (car (element-list (NODES g))))
;;; (MAP-ELEMENTS (NODES g) (lambda (elem) (ident elem)))
;;; (car (element-list (NODES g)))


(defmethod generate-dot ((self graph-class))
  "
RETURN: A string containing the dot file data for this graph.
NOTE:   dot graphs are directed.
"
  (apply 
   (function concatenate) 'string
   (flatten
    (list
     (format nil "digraph ~A~%" (or (get-property self :name) "Untitled"))
     (format
         nil
       (concatenate 'string
         "{~%"
         (format nil "rankdir=~A;~%" (or (get-property self :dot-rankdir) "TB"))
         (format nil "concentrate=~:[false~;true~];~%" (get-property self :dot-concentrate))

         "// attributes of graph:~%"
         "// page=8,11.4;    // page size (NeXTprinter:A4).~%"
         "// size=30,8;     // graph size (please edit to fit).~%"
         "// rotate=90;    // graph orientation (please edit to fit).~%"
         "// ratio=fill;  // fill the size (or compress, auto, aspect/ratio).~%"
         "nodesep=0.3;~%"
         "ranksep=0.3;~%"
         "center=1;~%"
         "// common attributes of NODES:~%"
         "node [height=0.2 width=0.5 shape=box fontsize=8 fontname=Times] ;~%"))
     (map-elements (nodes self) (lambda (node) (generate-dot node)))
     (format nil "// common attributes of edges:~%edge [style=solid];~%")
     (map-elements (edges self) (lambda (edge) (generate-dot edge)))
     (format nil "}~%")))))



;;;; graph-dot.lisp                   --                     --          ;;;;
