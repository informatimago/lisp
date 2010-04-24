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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPH"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST")
  (:EXPORT "GENERATE-DOT")
  (:DOCUMENTATION
   "This package exports methods for GRAPH to generate dot(1) files.
    
    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating dot files 

(DEFUN DOT-IDENT (IDENT)
  "
RETURN: A string containing the ident with the dash removed.
"
  (REMOVE (CHARACTER "-") (STRING IDENT)))


(DEFGENERIC GENERATE-DOT (SELF))

(DEFMETHOD GENERATE-DOT ((SELF ELEMENT-CLASS))
  "
RETURN: A string containing the dot file data for this ELEMENT-CLASS node.
"
  (LET ((STYLE     (OR (GET-PROPERTY SELF :DOT-STYLE) "filled"))
        (COLOR     (OR (GET-PROPERTY SELF :DOT-COLOR) "black"))
        (FILLCOLOR (OR (GET-PROPERTY SELF :DOT-FILL-COLOR) "LightYellow"))
        (LABEL     (OR (GET-PROPERTY SELF :DOT-LABEL)
                       (LET ((PNAMES (PROPERTY-NAMES SELF)))
                         (COND
                           ((NULL PNAMES) NIL)
                           ((= 1 (LENGTH PNAMES))
                            (FORMAT NIL "~A" (GET-PROPERTY SELF (CAR PNAMES))))
                           (T (UNSPLIT-STRING
                               (MAPCAR
                                (LAMBDA (PROP-NAME)
                                  (IF (LET ((NAME (SYMBOL-NAME PROP-NAME)))
                                        (AND (< 4 (LENGTH NAME) )
                                             (STRING-EQUAL
                                              "DOT-"
                                              (SUBSEQ
                                               (SYMBOL-NAME PROP-NAME) 0 4))))
                                      ""
                                      (FORMAT NIL "~A = ~A" PROP-NAME
                                              (GET-PROPERTY SELF PROP-NAME))))
                                PNAMES)
                               (FORMAT NIL "~%")))))
                       (DOT-IDENT (IDENT SELF)))))
    (FORMAT NIL "~A [ style=~A color=~A fillcolor=~A label=\"~A\" ];~%"
            (DOT-IDENT (IDENT SELF)) STYLE COLOR FILLCOLOR LABEL)))




(DEFMETHOD GENERATE-DOT ((SELF EDGE-CLASS))
  "
RETURN: A string containing the dot file data for this edge.
"
  (FORMAT NIL "~A -> ~A ;~%"
          (DOT-IDENT (IDENT (CAR (NODES SELF))))
          (DOT-IDENT (IDENT (CDR (NODES SELF))))))




(DEFMETHOD GENERATE-DOT ((SELF DIRECTED-EDGE-CLASS))
  "
RETURN: A string containing the dot file data for this edge.
"
  (FORMAT NIL "~A -> ~A ;~%"
          (DOT-IDENT (IDENT (FROM SELF)))
          (DOT-IDENT (IDENT (TO SELF)))))



(DEFMETHOD GENERATE-DOT ((SELF WEIGHTED-DIRECTED-EDGE-CLASS))
  "
RETURN: A string containing the dot file data for this edge.
"
  (FORMAT NIL "~A -> ~A [ weight=~D, style=~A, color=~A ];~%"
          (DOT-IDENT (IDENT (FROM SELF)))
          (DOT-IDENT (IDENT (TO   SELF)))
          (WEIGHT SELF)
          (COND
            ((< (WEIGHT SELF) 3)  "dotted")
            ((< (WEIGHT SELF) 10) "dashed")
            ((< (WEIGHT SELF) 15) "solid")
            (T                    "bold"))
          "black"))




;;; (DESCRIPTION (car (element-list (NODES g))))
;;; (MAP-ELEMENTS (NODES g) (lambda (elem) (ident elem)))
;;; (car (element-list (NODES g)))


(DEFMETHOD GENERATE-DOT ((SELF GRAPH-CLASS))
  "
RETURN: A string containing the dot file data for this graph.
NOTE:   dot graphs are directed.
"
  (APPLY 
   (FUNCTION CONCATENATE) 'STRING
   (FLATTEN
    (LIST
     (FORMAT NIL "digraph ~A~%" (OR (GET-PROPERTY SELF :NAME) "Untitled"))
     (FORMAT
         NIL
       (CONCATENATE 'STRING
         "{~%"
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
     (MAP-ELEMENTS (NODES SELF) (LAMBDA (NODE) (GENERATE-DOT NODE)))
     (FORMAT NIL "// common attributes of edges:~%edge [style=solid];~%")
     (MAP-ELEMENTS (EDGES SELF) (LAMBDA (EDGE) (GENERATE-DOT EDGE)))
     (FORMAT NIL "}~%")))))



;;;; graph-dot.lisp                   --                     --          ;;;;
