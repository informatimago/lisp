;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               tree-to-Diagram.lisp
;;;;LANGUAGE:           Common-Lisp 
;;;;SYSTEM:             Common-Lisp 
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This module generates a Diagram text file drawing a tree.
;;;;    The tree drawn is a list whose car is the node displayed, and
;;;;    whose cdr is the list of children.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 199? - 2002
;;;;
;;;;    This file is part of the PJB Common Lisp Library.
;;;;
;;;;    This  program is  free software;  you can  redistribute  it and/or
;;;;    modify it  under the  terms of the  GNU General Public  License as
;;;;    published by the Free Software Foundation; either version 2 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    This program  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU General Public License
;;;;    along with  this program; see the  file COPYING; if  not, write to
;;;;    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-DIAGRAM"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.LIST" "COMMON-LISP")
  (:EXPORT "TREE-GENERATE-RANDOM" "TREE-SIZE" "TREE-DEPTH" "TREE-TO-DIAGRAM")
  (:DOCUMENTATION
   "This package generates a Diagram text file drawing a tree.
    The tree drawn is a list whose car is the node displayed, and
    whose cdr is the list of children.

    Copyright Pascal J. Bourguignon 199? - 2002
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-DIAGRAM")




(DEFUN TREE-DIAGRAM-GENERATE-NODE (N L X Y NODE)
  "PRIVATE.
DOES:    Generates the Diagram text for a node.
n:       the Diagram symbol number (may be used in Diagram links).
l:       the Diagram layer.
x,y:     the coordinates.
node:    the text in the node rectangle.
RETURN:  a list (n' l' x' y') for the following brother node.
"
  (FORMAT T "symbol ~s~%" N)
  (FORMAT T "	layer ~s~%" L)
  (FORMAT T "	shape ~aRectangle~a~%" 
          (CODE-CHAR 34) (CODE-CHAR 34))
  (FORMAT T "	location ~s.00 ~s.00~%" X Y)
  (FORMAT T "	size 126.00 18.00~%")
  (FORMAT T "	framed~%")
  (FORMAT T "	fillColor colorIndex 0~%")
  (FORMAT T "	frameColor colorIndex 1~%")
  (FORMAT T "	shadowColor colorIndex 2~%")
  (FORMAT T "	lineWidth 1.00~%")
  (FORMAT T "	filled~%")
  (FORMAT T "	rtfText {\\rtf0\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\margl40\\margr40\\pard\\tx960\\tx1920\\tx2880\\tx3840\\tx4800\\tx5760\\tx6720\\tx7680\\tx8640\\tx9600\\f0\\b\\i0\\ulnone\\qc\\fs20\\fc0\\cf0 ~s}~%" NODE)
  (FORMAT T "	textPlacement middle~%")
  (FORMAT T "end~%~%")
  (LIST (1+ N) (1+ L) X (+ 27 Y))
  ) ;;TREE-DIAGRAM-GENERATE-NODE


(DEFUN TREE-DIAGRAM-GENERATE-INHERIT (N L X Y)
  "PRIVATE.
DOES:    Generates the Diagram text for a 'inherit' symbol (vertical triangle).
n:       the Diagram symbol number (may be used in Diagram links).
l:       the Diagram layer.
x,y:     the coordinates.
RETURN:  a list (n' l' x' y') for the first child node.
"
  (FORMAT T "symbol ~s~%" N)
  (FORMAT T "	layer ~s~%" L)
  (FORMAT T "	shape ~aVertical Triangle~a~%"
          (CODE-CHAR 34) (CODE-CHAR 34))
  (FORMAT T "	location ~s.00 ~s.00~%" (+ 52 X) (+ 10 Y))
  (FORMAT T "	size 21.00 11.00~%")
  (FORMAT T "	framed~%")
  (FORMAT T "	fillColor colorIndex 0~%")
  (FORMAT T "	frameColor colorIndex 1~%")
  (FORMAT T "	shadowColor colorIndex 2~%")
  (FORMAT T "	lineWidth 1.00~%")
  (FORMAT T "	filled~%")
  (FORMAT T "	textPlacement top~%")
  (FORMAT T "end~%")
  (LIST (1+ N) (1+ L) (+ 81 X) (+ 27 Y))
  ) ;;TREE-DIAGRAM-GENERATE-INHERIT


(DEFUN TREE-DIAGRAM-GENERATE-ADJUST-X (INC)
  "PRIVATE.
inc:     a list (n l x y) corresponding to the after last brother.
RETURN:  a list (n' l' x' y') for the next uncle node.
"
  (LIST (CAR INC) (CADR INC) (- (CADDR INC) 81) (CAR (CDDDR INC)))
  ) ;;TREE-DIAGRAM-GENERATE-ADJUST-X


(DEFUN TREE-DIAGRAM-GENERATE-TREE (N L X Y TREE)
  "PRIVATE.
DOES:    writes to the *standard-output* the Diagram text
         for the subtree 'tree'.
RETURN:  a list (n' l' x' y') for the next brother subtree.
"
  (IF (NULL (CDR TREE))
      (APPLY 'TREE-DIAGRAM-GENERATE-NODE (LIST N L X Y (CAR TREE)))
      (LET ((INC (APPLY 'TREE-DIAGRAM-GENERATE-INHERIT
                        (APPLY 'TREE-DIAGRAM-GENERATE-NODE
                               (LIST N L X Y (CAR TREE))))))
        (DO ((SUBTREES (CDR TREE) (CDR SUBTREES)))	((NULL SUBTREES))
          (SETQ INC (APPLY 'TREE-DIAGRAM-GENERATE-TREE 
                           (APPEND INC (LIST (CAR SUBTREES))))))
        (TREE-DIAGRAM-GENERATE-ADJUST-X INC)))) ;;TREE-DIAGRAM-GENERATE-TREE


(DEFUN TREE-TO-DIAGRAM (TREE)
  "
PRE:    tree is a cons of the node and the list of children.
DOES:   writes to the *standard-output* the Diagram file text.
"
  (TREE-DIAGRAM-GENERATE-TREE 1000 60 45 27 TREE)
  ) ;;TREE-TO-DIAGRAM



(DEFUN TREE-DEPTH (TREE)
  "
PRE:    tree is a cons of the node and the list of children.
RETURN: the depth of the tree.
"
  (IF (NULL TREE)
      0
      (1+ (APPLY 'MAX (CONS 0 (REMOVE NIL (MAPCAR 'TREE-DEPTH (CDR TREE)))))))
  ) ;;TREE-DEPTH



(DEFUN TREE-SIZE (TREE)
  "
PRE:    tree is a cons of the node and the list of children.
RETURN: The size of the tree (number of nodes)
"
  (LOOP WITH COUNT = 0
     FOR ITEM IN TREE
     DO (IF (LISTP ITEM)
            (SETQ COUNT (+ COUNT (TREE-SIZE ITEM)))
            (SETQ COUNT (1+ COUNT)))
     FINALLY (RETURN COUNT))
  ) ;;TREE-SIZE



(DEFUN TREE-GENERATE-RANDOM (DEPTH WIDTH)
  "
RETURN:  A random tree with random number as node, of maximal depth `depth'
         and where each node has a maximum of `width` children.
NOTE:    The result can easily be degenreate (a single node,
         or a much smaller tree).
"
  (IF (>= 1 DEPTH)
      (RANDOM MOST-POSITIVE-FIXNUM)
      (LOOP FOR I FROM 0 BELOW (RANDOM (1+ WIDTH))
         COLLECT (TREE-GENERATE-RANDOM (1- DEPTH) WIDTH) INTO CHILDREN
         FINALLY (RETURN (CONS (RANDOM MOST-POSITIVE-FIXNUM) CHILDREN))))
  ) ;;TREE-GENERATE-RANDOM


;; (insert (tree-to-ascii (generate-random-tree 7 3)))
;;; (let ((tree (generate-random-tree 3 50))
;;;       (*trace-output* (current-buffer)))
;;;   (printf *trace-output* "recursive-count: ")
;;;   (time (loop repeat 10 do (recursive-count tree)))
;;;   (printf *trace-output* "tree-size:   ")
;;;   (time (loop repeat 10 do (tree-size tree)))
;;;   )




;;;; tree-to-diagram.lisp             --                     --          ;;;;
