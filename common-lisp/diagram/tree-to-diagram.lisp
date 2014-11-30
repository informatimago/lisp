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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DIAGRAM.TREE-TO-DIAGRAM"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (:export "TREE-GENERATE-RANDOM" "TREE-SIZE" "TREE-DEPTH" "TREE-TO-DIAGRAM")
  (:documentation
   "

This package generates a Diagram! text file drawing a tree.
The tree drawn is a list whose car is the node displayed, and
whose cdr is the list of children.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 1994 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DIAGRAM.TREE-TO-DIAGRAM")




(defun tree-diagram-generate-node (n l x y node)
  "PRIVATE.
DOES:    Generates the Diagram text for a node.
n:       the Diagram symbol number (may be used in Diagram links).
l:       the Diagram layer.
x,y:     the coordinates.
node:    the text in the node rectangle.
RETURN:  a list (n' l' x' y') for the following brother node.
"
  (format t "symbol ~s~%" n)
  (format t "	layer ~s~%" l)
  (format t "	shape ~aRectangle~a~%" 
          (code-char 34) (code-char 34))
  (format t "	location ~s.00 ~s.00~%" x y)
  (format t "	size 126.00 18.00~%")
  (format t "	framed~%")
  (format t "	fillColor colorIndex 0~%")
  (format t "	frameColor colorIndex 1~%")
  (format t "	shadowColor colorIndex 2~%")
  (format t "	lineWidth 1.00~%")
  (format t "	filled~%")
  (format t "	rtfText {\\rtf0\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\margl40\\margr40\\pard\\tx960\\tx1920\\tx2880\\tx3840\\tx4800\\tx5760\\tx6720\\tx7680\\tx8640\\tx9600\\f0\\b\\i0\\ulnone\\qc\\fs20\\fc0\\cf0 ~s}~%" node)
  (format t "	textPlacement middle~%")
  (format t "end~%~%")
  (list (1+ n) (1+ l) x (+ 27 y)))


(defun tree-diagram-generate-inherit (n l x y)
  "PRIVATE.
DOES:    Generates the Diagram text for a 'inherit' symbol (vertical triangle).
n:       the Diagram symbol number (may be used in Diagram links).
l:       the Diagram layer.
x,y:     the coordinates.
RETURN:  a list (n' l' x' y') for the first child node.
"
  (format t "symbol ~s~%" n)
  (format t "	layer ~s~%" l)
  (format t "	shape ~aVertical Triangle~a~%"
          (code-char 34) (code-char 34))
  (format t "	location ~s.00 ~s.00~%" (+ 52 x) (+ 10 y))
  (format t "	size 21.00 11.00~%")
  (format t "	framed~%")
  (format t "	fillColor colorIndex 0~%")
  (format t "	frameColor colorIndex 1~%")
  (format t "	shadowColor colorIndex 2~%")
  (format t "	lineWidth 1.00~%")
  (format t "	filled~%")
  (format t "	textPlacement top~%")
  (format t "end~%")
  (list (1+ n) (1+ l) (+ 81 x) (+ 27 y)))


(defun tree-diagram-generate-adjust-x (inc)
  "PRIVATE.
inc:     a list (n l x y) corresponding to the after last brother.
RETURN:  a list (n' l' x' y') for the next uncle node.
"
  (list (car inc) (cadr inc) (- (caddr inc) 81) (car (cdddr inc))))


(defun tree-diagram-generate-tree (n l x y tree)
  "PRIVATE.
DOES:    writes to the *standard-output* the Diagram text
         for the subtree 'tree'.
RETURN:  a list (n' l' x' y') for the next brother subtree.
"
  (if (null (cdr tree))
      (tree-diagram-generate-node n l x y (car tree))
      (let ((inc (apply (function tree-diagram-generate-inherit)
                        (tree-diagram-generate-node n l x y (car tree)))))
        (do ((subtrees (cdr tree) (cdr subtrees)))	((null subtrees))
          (setq inc (apply (function tree-diagram-generate-tree) 
                           (append inc (list (car subtrees))))))
        (tree-diagram-generate-adjust-x inc))))


(defun tree-to-diagram (tree)
  "
PRE:    tree is a cons of the node and the list of children.
DOES:   writes to the *standard-output* the Diagram file text.
"
  (tree-diagram-generate-tree 1000 60 45 27 tree))


(defun tree-depth (tree)
  "
PRE:    tree is a cons of the node and the list of children.
RETURN: the depth of the tree.
"
  (if (null tree)
      0
      (1+ (reduce (function max) (cdr tree) :key (fnuction tree-depth)
                                            :initial-value 0))))


(defun tree-size (tree)
  "
PRE:    tree is a cons of the node and the list of children.
RETURN: The size of the tree (number of nodes)
"
  (loop with count = 0
     for item in tree
     do (if (listp item)
            (setq count (+ count (tree-size item)))
            (setq count (1+ count)))
     finally (return count)))


(defun tree-generate-random (depth width)
  "
RETURN:  A random tree with random number as node, of maximal depth `depth'
         and where each node has a maximum of `width` children.
NOTE:    The result can easily be degenreate (a single node,
         or a much smaller tree).
"
  (if (>= 1 depth)
      (random most-positive-fixnum)
      (loop for i from 0 below (random (1+ width))
         collect (tree-generate-random (1- depth) width) into children
         finally (return (cons (random most-positive-fixnum) children)))))


;; (insert (tree-to-ascii (generate-random-tree 7 3)))
;;; (let ((tree (generate-random-tree 3 50))
;;;       (*trace-output* (current-buffer)))
;;;   (printf *trace-output* "recursive-count: ")
;;;   (time (loop repeat 10 do (recursive-count tree)))
;;;   (printf *trace-output* "tree-size:   ")
;;;   (time (loop repeat 10 do (tree-size tree)))
;;;   )



;;;; THE END ;;;;
