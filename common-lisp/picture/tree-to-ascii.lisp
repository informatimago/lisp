;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              tree-to-ascii.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            common-lisp
;;;;USER-INTERFACE:    NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.TREE-TO-ASCII"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE")
  (:export "TREE-TO-ASCII-DRAW-TO-PICT" "TREE-DECORATE" "TREE-TO-ASCII")
  (:documentation
   "
This package draws a tree onto an ASCII-ART picture
The tree drawn is a list whose car is the node displayed, and
whose cdr is the list of children.


License:

    AGPL3

    Copyright Pascal J. Bourguignon 2002 - 2012

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.TREE-TO-ASCII")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree (list based)
;; -----------------
;; Each node is a consp whose car is the node value,
;;                  and whose cdr is the list of children.
;;

(defun tree-decorate (tree make-decoration)
  "
DO:      Converts the list based tree to a decorated tree.
         The building of the decoration is done by the make-decoration
         function.
RETURN:  The decorated tree.
"
  (declare (type (function (list list) list) make-decoration))
  (if (consp tree)
      (funcall make-decoration (car tree)
               (loop for sub-tree in (cdr tree)
                  collect (tree-decorate sub-tree make-decoration)
                  into decorated
                  finally (return decorated)))
      (funcall make-decoration tree nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-to-ascci decorated tree
;; ----------------------------
;; Each node is an array of 4 entries:
;;    node, children list, formated string, and box array
;;

(defstruct tree-to-ascii
  (node     nil)
  (children nil)
  (formated nil)
  (box      nil))



(defstruct tree-to-ascii-box
  (width  0)
  (height 0)
  (base   0))



;;; (DEFMACRO TREE-TO-ASCII-NODE         (R)      `(AREF ,R 0))
;;; (DEFMACRO TREE-TO-ASCII-CHILDREN     (R)      `(AREF ,R 1))
;;; (DEFMACRO TREE-TO-ASCII-FORMATED     (R)      `(AREF ,R 2))
;;; (DEFMACRO TREE-TO-ASCII-BOX          (R)      `(AREF ,R 3))

;;;  (DEFMACRO TREE-TO-ASCII-NODE-SET     (R VAL) `(SETF (AREF ,R 0) ,VAL))
;;;  (DEFMACRO TREE-TO-ASCII-CHILDREN-SET (R VAL) `(SETF (AREF ,R 1) ,VAL))
;;;  (DEFMACRO TREE-TO-ASCII-FORMATED-SET (R VAL) `(SETF (AREF ,R 2) ,VAL))
;;;  (DEFMACRO TREE-TO-ASCII-BOX-SET      (R VAL) `(SETF (AREF ,R 3) ,VAL))
;;;  (DEFSETF  TREE-TO-ASCII-NODE         TREE-TO-ASCII-NODE-SET)
;;;  (DEFSETF  TREE-TO-ASCII-CHILDREN     TREE-TO-ASCII-CHILDREN-SET)
;;;  (DEFSETF  TREE-TO-ASCII-FORMATED     TREE-TO-ASCII-FORMATED-SET)
;;;  (DEFSETF  TREE-TO-ASCII-BOX          TREE-TO-ASCII-BOX-SET)



(defun tree-to-ascii-make-decoration (node children)
  "
DO:      Builds a new decorated node: an array of 4 entries:
         node, children list, formated string, and box array
RETURN:  The decorated node.
"
  (make-tree-to-ascii :node node :children children)
  ;;   (LET ((DECORATION (MAKE-ARRAY '(4))))
  ;;     (SETF (TREE-TO-ASCII-NODE     DECORATION) NODE)
  ;;     (SETF (TREE-TO-ASCII-CHILDREN DECORATION) CHILDREN)
  ;;     DECORATION)
  )




;;; (DEFMACRO TREE-TO-ASCII-BOX-WIDTH      (B)      `(AREF ,B 0))
;;; (DEFMACRO TREE-TO-ASCII-BOX-HEIGHT     (B)      `(AREF ,B 1))
;;; (DEFMACRO TREE-TO-ASCII-BOX-BASE       (B)      `(AREF ,B 2))

;;;  (DEFMACRO TREE-TO-ASCII-BOX-WIDTH-SET  (B VAL) `(SETF (AREF ,B 0) ,VAL))
;;;  (DEFMACRO TREE-TO-ASCII-BOX-HEIGHT-SET (B VAL) `(SETF (AREF ,B 1) ,VAL))
;;;  (DEFMACRO TREE-TO-ASCII-BOX-BASE-SET   (B VAL) `(SETF (AREF ,B 2) ,VAL))
;;;  (DEFSETF  TREE-TO-ASCII-BOX-WIDTH      TREE-TO-ASCII-BOX-WIDTH-SET)
;;;  (DEFSETF  TREE-TO-ASCII-BOX-HEIGHT     TREE-TO-ASCII-BOX-HEIGHT-SET)
;;;  (DEFSETF  TREE-TO-ASCII-BOX-BASE       TREE-TO-ASCII-BOX-BASE-SET)



(defun tree-to-ascii-compute-boxes (tree &key boxed format-fun
                                           from-length to-length base)
  "
DO:             Compute the boxes and formated strings and store them in the
                decorated tree.
FROM-LENGTH:    The length of the stem from the child box (default = 2).
TO-LENGTH:      The length of the stem to the parent box (default = 2).
BASE:           (member :top :centered :bottom)
RETURN:         (tree-to-ascii-box tree)


NOTE:                     TO-LENGTH   FROM-LENGTH
                        |<--------->|<----------->|
          ---                                     +---------+
           ^                        +-------------| Child 3 |
           | base                   |             +---------+
           v                        |
          ---  +--------+           |             +---------+
               | Parent |-----------+-------------| Child 2 |
               +--------+           |             +---------+
                                    |
                                    |             +---------+
                                    +-------------| Child 3 |
                                                  +---------+
"
  (check-type format-fun (or null function))
  (let ((children     (tree-to-ascii-children tree))
        (children-box (make-tree-to-ascii-box))
        (node-box     (make-tree-to-ascii-box)))
    (setf (tree-to-ascii-formated tree)
          (if format-fun
              (funcall format-fun (tree-to-ascii-node tree))
              (format nil "~S" (tree-to-ascii-node tree))))
    (if boxed
        (setf (tree-to-ascii-box-width node-box)
              (+ 2 (length (tree-to-ascii-formated tree)))
              (tree-to-ascii-box-height node-box) 3
              (tree-to-ascii-box-base node-box)   1)
        (setf (tree-to-ascii-box-width node-box)
              (length (tree-to-ascii-formated tree))
              (tree-to-ascii-box-height node-box) 1
              (tree-to-ascii-box-base node-box)   0))
    (when children
      (loop
        :for child :in children
        :for child-box := (tree-to-ascii-compute-boxes
                           child  :boxed boxed  :format-fun format-fun
                                  :to-length to-length :from-length from-length
                                  :base base)
        :maximize (tree-to-ascii-box-width child-box)  :into width
        :sum (1+ (tree-to-ascii-box-height child-box)) :into height
        :finally (setf (tree-to-ascii-box-width  children-box) width
                       (tree-to-ascii-box-height children-box) (1- height)
                       (tree-to-ascii-box-base   children-box)
                       (ecase base
                         (:top      (- height 1 (tree-to-ascii-box-height node-box)))
                         (:centered (floor (/ (1- height) 2)))
                         (:bottom   (tree-to-ascii-box-base node-box)))))
      (setf (tree-to-ascii-box-width node-box)
            (+ (tree-to-ascii-box-width node-box)
               ;; TODO: WE COULD USE (MAX TO-LENGTH FROM-LENGTH) WITH 1 CHILD
               (if (cdr children) (+ to-length 1 from-length) from-length)
               (tree-to-ascii-box-width children-box)))
      (setf (tree-to-ascii-box-height node-box)
            (max (tree-to-ascii-box-height node-box)
                 (tree-to-ascii-box-height children-box)))
      (setf (tree-to-ascii-box-base node-box)
            (tree-to-ascii-box-base children-box)))
    (setf (tree-to-ascii-box tree) node-box)
    node-box))




(defun tree-to-ascii-draw-to-pict (tree pict left bottom
                                   &key boxed to-length from-length)
  "
DO:      Draw the decorated TREE into the PICT.
"
  (let ((box       (tree-to-ascii-box      tree))
        (str       (tree-to-ascii-formated tree))
        (children  (reverse (tree-to-ascii-children tree))))
    ;; draw the node:
    (if boxed
        (progn
          (draw-string pict (1+ left) (+ bottom (tree-to-ascii-box-base box))
                       str)
          (frame-rect pict left (+ bottom (tree-to-ascii-box-base box) -1)
                      (+ 2 (length str)) 3)
          (setq left (+ left 2 (length str))))
        (progn
          (draw-string pict left (+ bottom (tree-to-ascii-box-base box)) str)
          (setq left (+ left (length str)))))
    (when children
      ;; draw the childen:
      (if (cdr children)
          (let* ( ;; more than one child
                 (child-box (tree-to-ascii-box (car (last children))))
                 (min       (tree-to-ascii-box-base
                             (tree-to-ascii-box (car children))))
                 (max   (- (+ (tree-to-ascii-box-height box)
                              (tree-to-ascii-box-base child-box))
                           (tree-to-ascii-box-height child-box)))
                 (y       bottom)
                 )
            ;; draw the vertical line:
            (draw-line pict  (+ left to-length) (+ bottom min)
                       0 (1+ (- max min)) :foreground (character "|"))
            ;; draw the stem from the node:
            (draw-line pict  left (+ bottom (tree-to-ascii-box-base box))
                       to-length 0  :foreground (character "-"))
            (draw-point pict (+ left to-length)
                        (+ bottom (tree-to-ascii-box-base box))
                        (character "+"))
            (dolist (child children)
              (setq child-box (tree-to-ascii-box child))
              ;; draw the subnode:
              (tree-to-ascii-draw-to-pict
               child pict (+ left to-length 1 from-length) y
               :boxed boxed :to-length to-length :from-length from-length)
              ;; draw the stem to the subnode:
              (draw-line  pict (+ left to-length 1)
                          (+ y (tree-to-ascii-box-base child-box))
                          from-length 0 :foreground (character "-"))
              (draw-point pict (+ left to-length)
                          (+ y (tree-to-ascii-box-base child-box))
                          (character "+"))
              (setq y (+ y 1 (tree-to-ascii-box-height child-box))) ) ;;dolist
            )  ;;left*
          (progn ;; only one child
            ;; draw the stem from the node:
            ;; TODO: WE COULD USE (MAX TO-LENGTH FROM-LENGTH)
            (draw-line pict  left (+ bottom (tree-to-ascii-box-base box))
                       from-length 0 :foreground (character "-"))
            (tree-to-ascii-draw-to-pict
             (car children) pict (+ left from-length) bottom
             :boxed boxed :to-length to-length :from-length from-length))))))





(defun tree-to-ascii (tlee &key boxed format-fun background
                      to-length from-length (base :centered))
  "
tlee:        is a list-based tree, whose car is a \"node\",
             and whose cdr is the list of children.
boxed:       t if boxes should be drawn around the nodes.
format-fun:  a function taking a node (one of the car's) and
             returning a string to be displayed as the node.
             Defaults merely use (format nil \"~S\" node).
background:  is a character used as background. Default: space.
DO:          Draw the tree onto an ASCII-art picture.
RETURNS:     The string containing the ASCII-ART tree.

EXAMPLE:     (tree-to-ascii '(if (= a b) (decf b a) (decf a b)))
             -->
            \"          +--a
                 +--=--+
                 |     +--b
                 |
                 |        +--b
             if--+--decf--+
                 |        +--a
                 |
                 |        +--a
                 +--decf--+
                          +--b
             \"
"
  (unless from-length (setq from-length 2))
  (unless to-length   (setq to-length   2))
  (unless background  (setq background (character " ")))
  (let* ((tree (tree-decorate tlee (function tree-to-ascii-make-decoration)))
         (box (tree-to-ascii-compute-boxes tree
                                           :boxed boxed
                                           :format-fun format-fun
                                           :to-length  to-length
                                           :from-length from-length
                                           :base base))
         (pict (make-instance 'picture
                 :width (tree-to-ascii-box-width box)
                 :height (tree-to-ascii-box-height box)
                 :background background)))
    (tree-to-ascii-draw-to-pict
     tree pict 0 0 :boxed boxed :to-length to-length :from-length from-length)
    (to-string pict)))

;;;; THE END ;;;;
