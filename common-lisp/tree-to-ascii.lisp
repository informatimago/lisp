;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              tree-to-ascii.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            common-lisp
;;;;USER-INTERFACE:    NONE
;;;;DESCRIPTION
;;;;    This modules draws a tree onto an ASCII-ART picture (pict.lisp)
;;;;    The tree drawn is a list whose car is the node displayed, and
;;;;    whose cdr is the list of children.
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-ASCII"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.PICTURE" "COMMON-LISP")
  (:EXPORT "TREE-TO-ASCII-DRAW-TO-PICT" "TREE-DECORATE" "TREE-TO-ASCII")
  (:DOCUMENTATION
   "This package draws a tree onto an ASCII-ART picture
    The tree drawn is a list whose car is the node displayed, and
    whose cdr is the list of children.

    Copyright Pascal J. Bourguignon 2002 - 2002
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.TREE-TO-ASCII")





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree (list based)
;; -----------------
;; Each node is a consp whose car is the node value,
;;                  and whose cdr is the list of children.
;;

(DEFUN TREE-DECORATE (TREE MAKE-DECORATION)
  "
DOES:    Converts the list based tree to a decorated tree.
         The building of the decoration is done by the make-decoration
         function.
RETURN:  The decorated tree.
"
  (DECLARE (TYPE (FUNCTION (LIST LIST) LIST) MAKE-DECORATION))
  (IF (CONSP TREE)
      (FUNCALL MAKE-DECORATION (CAR TREE)
               (LOOP FOR SUB-TREE IN (CDR TREE)
                  COLLECT (TREE-DECORATE SUB-TREE MAKE-DECORATION)
                  INTO DECORATED
                  FINALLY (RETURN DECORATED)))
      (FUNCALL MAKE-DECORATION TREE NIL)))




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



(DEFUN TREE-TO-ASCII-MAKE-DECORATION (NODE CHILDREN)
  "
DOES:    Builds a new decorated node: an array of 4 entries:
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



(DEFUN TREE-TO-ASCII-COMPUTE-BOXES (TREE &KEY BOXED FORMAT-FUN
                                    FROM-LENGTH TO-LENGTH)
  "
DOES:           Compute the boxes and formated strings and store them in the
                decorated tree.
FROM-LENGTH:    The length of the stem from the child box (default = 2).
TO-LENGTH:      The length of the stem to the parent box (default = 2).
RETURN:         (tree-to-ascii-box tree)


NOTE:                     TO-LENGTH   FROM-LENGTH
                        |<--------->|<----------->|
                                                  +---------+
                                    +-------------| Child 3 |
                                    |             +---------+
                                    |
               +--------+           |             +---------+
               | Parent |-----------+-------------| Child 2 |
               +--------+           |             +---------+
                                    |
                                    |             +---------+
                                    +-------------| Child 3 |
                                                  +---------+
"
  (DECLARE (TYPE (OR NULL (FUNCTION (CONS) STRING)) FORMAT-FUN))
  (LET ((CHILDREN     (TREE-TO-ASCII-CHILDREN TREE))
        (CHILDREN-BOX (make-tree-to-ascii-box))
        (NODE-BOX     (make-tree-to-ascii-box)) )
    (SETF (TREE-TO-ASCII-FORMATED TREE)
          (IF FORMAT-FUN
              (FUNCALL FORMAT-FUN (TREE-TO-ASCII-NODE TREE))
              (FORMAT NIL "~S" (TREE-TO-ASCII-NODE TREE))))
    (IF BOXED
        (PROGN
          (SETF (TREE-TO-ASCII-BOX-WIDTH NODE-BOX)
                (+ 2 (LENGTH (TREE-TO-ASCII-FORMATED TREE))))
          (SETF (TREE-TO-ASCII-BOX-HEIGHT NODE-BOX) 3)
          (SETF (TREE-TO-ASCII-BOX-BASE NODE-BOX)   1) )
        (PROGN
          (SETF (TREE-TO-ASCII-BOX-WIDTH NODE-BOX)
                (LENGTH (TREE-TO-ASCII-FORMATED TREE)))
          (SETF (TREE-TO-ASCII-BOX-HEIGHT NODE-BOX) 1)
          (SETF (TREE-TO-ASCII-BOX-BASE NODE-BOX)   0) ))
    (WHEN CHILDREN
      (LOOP FOR CHILD IN CHILDREN
         FOR CHILD-BOX = (TREE-TO-ASCII-COMPUTE-BOXES
                          CHILD  :BOXED BOXED  :FORMAT-FUN FORMAT-FUN
                          :TO-LENGTH TO-LENGTH :FROM-LENGTH FROM-LENGTH)
         MAXIMIZE (TREE-TO-ASCII-BOX-WIDTH CHILD-BOX)  INTO WIDTH
         SUM (1+ (TREE-TO-ASCII-BOX-HEIGHT CHILD-BOX)) INTO HEIGHT
         FINALLY
         (SETF (TREE-TO-ASCII-BOX-WIDTH  CHILDREN-BOX) WIDTH)
         (SETF (TREE-TO-ASCII-BOX-HEIGHT CHILDREN-BOX) (1- HEIGHT))
         (SETF (TREE-TO-ASCII-BOX-BASE   CHILDREN-BOX)
               (FLOOR (/ (1- HEIGHT) 2)))
         ) ;;loop
      (SETF (TREE-TO-ASCII-BOX-WIDTH NODE-BOX)
            (+ (TREE-TO-ASCII-BOX-WIDTH NODE-BOX)
               ;; TODO: WE COULD USE (MAX TO-LENGTH FROM-LENGTH) WITH 1 CHILD
               (IF (CDR CHILDREN) (+ TO-LENGTH 1 FROM-LENGTH) FROM-LENGTH)
               (TREE-TO-ASCII-BOX-WIDTH CHILDREN-BOX)))
      (SETF (TREE-TO-ASCII-BOX-HEIGHT NODE-BOX)
            (MAX (TREE-TO-ASCII-BOX-HEIGHT NODE-BOX)
                 (TREE-TO-ASCII-BOX-HEIGHT CHILDREN-BOX)))
      (SETF (TREE-TO-ASCII-BOX-BASE NODE-BOX)
            (TREE-TO-ASCII-BOX-BASE CHILDREN-BOX))
      ) ;;when
    (SETF (TREE-TO-ASCII-BOX TREE) NODE-BOX)
    NODE-BOX))




(DEFUN TREE-TO-ASCII-DRAW-TO-PICT (TREE PICT LEFT BOTTOM
                                   &KEY BOXED TO-LENGTH FROM-LENGTH)
  "
DOES:    Draw the decorated TREE into the PICT.
"
  (LET ((BOX (TREE-TO-ASCII-BOX      TREE))
        (STR (TREE-TO-ASCII-FORMATED TREE))
        (CHILDREN  (REVERSE (TREE-TO-ASCII-CHILDREN TREE)))
        )
    ;; draw the node:
    (IF BOXED
        (PROGN
          (DRAW-STRING PICT (1+ LEFT) (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX)) 
                       STR)
          (FRAME-RECT PICT LEFT (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX) -1)
                      (+ 2 (LENGTH STR)) 3)
          (SETQ LEFT (+ LEFT 2 (LENGTH STR))))
        (PROGN
          (DRAW-STRING PICT LEFT (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX)) STR)
          (SETQ LEFT (+ LEFT (LENGTH STR)))))
    (WHEN CHILDREN
      ;; draw the childen:
      (IF (CDR CHILDREN)
          (LET* ( ;; more than one child
                 (CHILD-BOX (TREE-TO-ASCII-BOX (CAR (LAST CHILDREN))))
                 (MIN       (TREE-TO-ASCII-BOX-BASE
                             (TREE-TO-ASCII-BOX (CAR CHILDREN))))
                 (MAX   (- (+ (TREE-TO-ASCII-BOX-HEIGHT BOX)
                              (TREE-TO-ASCII-BOX-BASE CHILD-BOX))
                           (TREE-TO-ASCII-BOX-HEIGHT CHILD-BOX)))
                 (Y       BOTTOM)
                 )
            ;; draw the vertical line:
            (DRAW-LINE PICT  (+ LEFT TO-LENGTH) (+ BOTTOM MIN)
                       0 (1+ (- MAX MIN)) :foreground (CHARACTER "|"))
            ;; draw the stem from the node:
            (DRAW-LINE PICT  LEFT (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX))
                       TO-LENGTH 0  :foreground (CHARACTER "-"))
            (DRAW-POINT PICT (+ LEFT TO-LENGTH)
                        (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX))
                        (CHARACTER "+"))
            (DOLIST (CHILD CHILDREN)
              (SETQ CHILD-BOX (TREE-TO-ASCII-BOX CHILD))
              ;; draw the subnode:
              (TREE-TO-ASCII-DRAW-TO-PICT
               CHILD PICT (+ LEFT TO-LENGTH 1 FROM-LENGTH) Y
               :BOXED BOXED :TO-LENGTH TO-LENGTH :FROM-LENGTH FROM-LENGTH)
              ;; draw the stem to the subnode:
              (DRAW-LINE  PICT (+ LEFT TO-LENGTH 1)
                          (+ Y (TREE-TO-ASCII-BOX-BASE CHILD-BOX))
                          FROM-LENGTH 0 :foreground (CHARACTER "-"))
              (DRAW-POINT PICT (+ LEFT TO-LENGTH)
                          (+ Y (TREE-TO-ASCII-BOX-BASE CHILD-BOX))
                          (CHARACTER "+"))
              (SETQ Y (+ Y 1 (TREE-TO-ASCII-BOX-HEIGHT CHILD-BOX))) ) ;;dolist
            )  ;;left*
          (PROGN ;; only one child
            ;; draw the stem from the node:
            ;; TODO: WE COULD USE (MAX TO-LENGTH FROM-LENGTH)
            (DRAW-LINE PICT  LEFT (+ BOTTOM (TREE-TO-ASCII-BOX-BASE BOX))
                       FROM-LENGTH 0 :foreground (CHARACTER "-"))
            (TREE-TO-ASCII-DRAW-TO-PICT
             (CAR CHILDREN) PICT (+ LEFT FROM-LENGTH) BOTTOM
             :BOXED BOXED :TO-LENGTH TO-LENGTH :FROM-LENGTH FROM-LENGTH))))))





(DEFUN TREE-TO-ASCII (TLEE &KEY BOXED FORMAT-FUN BACKGROUND
                      TO-LENGTH FROM-LENGTH)
  "
tlee:        is a list-based tree, whose car is a \"node\",
             and whose cdr is the list of children.
boxed:       t if boxes should be drawn around the nodes.
format-fun:  a function taking a node (one of the car's) and
             returning a string to be displayed as the node.
             Defaults merely use (format nil \"~S\" node).
background:  is a character used as background. Default: space.
DOES:        Draw the tree onto an ASCII-art picture.
RETURNS:     The string containing the ASCII-ART tree.
"
  (UNLESS FROM-LENGTH (SETQ FROM-LENGTH 2))
  (UNLESS TO-LENGTH   (SETQ TO-LENGTH   2))
  (UNLESS BACKGROUND  (SETQ BACKGROUND (CHARACTER " ")))
  (LET* ((TREE (TREE-DECORATE TLEE (FUNCTION TREE-TO-ASCII-MAKE-DECORATION)))
         (BOX (TREE-TO-ASCII-COMPUTE-BOXES TREE
                                           :BOXED BOXED
                                           :FORMAT-FUN FORMAT-FUN
                                           :TO-LENGTH  TO-LENGTH
                                           :FROM-LENGTH FROM-LENGTH))
         (PICT (MAKE-instance 'PICTURE
                 :width (tree-to-ascii-box-width box) 
                 :height (tree-to-ascii-box-height box)
                 :background BACKGROUND))
         )
    (TREE-TO-ASCII-DRAW-TO-PICT
     TREE PICT 0 0 :BOXED BOXED :TO-LENGTH TO-LENGTH :FROM-LENGTH FROM-LENGTH)
    (to-string pict)))


;;;; tree-to-ascii.lisp               --                     --          ;;;;
