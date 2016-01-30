;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cons-to-ascii.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages draws ASCII art cons cell diagrams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-19 <PJB> Added PRINT-CONS and PRINT-IDENTIFIED-CONS.
;;;;    2004-09-24 <PJB> Corrected DRAW-LISP.
;;;;    2004-08-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.CONS-TO-ASCII"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE")
  (:export "PRINT-IDENTIFIED-CONSES" "PRINT-CONSES" "DRAW-CELL" "DRAW-LIST")
  (:documentation
   "

This packages draws ASCII art cons cell diagrams.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2004 - 2012
    
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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.CONS-TO-ASCII")




(defgeneric size-cell (pict cell &optional max-width))
(defgeneric draw-cons (pict x y cell))
(defgeneric draw-cell (pict x y cell)
  (:documentation "Draws the CELL in the picture PICT, at coordinates X,Y"))
(defgeneric draw-decorated-cell (pict x y dec))

(defvar +cell-width+ 12 "+---+---+")


(defmethod size-cell ((pict picture) cell &optional (max-width (width pict)))
  (cond
    ((null cell) (values :absent 0 0))
    ((atom cell) (let ((rep (format nil "~S" cell)))
                   (multiple-value-bind (l b w h) (size-string pict rep)
                     (declare (ignore l b))
                     (values :horizontal (+ w 4) (+ h 2)))))
    (t
     ;; first compute horizontal layout
     ;; if too large, then compute a vertical layout (could still be too large).
     (let ((width  0)
           (height 0)
           (dispo))
       (when (<= (length cell) (truncate max-width +cell-width+))
         ;; horizontal
         (setf dispo :horizontal)
         (do  ((items cell (cdr items))
               (i 0 (1+ i)))
              ((null items))
           (multiple-value-bind (d w h) (size-cell pict (car items))
             (declare (ignore d))
             (setf width  (max width  (+ w (* i +cell-width+))))
             (setf height (max height (+ h 5))))))
       (when (<= (width pict) width)
         ;; vertical
         ;; (setf dispo :vertical)
         )
       (values dispo width height))))) ;;SIZE-CELL


(defvar +none+ (cons nil nil))


(defclass decoration ()
  ((w :accessor w :initarg :w :initform 0 :type integer)
   (h :accessor h :initarg :h :initform 0 :type integer)
   (x :accessor x :initarg :x :initform 0 :type integer)
   (y :accessor y :initarg :y :initform 0 :type integer))) ;;DECORATION


(defclass cons-decoration (decoration)
  ((cell :accessor cell-value :initarg :cell :initform +none+ :type cons)
   (car  :accessor car-deco  :initarg :car  :initform nil    
         :type (or null decoration))
   (cdr  :accessor cdr-deco  :initarg :cdr  :initform nil
         :type (or null decoration)))) ;;CONS-DECORATION


(defmethod print-object ((self cons-decoration) stream)
  (print (list :cons-decoration (cell-value self)
               :car (car-deco self) :cdr (cdr-deco self)
               :w (w self) :h (h self) :x (x self) :y (y self)) stream))


(defclass atom-decoration (decoration)
  ((atom :accessor atom-value :initarg :atom :initform  nil    :type atom)))


(defmethod print-object ((self atom-decoration) stream)
  (print (list :atom-decoration (atom-value self)
               :w (w self) :h (h self) :x (x self) :y (y self)) stream))


(defvar +picture-instance+ (make-instance 'picture :width 1 :height 1))
(defvar +nil-decoration+   (make-instance 'atom-decoration))


(defmethod initialize-instance ((self atom-decoration) &rest args)
  (declare (ignore args))
  (call-next-method)
  (multiple-value-bind (d w h) (size-cell +picture-instance+ (atom-value self))
    (declare (ignore d))
    (setf (w self) w (h self) h))
  self) ;;INITIALIZE-INSTANCE

  
(defun decorate (cell)
  "
DOES:    Converts the list CELL to a decorated list.
         The building of the decoration is done by the make-decoration
         function.
RETURN:  The decorated list.
"
  (cond
    ((null  cell) +nil-decoration+)
    ((consp cell)
     (let ((dec (make-instance 'cons-decoration
                  :cell cell
                  :car (decorate (car cell))
                  :cdr (decorate (cdr cell)))))
       ;; Coordinates:
       ;;   #---+---+     
       ;;   $NIL| * |-->      # = (0,0) ;  $ = (0,-1)
       ;;   +---+---+     
       (when (cdr cell)
         ;; let's compute relative coordinates of (cdr cell)
         (setf (x (cdr-deco dec)) 12
               (y (cdr-deco dec)) 0))
       (when (car cell)
         ;; slightly more complex: if width of (car cell) is > 12
         ;; then move it down under the (cdr cell), unless it's null.
         (if (or (null (cdr cell)) (<= (w (car-deco dec)) 12))
             ;; no problem:
             (setf (x (car-deco dec)) 0
                   (y (car-deco dec)) -5)
             (setf (x (car-deco dec)) 0
                   (y (car-deco dec)) (min -5 (- (y (cdr-deco dec))
                                                 (h (cdr-deco dec))
                                                 1)))))
       (setf (w dec) (if (null (cdr cell))
                         (max (+ (x (car-deco dec)) (w (car-deco dec))) 9)
                         (max (+ (x (car-deco dec)) (w (car-deco dec)))
                              (+ (x (cdr-deco dec)) (w (cdr-deco dec)))))
             (h dec) (if (null (car cell))
                         (max (- (h (cdr-deco dec)) (y (cdr-deco dec))) 3)
                         (max (- (h (car-deco dec)) (y (car-deco dec)))
                              (- (h (cdr-deco dec)) (y (cdr-deco dec))))))
       dec))
    (t (make-instance 'atom-decoration :atom cell)))) ;;DECORATE


(defmethod draw-cons ((pict picture) x y cell)
  ;; @---+---+
  ;; | * |NIL|    @ = (0,0)
  ;; +---+---+
  (frame-rect pict    x    (- y 2) 5 3)
  (frame-rect pict (+ x 4) (- y 2) 5 3)
  (draw-string pict (+ x 1) (1- y) (if (car cell) " * " "NIL"))
  (draw-string pict (+ x 5) (1- y) (if (cdr cell) " * " "NIL"))
  pict) ;;DRAW-CONS


(defmethod draw-cell ((pict picture) x y cell)
  (draw-decorated-cell pict x y  (decorate cell)))


(defmethod draw-decorated-cell ((pict picture) x y (dec atom-decoration))
  (let ((rep (format nil "~S" (atom-value dec))))
    (frame-rect pict x (- y (h dec) -1) (w dec) (h dec))
    (draw-string pict (+ x 2) (- y 1) rep)))


(defmethod draw-decorated-cell ((pict picture) x y (dec cons-decoration))
  ;; +---+---+   +---+---+   +---+---+   +---+---+
  ;; | * | * |-->| * | * |-->| * | * |-->| * |NIL|
  ;; +---+---+   +---+---+   +---+---+   +---+---+
  ;;   |           |           |           |
  ;;   V           V           |           V
  ;; +---+       +----+        |         +---+---+   +------+
  ;; | A |       | 42 |        |         |NIL| * |-->| :FIN |
  ;; +---+       +----+        |         +---+---+   +------+
  ;;                           V           
  ;;                         +--------------------+
  ;;                         | "Es una cabronada" |
  ;;                         +--------------------+
  ;;(if (<= (length cell) (truncate (width pict) +cell-width+))
  ;; horizontal
  (draw-cons pict x y (cell-value dec))
  (when (cdr (cell-value dec))
    (draw-arrow pict (+ x 9) (- y 1) 2 0)
    (draw-decorated-cell pict
                         (+ x (x (cdr-deco dec)))
                         (+ y (y (cdr-deco dec)))
                         (cdr-deco dec)))
  (when (car (cell-value dec))
    (draw-arrow pict (+ x 2) (- y 3) 0 (+ (y (car-deco dec)) 4))
    (draw-decorated-cell pict
                         (+ x (x (car-deco dec)))
                         (+ y (y (car-deco dec))) 
                         (car-deco dec)))
  ) ;;DRAW-DECORATED-CELL


(defun draw-list (list &key (title (format nil "~(~S~)" list)))
  "
DO:         Draws the LIST structure.
TITLE:      An alternative title.
RETURN:     A string containing the drawing.
EXAMPLE:    (draw-list '(if (< a b) (decf b a) (decf a b)))
            returns: 
           \"+-----------------------------------------------------------------------+
            | (if (< a b) (decf b a) (decf a b))                                    |
            |                                                                       |
            | +---+---+   +---+---+   +---+---+   +---+---+                         |
            | | * | * |-->| * | * |-->| * | * |-->| * |NIL|                         |
            | +---+---+   +---+---+   +---+---+   +---+---+                         |
            |   |           |           |           |                               |
            |   v           |           |           v                               |
            | +----+        |           |         +---+---+   +---+---+   +---+---+ |
            | | if |        |           |         | * | * |-->| * | * |-->| * |NIL| |
            | +----+        |           |         +---+---+   +---+---+   +---+---+ |
            |               |           |           |           |           |       |
            |               |           |           v           v           v       |
            |               |           |         +------+    +---+       +---+     |
            |               |           |         | decf |    | a |       | b |     |
            |               |           |         +------+    +---+       +---+     |
            |               |           v                                           |
            |               |         +---+---+   +---+---+   +---+---+             |
            |               |         | * | * |-->| * | * |-->| * |NIL|             |
            |               |         +---+---+   +---+---+   +---+---+             |
            |               |           |           |           |                   |
            |               |           v           v           v                   |
            |               |         +------+    +---+       +---+                 |
            |               |         | decf |    | b |       | a |                 |
            |               |         +------+    +---+       +---+                 |
            |               v                                                       |
            |             +---+---+   +---+---+   +---+---+                         |
            |             | * | * |-->| * | * |-->| * |NIL|                         |
            |             +---+---+   +---+---+   +---+---+                         |
            |               |           |           |                               |
            |               v           v           v                               |
            |             +---+       +---+       +---+                             |
            |             | < |       | a |       | b |                             |
            |             +---+       +---+       +---+                             |
            +-----------------------------------------------------------------------+\"
"
  (let* ((dec (decorate list))
         (tw 0)
         (th 0)
         (pic))
    (multiple-value-setq (tw th) (size-string +picture-instance+ title))
    (setf th (abs th))
    (setf pic (make-instance 'picture 
                :width  (+ 4 (max tw (w dec)))
                :height (+ 4 th (h dec))))
    (frame-rect pic 0 0 (width pic) (height pic))
    (when title
      (draw-string pic 2 (- (height pic) 2) title))
    (draw-decorated-cell pic 2 (- (height pic) 4 th) dec)
    pic)) ;;DRAW-LIST


(defun transpose-tree (tree)
  (if (atom tree)
      tree
      (cons (transpose-tree (cdr tree))  (transpose-tree (car tree)))))

#||
(load "~/src/common/common-lisp/picture.lisp")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE")

(SETQ P (MAKE-instance 'PICTURE :width 72 :height 20 :background " "))
(frame-rect p 0 0 72 20)
(draw-string p  30 10 (format nil "Hello~%world~%howdy?"))


(dolist (dir  '(:E :ENE :NE :NNE :N :NNW :NW :WNW :W :WSW :SW :SSW :S :SSE :SE :ESE)) (progn  (SETQ P (frame-rect (MAKE-instance 'PICTURE :width 72 :height 30 :background " ") 0 0 72 30)) (draw-string p (setq x 30) (setq y 15) (setq s (format nil "Hello.~%world~%howdy?")) :direction dir) (multiple-value-bind (l b w h) (size-string p s :direction dir) (frame-rect p (+ x l -1)  (+ y b -1) (+ w 2) (+ h 2)) (draw-string p 1 1 (list :l l :b b :w w :h h)) (draw-string p 1 2 (list :x (+ x l) :y (+ y b) :w  w :h  h)) (draw-line p 3 15 4 0) (fill-rect p 3 8 4 4 :foreground "*") (frame-rect p 1 6 8 8) (frame-rect p 10 8 4 4)(print p))))


(progn
  (SETQ P (frame-rect (MAKE-instance 'PICTURE :width 72 :height 30 :background " ") 0 0 72 30))
  (draw-arrow p 14 4 -10 0)
  (draw-arrow p 15 4 0 -2 :tail "*")
  (draw-arrow p 20 10 10 0 )
  (draw-arrow p 20 10 0 8 :tail "+")
  p)

(decorate '((a b) (42 "hello" (:a b)) #(1 2 3 4 5 6)))
(draw-list '((a b) (42 "hello" (:a b)) #(1 2 3 4 5 6)) "Sample")

(progn
  (SETQ P (frame-rect (MAKE-instance 'PICTURE :width 72 :height 30 :background " ") 0 0 72 30))
  (draw-cell p 2 25 '((a b) (42 "hello" (:a b)) #(1 2 3 4 5 6)))
  p)


(draw-list #1='((a b) (42 "hello" (:a b)) #(1 2 3 4 5 6)) :title (format nil "~S" #1#))


(draw-list #1='(defun fact (n) (if (< 1 n) (* n (fact (1- n))) 1)) :title (format nil "~S" #1#))

(draw-list #1='(defun square (n) (* n n)) :title (format nil "~S" #1#))

||#



(defun print-tree (tree &optional (stream *standard-output*))
  ;; WARNING: doesn't handle circles nor identify EQ subtrees.
  (cond
    ((null tree) (princ "()"))
    ((atom tree) (princ tree stream))
    (t (princ "(" stream)
       (dolist (item (butlast tree))
         (print-tree item stream)
         (princ " " stream))
       (print-tree (car (last tree)) stream)
       (princ ")" stream)))
  tree)


(defun print-conses (tree &optional (stream *standard-output*))
  "
DO:         Print the TREE with all cons cells as dotted pairs.
TREE:       A sexp.
STREAM:     The output stream (default: *STANDARD-OUTPUT*)
WARNING:    doesn't handle circles nor identify EQ subtrees.
EXAMPLE:    (print-conses '(a b c d))
            prints:
            (a  . (b  . (c  . (d  . ()))))
"
  (cond
    ((null tree) (princ "()"))
    ((atom tree)  (princ tree stream) (princ " " stream))
    (t (princ "(" stream) 
       (print-conses (car tree) stream)
       (princ " . " stream)
       (print-conses (cdr tree) stream)
       (princ ")" stream)))
  tree)

#||
[31]> (print-conses '(a b c))
(A  . (B  . (C  . ())))
(A B C)
[32]> (print-conses '((a) (b) (c)))
((A  . ()) . ((B  . ()) . ((C  . ()) . ())))
((A) (B) (C))
||#

(defun find-nodes (tree table)
  (cond
    ((null tree) table)
    ((gethash tree table) (incf (gethash tree table))   table)
    ((atom tree)          (incf (gethash tree table 0)) table)
    (t (incf (gethash tree table 0))
       (find-nodes (cdr tree) (find-nodes (car tree) table)))))


(defun print-identified-conses (tree  &optional (stream *standard-output*))
    "
DO:      Print the TREE with all cons cells identified with a #n= notation.
TREE:    A sexp.
STREAM:  The output stream (default: *STANDARD-OUTPUT*)
NOTE:    Handles circles in the cons structure, but not thru the other
         atoms (vectors, structures, objects).
EXAMPLE: (print-identified-conses '((a . b) #1=(c . d) (e . #1#)))
         prints:
         ((a . b) . (#1=(c . d) . ((e . #1# ) . ())))
"
  (let ((table (find-nodes tree (make-hash-table :test (function eq))))
        (index 0))
    (maphash (lambda (k v)
               (if (= 1 v)
                   (remhash k table)
                   (setf (gethash k table) (- (incf index))))) table)
    (labels ((print-node (node)
               (if (null node)
                   (princ "()")
                   (let ((index (gethash node table)))
                     (if (and index (plusp index))
                         (format stream "#~A# " index)
                         (progn
                           (when index
                             (setf (gethash node table) (- index))
                             (format stream "#~A=" (- index)))
                           (if (atom node)
                               (princ node stream)
                               (progn
                                 (princ "(" stream) 
                                 (print-node (car node))
                                 (princ " . " stream)
                                 (print-node (cdr node))
                                 (princ ")" stream)))))))))
      (print-node tree)
      tree)))

#||
(defparameter tree '#2=(#1=(a) (b c a #1# #2#) . #2#))
(setf *print-circles* t)
(print-identified-conses tree)
#3=(#2=(#1=A . ()) . ((B . (C . (#1#  . (#2#  . (#3#  . ()))))) . #3# ))
#1=(#2=(A) (B C A #2# #1#) . #1#)                                      
||#

;;;; THE END ;;;;
