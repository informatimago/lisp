;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cons-to-ascii.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages draws ASCIi art cons cell diagrams.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-19 <PJB> Added PRINT-CONS and PRINT-IDENTIFIED-CONS.
;;;;    2004-09-24 <PJB> Corrected DRAW-LISP.
;;;;    2004-08-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CONS-TO-ASCII"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.PICTURE"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING" "COMMON-LISP")
  (:EXPORT "PRINT-IDENTIFIED-CONSES" "PRINT-CONSES" "DRAW-CELL" "DRAW-LIST")
  (:DOCUMENTATION
   "This packages draws ASCII art cons cell diagrams.

    Copyright Pascal J. Bourguignon 2004 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CONS-TO-ASCII")




(DEFGENERIC SIZE-CELL (PICT CELL &OPTIONAL MAX-WIDTH))
(DEFGENERIC DRAW-CONS (PICT X Y CELL))
(DEFGENERIC DRAW-CELL (PICT X Y CELL))
(DEFGENERIC DRAW-DECORATED-CELL (PICT X Y DEC))

(DEFVAR +CELL-WIDTH+ 12 "+---+---+")


(DEFMETHOD SIZE-CELL ((PICT PICTURE) CELL &OPTIONAL (MAX-WIDTH (WIDTH PICT)))
  (COND
    ((NULL CELL) (VALUES :ABSENT 0 0))
    ((ATOM CELL) (LET ((REP (FORMAT NIL "~S" CELL)))
                   (MULTIPLE-VALUE-BIND (L B W H) (SIZE-STRING PICT REP)
                     (DECLARE (IGNORE L B))
                     (VALUES :HORIZONTAL (+ W 4) (+ H 2)))))
    (T
     ;; first compute horizontal layout
     ;; if too large, then compute a vertical layout (could still be too large).
     (LET ((WIDTH  0)
           (HEIGHT 0)
           (DISPO))
       (WHEN (<= (LENGTH CELL) (TRUNCATE MAX-WIDTH +CELL-WIDTH+))
         ;; horizontal
         (SETF DISPO :HORIZONTAL)
         (DO  ((ITEMS CELL (CDR ITEMS))
               (I 0 (1+ I)))
              ((NULL ITEMS))
           (MULTIPLE-VALUE-BIND (D W H) (SIZE-CELL PICT (CAR ITEMS))
             (DECLARE (IGNORE D))
             (SETF WIDTH  (MAX WIDTH  (+ W (* I +CELL-WIDTH+))))
             (SETF HEIGHT (MAX HEIGHT (+ H 5))))))
       (WHEN (<= (WIDTH PICT) WIDTH)
         ;; vertical
         ;; (setf dispo :vertical)
         )
       (VALUES DISPO WIDTH HEIGHT))))) ;;SIZE-CELL


(DEFVAR +NONE+ (CONS NIL NIL))


(DEFCLASS DECORATION ()
  ((W :ACCESSOR W :INITARG :W :INITFORM 0 :TYPE INTEGER)
   (H :ACCESSOR H :INITARG :H :INITFORM 0 :TYPE INTEGER)
   (X :ACCESSOR X :INITARG :X :INITFORM 0 :TYPE INTEGER)
   (Y :ACCESSOR Y :INITARG :Y :INITFORM 0 :TYPE INTEGER))) ;;DECORATION


(DEFCLASS CONS-DECORATION (DECORATION)
  ((CELL :ACCESSOR CELL-VALUE :INITARG :CELL :INITFORM +NONE+ :TYPE CONS)
   (CAR  :ACCESSOR CAR-DECO  :INITARG :CAR  :INITFORM NIL    
         :TYPE (OR NULL DECORATION))
   (CDR  :ACCESSOR CDR-DECO  :INITARG :CDR  :INITFORM NIL
         :TYPE (OR NULL DECORATION)))) ;;CONS-DECORATION


(DEFMETHOD PRINT-OBJECT ((SELF CONS-DECORATION) STREAM)
  (PRINT (LIST :CONS-DECORATION (CELL-VALUE SELF)
               :CAR (CAR-DECO SELF) :CDR (CDR-DECO SELF)
               :W (W SELF) :H (H SELF) :X (X SELF) :Y (Y SELF)) STREAM))


(DEFCLASS ATOM-DECORATION (DECORATION)
  ((ATOM :ACCESSOR ATOM-VALUE :INITARG :ATOM :INITFORM  NIL    :TYPE ATOM)))


(DEFMETHOD PRINT-OBJECT ((SELF ATOM-DECORATION) STREAM)
  (PRINT (LIST :ATOM-DECORATION (ATOM-VALUE SELF)
               :W (W SELF) :H (H SELF) :X (X SELF) :Y (Y SELF)) STREAM))


(DEFVAR +PICTURE-INSTANCE+ (MAKE-INSTANCE 'PICTURE :WIDTH 1 :HEIGHT 1))
(DEFVAR +NIL-DECORATION+   (MAKE-INSTANCE 'ATOM-DECORATION))


(DEFMETHOD INITIALIZE-INSTANCE ((SELF ATOM-DECORATION) &REST ARGS)
  (DECLARE (IGNORE ARGS))
  (CALL-NEXT-METHOD)
  (MULTIPLE-VALUE-BIND (D W H) (SIZE-CELL +PICTURE-INSTANCE+ (ATOM-VALUE SELF))
    (DECLARE (IGNORE D))
    (SETF (W SELF) W (H SELF) H))
  SELF) ;;INITIALIZE-INSTANCE

  
(DEFUN DECORATE (CELL)
  "
DOES:    Converts the list CELL to a decorated list.
         The building of the decoration is done by the make-decoration
         function.
RETURN:  The decorated list.
"
  (COND
    ((NULL  CELL) +NIL-DECORATION+)
    ((CONSP CELL)
     (LET ((DEC (MAKE-INSTANCE 'CONS-DECORATION
                  :CELL CELL
                  :CAR (DECORATE (CAR CELL))
                  :CDR (DECORATE (CDR CELL)))))
       ;; Coordinates:
       ;;   #---+---+     
       ;;   $NIL| * |-->      # = (0,0) ;  $ = (0,-1)
       ;;   +---+---+     
       (WHEN (CDR CELL)
         ;; let's compute relative coordinates of (cdr cell)
         (SETF (X (CDR-DECO DEC)) 12
               (Y (CDR-DECO DEC)) 0))
       (WHEN (CAR CELL)
         ;; slightly more complex: if width of (car cell) is > 12
         ;; then move it down under the (cdr cell), unless it's null.
         (IF (OR (NULL (CDR CELL)) (<= (W (CAR-DECO DEC)) 12))
             ;; no problem:
             (SETF (X (CAR-DECO DEC)) 0
                   (Y (CAR-DECO DEC)) -5)
             (SETF (X (CAR-DECO DEC)) 0
                   (Y (CAR-DECO DEC)) (MIN -5 (- (Y (CDR-DECO DEC))
                                                 (H (CDR-DECO DEC))
                                                 1)))))
       (SETF (W DEC) (IF (NULL (CDR CELL))
                         (MAX (+ (X (CAR-DECO DEC)) (W (CAR-DECO DEC))) 9)
                         (MAX (+ (X (CAR-DECO DEC)) (W (CAR-DECO DEC)))
                              (+ (X (CDR-DECO DEC)) (W (CDR-DECO DEC)))))
             (H DEC) (IF (NULL (CAR CELL))
                         (MAX (- (H (CDR-DECO DEC)) (Y (CDR-DECO DEC))) 3)
                         (MAX (- (H (CAR-DECO DEC)) (Y (CAR-DECO DEC)))
                              (- (H (CDR-DECO DEC)) (Y (CDR-DECO DEC))))))
       DEC))
    (T (MAKE-INSTANCE 'ATOM-DECORATION :ATOM CELL)))) ;;DECORATE


(DEFMETHOD DRAW-CONS ((PICT PICTURE) X Y CELL)
  ;; @---+---+
  ;; | * |NIL|    @ = (0,0)
  ;; +---+---+
  (FRAME-RECT PICT    X    (- Y 2) 5 3)
  (FRAME-RECT PICT (+ X 4) (- Y 2) 5 3)
  (DRAW-STRING PICT (+ X 1) (1- Y) (IF (CAR CELL) " * " "NIL"))
  (DRAW-STRING PICT (+ X 5) (1- Y) (IF (CDR CELL) " * " "NIL"))
  PICT) ;;DRAW-CONS


(DEFMETHOD DRAW-CELL ((PICT PICTURE) X Y CELL)
  (DRAW-DECORATED-CELL PICT X Y  (DECORATE CELL)))


(DEFMETHOD DRAW-DECORATED-CELL ((PICT PICTURE) X Y (DEC ATOM-DECORATION))
  (LET ((REP (FORMAT NIL "~S" (ATOM-VALUE DEC))))
    (FRAME-RECT PICT X (- Y (H DEC) -1) (W DEC) (H DEC))
    (DRAW-STRING PICT (+ X 2) (- Y 1) REP)))


(DEFMETHOD DRAW-DECORATED-CELL ((PICT PICTURE) X Y (DEC CONS-DECORATION))
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
  (DRAW-CONS PICT X Y (CELL-VALUE DEC))
  (WHEN (CDR (CELL-VALUE DEC))
    (DRAW-ARROW PICT (+ X 9) (- Y 1) 2 0)
    (DRAW-DECORATED-CELL PICT
                         (+ X (X (CDR-DECO DEC)))
                         (+ Y (Y (CDR-DECO DEC)))
                         (CDR-DECO DEC)))
  (WHEN (CAR (CELL-VALUE DEC))
    (DRAW-ARROW PICT (+ X 2) (- Y 3) 0 (+ (Y (CAR-DECO DEC)) 4))
    (DRAW-DECORATED-CELL PICT
                         (+ X (X (CAR-DECO DEC)))
                         (+ Y (Y (CAR-DECO DEC))) 
                         (CAR-DECO DEC)))
  ) ;;DRAW-DECORATED-CELL


(DEFUN DRAW-LIST (LIST &KEY (TITLE ""))
  (LET* ((DEC (DECORATE LIST))
         (TW 0)
         (TH 0)
         (PIC))
    (MULTIPLE-VALUE-SETQ (TW TH) (SIZE-STRING +PICTURE-INSTANCE+ TITLE))
    (SETF TH (ABS TH))
    (SETF PIC (MAKE-INSTANCE 'PICTURE 
                :WIDTH  (+ 4 (MAX TW (W DEC)))
                :HEIGHT (+ 4 TH (H DEC))))
    (FRAME-RECT PIC 0 0 (WIDTH PIC) (HEIGHT PIC))
    (WHEN TITLE
      (DRAW-STRING PIC 2 (- (HEIGHT PIC) 2) TITLE))
    (DRAW-DECORATED-CELL PIC 2 (- (HEIGHT PIC) 4 TH) DEC)
    PIC)) ;;DRAW-LIST


(defun transpose-tree (tree)
  (if (atom tree)
      tree
      (cons (transpose-tree (cdr tree))  (transpose-tree (car tree)))))

#||
(load "~/src/common/common-lisp/picture.lisp")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE")

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
  ;; WARNING: doesn't handle circles nor identify EQ subtrees.
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

;;;; cons-to-ascii.lisp               --                     --          ;;;;
