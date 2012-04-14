;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cube.lisp
;;;;LANGUAGE:           Common-Lisp 
;;;;SYSTEM:             clisp
;;;;USER-INTERFACE:     clisp
;;;;DESCRIPTION
;;;;
;;;;    This program tries to resolve the Cube Puzzle, where a cube
;;;;    composed of 27 smaller cubes linked with a thread  must be
;;;;    recomposed.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2004-01-25 <PJB> Removed import from CLOS (everything is in COMMON-LISP).
;;;;    1995-??-?? <PJB> Creation.
;;;;BUGS
;;;;    Does not solve it yet.
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 1995 - 2004
;;;;
;;;;    This file is part of the Cube Puzzle program.
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

(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CUBE"
  (:DOCUMENTATION
   "This program tries to resolve the Cube Puzzle, where a cube
    composed of 27 smaller cubes linked with a thread  must be
    recomposed.
    
    Copyright Pascal Bourguignon 1995 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:USE "COMMON-LISP")
  (:EXPORT MAKE-CUBE-LIST
           CUBE
           SET-NUMBER SET-COORDINATE INPUT-VECTOR OUTPUT-VECTOR
           COLLIDE ROLL SOLVE ADD-OUTPUT-CUBE-TO-SIDE
           SET-INPUT-CUBE-TO-SIDE BOUNDS REVERSE-CUBES  )
  );;COM.INFORMATIMAGO.COMMON-LISP.CUBE
;;(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CUBE")


;; 3 2 3 2 3 2 3 3 2 3 3 3 3 
;;#
;;#
;;##
;; ###
;;   ###
;;     ##
;;      #
;;      ###
;;        ##
;;         ###
;;           #
;;           ###
;;             #
;;             #

;;###
;;  ##
;;   #
;;   ##
;;    #
;;    ##
;;     #
;;     ###
;;       ##
;;        ##
;;         #
;;         ###
;;           #
;;           ###



;;                      ^ z
;;                      |
;;               +------|----+
;;              /       |   /|
;;             /        |  / |    ^
;;            /      4  | /  |   / y
;;           /          |/   |  /
;;          +-----------+    | /
;;          |           | 6  |/
;;          |           |    +
;;          |           |   /
;;          |     2     |  /
;;          |           | /
;;          |           |/
;;          +-----------+------------> x


(defun v (x y z)
  (make-array '(3) :element-type number :initial-contents (list x y z)))


(defun o (a b c d e f g h i)
  (make-array '(3) :initial-contents (list (v a b c) (v d e f) (v g h i))))


(defun ov (c1 c2 c3)
  (make-array '(3) :initial-contents (list c1 c2 c3)))


(defmacro oref (o i j) `(aref (aref ,o ,i) ,j))


(defun v+ (&rest args)
  (let ((x 0)(y 0)(z 0))
    (dolist (arg args)
      (incf x (aref arg 0))
      (incf y (aref arg 1))
      (incf z (aref arg 2)))
    (v x y z)));;v+


(defun v- (arg1 &rest args)
  (if (null args)
    (v (- (aref arg1 0)) (- (aref arg1 1)) (- (aref arg1 2)))
    (let ((x (aref arg1 0)) (y (aref arg1 1)) (z (aref arg1 2)))
      (dolist (arg args)
        (decf x (aref arg 0))
        (decf y (aref arg 1))
        (decf z (aref arg 2)))
      (v x y z))));;v-
      

(defun o- (arg1 &rest args)
  (if (null args)
    (ov (v- (aref arg1 0)) (v- (aref arg1 1)) (v- (aref arg1 2)))
    (let ((a (oref arg1 0 0))(b (oref arg1 0 1))(c (oref arg1 0 2))
          (d (oref arg1 1 0))(e (oref arg1 1 1))(f (oref arg1 1 2))
          (g (oref arg1 2 0))(h (oref arg1 2 1))(i (oref arg1 2 2)))
      (dolist (arg args)
        (decf a (oref arg 0 0))
        (decf b (oref arg 0 1))
        (decf c (oref arg 0 2))
        (decf d (oref arg 1 0))
        (decf e (oref arg 1 1))
        (decf f (oref arg 1 2))
        (decf g (oref arg 2 0))
        (decf h (oref arg 2 1))
        (decf i (oref arg 2 2)))
      (o a b c d e f g h i))));;o-


(DEFUN o*v (OPER VECT)
  "
    ((a b c) (d e f) (g h i)) (x y z)
    (ax+dy+gz bx+ey+hz cx+fy+iz)
"
  (let ((x (aref vect 0))(y (aref vect 1))(z (aref vect 2)))
    (v (+ (* x (oref oper 0 0))
          (* y (oref oper 1 0))
          (* z (oref oper 2 0)))
       (+ (* x (oref oper 0 1))
          (* y (oref oper 1 1))
          (* z (oref oper 2 1)))
       (+ (* x (oref oper 0 2))
          (* y (oref oper 1 2))
          (* z (oref oper 2 2))))));;o*v




(DEFVAR origin #(0 0 0))
(DEFVAR X-AXIS #(1 0 0))
(DEFVAR Y-AXIS #(0 1 0))
(DEFVAR Z-AXIS #(0 0 1))
(DEFVAR -X-AXIS #(-1 0 0))
(DEFVAR -Y-AXIS #(0 -1 0))
(DEFVAR -Z-AXIS #(0 0 -1))
(DEFVAR X-AXIS-QUARTER-TURN #(#(1 0 0) #(0 0 1) #(0 -1 0))) ; x y z --> x z -y
(DEFVAR Y-AXIS-QUARTER-TURN #(#(0 0 -1) #(0 1 0) #(1 0 0))) ; x y z --> -z y x
(DEFVAR Z-AXIS-QUARTER-TURN #(#(0 1 0) #(-1 0 0) #(0 0 1))) ; x y z --> y -x z
(DEFVAR -X-AXIS-QUARTER-TURN #(#(-1 0 0) #(0 0 -1) #(0 1 0)))
(DEFVAR -Y-AXIS-QUARTER-TURN #(#(0 0 1) #(0 -1 0) #(-1 0 0)))
(DEFVAR -Z-AXIS-QUARTER-TURN #(#(0 -1 0) #(1 0 0) #(0 0 -1))
(DEFVAR identity #(#(1 0 0) #(0 1 0) #(0 0 1))) ; also the base.


(DEFUN QUARTER-TURN (VECT)
  (COND
   ((EQUAL VECT X-AXIS) X-AXIS-QUARTER-TURN)
   ((EQUAL VECT Y-AXIS) Y-AXIS-QUARTER-TURN)
   ((EQUAL VECT Z-AXIS) Z-AXIS-QUARTER-TURN)
   ((EQUAL VECT -X-AXIS) -X-AXIS-QUARTER-TURN)
   ((EQUAL VECT -Y-AXIS) -Y-AXIS-QUARTER-TURN)
   ((EQUAL VECT -Z-AXIS) -Z-AXIS-QUARTER-TURN)
   (T (ERROR "quarter-turn: general case not implemented~% vect must be a base vector or opposite thereof~%"))));;QUARTER-TURN




(DEFUN CHECK-OPERATOR (OPERATOR ARGUMENT EXPECTED)
  (FORMAT T "[~s]~a = ~a =? ~a (~a)~%"
          OPERATOR ARGUMENT
          (o*v OPERATOR ARGUMENT) EXPECTED
          (EQUAL (o*v OPERATOR ARGUMENT) EXPECTED)));;CHECK-OPERATOR
	

(DEFUN CHECK ()
  (CHECK-OPERATOR X-AXIS-QUARTER-TURN X-AXIS X-AXIS)
  (CHECK-OPERATOR X-AXIS-QUARTER-TURN Y-AXIS Z-AXIS)
  (CHECK-OPERATOR X-AXIS-QUARTER-TURN Z-AXIS (v- Y-AXIS))
  (CHECK-OPERATOR Y-AXIS-QUARTER-TURN X-AXIS (v- Z-AXIS))
  (CHECK-OPERATOR Y-AXIS-QUARTER-TURN Y-AXIS Y-AXIS)
  (CHECK-OPERATOR Y-AXIS-QUARTER-TURN Z-AXIS X-AXIS)
  (CHECK-OPERATOR Z-AXIS-QUARTER-TURN X-AXIS Y-AXIS)
  (CHECK-OPERATOR Z-AXIS-QUARTER-TURN Y-AXIS (v- X-AXIS))
  (CHECK-OPERATOR Z-AXIS-QUARTER-TURN Z-AXIS Z-AXIS)
  );;CHECK
 




;; A box is list with (car box) containing the left-bottom-far most 
;; place and (cdr box) containing the right-top-near most place of the 
;; box. Each is a list of three coordinate (x y z).
;; Sides of the box are parallel to the base planes.


(defun make-box (lbf rtn) (cons lbf rtn))
(defmacro box-lbf (box) `(car ,box))
(defmacro box-rtn (box) `(cdr ,box))


(DEFUN BOX-SIZE   (BOX)
  (let ((d (v- (box-lbf BOX) (box-rtn BOX))))
    (abs (* (aref d 0) (aref d 1) (aref d 2)))))


(DEFUN BOX-EXPAND (BOX POS)
  (LET ((LBF (box-lbf BOX)) (RTN (box-rtn BOX)) )
    (make-box (v (MIN (aref POS 0) (aref LBF 0))
                 (MIN (aref POS 1) (aref LBF 1))
                 (MIN (aref POS 2) (aref LBF 2 )))
              (v (MAX (aref POS 0) (aref RTN 0))
                 (MAX (aref POS 1) (aref RTN 1))
                 (MAX (aref POS 2) (aref RTN 2))))))


(defun check-box-expand ()
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   0   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   0   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   0   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v -1  0   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0  -1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   0  -1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   0   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   0   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v -1  0   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1  -1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   0  -1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   1   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v -1  1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0  -1   0) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   1  -1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 1   0   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   1   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   0   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v -1  0   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0  -1   1) ))
  (PRINT (BOX-EXPAND (make-box origin origin) (v 0   0  -1) ))
  )



;;----------------------------------------------------------------------

;; orientation = tri-vecteur ((1 0 0) (0 1 0) (0 0 1))
;; axe         = vecteur     (1 0 0)
;;
;;


(DEFCLASS CUBE ()
  (
   ;;Invariants:
   ;; coordinate    = input-cube.coordinate+input-cube.outputVector
   ;; orientation = rotation(input-cube.axe,input-cube.orientation)
   (INDEX        :ACCESSOR INDEX         :INITFORM 0)
   (COORDINATE   :ACCESSOR COORDINATE    :INITFORM '(0 0 0))
   (ORIENTATION  :ACCESSOR ORIENTATION   :INITFORM BASIS)
   (INPUT-SIDE   :ACCESSOR INPUT-SIDE    :INITFORM 0)
   (INPUT-CUBE   :ACCESSOR INPUT-CUBE    :INITFORM '())
   (OUTPUT-SIDE  :ACCESSOR OUTPUT-SIDE   :INITFORM 0)
   (OUTPUT-CUBE  :ACCESSOR OUTPUT-CUBE   :INITFORM '())
   )
  );;CUBE


;; use the following line to update the class summary, but skip the first
;; semicolon.
;; egrep 'defclass|defmethod' $file |sed -e 's/(defclass \(.*\)/	(format t "class \1~%")/' -e 's/(defmethod\(.*\)/    (format t "\1~%")/' -e 's/;/~%  /g'|grep -v egrep


(DEFMETHOD SET-INDEX           ((SELF CUBE) INDEX)
  (SETF (INDEX SELF) INDEX)
  (IF (NULL (OUTPUT-CUBE SELF))
    INDEX
    (SET-INDEX (OUTPUT-CUBE SELF) (1+ INDEX))));;SET-INDEX


(DEFMETHOD SET-COORDINATE       ((SELF CUBE) NEWCOORDINATE)
  (SETF (COORDINATE SELF) NEWCOORDINATE)
  (IF (NULL (OUTPUT-CUBE SELF))
    NEWCOORDINATE
    (SET-COORDINATE (OUTPUT-CUBE SELF) 
                    (ADD-VECTOR NEWCOORDINATE (OUTPUT-VECTOR SELF)))));;SET-COORDINATE
				

(DEFMETHOD INPUT-VECTOR         ((SELF CUBE))
  (IF (= 0 (INPUT-SIDE SELF))
    '(0 0 0)
    (OPPOSITE-VECTOR (OUTPUT-VECTOR (INPUT-CUBE SELF)))));;INPUT-VECTOR


(DEFMETHOD OUTPUT-VECTOR        ((SELF CUBE))
  (COND
   ((= 0 (OUTPUT-SIDE SELF)) '(0 0 0))
   ((= 1 (OUTPUT-SIDE SELF)) 
    (OPPOSITE-VECTOR (FIRST (ORIENTATION SELF))))
   ((= 2 (OUTPUT-SIDE SELF)) 
    (OPPOSITE-VECTOR (SECOND (ORIENTATION SELF))))
   ((= 3 (OUTPUT-SIDE SELF)) 
    (OPPOSITE-VECTOR (THIRD (ORIENTATION SELF))))
   ((= 4 (OUTPUT-SIDE SELF)) 
    (THIRD (ORIENTATION SELF)))
   ((= 5 (OUTPUT-SIDE SELF)) 
    (SECOND (ORIENTATION SELF)))
   ((= 6 (OUTPUT-SIDE SELF)) 
    (FIRST (ORIENTATION SELF)))
   (T (ERROR "Invalid output-side (~a) for ~a~%" 
             (OUTPUT-SIDE SELF) SELF)
      '(0 0 0))));;OUTPUT-VECTOR


(DEFMETHOD COLLIDE  ((SELF CUBE) OTHERCOORD)
  (COND
   ((NULL SELF) NIL)
   ((EQUAL (COORDINATE SELF) OTHERCOORD) T)
   ((NULL (INPUT-CUBE SELF)) NIL)
   (T (COLLIDE (INPUT-CUBE SELF) OTHERCOORD)))
  );;COLLIDE


(DEFMETHOD ROLL     ((SELF CUBE))
  (SETF (ORIENTATION SELF) 
		(MAPCAR
         (LAMBDA (V) 
           (APPLY-OPERATOR (QUARTER-TURN (OUTPUT-VECTOR (INPUT-CUBE SELF))) V))
         (ORIENTATION SELF)))
  (SET-COORDINATE SELF (COORDINATE SELF))
  );;ROLL


(DEFMETHOD SOLVE   ((SELF CUBE) TRY) ;; try in [0..3+1]
  (FORMAT T "--> ~a~%" (MAPCAR 'COORDINATE (INPUT-CUBE SELF)))
  (COND
   ((NULL SELF)    T)
   ((> TRY 3)      (BLOCK T (ROLL SELF) NIL))
   ((AND (INPUT-CUBE SELF) (OR 
                            (> (APPLY 'MAX (BOX-SIZE (BOUNDS SELF))) 3) 
                            (COLLIDE (INPUT-CUBE SELF) (COORDINATE SELF))))
    (ROLL SELF)
    (SOLVE SELF (1+ TRY)))
   ((OUTPUT-CUBE SELF)
    (IF (SOLVE (OUTPUT-CUBE SELF) 0)
      T
      (BLOCK T
        (ROLL SELF)
        (SOLVE SELF (1+ TRY)))))
   (T T)
   ));;SOLVE


(DEFMETHOD ADD-OUTPUT-CUBE-TO-SIDE ((SELF CUBE) (NEW-OUTPUT CUBE) SIDE)
  (SETF (OUTPUT-CUBE SELF) NEW-OUTPUT)
  (SETF (OUTPUT-SIDE SELF) SIDE)
  (SETF (ORIENTATION SELF) (ORIENTATION NEW-OUTPUT))
  (SET-INPUT-CUBE-TO-SIDE NEW-OUTPUT SELF (- 7 SIDE))
  (SETF (INDEX SELF)     (1- (INDEX NEW-OUTPUT)))
  (SETF (COORDINATE SELF)
        (ADD-VECTOR (COORDINATE NEW-OUTPUT)
                    (OPPOSITE-VECTOR (OUTPUT-VECTOR SELF))))
  );;ADD-OUTPUT-CUBE-TO-SIDE


(DEFMETHOD SET-INPUT-CUBE-TO-SIDE  ((SELF CUBE) (NEW-INPUT CUBE) SIDE)
  (SETF (INPUT-CUBE SELF) NEW-INPUT)
  (SETF (INPUT-SIDE SELF) SIDE));;SET-INPUT-CUBE-TO-SIDE


(DEFMETHOD BOUNDS              ((SELF CUBE)) ; returns a box.
  (IF (NULL (INPUT-CUBE SELF))
    (CONS (COORDINATE SELF) (COORDINATE SELF))
    (BOX-EXPAND (BOUNDS (INPUT-CUBE SELF)) (COORDINATE SELF))));;BOUNDS


(DEFMETHOD REVERSE-CUBES    ((SELF CUBE)) ; reverse the cube list.
  (LET ((C (INPUT-CUBE SELF)) (S (INPUT-SIDE SELF)))
    (SETF (INPUT-CUBE SELF) (OUTPUT-CUBE SELF))
    (SETF (INPUT-SIDE SELF) (OUTPUT-SIDE SELF))
    (SETF (OUTPUT-CUBE SELF) C)
    (SETF (OUTPUT-SIDE SELF) S)
	)
  (REVERSE-CUBES (INPUT-CUBE SELF)));;REVERSE-CUBES



(DEFUN MAKE-CUBE-LIST (L)
  (LET ((CURRENT ()))
    (MAPcar (LAMBDA (SIDE)
            (LET ((NEWCUBE    (MAKE-INSTANCE 'CUBE)))
              (IF (= 0 SIDE)
                (SETQ CURRENT NEWCUBE)
                (BLOCK T
                  (ADD-OUTPUT-CUBE-TO-SIDE NEWCUBE CURRENT SIDE)
                  (SETQ CURRENT NEWCUBE)))))
          L)));;MAKE-CUBE-LIST



;;(setq cubeList (reverse 
;;	(make-cube-list '(0 6 6 2 2 6 6 2 2 6 2 6 2 6 6 2 2 6 2 2 6 2 2 6 2 6 6))))
;;; (SETQ CUBELIST (REVERSE (MAKE-CUBE-LIST (REVERSE '(6 6 2 2 6 6 2 2 6 2 6 2 6 6 2 2 6 2 2 6 2 2 6 2 6 6 0)))))
;;; (SET-INDEX (CAR CUBELIST) 1)
;;; (SET-COORDINATE (CAR CUBELIST) '(0 0 0))



;;(setq box (bounds (fourth cubeList)))
;;(mapcar 'coordinate cubeList)
;;(mapcar 'bounds cubeList)
;;(mapcar (lambda (cube) (box-size (bounds cube)))cubeList)
;;(mapcar (lambda (cube) (apply 'max (box-size (bounds cube)))) cubeList)
;;(mapcar (lambda (cube) (apply 'max (box-size (bounds (output-cube cube))))) (butlast cubeList))
;;(max (box-size (bounds (output-cube self))))
;;(mapcar 'output-vector cubeList)	
;;(mapcar 'input-vector cubeList)	
;;(list (equal x-axis '(1 0 0))
;;(equal y-axis '(0 1 0))
;;(equal z-axis '(0 0 1)))


(defun test-solve ()
  (let ((CUBELIST (REVERSE (MAKE-CUBE-LIST (REVERSE '(6 6 2 2 6 6 2 2 6 2 6 2 6 6 2 2 6 2 2 6 2 2 6 2 6 6 0))))))
    (SOLVE (CAR CUBELIST) 0)));;test-solve



;;;; cube.lisp                        -- 2004-03-19 23:29:09 -- pascal   ;;;;
