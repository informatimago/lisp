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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1995 - 2012
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CUBE"
  (:documentation
   "This program tries to resolve the Cube Puzzle, where a cube
    composed of 27 smaller cubes linked with a thread  must be
    recomposed.
    
    Copyright Pascal J. Bourguignon 1995 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:use "COMMON-LISP")
  (:export make-cube-list
           cube
           set-number set-coordinate input-vector output-vector
           collide roll solve add-output-cube-to-side
           set-input-cube-to-side bounds reverse-cubes  )
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


(defun o*v (oper vect)
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




(defvar origin #(0 0 0))
(defvar x-axis #(1 0 0))
(defvar y-axis #(0 1 0))
(defvar z-axis #(0 0 1))
(defvar -x-axis #(-1 0 0))
(defvar -y-axis #(0 -1 0))
(defvar -z-axis #(0 0 -1))
(defvar x-axis-quarter-turn #(#(1 0 0) #(0 0 1) #(0 -1 0))) ; x y z --> x z -y
(defvar y-axis-quarter-turn #(#(0 0 -1) #(0 1 0) #(1 0 0))) ; x y z --> -z y x
(defvar z-axis-quarter-turn #(#(0 1 0) #(-1 0 0) #(0 0 1))) ; x y z --> y -x z
(defvar -x-axis-quarter-turn #(#(-1 0 0) #(0 0 -1) #(0 1 0)))
(defvar -y-axis-quarter-turn #(#(0 0 1) #(0 -1 0) #(-1 0 0)))
(defvar -z-axis-quarter-turn #(#(0 -1 0) #(1 0 0) #(0 0 -1)))
(defvar identity #(#(1 0 0) #(0 1 0) #(0 0 1))) ; also the base.


(defun quarter-turn (vect)
  (cond
   ((equal vect x-axis) x-axis-quarter-turn)
   ((equal vect y-axis) y-axis-quarter-turn)
   ((equal vect z-axis) z-axis-quarter-turn)
   ((equal vect -x-axis) -x-axis-quarter-turn)
   ((equal vect -y-axis) -y-axis-quarter-turn)
   ((equal vect -z-axis) -z-axis-quarter-turn)
   (t (error "quarter-turn: general case not implemented~% vect must be a base vector or opposite thereof~%"))));;QUARTER-TURN




(defun check-operator (operator argument expected)
  (format t "[~s]~a = ~a =? ~a (~a)~%"
          operator argument
          (o*v operator argument) expected
          (equal (o*v operator argument) expected)));;CHECK-OPERATOR
	

(defun check ()
  (check-operator x-axis-quarter-turn x-axis x-axis)
  (check-operator x-axis-quarter-turn y-axis z-axis)
  (check-operator x-axis-quarter-turn z-axis (v- y-axis))
  (check-operator y-axis-quarter-turn x-axis (v- z-axis))
  (check-operator y-axis-quarter-turn y-axis y-axis)
  (check-operator y-axis-quarter-turn z-axis x-axis)
  (check-operator z-axis-quarter-turn x-axis y-axis)
  (check-operator z-axis-quarter-turn y-axis (v- x-axis))
  (check-operator z-axis-quarter-turn z-axis z-axis)
  );;CHECK
 




;; A box is list with (car box) containing the left-bottom-far most 
;; place and (cdr box) containing the right-top-near most place of the 
;; box. Each is a list of three coordinate (x y z).
;; Sides of the box are parallel to the base planes.


(defun make-box (lbf rtn) (cons lbf rtn))
(defmacro box-lbf (box) `(car ,box))
(defmacro box-rtn (box) `(cdr ,box))


(defun box-size   (box)
  (let ((d (v- (box-lbf box) (box-rtn box))))
    (abs (* (aref d 0) (aref d 1) (aref d 2)))))


(defun box-expand (box pos)
  (let ((lbf (box-lbf box)) (rtn (box-rtn box)) )
    (make-box (v (min (aref pos 0) (aref lbf 0))
                 (min (aref pos 1) (aref lbf 1))
                 (min (aref pos 2) (aref lbf 2 )))
              (v (max (aref pos 0) (aref rtn 0))
                 (max (aref pos 1) (aref rtn 1))
                 (max (aref pos 2) (aref rtn 2))))))


(defun check-box-expand ()
  (print (box-expand (make-box origin origin) (v 0   0   0) ))
  (print (box-expand (make-box origin origin) (v 1   0   0) ))
  (print (box-expand (make-box origin origin) (v 0   1   0) ))
  (print (box-expand (make-box origin origin) (v 0   0   1) ))
  (print (box-expand (make-box origin origin) (v -1  0   0) ))
  (print (box-expand (make-box origin origin) (v 0  -1   0) ))
  (print (box-expand (make-box origin origin) (v 0   0  -1) ))
  (print (box-expand (make-box origin origin) (v 1   0   0) ))
  (print (box-expand (make-box origin origin) (v 1   1   0) ))
  (print (box-expand (make-box origin origin) (v 1   0   1) ))
  (print (box-expand (make-box origin origin) (v -1  0   0) ))
  (print (box-expand (make-box origin origin) (v 1  -1   0) ))
  (print (box-expand (make-box origin origin) (v 1   0  -1) ))
  (print (box-expand (make-box origin origin) (v 1   1   0) ))
  (print (box-expand (make-box origin origin) (v 0   1   0) ))
  (print (box-expand (make-box origin origin) (v 0   1   1) ))
  (print (box-expand (make-box origin origin) (v -1  1   0) ))
  (print (box-expand (make-box origin origin) (v 0  -1   0) ))
  (print (box-expand (make-box origin origin) (v 0   1  -1) ))
  (print (box-expand (make-box origin origin) (v 1   0   1) ))
  (print (box-expand (make-box origin origin) (v 0   1   1) ))
  (print (box-expand (make-box origin origin) (v 0   0   1) ))
  (print (box-expand (make-box origin origin) (v -1  0   1) ))
  (print (box-expand (make-box origin origin) (v 0  -1   1) ))
  (print (box-expand (make-box origin origin) (v 0   0  -1) ))
  )



;;----------------------------------------------------------------------

;; orientation = tri-vecteur ((1 0 0) (0 1 0) (0 0 1))
;; axe         = vecteur     (1 0 0)
;;
;;


(defclass cube ()
  (
   ;;Invariants:
   ;; coordinate    = input-cube.coordinate+input-cube.outputVector
   ;; orientation = rotation(input-cube.axe,input-cube.orientation)
   (index        :accessor index         :initform 0)
   (coordinate   :accessor coordinate    :initform '(0 0 0))
   (orientation  :accessor orientation   :initform basis)
   (input-side   :accessor input-side    :initform 0)
   (input-cube   :accessor input-cube    :initform '())
   (output-side  :accessor output-side   :initform 0)
   (output-cube  :accessor output-cube   :initform '())
   )
  );;CUBE


;; use the following line to update the class summary, but skip the first
;; semicolon.
;; egrep 'defclass|defmethod' $file |sed -e 's/(defclass \(.*\)/	(format t "class \1~%")/' -e 's/(defmethod\(.*\)/    (format t "\1~%")/' -e 's/;/~%  /g'|grep -v egrep


(defmethod set-index           ((self cube) index)
  (setf (index self) index)
  (if (null (output-cube self))
    index
    (set-index (output-cube self) (1+ index))));;SET-INDEX


(defmethod set-coordinate       ((self cube) newcoordinate)
  (setf (coordinate self) newcoordinate)
  (if (null (output-cube self))
    newcoordinate
    (set-coordinate (output-cube self) 
                    (add-vector newcoordinate (output-vector self)))));;SET-COORDINATE
				

(defmethod input-vector         ((self cube))
  (if (= 0 (input-side self))
    '(0 0 0)
    (opposite-vector (output-vector (input-cube self)))));;INPUT-VECTOR


(defmethod output-vector        ((self cube))
  (cond
   ((= 0 (output-side self)) '(0 0 0))
   ((= 1 (output-side self)) 
    (opposite-vector (first (orientation self))))
   ((= 2 (output-side self)) 
    (opposite-vector (second (orientation self))))
   ((= 3 (output-side self)) 
    (opposite-vector (third (orientation self))))
   ((= 4 (output-side self)) 
    (third (orientation self)))
   ((= 5 (output-side self)) 
    (second (orientation self)))
   ((= 6 (output-side self)) 
    (first (orientation self)))
   (t (error "Invalid output-side (~a) for ~a~%" 
             (output-side self) self)
      '(0 0 0))));;OUTPUT-VECTOR


(defmethod collide  ((self cube) othercoord)
  (cond
   ((null self) nil)
   ((equal (coordinate self) othercoord) t)
   ((null (input-cube self)) nil)
   (t (collide (input-cube self) othercoord)))
  );;COLLIDE


(defmethod roll     ((self cube))
  (setf (orientation self) 
		(mapcar
         (lambda (v) 
           (apply-operator (quarter-turn (output-vector (input-cube self))) v))
         (orientation self)))
  (set-coordinate self (coordinate self))
  );;ROLL


(defmethod solve   ((self cube) try) ;; try in [0..3+1]
  (format t "--> ~a~%" (mapcar 'coordinate (input-cube self)))
  (cond
   ((null self)    t)
   ((> try 3)      (block t (roll self) nil))
   ((and (input-cube self) (or 
                            (> (apply 'max (box-size (bounds self))) 3) 
                            (collide (input-cube self) (coordinate self))))
    (roll self)
    (solve self (1+ try)))
   ((output-cube self)
    (if (solve (output-cube self) 0)
      t
      (block t
        (roll self)
        (solve self (1+ try)))))
   (t t)
   ));;SOLVE


(defmethod add-output-cube-to-side ((self cube) (new-output cube) side)
  (setf (output-cube self) new-output)
  (setf (output-side self) side)
  (setf (orientation self) (orientation new-output))
  (set-input-cube-to-side new-output self (- 7 side))
  (setf (index self)     (1- (index new-output)))
  (setf (coordinate self)
        (add-vector (coordinate new-output)
                    (opposite-vector (output-vector self))))
  );;ADD-OUTPUT-CUBE-TO-SIDE


(defmethod set-input-cube-to-side  ((self cube) (new-input cube) side)
  (setf (input-cube self) new-input)
  (setf (input-side self) side));;SET-INPUT-CUBE-TO-SIDE


(defmethod bounds              ((self cube)) ; returns a box.
  (if (null (input-cube self))
    (cons (coordinate self) (coordinate self))
    (box-expand (bounds (input-cube self)) (coordinate self))));;BOUNDS


(defmethod reverse-cubes    ((self cube)) ; reverse the cube list.
  (let ((c (input-cube self)) (s (input-side self)))
    (setf (input-cube self) (output-cube self))
    (setf (input-side self) (output-side self))
    (setf (output-cube self) c)
    (setf (output-side self) s)
	)
  (reverse-cubes (input-cube self)));;REVERSE-CUBES



(defun make-cube-list (l)
  (let ((current ()))
    (mapcar (lambda (side)
            (let ((newcube    (make-instance 'cube)))
              (if (= 0 side)
                (setq current newcube)
                (block t
                  (add-output-cube-to-side newcube current side)
                  (setq current newcube)))))
          l)));;MAKE-CUBE-LIST



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
  (let ((cubelist (reverse (make-cube-list (reverse '(6 6 2 2 6 6 2 2 6 2 6 2 6 6 2 2 6 2 2 6 2 2 6 2 6 6 0))))))
    (solve (car cubelist) 0)));;test-solve



;;;; cube.lisp                        -- 2004-03-19 23:29:09 -- pascal   ;;;;
