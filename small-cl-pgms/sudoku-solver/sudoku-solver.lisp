;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               sudoku-solver.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A sudoku solver.
;;;;
;;;;    I never tried to solve a sudoku myself… and never will.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-02 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(ql:quickload :com.informatimago.common-lisp.cesarum)

(defpackage "COM.INFORMATIMAGO.SUDOKU-SOLVER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY")
  (:export "SUDOKU-SOLVER" "SUDOKU-PRINT"))

(in-package "COM.INFORMATIMAGO.SUDOKU-SOLVER")


(defun emptyp (slot)
  (or (null slot)
      (and (symbolp slot)
           (string= slot 'x))))


(defun row (sudoku row)
  "Return the list of elements present in the row ROW of the sudoku."
  (loop
    :for col :below (array-dimension sudoku 1)
    :for item = (aref sudoku row col)
    :unless (emptyp item)
    :collect item))


(defun col (sudoku col)
  "Return the list of elements present in the column COL of the sudoku."
  (loop
    :for row :below (array-dimension sudoku 0)
    :for item = (aref sudoku row col)
    :unless (emptyp item)
    :collect item))


(defun reg (sudoku row col)
  "Return the list of elements present in the region containing slot
\(row col) of the sudoku."
  (loop
    :with bar = (* (truncate row 3) 3)
    :with bac = (* (truncate col 3) 3)
    :repeat 3
    :for i :from bar
    :nconc (loop
             :repeat 3
             :for j :from bac
             :for item = (aref sudoku i j)
             :unless (emptyp item)
             :collect item)))


#+emacs (put 'for-each-slot 'common-lisp-indent-function 1)

(defmacro for-each-slot ((slot-and-var sudoku) &body body)
  "
SLOT-AND-VAR: either a symbol naming the variable that will be bound to the slot,
              or a list (slot i j) of three symbols naming the variables that
              will be bound to the slot, the row and the column indices.

SUDOKU:       An expression that evaluates to a sudoku board.

BODY:         A list of lisp forms.

DO:           Evaluates the BODY for each slot in the sudoku board, in
              a lexical context where the slot variable and when
              present, the row and column indices variables are bound
              to the slot and indices, for each slot of the sudoku
              board. The slot variable can be modified and this
              changes the slot in the sudoku board.

RETURN:       no value.
"
  (let ((i    (gensym "i"))
        (j    (gensym "j"))
        (di   (gensym "di"))
        (dj   (gensym "dj"))
        (s    (gensym "s"))
        (slot (if (atom slot-and-var)
                  slot-and-var
                  (first slot-and-var)))
        (ivar (if (atom slot-and-var)
                  nil
                  (progn
                    (assert (second slot-and-var) () "The ivar must not be NIL")
                    (second slot-and-var))))
        (jvar (if (atom slot-and-var)
                  nil
                  (progn
                    (assert (third slot-and-var) () "The jvar must not be NIL")
                    (third slot-and-var)))))
    `(loop
       :with ,s = ,sudoku
       :with ,di = (array-dimension ,s 0)
       :with ,dj = (array-dimension ,s 1)
       :for ,i :below ,di
       :do ,(if ivar
                `(loop
                   :with ,ivar = ,i
                   :for ,j :below ,dj
                   :do (symbol-macrolet ((,slot (aref ,s ,i ,j)))
                         (let ((,jvar ,j))
                           ,@body)))
                `(loop
                   :for ,j :below ,dj
                   :do (symbol-macrolet ((,slot (aref ,s ,i ,j)))
                         ,@body)))
       :finally (return (values)))))


(defun optimalize (matrix &key (key (function identity)) (lessp (function <)))
  "
DO:      Find the extremum of the values obtained by calling the KEY
         function on each slot of the MATRIX, using the LESSP
         comparator.

RETURN:  If an extremum is found: the extremum value; the row; the column;
         otherwise NIL; -1; -1.
"
  (let ((mini -1)
        (minj -1)
        (minv nil))
    (for-each-slot ((slot i j) matrix)
      (let ((val (funcall key slot)))
        (if minv
            (when (funcall lessp val minv)
              (setf minv val
                    mini i
                    minj j))
            (setf minv val
                  mini i
                  minj j))))
    (values minv mini minj)))


(defun sudoku-backtracking (sudoku)
  "
PRE:        The slots of sudoku contain either an atom, an empty list,
            or a list of two or more atoms.


DO:         If there is an empty list in one of the slots, then throws
            the SUDOKU-BACKTRACK symbol.

            Else finds a slot with a small list, and tries each atom
            in it in turn.

RETURN:     A list of sudoku solutions boards.
"
  (multiple-value-bind (possibles i j)
      (optimalize sudoku :key (let ((infinite (reduce (function +)
                                                      (array-dimensions sudoku))))
                                (lambda (slot)
                                  (cond
                                    ((null slot)  (throw 'sudoku-backtrack nil))
                                    ((listp slot) (length slot))
                                    (t            infinite)))))
    (declare (ignore possibles))
    (if (consp (aref sudoku i j))
        (loop
          :with results = '()
          :for val :in (aref sudoku i j)
          :do (catch 'sudoku-backtrack
                (let ((sudoku (copy-array sudoku)))
                  (setf (aref sudoku i j) val)
                  (loop
                    :named update-row
                    :for row :below (array-dimension sudoku 0)
                    :when (listp (aref sudoku row j))
                    :do (setf (aref sudoku row j) (remove val (aref sudoku row j))))
                  (loop
                    :named update-col
                    :for col :below (array-dimension sudoku 1)
                    :when (listp (aref sudoku i col))
                    :do (setf (aref sudoku i col) (remove val (aref sudoku i col))))
                  (loop
                    :named update-reg
                    :repeat 3
                    :for row :from (* (truncate i 3) 3)
                    :do (loop
                          :repeat 3
                          :for col :from (* (truncate j 3) 3)
                          :when (listp (aref sudoku row col))
                          :do (setf (aref sudoku row col) (remove val (aref sudoku row col)))))
                  (setf results (nconc results (sudoku-backtracking sudoku)))))
          :finally (return results))
        (list sudoku))))


(defun sudoku-solver (sudoku)
  "
DO:     Solves the SUDOKU board (it contains atoms and X or NIL that
        are replaced in the solutions by the atoms required by the
        rules.

RETURN: A list of sudoku solution boards.
"
  (let* ((sudoku (copy-array sudoku))
         ;; Well for now, the atoms are integers from 1 up to the
         ;; maximal dimension of the matrix.
         (all    (iota (max (array-dimension sudoku 0)
                            (array-dimension sudoku 1))
                       1))
         (rows   (coerce (loop :for row :below (array-dimension sudoku 0) :collect (row sudoku row)) 'vector))
         (cols   (coerce (loop :for col :below (array-dimension sudoku 1) :collect (col sudoku col)) 'vector))
         (regs   (let ((regs (make-array (mapcar (lambda (x) (truncate x 3)) (array-dimensions sudoku)))))
                   (loop
                     :for i :below (array-dimension regs 0)
                     :do (loop
                           :for j :below (array-dimension regs 1)
                           :do (setf (aref regs i j) (reg sudoku (* 3 i) (* 3 j)))))
                   regs)))
    (for-each-slot ((slot i j) sudoku)
      (when (emptyp slot)
        (let ((possibles (set-difference all (union
                                              (union (aref rows i) (aref cols j))
                                              (aref regs (truncate i 3) (truncate j 3))))))
          (setf slot (if (null (rest possibles))
                         (first possibles)
                         possibles)))))
    (sudoku-backtracking sudoku)))


(defun sudoku-print (sudoku &optional (*standard-output* *standard-output*))
  "
DO:     Prints the SUDOKU board to the optional stream given.
RETURN  SUDOKU.
"
  (loop
    :with =line = (with-output-to-string (*standard-output*)
                    (loop
                      :repeat (array-dimension sudoku 0)
                      :do (princ "+---")
                      :finally (princ "+") (terpri)))
    :with -line = (with-output-to-string (*standard-output*)
                    (loop
                      :for i :below (array-dimension sudoku 0)
                      :do (princ (if (zerop (mod i 3)) "|   " "+   "))
                      :finally (princ "|") (terpri)))
    :for j :below (array-dimension sudoku 1)
    :do (princ (if (zerop (mod j 3)) =line -line))
    :do (loop
          :for i :below (array-dimension sudoku 0)
          :do (format t (if (zerop (mod i 3)) "|~2@A " " ~2@A ")
                      (let ((slot  (aref sudoku i j)))
                        (if (emptyp slot)
                            "."
                            slot)))
          :finally (princ "|") (terpri))
    :finally (princ =line) (terpri))
  sudoku)


;;----------------------------------------------------------------------

(let* ((sudoku #2A((x x x 8 x 4 2 x x)
                   (6 x 8 x 2 x x x 4)
                   (2 1 x 6 5 3 x x 8)
                   (x 7 x 2 x 6 x 9 x)
                   (x x x x 3 x 1 x x)
                   (4 2 3 x x 9 x 5 7)
                   (x 6 x 4 1 5 7 x x)
                   (x x 7 x x 8 3 x x)
                   (x 5 9 x x x x 1 x)))
       (solutions (sudoku-solver sudoku)))
  (terpri)
  (sudoku-print sudoku)
  (format t " has ~D solution~:*~P.~2%" (length solutions))
  (map nil 'sudoku-print solutions))

;;;; THE END ;;;;
