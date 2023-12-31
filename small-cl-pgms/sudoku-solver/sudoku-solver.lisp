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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2023
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage "COM.INFORMATIMAGO.SUDOKU-SOLVER"
  (:use "COMMON-LISP")
  (:export "SUDOKU-SOLVER" "SUDOKU-PRINT"))

(in-package "COM.INFORMATIMAGO.SUDOKU-SOLVER")

(defun iota (count &optional (start 0) (step 1))
  "
RETURN:   A list containing the elements
          (start start+step ... start+(count-1)*step)
          The start and step parameters default to 0 and 1, respectively.
          This procedure takes its name from the APL primitive.
EXAMPLE:  (iota 5) => (0 1 2 3 4)
          (iota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
"
  (loop
    :for item = start :then (+ item step)
    :repeat count
    :collect item))

(defun copy-array (array &key copy-fill-pointer copy-adjustable
                   copy-displacement)
  "
RETURN:             A copy of the ARRAY.
ARRAY:              An array.
COPY-FILL-POINTER:  Indicate whether the copy must have the same
                    FILL-POINTER as the ARRAY.
COPY-ADJUSTABLE:    Indicate whether the copy must be an adjustable
                    array when the ARRAY is adjustable.
COPY-DISPLACEMENT:  Indicate whether the copy must be an array
                    displaced to the same array as the ARRAY.
"
  (when copy-displacement
    (multiple-value-bind (disto disoff) (array-displacement array)
      (when disto
        (return-from copy-array
          (make-array (array-dimensions array)
                      :element-type (array-element-type array)
                      :displaced-to disto
                      :displaced-index-offset disoff
                      :adjustable (when copy-adjustable
                                    (adjustable-array-p array))
                      :fill-pointer (when copy-fill-pointer
                                      (fill-pointer array)))))))
  (let ((copy (make-array (array-dimensions array)
                          :adjustable (when copy-adjustable
                                        (adjustable-array-p array))
                          :fill-pointer (when copy-fill-pointer
                                          (fill-pointer array))
                          :element-type (array-element-type array))))
    (dotimes (i (array-total-size copy))
      (setf (row-major-aref copy i) (row-major-aref array i)))
    copy))

(defun make-sudoku ()
  (make-array '(9 9) :initial-element 'x))

(defun emptyp (slot)
  (or (null slot)
      (and (symbolp slot)
           (string= slot 'x))))

(defun list-or-single-element (list)
  (if (null (rest list))
      (first list)
      list))



(defun row (sudoku row)
  "Return the list of elements present in the row ROW of the sudoku."
  (loop
    :for col :below (array-dimension sudoku 0)
    :for item = (aref sudoku col row)
    :unless (emptyp item)
    :collect item))


(defun col (sudoku col)
  "Return the list of elements present in the column COL of the sudoku."
  (loop
    :for row :below (array-dimension sudoku 1)
    :for item = (aref sudoku col row)
    :unless (emptyp item)
    :collect item))


(defun reg (sudoku col row)
  "Return the list of elements present in the region containing slot
\(col row) of the sudoku."
  (loop
    :with bac = (* (truncate col 3) 3)
    :with bar = (* (truncate row 3) 3)
    :for i :from bac :below (+ bac 3)
    :nconc (loop
             :for j :from bar :below (+ bar 3)
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


(defun conflictp (sudoku col row)
  "Predicates whether there's a conflict around slot (col row)."
  (let ((val (aref sudoku col row)))
    (loop
      :for i :below (array-dimension sudoku 0)
      :when (and (/= i col) (eql val (aref sudoku i row)))
        :do (return-from conflictp :row-conflict))
    (loop
      :for j :below (array-dimension sudoku 1)
      :when (and (/= j row) (eql val (aref sudoku col j)))
        :do (return-from conflictp :col-conflict))
    (loop
      :with from := (* (truncate col 3) 3)
      :for i :from from :below (+ from 3)
      :when (/= i col)
        :do (loop
              :with from := (* (truncate row 3) 3)
              :for j :from from :below (+ from 3)
              :when (and (/= j row) (eql val (aref sudoku i j)))
                :do (return-from conflictp :reg-conflict)))
    nil))


(defvar *sudoku-tries* 0)

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
    ;; (format t "Found a small set of choices at (~D ~D): ~S~%" i j (aref sudoku i j))
    (if (consp (aref sudoku i j))
        (loop
          :with results = '()
          :for val :in (aref sudoku i j)
          :do (catch 'sudoku-backtrack
                (incf *sudoku-tries*)
                (let ((sudoku (copy-array sudoku))
                      (check-list '()))
                  (setf (aref sudoku i j) val)
                  ;; (format t "Trying to put ~D at (~D ~D)~%" val i j)
                  ;; (sudoku-print sudoku)
                  (loop
                    :named update-col
                    :for col :below (array-dimension sudoku 0)
                    :do (cond
                          ((= col i))
                          ((listp (aref sudoku col j))
                           (setf (aref sudoku col j) (list-or-single-element (remove val (aref sudoku col j))))
                           (when (atom (aref sudoku col j))
                             (push (list col j) check-list)))
                          ((eql (aref sudoku col j) val)
                           ;; (format t "  won't do, there's already a ~D on the same row.~%" val)
                           (throw 'sudoku-backtrack nil))))
                  (loop
                    :named update-row
                    :for row :below (array-dimension sudoku 1)
                    :do (cond
                          ((= row j))
                          ((listp (aref sudoku i row))
                           (setf (aref sudoku i row) (list-or-single-element (remove val (aref sudoku i row))))
                           (when (atom (aref sudoku i row))
                             (push (list i row) check-list)))
                          ((eql (aref sudoku i row) val)
                           ;; (format t "  won't do there's already a ~D on the same column.~%" val)
                           (throw 'sudoku-backtrack nil))))
                  (loop
                    :named update-reg
                    :with from := (* (truncate i 3) 3)
                    :for col :from from :below (+ from 3)
                    :do (loop
                          :with from := (* (truncate j 3) 3)
                          :for row :from from :below (+ from 3)
                          :do (cond
                                ((and (= col i) (= row j)))
                                ((listp (aref sudoku col row))
                                 (setf (aref sudoku col row) (list-or-single-element (remove val (aref sudoku col row))))
                                 (when (atom (aref sudoku col row))
                                   (push (list col row) check-list)))
                                ((eql (aref sudoku col row) val)
                                 ;; (format t "  won't do there's already a ~D in the same region.~%" val)
                                 (throw 'sudoku-backtrack nil)))))
                  (loop
                    :for (col row) :in check-list
                    :for conflict = (conflictp sudoku col row)
                    :do (when conflict
                          ;; (format t "  won't do, there'd be a ~(~A~) at (~D ~D).~%" conflict col row)
                          (throw 'sudoku-backtrack nil)))
                  ;; (format t "  fits so far.~%")
                  ;; (sudoku-print sudoku)
                  (setf results (nconc (sudoku-backtracking sudoku) results))))
          :finally (return results))
        (list sudoku))))


(defun sudoku-solver (sudoku)
  "
DO:     Solves the SUDOKU board (it contains atoms and X or NIL that
        are replaced in the solutions by the atoms required by the
        rules.

RETURN: A list of sudoku solution boards.
"
  (let* ((*sudoku-tries* 1)
         (sudoku (copy-array sudoku))
         ;; Well for now, the atoms are integers from 1 up to the
         ;; maximal dimension of the matrix.
         (all    (iota (max (array-dimension sudoku 0)
                            (array-dimension sudoku 1))
                       1))
         (cols   (coerce (loop :for col :below (array-dimension sudoku 0) :collect (col sudoku col)) 'vector))
         (rows   (coerce (loop :for row :below (array-dimension sudoku 1) :collect (row sudoku row)) 'vector))
         (regs   (let ((regs (make-array (mapcar (lambda (x) (ceiling x 3)) (array-dimensions sudoku)))))
                   (loop
                     :for i :below (array-dimension regs 0)
                     :do (loop
                           :for j :below (array-dimension regs 1)
                           :do (setf (aref regs i j) (reg sudoku (* 3 i) (* 3 j)))))
                   regs)))
    (for-each-slot ((slot i j) sudoku)
      (when (emptyp slot)
        (let ((possibles (set-difference all (union
                                              (union (aref cols i) (aref rows j))
                                              (aref regs (truncate i 3) (truncate j 3))))))
          (setf slot (list-or-single-element possibles)))))
    (catch 'sudoku-backtrack
      (values (sudoku-backtracking sudoku) *sudoku-tries*))))


(defun sudoku-print (sudoku &optional (*standard-output* *standard-output*))
  "
DO:     Prints the SUDOKU board to the optional stream given.
RETURN  SUDOKU.
"
  (loop
    :with =line = (with-output-to-string (*standard-output*)
                    (loop
                      :repeat (array-dimension sudoku 1)
                      :do (princ "+---")
                      :finally (princ "+") (terpri)))
    :with -line = (with-output-to-string (*standard-output*)
                    (loop
                      :for i :below (array-dimension sudoku 1)
                      :do (princ (if (zerop (mod i 3)) "|   " "+   "))
                      :finally (princ "|") (terpri)))
    :for i :below (array-dimension sudoku 0)
    :do (princ (if (zerop (mod i 3)) =line -line))
    :do (loop
          :for j :below (array-dimension sudoku 1)
          :do (format t (if (zerop (mod j 3)) "|~2@A " " ~2@A ")
                      (let ((slot  (aref sudoku i j)))
                        (if (emptyp slot)
                            "."
                            slot)))
          :finally (princ "|") (terpri))
    :finally (princ =line) (terpri))
  sudoku)


(defun sudoku-count-empty-slots (sudoku)
  (let ((empty-count 0))
    (for-each-slot (slot sudoku)
                   (when (emptyp slot)
                     (incf empty-count)))
    empty-count))


;;----------------------------------------------------------------------

(defparameter *royco-minut-soup* #2A((x x x 8 x 4 2 x x)
                                     (6 x 8 x 2 x x x 4)
                                     (2 1 x 6 5 3 x x 8)
                                     (x 7 x 2 x 6 x 9 x)
                                     (x x x x 3 x 1 x x)
                                     (4 2 3 x x 9 x 5 7)
                                     (x 6 x 4 1 5 7 x x)
                                     (x x 7 x x 8 3 x x)
                                     (x 5 9 x x x x 1 x)))

(defparameter *20-minutes/1499/facile* #2A((2 x 4 1 5 x 8 7 x)
                                           (x x x 3 x x x 9 1)
                                           (x 7 x 8 6 x x x 4)
                                           (x x 3 x 2 1 x 8 x)
                                           (x x 1 x 8 x 3 x x)
                                           (x 8 x 4 3 x 9 x x)
                                           (9 x x x 1 3 x 6 x)
                                           (3 2 x x x 4 x x x)
                                           (x 1 7 x 9 8 4 x 3)))

(defparameter *20-minutes/1501/difficile* #2A((x 1 5 x 8 9 x x x)
                                              (2 x 6 3 x 5 x x x)
                                              (x 7 x x x x x 8 x)
                                              (x 6 9 x x 1 x x 3)
                                              (x x x 8 x 3 x x x)
                                              (3 x x 6 x x 2 9 x)
                                              (x 9 x x x x x 2 x)
                                              (x x x 9 x 6 1 x 5)
                                              (x x x 7 1 x 9 3 x)))


(defparameter *20-minutes/1502/expert* #2A((5 7 x x x x x x x)
                                           (x x 8 x x x 1 7 x)
                                           (x 1 x 7 x 4 x 2 8)
                                           (x x 1 x 4 x x 8 5)
                                           (x 5 x x 1 x x 3 x)
                                           (8 6 x x 7 x 4 x x)
                                           (1 4 x 3 x 9 x 6 x)
                                           (x 3 2 x x x 9 x x)
                                           (x x x x x x x 1 3)))

(defparameter *20-minutes/1505/moyen* #2A((1 x 2 x 6 5 9 x x)
                                          (x 4 x 8 x x x x 5)
                                          (x 8 x 1 x x 4 3 6)
                                          (x x 1 9 x x x x x)
                                          (6 5 x x x x x 4 9)
                                          (x x x x x 6 1 x x)
                                          (2 1 3 x x 8 x 5 x)
                                          (8 x x x x 2 x 9 x)
                                          (x x 4 5 7 x 3 x 2)))

(defparameter *20-minutes/1506/facile* #2A((3 x 5 8 4 x x x 1)
                                           (7 x x x x x 5 x x)
                                           (4 x 1 6 3 x 8 x x)
                                           (8 x x x x x 3 5 7)
                                           (x x 3 x x x 9 x x)
                                           (6 1 9 x x x x x 8)
                                           (x x 8 x 6 2 4 x 5)
                                           (x x 7 x x x x x 2)
                                           (2 x x x 5 1 7 x 3)))

(defparameter *metrofrance/694/moyen* #2A((x x 1 2 x x x x 8)
                                          (x x x x 5 1 x x 3)
                                          (x 7 x x x x 6 x 1)
                                          (9 x 4 x 1 7 x x 5)
                                          (1 3 x x x x x 2 9)
                                          (8 x x 9 2 x 1 x 4)
                                          (4 x 6 x x x x 1 x)
                                          (3 x x 8 7 x x x x)
                                          (7 x x x x 4 3 x x)))

(defparameter *metrofrance/696/facile* #2A((2 4 x 9 x x 7 6 x)
                                           (3 x x x 8 x x x x)
                                           (8 5 x 4 x x x x x)
                                           (5 x x x x 8 4 x x)
                                           (7 x 4 6 9 3 2 x 5)
                                           (x x 9 5 x x x x 3)
                                           (x x x x x 1 x 2 4)
                                           (x x x x 6 x x x 1)
                                           (x 6 8 x x 9 x 5 7)))

(defparameter *metrofrance/700/moyen* #2A ((1 9 x 8 3 7 x x x)
                                           (7 5 x x 2 x x x x)
                                           (4 x x x 9 x x x 7)
                                           (x 3 4 x x x x x 9)
                                           (9 x 7 x x x 6 x 2)
                                           (8 x x x x x 4 7 x)
                                           (3 x x x 1 x x x 6)
                                           (x x x x 6 x x 3 4)
                                           (x x x 9 4 3 x 2 8)))

(defparameter *andre* #2A ((x x x x x 2 x x x)
                           (1 x x x 3 x x 4 x)
                           (x x x x 1 9 x 6 x)
                           (7 x x 3 4 x 1 x 5)
                           (x x x x x x 8 x x)
                           (9 x x x x 5 x 3 4)
                           (2 x 6 x 5 1 x x x)
                           (x 7 x x x x x x x)
                           (x x 8 9 x 4 5 7 x)))

(defparameter *240* #2A((7 8 x 2 6 x x x x)
                        (3 2 4 x x 9 5 x x)
                        (1 6 x x 3 5 4 x 2)
                        (x 1 x 4 2 8 x x 9)
                        (4 x 3 5 x 7 x x x)
                        (2 x x 6 x 3 7 x 4)
                        (9 x x x x 1 8 x 5)
                        (x x 1 9 8 x 6 3 7)
                        (8 7 x x 5 6 9 x 1)))


(defun solve-sudokus (&key (print t))
  (dolist (sudoku '(
                    *20-minutes/1499/facile*
                    *20-minutes/1501/difficile*
                    *20-minutes/1502/expert*
                    *20-minutes/1505/moyen*
                    *20-minutes/1506/facile*
                    *metrofrance/694/moyen*
                    *metrofrance/696/facile*
                    *metrofrance/700/moyen*
                    *royco-minut-soup*
                    *andre*
                    *240*
                    ))
    (multiple-value-bind (solutions tries) (sudoku-solver (symbol-value sudoku))
      (terpri)
      (when print
        (print sudoku) (terpri)
        (sudoku-print (symbol-value sudoku)))
      (format t "  ~A (with ~D empty slots)~%  has ~D solution~:*~P,~%  found in ~D tries.~2%"
              sudoku (sudoku-count-empty-slots (symbol-value sudoku))
              (length solutions) tries)
      (when print
        (map nil 'sudoku-print solutions)))))


;;----------------------------------------------------------------------

(defun read-grid (stream)
  (let ((line (read-line stream nil nil)))
    (when (and line (plusp (length line)))
      (if (= 81 (length line))
          (let ((sudoku (make-sudoku))
                (i -1))
            (for-each-slot (slot sudoku)
                           (setf slot (let ((value (digit-char-p (aref line (incf i)))))
                                        (if (and value (plusp value))
                                            value
                                            'x))))
            sudoku)
          (loop
            :with sudoku = (make-sudoku)
            :for j :below 9
            :for line = (read-line stream)
            :do (loop
                  :for i :below 9
                  :for value = (digit-char-p (aref line i))
                  :do (setf (aref sudoku i j) (if (and value (plusp value)) value 'x)))
            :finally (return sudoku))))))


(defun solve-grids (grid-file)
  (with-open-file (grids grid-file)
    (loop
      :for sudoku = (read-grid grids)
      :while sudoku
      :do (multiple-value-bind (solutions tries) (sudoku-solver sudoku)
            (sudoku-print sudoku)
            (format t "  (with ~D empty slots)~%  has ~D solution~:*~P,~%  found in ~D tries.~2%"
                    (sudoku-count-empty-slots sudoku)
                    (length solutions) tries)
            (map nil 'sudoku-print solutions)))))


(defun solve-grids/no-print (grid-file)
  (with-open-file (grids grid-file)
    (loop
      :for sudoku = (read-grid grids)
      :while sudoku
      :collect (multiple-value-bind (solutions tries) (sudoku-solver sudoku)
                 (declare (ignore solutions))
                 tries))))

(defun time-dim-grids ()
  (dolist (file '(#P"~/src-lisp/usenet/dim-sudoku/sudoku.txt"
                  #P"~/src-lisp/usenet/dim-sudoku/easy50.txt"
                  #P"~/src-lisp/usenet/dim-sudoku/top95.txt"
                  #P"~/src-lisp/usenet/dim-sudoku/hardest.txt"))
    (terpri) (print file) (terpri)
    (print (time (solve-grids/no-print file)))))

;; (solve-sudokus :print t)
;; (solve-sudokus :print nil)
;;;; THE END ;;;;
