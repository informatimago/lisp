;; -*- mode:lisp -*-
(in-package "COM.INFORMATIMAGO.SUDOKU-SOLVER"))

;; Linked-in Mini-Sudoku https://www.linkedin.com/games/mini-sudoku/?isGameStart=true
;; Seems the max value is 6 too.
;; and the horizontal separations are each 2 lines to have 6 boxes in groups.

(defparameter *linkedin*
  #2A((x 2 3 4 5 6)
      (x 4 6 2 x 3)
      ;;--
      (x x 4 x 6 1)
      (x x x 3 2 4)
      ;;--
      (4 x x x 3 5)
      (3 1 x x x x)))

(sudoku-print  *linkedin*)
;; +---+---+---+---+---+---+
;; | .   2   3 | 4   5   6 |
;; |   +   +   |   +   +   |
;; | .   4   6 | 2   .   3 |
;; |   +   +   |   +   +   |
;; | .   .   4 | .   6   1 |
;; +---+---+---+---+---+---+
;; | .   .   . | 3   2   4 |
;; |   +   +   |   +   +   |
;; | 4   .   . | .   3   5 |
;; |   +   +   |   +   +   |
;; | 3   1   . | .   .   . |
;; +---+---+---+---+---+---+


(sudoku-solver *linkedin*)
