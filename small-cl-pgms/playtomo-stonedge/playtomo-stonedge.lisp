;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               playtomo-stonedge.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the Playtomo Stonedge Game, and its solver.
;;;;    See http://www.playtomo.com/ (not much here when I went);
;;;;    Download the playtomo games on BlackBerry.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(asdf-load :split-sequence)
(asdf-load :com.informatimago.common-lisp)
(use-package :com.informatimago.common-lisp.graph)
(use-package :com.informatimago.common-lisp.graph-dot)
(shadow 'copy) ; from :com.informatimago.common-lisp.graph

;;;-----------------------------------------------------------------------------
;;;
;;; STONE
;;;


(defclass stone ()
  ((x :initform 0
      :initarg :x
      :reader stone-x
      :documentation "Ordinate of the first cube of the stone.")
   (y :initform 0
      :initarg :y
      :reader stone-y
      :documentation "Coordinate of the first cube of the stone.")
   (direction :initform (vector 0 0 1)
              :initarg :direction
              :reader stone-direction
              :documentation "A unit vector indicating the direction of the stone.
The coordinate of the other cube of the stone is given by adding this vector to
the coordinates of the first cube.
Note: The stone is normalized so that the vertical coordinate of the direction is either 0 or 1."))
  (:documentation "A stone is made of two cubes of size equal to the cells.
To move, it rotates 1/4 turn on one of its edges that is in contact with the cells."))


(defconstant +right+ 0)
(defconstant +left+  1)
(defconstant +front+ 2)
(defconstant +back+  3)


;;
;;              ^
;;             y|front
;;              |
;;              |
;; left         |                 right
;; -------------+-------------------->
;;             0|                   x
;;              |
;;              |
;;              |back
;;

(defparameter *rotations*
  ;;                   x               x                  x                  x
  ;;                   y               y                  y                  y
  ;;          right    z      left     z         front    z         back     z
  ;;        -------   ---   --------  ---      --------  ---      -------   ---
  ;;          0 0 1    z      0 0 -1  -z        1  0  0   x        1  0  0   x
  ;;          0 1 0    y      0 1 0    y        0  0  1   z        0  0 -1  -z
  ;;         -1 0 0   -x      1 0 0    x        0 -1  0  -y        0  1  0   y
  #(
    ;; +right+
    #2a((0 0 1)
        (0 1 0)
        (-1 0 0))
    ;; +left+
    #2a((0 0 -1)
        (0 1 0)
        (1 0 0))
    ;; +front+
    #2a((1 0 0)
        (0 0 1)
        (0 -1 0))
    ;; +back+
    #2a((1 0 0)
        (0 0 -1)
        (0 1 0)))
  "A vector of 3D rotation matrices for right, left, front and back rotations.")


(defun rotate (matrix vector)
  "Returns matrix * vector"
  (coerce
   (loop
      :for i :from 0 :to 2
      :collect (loop
                  :for j :from 0 :to 2
                  :sum (* (aref vector j) (aref matrix i j))))
   'vector))


(defun test-rotate ()
  (assert
   (equalp
    (let ((result '()))
      (dolist (direction (list #(1 0 0) #(-1 0 0) #(0 1 0) #(0 -1 0) #(0 0 1) #(0 0 -1))
               result)
        (dotimes (rotation 4)
          (push (list direction (aref #(left--> right-> front-> back-->) rotation)
                      (rotate (aref *rotations* rotation)  direction))
                result))))
    '((#(0 0 -1) back--> #(0 1 0)) (#(0 0 -1) front-> #(0 -1 0))
      (#(0 0 -1) right-> #(1 0 0)) (#(0 0 -1) left--> #(-1 0 0))
      (#(0 0 1) back--> #(0 -1 0)) (#(0 0 1) front-> #(0 1 0))
      (#(0 0 1) right-> #(-1 0 0)) (#(0 0 1) left--> #(1 0 0))
      (#(0 -1 0) back--> #(0 0 -1)) (#(0 -1 0) front-> #(0 0 1))
      (#(0 -1 0) right-> #(0 -1 0)) (#(0 -1 0) left--> #(0 -1 0))
      (#(0 1 0) back--> #(0 0 1)) (#(0 1 0) front-> #(0 0 -1))
      (#(0 1 0) right-> #(0 1 0)) (#(0 1 0) left--> #(0 1 0))
      (#(-1 0 0) back--> #(-1 0 0)) (#(-1 0 0) front-> #(-1 0 0))
      (#(-1 0 0) right-> #(0 0 -1)) (#(-1 0 0) left--> #(0 0 1))
      (#(1 0 0) back--> #(1 0 0)) (#(1 0 0) front-> #(1 0 0))
      (#(1 0 0) right-> #(0 0 1)) (#(1 0 0) left--> #(0 0 -1)))))
  :success)


(test-rotate)


(defun invert (vector)
  "Returns   - vector"
  (map 'vector (lambda (x) (- x)) vector))


(defgeneric normalize (stone direction)
  (:documentation "
Normalize the stone for a rotation in the given direction.
DIRECTION: (member :left :right :front :back)
")
  (:method ((self stone) (direction (eql :right)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (when (plusp (aref direction 0))
        (progn
          (incf x)
          (setf direction (invert direction)))))
    self)
  (:method ((self stone) (direction (eql :left)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (when (minusp (aref direction 0))
        (progn
          (decf x)
          (setf direction (invert direction)))))
    self)
  (:method ((self stone) (direction (eql :front)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (when (plusp (aref direction 1))
        (progn
          (incf y)
          (setf direction (invert direction)))))
    self)
  (:method ((self stone) (direction (eql :back)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (when (minusp (aref direction 1))
        (progn
          (decf y)
          (setf direction (invert direction)))))
    self))


(defgeneric move (stone direction)
  (:documentation "Move the stone in the given direction.")
  (:method ((self stone) (direction (eql :right)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (incf x)
      (setf direction  (rotate (aref *rotations* +right+) direction)))
    self)
  (:method ((self stone) (direction (eql :left)))
    (declare (ignorable direction))
    (with-slots (x direction) self
      (decf x)
      (setf direction  (rotate (aref *rotations* +left+) direction)))
    self)
  (:method ((self stone) (direction (eql :front)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (incf y)
      (setf direction  (rotate (aref *rotations* +front+) direction)))
    self)
  (:method ((self stone) (direction (eql :back)))
    (declare (ignorable direction))
    (with-slots (y direction) self
      (decf y)
      (setf direction  (rotate (aref *rotations* +back+) direction)))
    self))

(defmethod move :before ((self stone) direction) (normalize self direction))



(defun verticalp (direction)
  (not (zerop (aref direction 2))))

(defun lateralp (direction)
  (not (zerop (aref direction 0))))

(defun frontp (direction)
  (not (zerop (aref direction 1))))


(defmethod print-object ((self stone) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (x y direction) self
      (format stream "~A,~A ~S~%" x y direction)
      (cond
        ((verticalp direction)
         (format stream "\"
     +---+
    /   /|
   +---+ |
   |   | |
   |   | +
   |   |/
   +---+  ~A,~A
\"" x y))

        ((lateralp direction)
         (apply (function format) stream "\"
     +------+
    /      /|
   +------+ +
   |      |/
   +------+ ~A,~A
\""
                (if (minusp (aref direction 0))
                    (list x y)
                    (list (1+ x) y))))

        ((frontp direction)
         (apply (function format) stream "\"
       +---+
      /   /|
     /   / +
    /   / /
   +---+ /
   |   |/
   +---+ ~A,~A
\""
                (if (plusp (aref direction 1))
                    (list x y)
                    (list x (1- y))))))))
  self)



;;;-----------------------------------------------------------------------------
;;;
;;; CELLS
;;;

(defclass cell ()
  ((x :initform 0
      :initarg :x
      :reader cell-x
      :documentation "Lateral coordinate.")
   (y :initform 0
      :initarg :y
      :reader cell-y
      :documentation "Front coordinate."))
  (:documentation "This is an abstract cell. Cells are square, and all of the same size."))

(define-condition game-won  () ())
(define-condition game-lost () ())

(defgeneric stone-moved-over-cell (stone cell)
  (:documentation "Do cell specific behavior when the stone moves over the cell.
May signal a game-won or game-lost condition.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    (values)))

(defgeneric stone-left-cell (stone cell)
  (:documentation "Do cell specific behavior when the stone moves from the cell.
May signal a game-won or game-lost condition.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    (values)))

(defgeneric game-status (stone cell)
  (:documentation "Returns nil :win or :lose depending on what would happen if the stone was on the cell.")
  (:method (stone cell)
    (declare (ignorable stone cell))
    nil))




(defclass solid-cell (cell)
  ()
  (:documentation "The stone may remain securely on a solid cell."))



(defclass target-cell (cell)
  ()
  (:documentation "Once the stone is in vertical position on a target cell,
the game is won."))

(defmethod stone-moved-over-cell (stone (cell target-cell))
  (declare (ignorable stone cell))
  (when (verticalp (stone-direction stone))
    (signal 'game-won)))

(defmethod game-status (stone (cell target-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction stone))
    :win))



(defclass empty-cell (cell)
  ()
  (:documentation "When the stone is over an empty cell,
the game is lost."))

(defmethod stone-moved-over-cell (stone (cell empty-cell))
  (declare (ignorable stone cell))
  (signal 'game-lost))

(defmethod game-status (stone (cell empty-cell))
  (declare (ignorable stone cell))
  :lose)


(defclass button-cell (cell)
  ((switches :initform '()
             :initarg :switches
             :accessor button-cell-switches
             :documentation "A list of cells that may be switched when the stone is over the button cell."))
  (:documentation "This is an abstract button cell.
Button cells may switch the state of pathway-cells."))

(defgeneric switch-pathway-cells (button-cell)
  (:documentation "Switches the associated pathway-cells.")
  (:method ((self button-cell))
    (map nil (function switch-cell) (button-cell-switches self))
    self))



(defclass red-button-cell (button-cell)
  ()
  (:documentation "A red button cell switches its pathway cells
as soon as the stone is over it."))

(defmethod stone-moved-over-cell ((s stone) (cell red-button-cell))
  (declare (ignorable s))
  (switch-pathway-cells cell))



(defclass blue-button-cell (button-cell)
  ()
  (:documentation "A blue button cell switches its pathway cells
only when the stone is over it in vertical position."))

(defmethod stone-moved-over-cell ((s stone) (cell blue-button-cell))
  (when (verticalp (stone-direction s))
    (switch-pathway-cells cell)))



(defclass pathway-cell (cell)
  ((state :initform :closed
          :initarg :state
          :reader pathway-cell-state
          :type (member :closed :open)
          :documentation "A pathway cell may be :open or :closed."))
  (:documentation "When a pathway cell is :open, it supports a stone;
when it's :closed the stone falls down and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell pathway-cell))
  (declare (ignorable s))
  (when (eql :closed (pathway-cell-state cell))
    (signal 'game-lost)))

(defmethod game-status (stone (cell pathway-cell))
  (declare (ignorable stone))
  (when (eql :closed (pathway-cell-state cell))
    :lose))

(defmethod switch-cell ((self pathway-cell))
  (setf (slot-value self 'state) (ecase (pathway-cell-state self)
                                   ((:open)   :closed)
                                   ((:closed) :open)))
  self)



(defclass crumble-cell (cell)
  ((state :initform :open
          :initarg :state
          :reader crumble-cell-state
          :type (member :closed :open)
          :documentation "A crumble cell goes from :open to :closed the first time
it's walked over, and stays :closed thereafter."))
  (:documentation "When a crumble cell is :open, it supports a stone;
when it's :closed the stone falls down and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell crumble-cell))
  (declare (ignorable s))
  (when (eql :closed (crumble-cell-state cell))
    (signal 'game-lost)))

(defmethod stone-left-cell (stone (cell crumble-cell))
  (declare (ignorable stone))
  (setf (slot-value cell 'state) :closed))

(defmethod game-status (stone (cell crumble-cell))
  (declare (ignorable stone))
  (when (eql :closed (crumble-cell-state cell))
    :lose))


(defclass ice-cell (cell)
  ()
  (:documentation "An ice cell supports an horizontal stone, but
when the stone is over it in vertical position, it breaks, the stone falls down, and
the game is lost."))

(defmethod stone-moved-over-cell ((s stone) (cell ice-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction s))
    (signal 'game-lost)))

(defmethod game-status (stone (cell ice-cell))
  (declare (ignorable cell))
  (when (verticalp (stone-direction stone))
    :lose))



;;;-----------------------------------------------------------------------------
;;;
;;; GAME
;;;


(defclass game ()
  ((stone :initform (make-instance 'stone)
          :initarg :stone
          :reader game-stone
          :documentation "The stone.")
   (cells :initform #2a()
          :initarg :cells
          :reader game-cells
          :documentation "The cells.")))


(defgeneric text-icon (cell)
  (:documentation "Returns a three-character strings denoting graphically the cell.")
  (:method ((cell empty-cell))       (declare (ignorable cell)) "   ")
  (:method ((cell solid-cell))       (declare (ignorable cell)) "SSS")
  (:method ((cell red-button-cell))  (declare (ignorable cell)) "[R]")
  (:method ((cell blue-button-cell)) (declare (ignorable cell)) "[B]")
  (:method ((cell ice-cell))         (declare (ignorable cell)) ",,,")
  (:method ((cell target-cell))      (declare (ignorable cell)) "TTT")
  (:method ((cell crumble-cell))
    (declare (ignorable cell))
    (if (eql :open (crumble-cell-state cell))
        "CCC"
        "   "))
  (:method ((cell pathway-cell))
    (declare (ignorable cell))
    (if (eql :open (pathway-cell-state cell))
        "---"
        " / ")))


(defun stone-coverage (stone)
  "
Returns:   direction; left; back; right; front
DIRECTION: (member :vertical :lateral :front)
"
  (let ((direction (cond
                     ((verticalp (stone-direction stone)) :vertical)
                     ((lateralp  (stone-direction stone)) :lateral)
                     (t                                   :front))))
    (if (eql direction :vertical)
        (values direction  (stone-x stone) (stone-y stone) (stone-x stone) (stone-y stone))
        (let* ((x0  (stone-x stone))
               (x1  (+ x0 (aref (stone-direction stone) 0)))
               (y0  (stone-y stone))
               (y1  (+ y0 (aref (stone-direction stone) 1))))
          (values direction (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))))))


(defun print-game (game stream)
  "
Prints an ASCII-art representation of the GAME onto the STREAM.
"
  (let* ((cells (game-cells game))
         (line  (with-output-to-string (out)
                  (loop
                    :initially (princ "+" out)
                    :repeat (array-dimension cells 0)
                    :do (princ "---+" out)))))
    (multiple-value-bind (direction stone-left stone-back stone-right stone-front)
        (stone-coverage  (game-stone game))
      (loop
         :for j :from (1-  (array-dimension cells 1)) :downto 0
         :initially (princ line stream) (terpri stream)
         :do (loop
                :for i :from 0 :below (array-dimension cells 0)
                :initially (princ "|" stream)
                :do (unless (ecase direction
                              ((:vertical)
                               (when (and (= stone-left i) (= stone-back j))
                                 (princ "BBB" stream) (princ "|" stream) t))
                              ((:lateral)
                               (cond
                                 ((and (= stone-left i) (= stone-back j))
                                  (princ "BBBB" stream) t)
                                 ((and (= stone-right i) (= stone-back j))
                                  (princ "BBB" stream) (princ "|" stream) t)))
                              ((:front)
                               (when (and (= stone-left i) (or (= stone-back j)
                                                               (= stone-front j)))
                                 (princ "BBB" stream) (princ "|" stream) t)))
                      (princ (text-icon (aref cells i j)) stream) (princ "|" stream))
                :finally (progn
                           (terpri stream)
                           (if (and (eql direction :front) (= stone-front j))
                               (let ((line (copy-seq line)))
                                 (replace line "BBB" :start1 (+ 1 (* 4 stone-left)))
                                 (princ line stream))
                               (princ line stream))
                           (terpri stream)))))))


(defmethod print-object ((self game) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "\"~%")
    (print-game self stream)
    (format stream "~%\""))
  self)


(defun parse-game (level)
  "
LEVEL:   A CONS whose car is a string drawing the cells, and whose cdr
         is a list of cell definitions.  The string is a multiline
         string, each line containing one character per cell.  The
         character encodes the class of cell (case insensitively):

            . or space:  empty-cell
            S            solid-cell starting position
            O            solid-cell
            T            target-cell
            I            ice-cell
            C            crumble-cell
            other:       the character is searched in the list of cell definitions.

         The list of cell definitions contains sublists of these forms:
            (cell-name  :pathway  :open|:closed)
            (cell-name  :red      . list-of-cell-names)
            (cell-name  :blue     . list-of-cell-names)

         Cell-name is either a character, a single-character string or
         symbol, or a digit (0 from 9).

         :pathway indicates the class of the cell is pathway-cell, and
         the following keyword indicates its initial state.

         :red indicates the class of the cell is red-button-cell;
         :blue indicates the class of the cell is blue-button-cell; in
         both cases, the rest is the list of pathway cell-names
         connected to the button.

         There must be a start position, and all the non empty cells
         must be at least two steps from the borders.
"
  (let* ((lines (split-sequence:split-sequence #\newline (first level)))
         (depth (length lines))
         (width (reduce (function max) lines :key (function length)))
         (cells (make-array (list width depth)))
         (links (rest level))
         (linked-cells '())
         (stone))
    (flet ((cell-key (designator)
             (typecase designator
               ((or symbol string character) (char (string-upcase designator) 0))
               ((integer 0 9) (char (princ-to-string designator) 0))
               (t (error "Invalid reference in level links: ~S" designator)))))
      (loop
         :with start-x :with start-y
         :for line :in lines
         :for j :from (1- depth) :downto 0
         :do (loop
                :for i :from 0 :below width
                :for ch = (if (< i (length line))
                              (aref line i)
                              #\.)
                :do (setf (aref cells i j)
                          (flet ((make-cell (class &rest args)
                                   (unless (eql class 'empty-cell)
                                     (assert (and (< 1 i (- width  2))
                                                  (< 1 j (- depth 2)))
                                             (i j)
                                             "Non empty cells must be at more than two steps from the border."))
                                   (apply (function make-instance) class :x i :y j args)))
                            (case (char-upcase ch)
                              ((#\. #\space)  (make-cell 'empty-cell))
                              ((#\S)          (progn
                                                (setf start-x i
                                                      start-y j)
                                                (make-cell 'solid-cell)))
                              ((#\O)          (make-cell 'solid-cell))
                              ((#\I)          (make-cell 'ice-cell))
                              ((#\C)          (make-cell 'crumble-cell))
                              ((#\T)          (make-cell 'target-cell))
                              (otherwise
                               (let ((link (assoc (char-upcase ch) links :key (function cell-key))))
                                 (if link
                                     (let ((cell
                                            (ecase (second link)
                                              ((:red)     (make-cell 'red-button-cell))
                                              ((:blue)    (make-cell 'blue-button-cell))
                                              ((:pathway) (make-cell 'pathway-cell :state (third link))))))
                                       (push (cons (cell-key ch) cell) linked-cells)
                                       cell)
                                     (error "Invalid character in level map: ~S" ch))))))))
         :finally (progn
                    (unless start-x
                      (error "The level is missing a start position. ~%~S" level))
                    (setf stone (make-instance 'stone :x start-x :y start-y))
                    (loop
                       ;; Put the pathways in the switches list of the buttons.
                       :for (key . cell) :in linked-cells
                       :for link = (assoc key links :key (function cell-key))
                       :do (ecase (second link)
                             ((:red :blue) (dolist (pathway (cddr link))
                                             (pushnew (cdr (assoc (cell-key pathway) linked-cells))
                                                      (button-cell-switches cell))))
                             ((:pathway))))
                    (return (make-instance 'game :stone stone :cells cells)))))))


(defmethod move ((game game) direction)
  "
Moves the stone of the game in the given direction.
"
  (let ((stone (game-stone game))
        (cells (game-cells game)))
    (multiple-value-bind (direction stone-left stone-back stone-right stone-front) (stone-coverage stone)
      (if (eql direction :vertical)
          (stone-left-cell stone (aref cells stone-left stone-back))
          (progn
            (stone-left-cell stone (aref cells stone-left stone-back))
            (stone-left-cell stone (aref cells stone-right stone-front)))))
    (move (game-stone game) direction)
    (multiple-value-bind (direction stone-left stone-back stone-right stone-front) (stone-coverage stone)
      (if (eql direction :vertical)
          (stone-moved-over-cell stone (aref cells stone-left stone-back))
          (progn
            (stone-moved-over-cell stone (aref cells stone-left stone-back))
            (stone-moved-over-cell stone (aref cells stone-right stone-front))))))
  game)


(defun stonedge (level)
  "
Play the playtomo stonedge game for the given LEVEL.
See PARSE-GAME for the description of LEVEL.
"
  (let ((game (parse-game level)))
    (handler-case
        (loop
           (print-game game *query-io*)
           (format *query-io* "Your move: ")
           (block :abort
             (move game
                   (case (char (string-trim #(#\space #\tab) (read-line *query-io*)) 0)
                     ((#\j #\4) :left)
                     ((#\l #\6) :right)
                     ((#\i #\8) :front)
                     ((#\k #\2) :back)
                     (otherwise (return-from :abort))))))
      (game-won  () (format t "~%You win!~2%"))
      (game-lost () (format t "~%You lose!~2%")))
    (values)))


;;;-----------------------------------------------------------------------------
;;;
;;; Solver
;;;

(defgeneric cell-state (cell)
  (:documentation "Return NIL or the state of the cell.")
  (:method (cell)                (declare (ignorable cell)) nil)
  (:method ((cell crumble-cell)) (declare (ignorable cell)) (crumble-cell-state cell))
  (:method ((cell pathway-cell)) (declare (ignorable cell)) (pathway-cell-state cell)))


(defgeneric game-state (game)
  (:documentation "Return a list containing in a concise form the full state of the game.")
  (:method ((game game))
    (let ((cells (game-cells game))
          (stone (game-stone game)))
      (multiple-value-bind (direction stone-left stone-back stone-right stone-front)
          (stone-coverage stone)
        (declare (ignore direction))
        (list* (or (game-status stone (aref cells stone-left  stone-back))
                   (game-status stone (aref cells stone-right stone-front)))
               stone-left stone-back stone-right stone-front
               (loop
                  :for i :from 0 :below (array-total-size cells)
                  :for state = (cell-state (row-major-aref cells i))
                  :when state :collect state))))))



(defgeneric copy (object &key &allow-other-keys)

  (:documentation "Copy the game objects.  Stateless cells are returned uncopied.")

  (:method ((stone stone) &key &allow-other-keys)
    (make-instance 'stone
        :x (stone-x stone)
        :y (stone-y stone)
        :direction (stone-direction stone)))

  (:method ((cell cell) &key &allow-other-keys)
    cell)

  (:method ((cell button-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :switches (button-cell-switches cell)))

  (:method ((cell pathway-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :state (pathway-cell-state cell)))

  (:method ((cell crumble-cell) &key &allow-other-keys)
    (make-instance (class-of cell)
        :x (cell-x cell)
        :y (cell-y cell)
        :state (crumble-cell-state cell)))

  (:method ((game game) &key &allow-other-keys)
    (make-instance 'game
        :stone (copy (game-stone game))
        :cells (loop
                  :with cells    = (com.informatimago.common-lisp.array:copy-array (game-cells game))
                  :with pathways = '()
                  :with buttons  = '()
                  :for i :from 0 :below (array-total-size cells)
                  :for original = (row-major-aref cells i)
                  :for copy     = (copy original)
                  :do (setf (row-major-aref cells i) copy)
                  :do (typecase original
                        (pathway-cell (push (cons original copy) pathways))
                        (button-cell  (push copy                 buttons)))
                  :finally (progn
                             (dolist (button buttons)
                               (setf (button-cell-switches button)
                                     (mapcar (lambda (old) (cdr (assoc old pathways)))
                                             (button-cell-switches button))))
                             (return cells))))))



(defstruct node
  "A node of the graph of the stonedge game."
  state
  game
  path
  visitedp
  startp
  ;; neighbors is nil or a vector of neighbor nodes in (:right :left :front :back) order.
  neighbors)


(defvar *states* (make-hash-table :test (function equal))
  "A hash-table mapping game states to nodes.")


(defun make-graph-from-states (states)
  (let* ((ne (make-hash-table))
         (en (make-hash-table))
         (elements
          (let ((elements '()))
            (maphash
             (lambda (state node)
               (let ((element (make-instance 'element-class :ident state)))
                 (set-property element :path (reverse (node-path node)))
                 (set-property element :dot-label
                               #- (and) (with-output-to-string (out)
                                                     (dolist (line (split-sequence:split-sequence
                                                                    #\newline
                                                                    (with-output-to-string (out)
                                                                      (print-game (node-game node) out))))
                                                       (princ line out)
                                                       (princ "\\n" out)))
                               #+ (and) "x")
                 (set-property element :dot-fill-color (if (node-startp node)
                                                           "Yellow"
                                                           (ecase (first state)
                                                             ((:win)  "Green")
                                                             ((:lose) "Red")
                                                             ((nil)   "White"))))
                 (setf (gethash element en) node)
                 (setf (gethash node ne) element)
                 (push element elements)))
             states)
            elements))
         (graph (make-instance 'graph-class
                    :nodes (make-instance 'set-class :elements elements)
                    :edge-class 'directed-edge)))
    (print (list (length elements)  'elements))
    (dolist (element elements)
      (let ((node (gethash element en)))
        (when (node-neighbors node)
          (loop
             :for successor :across (node-neighbors node)
             :for direction :in '(:right :left :front :back)
             :do (when successor
                   (let ((edge (make-instance 'directed-edge-class
                                   :from element
                                   :to (gethash successor ne))))
                     (set-property edge :dot-label direction)
                     (add-edge graph edge)))))))
    graph))

(defun reset ()
  "Cleans up the *STATES* hash-table."
  (setf  *states* (make-hash-table :test (function equal))))


(defun explore (game path)
  "Walks the game graph, recoding each node in the *STATES* hash-table."
  (let* ((state (game-state game))
         (node  (gethash state *states*)))
    (unless node
      (setf node (setf (gethash state *states*)
                       (make-node :state state
                                  :game game
                                  :path path
                                  :visitedp nil))))
    (unless (node-visitedp node)
      (setf (node-visitedp node) t)
      (unless (first state)
        (setf (node-neighbors node)
              (coerce
               (loop
                  :for move    :in '(:right :left :front :back)
                  :for reverse :in '(:left :right :back :front)
                  :collect (let ((child (copy game)))
                             (move child move)
                             (explore child (cons move path))))
               'vector))))
    node))


(defun print-wins ()
  "Find all the WIN nodes from the *STATES* hash-table, and print thei state and path."
  (maphash (lambda (state node)
             (when (eq :win (first state))
               (print (list state (reverse (node-path node))))))
           *states*))


(defun solve-problem (problem)
  "Solves the playtomo-stonedge game level PROBLEM,
printing the number of states and the win states."
  (time (progn
          (reset)
          (setf (node-startp (explore (parse-game problem) '())) t)
          (print `(number of states = ,(hash-table-count *states*)))
          (print-wins))))



;;;-----------------------------------------------------------------------------

(defparameter *simple*
  '("
...............
...............
......AO.......
....OOOOOO.....
..SOOOOOOO1OT..
...IIOOOOO.....
......IO.......
...............
...............
"
    (a :red     1)
    (1 :pathway :closed)))

(defparameter *level-36*
  '("
...............
...............
......AO.......
....OOOIIO.....
..SOOOICOO1OT..
...IIOIOOO.....
......IO.......
...............
...............
"
    (a :red     1)
    (1 :pathway :closed)))

(defparameter *level-37*
  '("
...........
...........
.....OC....
....CII....
..OO1BS23..
..OR4TCOO..
....COL....
.....O5....
...........
...........
"
    (b :blue 1)
    (r :red  4)
    (l :red  2 3 5)
    (1 :pathway :closed)
    (2 :pathway :closed)
    (3 :pathway :closed)
    (4 :pathway :open)
    (5 :pathway :open)))

(defparameter *level-38*
  '("
.................
.................
..II.IIIICIIOIT..
..II..III.IIIII..
..SOII.II.IIIII..
..IIICOOC........
.................
.................
"))

(defparameter *level-39*
  '("
..............
..............
..IIB1R2CLO...
..IIO.....O...
....O.....O...
....O.....3O..
....O.....OO..
....IO...OTO..
....SO...CO...
.....OCOCOO...
..............
..............
"
    (b :blue 1)
    (r :red  2)
    (l :red  3)
    (1 :pathway :closed)
    (2 :pathway :closed)
    (3 :pathway :closed)))

(defparameter *level-52*
  '("
.........
...RO
...OC
..SBOC1
..OTIO2
..3OCO
..OC


"
    (r :red 3)
    (b :blue 1 2)
    (1 :pathway :closed)
    (2 :pathway :closed)
    (3 :pathway :closed)))

;; (defparameter *game* (parse-game *problem*))
;; (stonedge *level-39*)
;; (solve-problem *problem*) (solve-problem *simple*)
;; (time (progn (reset) (explore (parse-game ) '()) (find-win)))
;;
;; (solve-problem *level-37*)
;; (solve-problem *level-38*)
;; (solve-problem *level-39*)

;; (solve-problem *level-52*)
;;
;; (let ((name "g"))
;;   (with-open-file (dot (format nil "~A.dot" name)
;;                        :direction :output
;;                        :if-does-not-exist :create
;;                        :if-exists :supersede)
;;     (princ (generate-dot (setf *g* (make-graph-from-states *states*))) dot))
;;   (ext:shell (format nil "dot -Tpng -o ~A.png  ~:*~A.dot" name)))

;;;; THE END ;;;;

