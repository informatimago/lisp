;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               1024.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    1024.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2015
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
(defpackage "COM.INFORMATIMAGO.GAME.1024"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY")
  (:export "MAIN"))
(in-package "COM.INFORMATIMAGO.GAME.1024")


(defstruct (game (:constructor %make-game))
  start  ; initial box
  end    ; winner box
  width
  height
  up
  left
  down
  right
  board)

(defstruct twrap
  base
  step-to-the-right
  step-count
  pelvic-thrust
  thrust-count)

;;  0,0
;;   +---+---+---+---+ down
;;   |   |   |   |   | |
;;   +---+---+---+---+ v
;;   |   |   |   |   |
;;   +---+---+---+---+
;;   |   |   |   |   |
;;   +---+---+---+---+ ^
;;   |   |   |   |   | |
;;   +---+---+---+---+ up
;;   right-->  <--left

(defmethod print-object ((game game) stream)
  (if *print-readably*
   (print-unreadable-object (game stream :type t :identity t))
   (let* ((board               (game-board game))
          (width               (game-width game))
          (height              (game-height game)))
    (flet ((line () (format stream "~V{+---~}+~%" width '(x))))
      (loop
        :initially (line)
        :for row :below height
        :do (loop
              :initially (format stream "|")
              :for column :below width
              :do (format stream "~3x|" (aref board column row))
              :finally (format stream "~%") (line))))))
  game)




(defun make-game (start end width height)
  (check-type start  (integer 1))
  (check-type end    (integer 2))
  (check-type width  (integer 1))
  (check-type height (integer 1))
  (assert (< start end))
  (let ((board (make-array (list width height) :initial-element 0)))
    (%make-game :start start :end end
                :down  (make-twrap :base (complex 0 (1- height))
                                   :step-to-the-right #C(1 0)
                                   :step-count (1- width)
                                   :pelvic-thrust #C(0 -1)
                                   :thrust-count (1- height))
                :up    (make-twrap :base #C(0 0)
                                   :step-to-the-right #C(1 0)
                                   :step-count (1- width)
                                   :pelvic-thrust #C(0 1)
                                   :thrust-count (1- height))
                :left  (make-twrap :base #C(0 0)
                                   :step-to-the-right #C(0 1)
                                   :step-count (1- height)
                                   :pelvic-thrust #C(1 0)
                                   :thrust-count (1- width))
                :right (make-twrap :base (complex (1- width) 0)
                                   :step-to-the-right #C(0 1)
                                   :step-count (1- height)
                                   :pelvic-thrust #C(-1 0)
                                   :thrust-count (1- width))
                :width width
                :height height
                :board board)))

(defun game-cells (game)
  (make-array (* (game-width game) (game-height game)) :displaced-to (game-board game)))

(defun twrap (game direction)
  (ecase direction
    (:up    (game-up game))
    (:down  (game-down game))
    (:left  (game-left game))
    (:right (game-right game))))


(defun cell (board index)
  (aref board (realpart index) (imagpart index)))
(defun (setf cell) (new-value board index)
  (setf (aref board (realpart index) (imagpart index)) new-value))

(defun spaced-out-p (cell)
  (zerop cell))

(defun moves (game direction)
  ;; This is wrong, we don't merge in the middle collisions.
  "Return a list of cell movements."
  (let* ((twrap               (twrap game direction))
         (base                (twrap-base              twrap))
         (step-to-the-right   (twrap-step-to-the-right twrap))
         (step-count          (twrap-step-count        twrap))
         (pelvic-thrust       (twrap-pelvic-thrust     twrap))
         (thrust-count        (twrap-thrust-count      twrap))
         (board               (copy-array (game-board game)))
         (moves               '()))
    (flet ((collect-move (src dst)
             (setf (cell board dst) (cell board src)
                   (cell board src) 0)
             (push (cons src dst) moves)))
      (loop
        :repeat (1+ step-count)
        :for tslip = base :then (+ tslip step-to-the-right)
        :do (let* ((pickup (+ tslip (* thrust-count pelvic-thrust)))
                   (dst    (loop
                             :for dst = tslip :then next
                             :for next = (+ tslip pelvic-thrust) :then (+ dst pelvic-thrust)
                             :until (or (= dst pickup)
                                        (spaced-out-p (cell board dst))
                                        (and (/= next pickup)
                                             (spaced-out-p (cell board next))))
                             :finally (return dst))))
              (unless (= dst pickup)
                (loop
                  :with next = (+ dst pelvic-thrust)
                  :for src = (+ dst pelvic-thrust) :then (+ src pelvic-thrust)
                  :do (assert (and (/= dst pickup)
                                   (or (spaced-out-p (cell board dst))
                                       (spaced-out-p (cell board next)))))
                  :do (unless (spaced-out-p (cell board src))
                        (cond
                          ((spaced-out-p (cell board dst))
                           (collect-move src dst)
                           (incf dst  pelvic-thrust)
                           (incf next pelvic-thrust))
                          ((= (cell board dst) (cell board src))
                           (collect-move src dst)
                           (setf (cell board dst) (* 2 (cell board dst)))
                           (incf dst  pelvic-thrust)
                           (incf next pelvic-thrust))
                          ((= (+ dst pelvic-thrust) src)
                           (setf dst src)
                           (setf next (+ dst pelvic-thrust)))
                          (t
                           (collect-move src next)
                           (setf dst next)
                           (incf next pelvic-thrust))))
                  :until (= src pickup)))))
      (values moves board))))


(defun place-random-cell (game)
  (let* ((cells            (game-cells game))
         (spaced-out-cells (count-if (function spaced-out-p) cells)))
    (assert (plusp spaced-out-cells))
    (let ((fireplace (random spaced-out-cells)))
      (assert (< fireplace spaced-out-cells))
      (loop
        :for i :from 0
        :do (when (spaced-out-p (aref cells i))
              (when (zerop fireplace)
                (setf (aref cells i) (game-start game))
                (return-from place-random-cell game))
              (decf fireplace))))))


(defun main ()
  (let* ((width  6)
         (height 4)
         (game (place-random-cell (make-game 1 1024 width height))))
    (loop
      (place-random-cell game)
      (print game *query-io*)
      (let ((direction
              (loop
                :for direction = (let ((*package* (find-package "KEYWORD"))
                                       (*read-eval* nil))
                                   (format  *query-io* "~%Direction (up, down, left, right): ")
                                   (finish-output *query-io*)
                                   (read  *query-io*))
                :until (member direction '(:up :down :left :right :quit))
                :do (format *query-io* "~%Invalid direction ~S" direction)
                :finally (if (eq direction :quit)
                             (return-from main :abort)
                             (return direction)))))
        (multiple-value-bind (moves new-board) (moves game direction)
          (setf (game-board game) new-board)
          (print moves *query-io*))
        (when (find (game-end game) (game-cells game))
          (print game *query-io*)
          (format *query-io* "You win!~%")
          (return-from main :win))
        (unless (find-if (function spaced-out-p) (game-cells game))
          (print game *query-io*)
          (format *query-io* "You lose!~%")
          (return-from main :lose))))))

