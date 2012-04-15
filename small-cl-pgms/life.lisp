;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               life.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Conway's Life Game.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
;;;;****************************************************************************

(defstruct (world (:constructor %make-world))
  current
  next)

  
(defun make-world (width height)
  (flet ((make-plane (width height)
           (make-array (list width height)
                       :element-type 'bit
                       :initial-element 0)))
    (%make-world
     :current (make-plane  width height)
     :next    (make-plane  width height))))


(defun sum-neighbors (plane i j)
  (let ((width  (array-dimension plane 0))
        (height (array-dimension plane 1)))
    (+ (aref plane (mod (- i 1) width)  (mod (- j 1) height))
       (aref plane (mod (- i 1) width)  j)
       (aref plane (mod (- i 1) width)  (mod (+ j 1) height))
       (aref plane i                    (mod (- j 1) height))
       (aref plane i                    (mod (+ j 1) height))
       (aref plane (mod (+ i 1) width)  (mod (- j 1) height))
       (aref plane (mod (+ i 1) width)  j)
       (aref plane (mod (+ i 1) width)  (mod (+ j 1) height)))))
       

(defun simple-sum-neighbors (plane i j)
  (let ((width  (array-dimension plane 0))
        (height (array-dimension plane 1)))
    (+ (aref plane (- i 1)  (- j 1))
       (aref plane (- i 1)  j)
       (aref plane (- i 1)  (+ j 1))
       (aref plane i        (- j 1))
       (aref plane i        (+ j 1))
       (aref plane (+ i 1)  (- j 1))
       (aref plane (+ i 1)  j)
       (aref plane (+ i 1)  (+ j 1)))))


(defun life-step (world)
  (loop
     with old = (world-current world)
     with new = (world-next    world)
     for i from 1 below (1- (array-dimension old 0))
     do (loop for j from 1 below (1- (array-dimension old 1))
           do (setf (aref new i j)
                    (if (zerop (aref old i j))
                        (if (= 3  (simple-sum-neighbors old i j)) 1 0)
                        (if (<= 2 (simple-sum-neighbors old i j) 3) 1 0)))))
  (loop
     with old = (world-current world)
     with new = (world-next    world)
     for i from 0 below (array-dimension old 0)
     do
     (let ((j 0))
       (setf (aref new i j)
             (if (zerop (aref old i j))
                 (if (= 3  (sum-neighbors old i j)) 1 0)
                 (if (<= 2 (sum-neighbors old i j) 3) 1 0))))
     (let ((j (1- (array-dimension old 1))))
       (setf (aref new i j)
             (if (zerop (aref old i j))
                 (if (= 3  (sum-neighbors old i j)) 1 0)
                 (if (<= 2 (sum-neighbors old i j) 3) 1 0)))))
  (loop      
     with old = (world-current world)
     with new = (world-next    world)
     for j from 1 below (1- (array-dimension old 1))
     do
     (let ((i 0))
       (setf (aref new i j)
             (if (zerop (aref old i j))
                 (if (= 3  (sum-neighbors old i j)) 1 0)
                 (if (<= 2 (sum-neighbors old i j) 3) 1 0))))
     (let ((i (1- (array-dimension old 0))))
       (setf (aref new i j)
             (if (zerop (aref old i j))
                 (if (= 3  (sum-neighbors old i j)) 1 0)
                 (if (<= 2 (sum-neighbors old i j) 3) 1 0)))))
  (rotatef (world-current world) (world-next world))
  world)
                  

(defun set-random (world)
  (loop
     with plane = (world-current world)
     for i from 0 below (array-dimension plane 0)
     do (loop for j from 0 below (array-dimension plane 1)
           do (setf (aref plane i j) (random 2))))
  world)


(defun print-world (world)
  (loop
     with old = (world-current world)
     for j  below (array-dimension old 1)
     do (loop for i below (array-dimension old 0)
           do (princ (aref ".o" (aref old i j)))
           finally (terpri)))
  world)


(defun terminal-size ()
  #+clisp (let ((s (ext:run-program "stty"
                     :arguments '("size") :output :stream)))
            (nreverse (list (1- (read s)) (1- (read s)))))
  #-clisp (list 78 23))


(defun random-game ()
  (let ((world (apply (function make-world) (terminal-size))))
    (set-random world)
    (format t "~Cc" (code-char 27))
    (loop
       (format t "~C[0;0H" (code-char 27)) ; CUP
       (print-world (life-step world))
       (finish-output))))

