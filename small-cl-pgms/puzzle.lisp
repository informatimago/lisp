;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               puzzle.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Simulate a puzzle with n²-1 moving squares.
;;;;
;;;;USAGE
;;;;
;;;;    (load "puzzle.lisp")
;;;;    (com.informatimago.common-lisp.puzzle:main)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-03-13 <PJB> Corrected bugs signaled by
;;;;                     salma tariq <learningbug2004@yahoo.co.in>.
;;;;    2004-03-09 <PJB> Created.
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PUZZLE"
  (:documentation
   "This package simulates a puzzle with n²-1 moving squares.

    This software is in Public Domain.
    You're free to do with it as you please.")
  (:use "COMMON-LISP")
  (:export  "MAIN")
  );;COM.INFORMATIMAGO.COMMON-LISP.PUZZLE
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PUZZLE")


(defclass square ()
  ;; What? A class for a mere integer?  No. We just leave to the reader
  ;; the pleasure to implement an picture-square subclass that would
  ;; display a picture part like on the real puzzle games.
  ((label :accessor label  :initform 0 :initarg :label :type integer)));;square


(defclass puzzle ()
  ((size   :accessor size   :initform 4 :initarg :size :type (integer 2))
   (places :accessor places :initform nil
           :type (simple-array (or null square) (* *)))
   (empty  :accessor empty  :initform nil :type cons)));;puzzle


(defgeneric get-coordinates  (puzzle relative-move))
(defgeneric get-movable-list (puzzle))
(defgeneric move-square      (puzzle x y))
(defgeneric play             (puzzle))



(defmethod initialize-instance ((self puzzle) &rest args)
  (declare (ignore args))
  (call-next-method)
  (let ((places (make-array (list (size self) (size self))
                            :element-type '(or null square)
                            :initial-element nil)))
    (declare (type (simple-array (or null square) (* *)) places))
    (loop with size = (size self)
          for i from 0 below size do
          (loop for j from 0 below size do
                (unless (and (= i (1- size)) (= j (1- size)))
                  (setf (aref places i j)
                        (make-instance 'square :label (1+ (+ (* i size) j)))))))
    (setf (places self) places)
    (setf (empty  self) (cons (1- (size self)) (1- (size self))))
    self));;initialize-instance


(defmethod print-object ((self puzzle) (out stream))
  (let ((width (truncate (1+ (log (1- (* (size self) (size self))) 10)))))
    (format out "~&")
    (loop with size = (size self)
          for i from 0 below size do
          (loop for j from 0 below size do
                (if (aref (places self) i j)
                  (format out " ~VD " width (label (aref (places self) i j)))
                  (format out " ~VA " width "")))
          (format out "~%")))
  (format out "~%")
  self);;print-object


(defmethod get-coordinates ((self puzzle) relative-move)
  (block nil
    (destructuring-bind (x . y) (empty self)
      (case relative-move
        ((:u) (when (< 0 x)                (return (values (1- x) y))))
        ((:d) (when (< x (1- (size self))) (return (values (1+ x) y))))
        ((:l) (when (< 0 y)                (return (values x (1- y)))))
        ((:r) (when (< y (1- (size self))) (return (values x (1+ y)))))
        (otherwise
         (error "Invalid relative move, must be (member :l :r :u :d).")))
      (error "Cannot move empty toward this direction."))));;get-coordinates

    

(defmethod get-movable-list ((self puzzle))
  (mapcan
   (lambda (d) (handler-case
              (multiple-value-bind (x y) (get-coordinates self d)
                (list (list d (aref (places self) x y))))
            (error () nil)))
   '(:l :r :u :d)));;get-movable-list



(defmethod move-square ((self puzzle) (x integer) (y integer))
  (when (and (<= 0 x (1- (size self)))  (<= 0 y (1- (size self))))
    (destructuring-bind (ex . ey) (empty self)
      (psetf (aref (places self) x y)   (aref (places self) ex ey)
             (aref (places self) ex ey) (aref (places self) x y))
      (setf (empty self) (cons x y)))));;move-square


(defmethod play ((self puzzle))
  (loop
   (tagbody
    :loop
    (format t "~&----------------------------------------~%")
     (format t "~A" self)
     (format t "square to move: ")
     (let ((input (let ((*package*
                         (find-package "COM.INFORMATIMAGO.COMMON-LISP.PUZZLE")))
                    ;; To be able to read mere symbols (instead of keywords).
                    (read)))
           (movable (get-movable-list self))
           square x y)
       (typecase input
         (integer
          (let ((m (member input movable
                           :key (lambda (x) (label (second x))) :test (function =))))
            (if m
              (progn
                (setf square (second (car m)))
                (multiple-value-setq (x y)
                    (get-coordinates self (first (car m)))))
              (progn
                (format t "Cannot move square ~D.~%" input)
                (go :loop)))))
         (symbol
          (handler-case
              (progn
                (multiple-value-setq (x y)
                    (get-coordinates
                     self (case input
                            ((:l :left l left)   :l)
                            ((:r :right r right) :r)
                            ((:u :up u up)       :u)
                            ((:d :down d down)   :d)
                            ((:q :quit q quit :exit exit
                              :abort abort :break break)
                             (return-from play))
                            (otherwise input))))
                (setf square (aref (places self) x y)))
            (error (err) (format t "~A~%" err) (go :loop))))
         (otherwise (format t "Invalid input.~%") (go :loop)))
       (move-square self x y)))));;play


(defun main ()
  (format t "~% Size of the puzzle: ")
  (let ((input (read)))
    (typecase input
      (integer
       (unless (<= 2 input 16)  (error "Cannot display such a puzzle.")))
      (otherwise
       (error "Please choose an integer size between 2 and 16 inclusive.")))
    (play (make-instance 'com.informatimago.common-lisp.puzzle
            :size input))));;main


;;;; puzzle.lisp                      -- 2004-03-17 20:09:19 -- pascal   ;;;;
