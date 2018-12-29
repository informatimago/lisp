;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               angles.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-02-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.ANGLES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.ANGLES")



(defstruct problem
  points
  angles)

(defun problem-n (p)
  (length (problem-points p)))

(defun point (p i)
  (aref (problem-points p) (mod i (problem-n p))))

(defun point-x (p) (realpart p))
(defun point-y (p) (imagpart p))

(defun angle (p q r)
  "Angle between QP and QR"
  (phase (/ (- r q)
            (- p q))))

(defun compute-angles (p)
  (loop
    :with n := (problem-n p)
    :with r := (make-array n)
    :for i :from -1
    :for j :from  0 :below n
    :for k :from  1
    :do (setf (aref r j) (angle (point p i) (point p j) (point p k)))
    :finally (return r)))

(defun create-points (npoints)
  (loop :with r := (cis (/ (* 2.0d0 pi) npoints))
        :with v := (make-array npoints)
        :for i :below npoints
        :for p := #C(1.0d0 0.0d0) :then (* p r)
        :do (setf (aref v i) p)
        :finally (return v)))

(defun symetric (p c)
  (- (+ c c) p))

;; (compute-angles (make-problem :points (create-points 19)))

(defparameter *size*       50)
(defparameter *point-size*  5)

(defun make-display-point (point cx cy)
  (let ((x (+ cx (round (point-x point))))
        (y (+ cy (round (point-y point)))))
    (make-instance 'oval :x x :y y :width *point-size* :height *point-size* :filled t)))

(defmacro dovector ((var vector &optional result) &body body)
  (let ((vvector (gensym "vector"))
        (vindex  (gensym "index"))
        (vlength (gensym "length")))
    `(block nil
       (let* ((,vvector ,vector)
              (,vlength (length ,vvector))
              (,vindex  -1))
         (tagbody
            (go :test)
          :loop
            (let ((,var (aref ,vvector ,vindex)))
              ,@body)
          :test
            (incf ,vindex)
            (if (< ,vindex ,vlength)
                (go :loop))
            (return ,result))))))

(defun square (x) (* x x))

(defun closest-point (x0 y0 ovals)
  (loop
    :with offset := (/ *point-size* 2)
    :with min-o := nil
    :with min-d := nil
    :for o :across ovals
    :for x1 := (+ (x o) offset)
    :for y1 := (+ (y o) offset)
    :for d := (sqrt (+ (square (- x0 x1)) (square (- y0 y1))))
    :do (when (or (null min-d)
                  (< d min-d))
          (setf min-d d
                min-o o))
    :finally (return min-o)))


(defun run (&optional (npoints 3))
  (let* ((prob   (make-problem :points (map 'vector (lambda (p) (* p *size*)) (create-points npoints))))
         (win    (make-instance 'window :width 512 :height 342 :title "Problem"
                                        :resizable t))
         (cx     (truncate (width  win) 2))
         (cy     (truncate (height win) 2))
         (ovals  (map 'vector (lambda (point)
                                (let ((oval (make-display-point point cx cy)))
                                  (compound-add win oval)
                                  oval))
                   (problem-points prob))))
    (loop
      :with state := 'select-center
      :with selected := nil
      :for event := (get-next-event (logior +click-event+ +window-event+))
      :do (case (event-type-keyword event)
            (:window-closed
             (print :window-closed)
             (loop-finish))
            (:window-resized
             (print :window-resized)
             (let* ((nx     (truncate (width  win) 2))
                    (ny     (truncate (height win) 2))
                    (dx     (- nx cx))
                    (dy     (- ny cy)))
               (setf cx nx
                     cy ny)
               (dovector (o ovals)
                 (let ((p (location o)))
                   (print (list (+ (x p) dx) (+ (y p) dy)))
                   (set-location o (+ (x p) dx) (+ (y p) dy))))))
            (:mouse-clicked
             (print (list :mouse-clicked  (event-x event) (event-y event)))
             (let ((o (closest-point (event-x event) (event-y event) ovals)))
               (when o
                 (case state
                   ((select-center)
                    (setf selected o)
                    (set-color selected *red*)
                    (print selected)
                    (setf state 'select-permuted))
                   ((select-permuted)
                    (unless (eql selected o)
                      (print o)
                      (let* ((si  (position selected ovals))
                             (so  (position o        ovals))
                             (new (symetric (point prob so) (point prob si))))
                        (setf (aref (problem-points prob) so) new)
                        (set-location o (+ cx (truncate (point-x new))) (+ cy (truncate (point-y new))))
                        (print (list (+ cx (truncate (point-x new))) (+ cy (truncate (point-y new)))))))
                    (set-color selected *black*)
                    (setf selected nil)
                    (setf state 'select-center)))))))))
  (close-backend))


;;;; THE END ;;;;
