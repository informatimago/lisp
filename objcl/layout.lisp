;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               layout.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines layout functions, to place graphical objects relative
;;;;    to each others.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-04-06 <PJB> Extracted from FLPL figures.lisp.
;;;                      http://www.informatimago.com/articles/flpl/
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2011
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "OBJCL" "OCLO" "NS"))

(defpackage "COM.INFORMATIMAGO.LAYOUT"
  (:use "COMMON-LISP")
  (:export
   "ABOVE" "BELOW" "BOUNDS"
   "DISTANCE-SQUARED" "DRAW" "FRAME" "LEFT-OF" "MAKE-POINT" "MAKE-RECT"
   "MAKE-SIZE" "ORIGIN" "PILE-DOWN" "PLACE" "POINT-X" "POINT-Y"
   "RECT-BOTTOM" "RECT-HEIGHT" "RECT-HORIZONTAL-CENTER" "RECT-LEFT"
   "RECT-OFFSET" "RECT-ORIGIN" "RECT-RIGHT" "RECT-SIZE" "RECT-TOP"
   "RECT-UNION" "RECT-VERTICAL-CENTER" "RECT-WIDTH" "RECT-X" "RECT-Y"
   "RIGHT-OF" "SIZE-HEIGHT" "SIZE-WIDTH" "SQUARE" "STACK-OBJECTS"
   "STACK-UP" "UNIT-VECTOR" "VECTOR*" "VECTOR+" "VECTOR-" "VECTOR-ABS"
   "VECTOR-ROTATE"))
(in-package "COM.INFORMATIMAGO.LAYOUT")



;;;------------------------------------------------------------
;;; POINT
;;;------------------------------------------------------------
;;; POINTs are used both as vectors and affine points.

(declaim (inline square))

(defun square (x) (* x x))


(declaim (inline make-point point-x point-y))

(defun make-point (&key (x 0.0) (y 0.0)) (ns:make-ns-point x y))
(defun point-x (p) (ns:ns-point-x p))
(defun point-y (p) (ns:ns-point-y p))
(defun (setf point-x) (new-value p) (setf (ns:ns-point-x p) new-value))
(defun (setf point-y) (new-value p) (setf (ns:ns-point-y p) new-value))



(defmethod above ((self ns:ns-point) &optional (offset 0))
  (make-point :x (point-x self) :y (+ (point-y self) offset)))

(defmethod below ((self ns:ns-point) &optional (offset 0))
  (make-point :x (point-x self) :y (- (point-y self) offset)))

(defmethod left-of ((self ns:ns-point) &optional (offset 0))
  (make-point :x (- (point-x self) offset) :y (point-y self)))

(defmethod right-of ((self ns:ns-point) &optional (offset 0))
  (make-point :x (+ (point-x self) offset) :y (point-y self)))



(defun vector+ (a b)
  (make-point :x (+ (point-x b) (point-x a))
              :y (+ (point-y b) (point-y a))))

(defun vector- (a b)
  (make-point :x (- (point-x a) (point-x b))
              :y (- (point-y a) (point-y b))))


(defun distance-squared (p q)
  (+ (square (- (point-x p) (point-x q)))
     (square (- (point-y p) (point-y q)))))

(defun vector-abs (vec)
  (sqrt (+ (square (point-x vec))
           (square (point-y vec)))))

(defun vector* (scalar vec)
  (make-point :x (* scalar (point-x vec))
              :y (* scalar (point-y vec))))

(defun vector-rotate (vec angle)
  (let ((s (sin angle))
        (c (cos angle)))
    (make-point :x (- (* c (point-x vec)) (* s (point-y vec)))
                :y (+ (* s (point-x vec)) (* c (point-y vec))))))

(defun unit-vector (v)
  (vector* (/ (vector-abs v)) v))


;;;------------------------------------------------------------
;;; Generic Functions
;;;------------------------------------------------------------

(defgeneric origin (object)
  (:documentation "The point origin of the coordinates of the ``OBJECT``."))

(defgeneric (setf origin) (new-value object)
  (:documentation "Change the origin of the ``OBJECT``."))

(defgeneric bounds (object)
  (:documentation "
The rectangle surrounding the ``OBJECT``, in the coordinate system
relative to the ``ORIGIN``.
"))

(defgeneric frame (object)
  (:documentation "
The rectangle surrounding the ``OBJECT``, in the coordinate system
where the object is drawn (same coordinate system in which ``ORIGIN`` is
expressed). ::

    (frame object) == (rect-offset (bounds object)
                                   (point-x (origin object))
                                   (point-y (origin object)))

")
  (:method (object)
    (rect-offset (bounds object)
                 (point-x (origin object))
                 (point-y (origin object)))))

(defgeneric place (object point)
  (:documentation "Change the origin of the ``OBJECT`` to be the ``POINT``.")
  (:method (object (to ns:ns-point))
    (setf (origin object) to)
    object))


;;;------------------------------------------------------------
;;; RECT & SIZE
;;;------------------------------------------------------------

(declaim (inline make-size size-width size-height))

(defun make-size (&key (width 0.0) (height 0.0)) (ns:make-ns-size width height))
(defun size-width  (p) (ns:ns-size-width p))
(defun size-height (p) (ns:ns-size-height p))
(defun (setf size-width)  (new-value p) (setf (ns:ns-size-width  p) new-value))
(defun (setf size-height) (new-value p) (setf (ns:ns-size-height p) new-value))



(declaim (inline make-rect  rect-x rect-y rect-width rect-height
                 rect-origin rect-size
                 rect-left rect-bottom rect-right rect-top))

(defun make-rect (&key (x 0.0) (y 0.0) (width 0.0) (height 0.0) origin size)
  (ns:make-ns-rect (if origin (point-x origin) x)
                   (if origin (point-y origin) y)
                   (if size   (size-width  size) width)
                   (if size   (size-height size) height)))

(defun rect-x      (p) (ns:ns-rect-x      p))
(defun rect-y      (p) (ns:ns-rect-y      p))
(defun rect-width  (p) (ns:ns-rect-width  p))
(defun rect-height (p) (ns:ns-rect-height p))
(defun (setf rect-x)      (new-value p) (setf (ns:ns-rect-x      p) new-value))
(defun (setf rect-y)      (new-value p) (setf (ns:ns-rect-y      p) new-value))
(defun (setf rect-width)  (new-value p) (setf (ns:ns-rect-width  p) new-value))
(defun (setf rect-height) (new-value p) (setf (ns:ns-rect-height p) new-value))

(defun rect-origin (r) (make-point :x (rect-x r) :y (rect-y r)))
(defun rect-size   (r) (make-size :width (rect-width r) :height (rect-height r)))

(defun (setf rect-origin) (point r)
  (setf (rect-x r) (point-x point)
        (rect-y r) (point-y point))
  point)

(defun (setf rect-size)   (size  r)
  (setf (rect-width  r) (size-width  size)
        (rect-height r) (size-height size))
  size)

(defun rect-left   (r) (rect-x r))
(defun rect-bottom (r) (rect-y r))
(defun rect-right  (r) (+ (rect-x r) (rect-width  r)))
(defun rect-top    (r) (+ (rect-y r) (rect-height r)))
(defun rect-horizontal-center (r) (+ (rect-x r) (/ (rect-width  r) 2)))
(defun rect-vertical-center   (r) (+ (rect-y r) (/ (rect-height r) 2)))

(defun rect-union (a b)
  (let ((x  (min (rect-left a) (rect-left b)))
        (y  (min (rect-bottom a) (rect-bottom b))))
   (make-rect :x x
              :y y
              :width  (- (max (rect-right a) (rect-right b)) x)
              :height (- (max (rect-top   a) (rect-top   b)) y))))

(defun rect-offset (r dx dy)
  (make-rect  :x (+ dx (rect-x r))
              :y (+ dy (rect-y r)) 
              :width  (rect-width r)
              :height (rect-height r)))





(defmethod origin ((self ns:ns-rect))
  (rect-origin self))

(defmethod (setf origin) (new-value (self ns:ns-rect))
  (setf (rect-origin self) new-value))

(defmethod frame ((self ns:ns-rect))
  self)


(defmethod bounds ((self ns:ns-rect))
  (make-rect :size (rect-size self)))


(defmethod place ((self ns:ns-rect) (to ns:ns-point))
  (setf (rect-origin self) to)
  self)


(defmethod above ((self ns:ns-rect) &optional (offset 0))
  (make-point :x (rect-left self)
              :y (+ (rect-top self) offset)))

(defmethod below ((self ns:ns-rect) &optional (offset 0))
  (make-point :x (rect-left self)
              :y (- (rect-bottom self) offset)))

(defmethod left-of ((self ns:ns-rect) &optional (offset 0))
  (make-point :x (- (rect-left self) offset)
              :y (rect-bottom self)))

(defmethod right-of ((self ns:ns-rect) &optional (offset 0))
  (make-point :x (+ (rect-right self) offset)
              :y (rect-bottom self)))



#-(and)
(defmethod draw ((self ns:ns-rect))
  (let ((left   (rect-left   self))
        (right  (rect-right  self))
        (top    (rect-top    self))
        (bottom (rect-bottom self)))
    (move-to left bottom)
    (line-to left top)
    (line-to right top)
    (line-to right bottom)
    (line-to left bottom)
    (close-subpath)
    (stroke)))




(defun stack-objects (objects &key (direction :up) (align :left) (spacing 0))
  "
Stack up or down the ``OBJECTS`` based on the position of the first one.
"
  (when objects
    (let* ((frame (frame (first objects)))
           (x  (ecase align
                 (:left   (rect-left              frame))
                 (:right  (rect-right             frame))
                 (:center (rect-horizontal-center frame))))
           (y  (ecase direction
                 (:up   (rect-top    frame))
                 (:down (rect-bottom frame)))))
      (loop
         :for object :in (rest objects)
         :for frame = (frame object)
         :do (when (eq direction :down)
               (decf y (+ spacing (rect-height frame))))
         :do (place object (ecase align
                             (:left   (make-point :x x                              :y y))
                             (:right  (make-point :x (- x (rect-width frame))       :y y))
                             (:center (make-point :x (- x (/ (rect-width frame) 2)) :y y))))
         :do (when (eq direction :up)
               (incf y (+ spacing (rect-height frame)))))))
  objects)

(defun stack-up (objects &key (align :left) (spacing 0))
  (stack-objects objects :direction :up :align align :spacing spacing))

(defun pile-down (objects &key (align :left) (spacing 0))
  (stack-objects objects :direction :down :align align :spacing spacing))


;;;; THE END ;;;;
