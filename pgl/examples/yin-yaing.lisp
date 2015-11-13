;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               yarn-pattern.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Example taken from https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;    
;;;;    This program illustrates the use of the GLine class to simulate
;;;;    winding a piece of colored yarn around a set of pegs equally
;;;;    spaced along the edges of the canvas.  At each step, the yarn is
;;;;    stretched from its current peg to the one DELTA pegs further on.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.YARN-PATTERN"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.YARN-PATTERN")

(defvar *n-across* 50)
(defvar *n-down*   30)
(defvar *peg-sep*  10)
(defvar *delta*    67)

(defun create-peg-list (across down peg-sep)
  (coerce (nconc (loop :for i :below across
                       :collect (make-point :x (* i peg-sep)))
                 (loop :for i :below down
                       :collect (make-point :x (* across peg-sep) :y (* i peg-sep)))
                 (loop :for i :from across :above 0
                       :collect (make-point :x (* i peg-sep) :y (* down peg-sep)))
                 (loop :for i :from down :above 0
                       :collect (make-point  :y (* i peg-sep))))
          'vector))

(defun run ()
  (let* ((width     521)
         (height    342)
         (win       (make-instance 'window :width 512 :height 342
                                           :title "Yarn Pattern"))
         ;; (cx        (/ width 2))
         ;; (cy        (/ height 2))
         (cx 0)
         (cy 0)
         (pegs      (create-peg-list *n-across* *n-down* *peg-sep*))
         (pegs.size (length pegs))
         (this-peg  0)
         (next-peg  -1))
    (loop :repeat pegs.size ; :while (or (plusp this-peg) (minusp next-peg))
          :do (setf next-peg (mod (+ this-peg *delta*) pegs.size))
              (let* ((p0   (aref pegs this-peg))
                     (p1   (aref pegs next-peg))
                     (line (make-instance 'line :x0 (+ cx (x p0))
                                                :y0 (+ cy (y p0))
                                                :x1 (+ cx (x p1))
                                                :y1 (+ cy (y p1))
                                                :color *magenta*)))
                (compound-add win line))
              (setf this-peg next-peg))))

;;;; THE END ;;;;
