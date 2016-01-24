;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               felt-board.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Example taken from https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;    
;;;;    This program offers a simple example of the acm.graphics package
;;;;    that draws a red rectangle and a green oval.  The dimensions of
;;;;    the rectangle are chosen so that its sides are in proportion to
;;;;    the "golden ratio" thought by the Greeks to represent the most
;;;;    aesthetically pleasing geometry. 
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.FELT-BOARD"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.FELT-BOARD")


(defun run ()
  (let* ((phi  1.618)
         (win  (make-instance 'window :width 512 :height 342
                              :title "Felt Board"))
         (rect (make-instance 'rect :x 100 :y  50               :width 100 :height (/ 100 phi)))
         (oval (make-instance 'oval :x 150 :y (+ 50 (/ 50 phi)) :width 100 :height (/ 100 phi))))
    (set-filled rect t)
    (set-color rect *orange*)
    (set-fill-color rect *red*)
    (compound-add win rect)
    (set-filled oval t)
    (set-color oval *blue*)
    (set-fill-color oval *green*)
    (compound-add win oval)))

;;;; THE END ;;;;
