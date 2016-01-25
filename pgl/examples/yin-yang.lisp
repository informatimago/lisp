;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               yin-yang.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Example taken from https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;    
;;;;    This program draws the Taoist yin-yang symbol at the center of
;;;;    the graphics window.  The height and width of the entire figure
;;;;    are both specified by the constant FIGURE_SIZE.
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.YIN-YANG"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.YIN-YANG")

(defun run (&optional (figure-size 150))
  (let* ((width     521)
         (height    342)
         (win       (make-instance 'window :width 512 :height 342
                                           :title "Yin-Yang"))
         (x (/ width 2))
         (y (/ height 2))
         (r (/ figure-size 2))
         (big-black     (make-instance 'arc :x (- x r) :y (- y r)
                                            :width (* 2 r) :height (* 2 r)
                                            :start -90 :sweep 180
                                            :filled t))
         (small-white   (make-instance 'arc :x (- x (/ r 2)) :y (- y  r)
                                            :width r :height r
                                            :start -90 :sweep 180
                                            :filled t
                                            :color *white*
                                            :fill-color *white*))
         (small-black   (make-instance 'arc :x (- x (/ r 2)) :y y
                                            :width r :height r
                                            :start 90 :sweep 180
                                            :filled t))
         (outer-circle  (make-instance 'arc :x (- x r) :y (- y  r)
                                            :width (* 2 r) :height (* 2 r)
                                            :start 0 :sweep 360)))
    (compound-add win big-black)
    (compound-add win small-white)
    (compound-add win small-black)
    (compound-add win outer-circle)))

;;;; THE END ;;;;
