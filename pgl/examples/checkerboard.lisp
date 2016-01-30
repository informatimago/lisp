;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               checkerboard.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Example taken from https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;    
;;;;    This program draws a checkerboard.  The dimensions of the
;;;;    checkerboard is specified by the constants NROWS and
;;;;    NCOLUMNS, and the size of the squares is chosen so
;;;;    that the checkerboard fills the available vertical space.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.CHECKERBOARD"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.CHECKERBOARD")


(defun run (&optional (nrows 8) (ncolumns 8))
  (let* ((width 512)
         (height 342)
         (win  (make-instance 'window :width width :height height
                                      :title "Checkerboard"))
         (sqsize (/ height nrows)))
    (loop :for i :below nrows
          :for y := (* i sqsize)
          :do (loop :for j :below ncolumns
                    :for x := (* j sqsize)
                    :for sq := (make-instance 'rect :x x :y y :width sqsize :height sqsize)
                    :do (set-filled sq (plusp (mod (+ i j) 2)))
                        (compound-add win sq)))))

;;;; THE END ;;;;
