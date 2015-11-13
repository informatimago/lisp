;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               image.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Example taken from https://cs.stanford.edu/people/eroberts/jtf/tutorial/UsingTheGraphicsPackage.html
;;;;    
;;;;    This program displays an image file give in argument.
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
(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.IMAGE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.IMAGE")

(defun run (image-filename)
  (let* ((image (make-instance 'image :filename (namestring image-filename)))
         (width     (width image))
         (height    (height image))
         (win       (make-instance 'window :width width :height height
                                           :title (file-namestring image-filename))))
    (compound-add win image)))

;;;; THE END ;;;;
