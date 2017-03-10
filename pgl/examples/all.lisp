;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               all.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Main examples driver.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-03-10 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.PGL.EXAMPLES"
  (:use "COMMON-LISP")
  (:export "RUN-ALL"))
(in-package "COM.INFORMATIMAGO.PGL.EXAMPLES")

(defun run-all ()
  (setf com.informatimago.pgl:*program-name* "Examples")
  (unwind-protect
       (progn
         (ignore-errors (com.informatimago.portable-graphics-library.example.yarn-pattern:run))
         (ignore-errors (com.informatimago.portable-graphics-library.example.yin-yang:run))
         (ignore-errors (com.informatimago.portable-graphics-library.example.checkerboard:run))
         (ignore-errors (com.informatimago.portable-graphics-library.example.felt-board:run))
         (ignore-errors (com.informatimago.portable-graphics-library.example.image:run
                         (merge-pathnames #P"tierra-desde-luna.jpg"
                                          (or *compile-file-pathname* *load-pathname))))
         #+bordeaux-threads
         (bt:make-thread
          (lambda ()
            (ignore-errors (com.informatimago.portable-graphics-library.example.ball:run))))
         #-bordeaux-threads
         (ignore-errors (com.informatimago.portable-graphics-library.example.ball:run)))
    (com.informatimago.pgl:close-backend))
  (values))


;;;; THE END ;;;;
