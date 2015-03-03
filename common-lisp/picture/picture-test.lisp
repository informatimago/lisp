;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               picture-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test picture.lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-25 <PJB> Extracted from picture.lisp
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE.TEST")


(defun test-picture-object ()
  "
DO:      Creates a test picture.
RETURN:  the picture string.
"
  (let ((p (make-instance 'picture :width 40 :height 20 :background ".")))
    (frame-rect p 0 0 39 19)

    (draw-string p 21 10 "East")
    (draw-string p 19 10 "West"  :direction :w)
    (draw-string p 20 11 "North" :direction :n)
    (draw-string p 20  9 "South" :direction :s)
    (draw-string p 21 11 "North-East" :direction :ene)
    (draw-string p 19 11 "North-West" :direction :wnw)
    (draw-string p 21  9 "South-East" :direction :ese)
    (draw-string p 19  9 "South-West" :direction :wsw)

    (erase-rect p 2 10 5 3)
    (fill-rect p 30 10 6 4 :foreground ":")
    (frame-rect p 30 10 6 4 :top-left "/" :top-right "\\" 
                :bottom-left "\\" :bottom-right "/" 
                :left "<" :top "^" :bottom "v" :right ">")
    (draw-line p 0 0 40 20 :foreground "$")
    (draw-point  p 20 10 "*")
    (prin1-to-string p)))


(define-test test/picture-object ()
 (assert-true (string= (test-picture-object)
                       "...s.................................s$$
+----e-----------------------------a$$+.
|......W.........................E$$..|.
|........-.....................-$$....|.
|..........h........h........h$$......|.
|............t......t......t$$........|.
|..............r....r....r$$../^^^^\\..|.
|................o..o..o$$....<::::>..|.
|..................NNN$$......<::::>..|.
|...............tseW*$ast.....\\vvvv/..|.
|.................$$SS................|.
|...............$$..o..o..............|.
|.............$$....u....u............|.
|...........$$......t......t..........|.
|.........$$........h........h........|.
|.......$$.....................-......|.
|.....$$.........................E....|.
|...$$.............................a..|.
|.$$.................................s|.
$$------------------------------------+t
")))


(define-test test/all ()
  (test/picture-object))

;;;; THE END ;;;;




