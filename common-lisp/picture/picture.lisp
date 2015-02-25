;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              picture.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-13 <PJB> Removed the PICTURE- interface.
;;;;    2004-08-13 <PJB> Added multiline string support to DRAW-STRING.
;;;;    2003-01-08 <PJB> Added SPRITE and PICTURE classes.
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;    - In sprites there should be one spot per frame.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export
   "DRAW-ON-PICTURE" "SET-DATA" "FRAMES" "HEIGHT" "WIDTH" "TRANSPARENT"
   "SPOT-Y" "SPOT-X" "DATA" "NAME" "SPRITE" "TO-STRING" "FRAME-RECT"
   "ERASE-RECT" "FILL-RECT" "DRAW-ARROW" "DRAW-LINE" "DRAW-STRING" "SIZE-STRING"
   "DRAW-POINT" "POINT-AT" "HEIGHT" "WIDTH" "BACKGROUND" "PICTURE")
  (:documentation
   "
This package exports functions to draw ASCII-ART pictures.

ASCII-ART primitives.

A picture is a matrix of characters.
There are primitives to draw points, lines, rectangles, 
circles and ellipses, and strings.

The coordinate system is the direct one: 
- x increases toward the right,
- y increases toward the top. Bottom left is (0,0).

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2002 - 2012
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE.PICTURE")



(defgeneric to-string (pict)
  (:documentation
     "
RETURN:  A string containing the picture characters, (pict height) lines
         of (pict width) characters.
"))


(defgeneric point-at (pict x y)
  (:documentation  "
PRE:     inside = (AND (<= 0 X) (< X (WIDTH PICT)) (<= 0 Y) (< Y (HEIGHT PICT)))
RETURN:  inside       ==> The character at coordinate (X,Y).
         (NOT inside) ==> (BACKGROUND PICT)
"))


(defgeneric draw-point (pict x y foreground)
  (:documentation  "
PRE:    inside = (AND (<= 0 X) (<= 0 Y) (< Y (HEIGHT PICT)) (< X (WIDTH PICT)))
        old-point = (POINT-AT PICT X Y)
POST:   inside       ==> (EQ FOREGROUND (POINT-AT PICT X Y))
        (NOT inside) ==> (EQ old-point (POINT-AT PICT X Y))
RETURN: PICT
"))


(defgeneric size-string (pict string &key direction)
  (:documentation   "
RETURN:  left bottom width height of the rectangle in which the
         STRING will be drawn by DRAW-STRING, relative to the point
         where it'll drawn.
"))


(defgeneric draw-string (pict x y string &key direction)
  (:documentation   "
PRE:     (MEMBER DIRECTION '(:E :W :N :S :NE :NW :SE :SW
                           :NNE :NNW :SSE :SSW :ENE :ESE :WNW :WSW
                           :LEFT :RIGHT :UP :DOWN NIL) :TEST (FUNCTION 'EQ))
DO:      Draws the STRING in the given DIRECTION (default :RIGHT = :E).
         STRING may be anything, it will be formated with ~A.
         If it contains +NEW-LINE+ characters then it's split and
         each line is written ''under'' the other, according to the DIRECTION.
RETURN:  PICT
"))


(defgeneric draw-line (pict x y w h &key foreground)
  (:documentation   "
DO:      Draw a line between (x,y) and (x+w-1,y+h-1) 
         with the foreground character.
RETURN:  PICT
"))


(defgeneric draw-arrow (pict x y w h &key tail)
  (:documentation "
DO:      Draw a line between (x,y) and (x+w-1,y+h-1) and end it with
         with an arrow tip. If TAIL is specified, draw it at the
         start.
RETURN:  PICT
"))


(defgeneric fill-rect (pict x y w h &key foreground)
  (:documentation   "
DO:      Fills the specified rectangle with FOREGROUND.
RETURN:  PICT
"))


(defgeneric erase-rect (pict x y w h)
  (:documentation   "
DO:      Fills the specified rectangle with (BACKGROUND PICT).
RETURN:  PICT
"))


(defgeneric frame-rect (pict x y w h &key top-left top-right bottom-left bottom-right top bottom left right)
  (:documentation   "
DO:      Draws the frame of a rect parallel to the axis
         whose diagonal is [(x,y),(x+w-1,y+h-1)].
RETURN:  PICT
"))


(defgeneric width (object)
  (:documentation   "
RETURN:  The width of the object.
"))


(defgeneric height (object)
  (:documentation   "
RETURN:  The height of the object.
"))


(defgeneric frames (sprite)
  (:documentation "
RETURN:  The number of frames in the SPRITE.
"))


(defgeneric set-data (sprite data)
  (:documentation   "
DATA may be either:
       - a single string with frames separated by FF and lines separated by LF,
       - a list of string frames with lines separated by LF,
       - a list of list of string lines.
       - a list of list of list of single character strings or symbols
         or characters or character codes.
       - a tri-dimentional array of characters.
RETURN: SPRITE
"))

(defgeneric draw-on-picture (sprite pict x y &optional frame)
  (:documentation   "
DO:      Draws the frame FRAME of the SPRITE on the picture PICT,
         placing the spot of the sprite at coordinates (X,Y).
         Transparent pixels are not drawn.
"))


(defvar +form-feed+ (make-string 1 :initial-element (code-char 12))
  "A string containing a single +form-feed+ character.")

(defvar +new-line+  (make-string 1 :initial-element (code-char 10))
  "A string containing a single +new-line+ character.")

;; ------------------------------------------------------------------------
;; PICTURE
;; ------------------------------------------------------------------------

(defgeneric background (picture)
  (:documentation "The background character of the picture."))

(pjb-defclass picture nil
  (:att data        (array character 2)       "Picture data.")
  (:att background  character (character " ") "The background character.")
  (:doc "A picture is a bi-dimentional (y,x) array of characters."))


(defun to-char (stuff)
  (cond ((characterp stuff) stuff)
        ((stringp    stuff) (aref stuff 0))
        ((symbolp    stuff) (aref (symbol-name stuff) 0))
        ((numberp    stuff) (code-char stuff))
        (t (error "~S IS AN INVALID CHARACTER!" stuff))))


(defmethod initialize-instance ((self picture) 
                                &key (width 72) (height 24) (background " "))
  "
RETURN:  SELF
POST:    for all X in [0..WIDTH-1], for all Y in [0..HEIGHT-1],
           (EQ (to-char background) (point-at self x y))
"
  (setf width  (truncate width)
        height (truncate height)
        (background self)  (to-char background)
        (data self) (make-array (list (truncate height)
                                      (truncate width))
                                :element-type 'character
                                :initial-element (background self)))
  self)


(defmethod to-string ((self picture))
  "
RETURN:  A string containing the picture character (pict height) lines
         of (pict width) characters, separated by a +new-line+.
"
  (loop
     :with str = (make-string (* (height self) (1+ (width self))))
     :with data = (data self)
     :with i = 0
     :with nl = (to-char +new-line+)
     :for y :from (1- (height self)) :downto 0
     :do (progn
          (loop for x from 0 below (width self)
             do
             (setf (aref str i) (aref data y x))
             (setf i (1+ i)))
          (setf (aref str i) nl)
          (setf i (1+ i)))
     :finally (return str)))



(defmethod print-object ((self picture) stream)
  (princ (to-string self) stream)
  self)


(defmethod width ((self picture))
  "
RETURN:  The width of the picture.
"
  (array-dimension (data self) 1))


(defmethod height ((self picture))
  "
RETURN:  The height of the picture.
"
  (array-dimension (data self) 0))


(defmethod point-at ((self picture) (x number) (y number))
  "
PRE:     inside = (AND (<= 0 X) (< X (WIDTH SELF)) (<= 0 Y) (< Y (HEIGHT SELF)))
RETURN:  inside       ==> The character at coordinate (X,Y).
         (NOT inside) ==> (BACKGROUND SELF)
"
  (setq x (truncate x) y (truncate y))
  (if (and (<= 0 x) (<= 0 y) (< y (height self)) (< x (width self)))
      (aref (data self) y x)
      (background self)))


(defmethod draw-point ((self picture) (x number) (y number) foreground)
  "
PRE:    inside = (AND (<= 0 X) (<= 0 Y) (< Y (HEIGHT SELF)) (< X (WIDTH SELF)))
        old-point = (POINT-AT SELF X Y)
POST:   inside       ==> (EQ FOREGROUND (POINT-AT SELF X Y))
        (NOT inside) ==> (EQ old-point (POINT-AT SELF X Y))
RETURN: SELF
"
  (setq x (truncate x) y (truncate y))
  (setq foreground (to-char foreground))
  (when (and (<= 0 x) (<= 0 y) (< y (height self)) (< x (width self)))
    (setf (aref (data self) y x) foreground))
  self)


(defstruct (deplacement (:type list))  name dx dy line-dx line-dy)
(defvar +deplacements+ '((:e      1  0  0 -1) 
                         (:right  1  0  0 -1) 
                         (:w     -1  0  0  1) 
                         (:left  -1  0  0  1) 
                         (:n      0  1  1  0) 
                         (:up     0  1  1  0) 
                         (:s      0 -1 -1  0) 
                         (:down   0 -1 -1  0) 
                         (:ne     1  1  1 -1) 
                         (:nw    -1  1  1  1) 
                         (:se     1 -1 -1 -1) 
                         (:sw    -1 -1 -1  1) 
                         (:nne    1  2  2 -1) 
                         (:nnw   -1  2  2  1) 
                         (:sse    1 -2 -2 -1) 
                         (:ssw   -1 -2 -1  2) 
                         (:ene    2  1  1 -2) 
                         (:ese    2 -1 -1 -2) 
                         (:wnw   -2  1  1  2) 
                         (:wsw   -2 -1 -1  2)))


(defvar *string-cache* nil "PRIVATE")


(defun string-cache-split (string separator)
  "PRIVATE"
  (unless (and (eq string    (first  *string-cache*))
               (eq separator (second *string-cache*)))
    (setf *string-cache* (list string separator 
                               (split-string string separator))))
  (third *string-cache*))


(defmethod size-string ((self picture) string &key (direction :e))
  "
RETURN:  left bottom width height of the rectangle in which the
         STRING will be drawn by DRAW-STRING, relative to the point
         where it'll drawn.
"
  (declare (ignore self))
  (labels ( ;; (sign (mag)     (cond ((= 0 mag) 0) ((< mag 0) -1) (t +1)))
           (off  (amp dep) (* (1- amp) dep)))
    (let* ((depl   (assoc direction +deplacements+))
           (lines  (string-cache-split string +new-line+))
           (width  (reduce (function max) lines :key (function length) 
                           :initial-value 0))
           (height (length lines))
           (lb-x   0)
           (lb-y   0)
           (lt-x   (off height (deplacement-line-dx depl)))
           (lt-y   (off height (deplacement-line-dy depl)))
           (rb-x   (off width  (deplacement-dx      depl)))
           (rb-y   (off width  (deplacement-dy      depl)))
           (rt-x   (+ rb-x lt-x))
           (rt-y   (+ rb-y lt-y))
           (left   (min lb-x lt-x rb-x rt-x))
           (bottom (min lb-y lt-y rb-y rt-y))
           (right  (max lb-x lt-x rb-x rt-x))
           (top    (max lb-y lt-y rb-y rt-y)))
      (values left bottom (- right left -1) (- top bottom -1)))))
         
    
(defmethod draw-string ((self picture) (x number) (y number)
                        string &key (direction :e))
  "
PRE:     (MEMBER DIRECTION '(:E :W :N :S :NE :NW :SE :SW
                           :NNE :NNW :SSE :SSW :ENE :ESE :WNW :WSW
                           :LEFT :RIGHT :UP :DOWN NIL) :TEST (FUNCTION 'EQ))
DO:      Draws the STRING in the given DIRECTION (default :RIGHT = :E).
         STRING may be anything, it will be formated with ~A.
         If it contains +NEW-LINE+ characters then it's split and
         each line is written ''under'' the other, according to the DIRECTION.
RETURN:  SELF
NOTE:    A future implementation won't use DRAW-POINT for performance.
"
  (setq x (truncate x) y (truncate y))
  (unless (stringp string) (setq string    (format nil "~A" string)))
  (let ((lines (string-cache-split string +new-line+))
        (depl  (assoc direction +deplacements+)))
    (if (cdr lines)
        (do ((dlx (deplacement-line-dx depl))
             (dly (deplacement-line-dy depl))
             (x x (+ x dlx))
             (y y (+ y dly))
             (lines lines (cdr lines)))
            ((null lines) self)
          (draw-string self x y (car lines) :direction direction))
        (do ((dx (deplacement-dx depl))
             (dy (deplacement-dy depl))
             (i 0 (1+ i))
             (x x (+ x dx))
             (y y (+ y dy)))
            ((>= i (length string)) self)
          (draw-point self x y (aref string i)))))) ;;DRAW-STRING


(defmethod draw-line ((self picture)
                      (x number) (y number) (w number) (h number)
                      &key (foreground "*"))
  "
DO:      Draw a line between (x,y) and (x+w-1,y+h-1) 
         with the foreground character.
RETURN:  SELF
NOTE:    A future implementation won't use DRAW-POINT for performance.
"
  (setq x (truncate x) y (truncate y) w (truncate w) h (truncate h))
  (setq foreground (to-char foreground))
  ;; We should compute the clipping here.
  (let ((dx (abs w))
        (dy (abs h))
        (xinc (if (> w 0) 1 -1))
        (yinc (if (> h 0) 1 -1))
        cumul)
    (when (/= 0 dx) (setq dx (1- dx)))
    (when (/= 0 dy) (setq dy (1- dy)))
    (draw-point self x y foreground)
    (if (> dx dy)
        (progn
          (setq cumul (floor (/ dx 2)))
          (loop for i from 1 to dx
             do
             (setq x     (+ x xinc)
                   cumul (+ cumul dy))
             (when (>= cumul dx)
               (setq cumul (- cumul dx)
                     y     (+ y yinc)))
             (draw-point self x y foreground) ))
        (progn
          (setq cumul (floor (/ dy 2)))
          (loop for i from 1 to dy
             do
             (setq y     (+ y yinc)
                   cumul (+ cumul dx))
             (when (>= cumul dy)
               (setq cumul (- cumul dy)
                     x     (+ x xinc)))
             (draw-point self x y foreground) ))))
  self)


(defmethod draw-arrow ((pict picture) 
                       (x number) (y number) (w number) (h number)
                       &key tail)
  (draw-line pict x y w h :foreground (if (= 0 w) "|" "-"))
  (draw-point pict (+ x w) (+ y h) (if (= 0 w)
                                       (if (< 0 h) "^" "v")
                                       (if (< 0 w) ">" "<")))
  (when tail (draw-point pict x y tail))
  pict)
  


(defmethod fill-rect  ((self picture)
                       (x number) (y number) (w number) (h number) 
                       &key (foreground "*"))
  "
DO:      Fills the specified rectangle with FOREGROUND.
RETURN:  SELF
"
  (setf x (truncate x)
        y (truncate y)
        w (truncate w) 
        h (truncate h)
        foreground (to-char foreground))
  (dotimes (i (1- h))
    (draw-line self x y w 0 :foreground foreground)
    (incf y))
  self) ;;FILL-RECT


(defmethod erase-rect ((self picture)
                       (x number) (y number) (w number) (h number))
  "
DO:      Fills the specified rectangle with (BACKGROUND SELF).
RETURN:  SELF
"
  (fill-rect self x y w h :foreground (background self))) ;;ERASE-RECT


(defmethod frame-rect ((self picture)
                       (x number) (y number) (w number) (h number)
                       &key (top-left "+") (top-right "+") 
                       (bottom-left "+")  (bottom-right "+")
                       (top "-") (bottom "-")  (left "|") (right "|"))
  "
DO:      Draws the frame of a rect parallel to the axis
         whose diagonal is [(x,y),(x+w-1,y+h-1)].
RETURN:  SELF
NOTE:    A future implementation won't use DRAW-POINT for performance.
"
  (setf x            (truncate x)
        y            (truncate y) 
        w            (1- (truncate w))
        h            (1- (truncate h))
        top-left     (to-char top-left)
        top          (to-char top)
        top-right    (to-char top-right)
        left         (to-char left)
        right        (to-char right)
        bottom-left  (to-char bottom-left)
        bottom       (to-char bottom)
        bottom-right (to-char bottom-right))
  (draw-line  self x       y       w 0 :foreground bottom)
  (draw-line  self x       (+ y h) w 0 :foreground top)
  (draw-line  self x       y       0 h :foreground left)
  (draw-line  self (+ x w) y       0 h :foreground right)
  (draw-point self x       y           bottom-left)
  (draw-point self (+ x w) y           bottom-right)
  (draw-point self x       (+ y h)     top-left)
  (draw-point self (+ x w) (+ y h)     top-right)
  self) ;;FRAME-RECT

  


;; ------------------------------------------------------------------------
;; SPRITE
;; ------------------------------------------------------------------------

(defgeneric name (sprite)
  (:documentation "Name of this sprite."))
(defgeneric data (sprite)
  (:documentation "Sprite data."))
(defgeneric spot-x (sprite)
  (:documentation "X coordinate of the spot of the sprite."))
(defgeneric spot-y (sprite)
  (:documentation "Y coordinate of the spot of the sprite"))
(defgeneric transparent (sprite)
  (:documentation "The transparent character of the sprite."))

(pjb-defclass sprite nil
  (:att name        string    "sprite"   "Name of this sprite.")
  (:att data        (array character 3)  "Sprite data.")
  (:att spot-x      fixnum    0          "X coordinate of spot.")
  (:att spot-y      fixnum    0          "Y coordinate of spot.")
  (:att transparent character (character " ") "The transparent character.")
  (:doc "A sprite is a tri-dimentional (time,y,x) array of characters."))


(defmethod width ((self sprite))
  "
RETURN:  The width of the sprite.
"
  (array-dimension (data self) 2))


(defmethod height ((self sprite))
  "
RETURN:  The height of the sprite.
"
  (array-dimension (data self) 1))


(defmethod frames ((self sprite))
  "
RETURN:  The number of frames of the sprite.
"
  (array-dimension (data self) 0))


(defmethod set-data ((self sprite) data)
  "
DATA may be either:
       - a single string with frames separated by FF and lines separated by LF,
       - a list of string frames with lines separated by LF,
       - a list of list of string lines.
       - a list of list of list of single character strings or symbols
         or characters or character codes.
       - a tri-dimentional array of characters.
RETURN: SELF
"
  (cond
    ((stringp data)
     (set-data self
               (mapcar (lambda (frame) (split-string frame +new-line+))
                       (split-string data +form-feed+))))
    ((and (consp data) (stringp (car data)))
     (set-data self
               (mapcar (lambda (frame) (split-string frame +new-line+))
                       data)))
    ((and (consp data) (consp (car data)) (or (stringp (caar data))
                                              (consp (caar data))))
     (let ((adata
            (make-array
             (list (length data)
                   (apply (function max)
                          (mapcar (function length) data))
                   (apply (function max)
                          (apply (function append)
                                 (mapcar (lambda (l)
                                           (mapcar (function length) l))
                                         data))))
             :element-type 'character))
           (transparent (transparent self)))
       (loop
          for f from 0 below (array-dimension adata 0)
          for frames = data then (cdr frames)
          do (loop
                for y from (1- (array-dimension adata 1)) downto 0
                for lines = (car frames) then (cdr lines)
                do (if (stringp (car lines))
                       (loop
                          for x from 0 below (array-dimension adata 2)
                          for cur-char = (if (< x (length (car lines)))
                                             (aref (car lines) x)
                                             transparent)
                          do (setf (aref adata f y x)
                                   (cond
                                     ((null cur-char) transparent)
                                     ((characterp cur-char) cur-char)
                                     ((stringp cur-char) (aref cur-char 0))
                                     ((numberp cur-char) (code-char cur-char))
                                     ((symbolp cur-char)
                                      (aref (symbol-name cur-char) 0))
                                     (t (error "~S is not a character!" cur-char))
                                     )))
                       (loop for x from 0 below (array-dimension adata 2)
                          for characters = (car lines) then (cdr characters)
                          for cur-char = (car characters) then (car characters)
                          do (setf (aref adata f y x)
                                   (cond
                                     ((null cur-char) transparent)
                                     ((characterp cur-char) cur-char)
                                     ((stringp cur-char) (aref cur-char 0))
                                     ((numberp cur-char) (code-char cur-char))
                                     ((symbolp cur-char)
                                      (aref (symbol-name cur-char) 0))
                                     (t (error "~S is not a character!"
                                               cur-char))
                                     ))))))
       (setf (data self) adata)))
    ((arrayp data)
     (setf (data self) data)))
  self)


(defmethod draw-on-picture ((self sprite) (pict picture) (x number) (y number)
                            &optional frame)
  "
DO:      Draws the frame FRAME of the sprite on the picture PICT,
         placing the spot of the sprite at coordinates (X,Y).
         Transparent pixels are not drawn.
"
  (unless frame (setq frame 0))
  (setq x (truncate x) y (truncate y) frame (truncate frame))
  (setq x (- x (spot-x self)) y (- y (spot-y self)))
  (loop with data = (data self)
     with transparent = (transparent self)
     for j from 0 below (height self)
     for yj = (+ y j) then (+ y j)
     do (loop for i from 0 below (width self)
           for pixel = (aref data frame j i) then (aref data frame j i)
           unless (eq pixel transparent)
           do (draw-point pict (+ x i) yj pixel)))
  self)



;;;; THE END ;;;;
