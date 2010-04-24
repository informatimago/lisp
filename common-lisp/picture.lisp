;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              picture.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    ASCII-ART primitives.
;;;;    A  picture is an array of characters.
;;;;    There are primitives to draw points, lines, rectangles, 
;;;;    circles and ellipses, and strings.
;;;;    The coordinate system is the direct one: 
;;;;    - x increases toward the right,
;;;;    - y increases toward the top. Bottom left is (0,0).
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
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2004
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PICTURE"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COMMON-LISP")
  (:EXPORT
   "DRAW-ON-PICTURE" "SET-DATA" "FRAMES" "HEIGHT" "WIDTH" "TRANSPARENT"
   "SPOT-Y" "SPOT-X" "DATA" "NAME" "SPRITE" "TO-STRING" "FRAME-RECT"
   "ERASE-RECT" "FILL-RECT" "DRAW-ARROW" "DRAW-LINE" "DRAW-STRING" "SIZE-STRING"
   "DRAW-POINT" "POINT-AT" "HEIGHT" "WIDTH" "BACKGROUND" "PICTURE")
  (:DOCUMENTATION
   "This package exports functions to draw ASCII-ART pictures.

    Copyright Pascal J. Bourguignon 2002 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PICTURE")



(DEFGENERIC TO-STRING (SELF))
(DEFGENERIC POINT-AT (SELF X Y))
(DEFGENERIC DRAW-POINT (SELF X Y FOREGROUND))
(DEFGENERIC size-string (self string &key direction))
(DEFGENERIC DRAW-STRING (SELF X Y STRING &KEY DIRECTION))
(DEFGENERIC DRAW-LINE (SELF X Y W H &key FOREGROUND))
(DEFGENERIC draw-arrow (pict x y w h &key tail))
(DEFGENERIC FILL-RECT (SELF X Y W H &key foreground))
(DEFGENERIC ERASE-RECT (SELF X Y W H))
(DEFGENERIC FRAME-RECT (SELF X Y W H &key TOP-LEFT TOP-RIGHT BOTTOM-LEFT BOTTOM-RIGHT TOP BOTTOM LEFT RIGHT))
(DEFGENERIC WIDTH (SELF))
(DEFGENERIC HEIGHT (SELF))
(DEFGENERIC FRAMES (SELF))
(DEFGENERIC SET-DATA (SELF DATA))
(DEFGENERIC DRAW-ON-PICTURE (SELF PICT X Y &OPTIONAL FRAME))


(defvar +FORM-FEED+ (MAKE-STRING 1 :INITIAL-ELEMENT (CODE-CHAR 12))
  "A string containing a single +form-feed+ character.") ;;+FORM-FEED+

(defvar +NEW-LINE+  (MAKE-STRING 1 :INITIAL-ELEMENT (CODE-CHAR 10))
  "A string containing a single +new-line+ character.") ;;+NEW-LINE+

;; ------------------------------------------------------------------------
;; PICTURE
;; ------------------------------------------------------------------------

(PJB-DEFCLASS PICTURE NIL
  (:ATT DATA        (ARRAY CHARACTER 2)       "Picture data.")
  (:ATT BACKGROUND  CHARACTER (CHARACTER " ") "The background character.")
  (:DOC "A picture is a bi-dimentional (y,x) array of characters.")
  ) ;;PICTURE


(DEFUN TO-CHAR (STUFF)
  (COND ((CHARACTERP STUFF) STUFF)
        ((STRINGP    STUFF) (AREF STUFF 0))
        ((SYMBOLP    STUFF) (AREF (SYMBOL-NAME STUFF) 0))
        ((NUMBERP    STUFF) (CODE-CHAR STUFF))
        (T (ERROR "~S IS AN INVALID CHARACTER!" STUFF)))
  ) ;;TO-CHAR


(DEFMETHOD initialize-instance ((SELF PICTURE) 
                                &key (WIDTH 72) (HEIGHT 24) (BACKGROUND " "))
  "
RETURN:  SELF 
POST:    for all X in [0..WIDTH-1], for all Y in [0..HEIGHT-1],
           (EQ (to-char background) (point-at self x y))
"
  (SETF WIDTH  (TRUNCATE WIDTH)
        HEIGHT (TRUNCATE HEIGHT)
        (BACKGROUND self)  (TO-CHAR BACKGROUND)
        (DATA SELF) (MAKE-ARRAY (LIST (TRUNCATE HEIGHT)
                                      (TRUNCATE WIDTH))
                                :ELEMENT-TYPE 'CHARACTER
                                :initial-element (background self)))
  SELF) ;;initialize-instance


(DEFMETHOD TO-STRING ((SELF PICTURE))
  "
RETURN:  A string containing the picture character (pict height) lines
         of (pict width) characters, separated by a +new-line+.
"
  (LOOP WITH STR = (MAKE-STRING (* (HEIGHT SELF) (1+ (WIDTH SELF))))
     WITH DATA = (DATA SELF)
     WITH I = 0
     WITH NL = (TO-CHAR +NEW-LINE+)
     FOR Y FROM (1- (HEIGHT SELF)) DOWNTO 0
     DO
     (LOOP FOR X FROM 0 BELOW (WIDTH SELF)
        DO
        (SETF (AREF STR I) (AREF DATA Y X))
        (SETF I (1+ I)))
     (SETF (AREF STR I) NL)
     (SETF I (1+ I))
     FINALLY (RETURN STR))
  ) ;;TO-STRING



(defmethod print-object ((self picture) stream)
  (princ (to-string self) stream)
  self)


(DEFMETHOD WIDTH ((SELF PICTURE))
  "
RETURN:  The width of the picture.
"
  (ARRAY-DIMENSION (DATA SELF) 1)) ;;WIDTH


(DEFMETHOD HEIGHT ((SELF PICTURE))
  "
RETURN:  The height of the picture.
"
  (ARRAY-DIMENSION (DATA SELF) 0)) ;;HEIGHT


(DEFMETHOD POINT-AT ((SELF PICTURE) (X NUMBER) (Y NUMBER))
  "
PRE:     inside = (AND (<= 0 X) (< X (WIDTH SELF)) (<= 0 Y) (< Y (HEIGHT SELF)))
RETURN:  inside       ==> The character at coordinate (X,Y).
         (NOT inside) ==> (BACKGROUND SELF)
"
  (SETQ X (TRUNCATE X) Y (TRUNCATE Y))
  (IF (AND (<= 0 X) (<= 0 Y) (< Y (HEIGHT SELF)) (< X (WIDTH SELF)))
      (AREF (DATA SELF) Y X)
      (BACKGROUND SELF))) ;;POINT-AT


(DEFMETHOD DRAW-POINT ((SELF PICTURE) (X NUMBER) (Y NUMBER) FOREGROUND)
  "
PRE:    inside = (AND (<= 0 X) (<= 0 Y) (< Y (HEIGHT SELF)) (< X (WIDTH SELF)))
        old-point = (POINT-AT SELF X Y)
POST:   inside       ==> (EQ FOREGROUND (POINT-AT SELF X Y))
        (NOT inside) ==> (EQ old-point (POINT-AT SELF X Y))
RETURN: SELF
"
  (SETQ X (TRUNCATE X) Y (TRUNCATE Y))
  (SETQ FOREGROUND (TO-CHAR FOREGROUND))
  (WHEN (AND (<= 0 X) (<= 0 Y) (< Y (HEIGHT SELF)) (< X (WIDTH SELF)))
    (SETF (AREF (DATA SELF) Y X) FOREGROUND))
  SELF) ;;DRAW-POINT


(defstruct (deplacement (:type list))  name dx dy line-dx line-dy)
(defvar +deplacements+ '((:E      1  0  0 -1) 
                         (:RIGHT  1  0  0 -1) 
                         (:W     -1  0  0  1) 
                         (:LEFT  -1  0  0  1) 
                         (:N      0  1  1  0) 
                         (:UP     0  1  1  0) 
                         (:S      0 -1 -1  0) 
                         (:DOWN   0 -1 -1  0) 
                         (:NE     1  1  1 -1) 
                         (:NW    -1  1  1  1) 
                         (:SE     1 -1 -1 -1) 
                         (:SW    -1 -1 -1  1) 
                         (:NNE    1  2  2 -1) 
                         (:NNW   -1  2  2  1) 
                         (:SSE    1 -2 -2 -1) 
                         (:SSW   -1 -2 -1  2) 
                         (:ENE    2  1  1 -2) 
                         (:ESE    2 -1 -1 -2) 
                         (:WNW   -2  1  1  2) 
                         (:WSW   -2 -1 -1  2))) ;;+deplacements+


(defvar *string-cache* nil "PRIVATE")


(defun string-cache-split (string separator)
  "PRIVATE"
  (unless (and (eq string    (first  *string-cache*))
               (eq separator (second *string-cache*)))
    (setf *string-cache* (list string separator 
                               (split-string string separator))))
  (third *string-cache*)) ;;string-cache-split


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
      (values left bottom (- right left -1) (- top bottom -1))))) ;;size-string
         
    
(DEFMETHOD DRAW-STRING ((SELF PICTURE) (X NUMBER) (Y NUMBER)
                        STRING &KEY (DIRECTION :E))
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
  (SETQ X (TRUNCATE X) Y (TRUNCATE Y))
  (UNLESS (STRINGP STRING) (SETQ STRING    (FORMAT NIL "~A" STRING)))
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
          (DRAW-POINT SELF x y (AREF STRING I)))))) ;;DRAW-STRING


(DEFMETHOD DRAW-LINE ((SELF PICTURE)
                      (X NUMBER) (Y NUMBER) (W NUMBER) (H NUMBER)
                      &key (FOREGROUND "*"))
  "
DOES:    Draw a line between (x,y) and (x+w-1,y+h-1) 
         with the foreground character.
RETURN:  SELF
NOTE:    A future implementation won't use DRAW-POINT for performance.
"
  (SETQ X (TRUNCATE X) Y (TRUNCATE Y) W (TRUNCATE W) H (TRUNCATE H))
  (SETQ FOREGROUND (TO-CHAR FOREGROUND))
  ;; We should compute the clipping here.
  (LET ((DX (ABS W))
        (DY (ABS H))
        (XINC (IF (> W 0) 1 -1))
        (YINC (IF (> H 0) 1 -1))
        CUMUL)
    (WHEN (/= 0 DX) (SETQ DX (1- DX)))
    (WHEN (/= 0 DY) (SETQ DY (1- DY)))
    (DRAW-POINT SELF X Y FOREGROUND)
    (IF (> DX DY)
        (PROGN
          (SETQ CUMUL (FLOOR (/ DX 2)))
          (LOOP FOR I FROM 1 TO DX
             DO
             (SETQ X     (+ X XINC)
                   CUMUL (+ CUMUL DY))
             (WHEN (>= CUMUL DX)
               (SETQ CUMUL (- CUMUL DX)
                     Y     (+ Y YINC)))
             (DRAW-POINT SELF X Y FOREGROUND) ))
        (PROGN
          (SETQ CUMUL (FLOOR (/ DY 2)))
          (LOOP FOR I FROM 1 TO DY
             DO
             (SETQ Y     (+ Y YINC)
                   CUMUL (+ CUMUL DX))
             (WHEN (>= CUMUL DY)
               (SETQ CUMUL (- CUMUL DY)
                     X     (+ X XINC)))
             (DRAW-POINT SELF X Y FOREGROUND) ))))
  SELF) ;;DRAW-LINE


(defmethod draw-arrow ((pict picture) 
                       (x number) (y number) (w number) (h number)
                       &key tail)
  (draw-line pict x y w h :foreground (if (= 0 w) "|" "-"))
  (draw-point pict (+ x w) (+ y h) (if (= 0 w)
                                       (if (< 0 h) "^" "v")
                                       (if (< 0 w) ">" "<")))
  (when tail (draw-point pict x y tail))
  pict) ;;draw-arrow
  


(DEFMETHOD FILL-RECT  ((SELF PICTURE)
                       (X NUMBER) (Y NUMBER) (W NUMBER) (H NUMBER) 
                       &key (foreground "*"))
  "
DOES:    Fills the specified rectangle with FOREGROUND.
RETURN:  SELF
"
  (SETF X (TRUNCATE X)
        Y (TRUNCATE Y)
        W (TRUNCATE W) 
        H (TRUNCATE H)
        FOREGROUND (TO-CHAR FOREGROUND))
  (dotimes (i (1- h))
    (DRAW-LINE SELF X Y W 0 :foreground FOREGROUND)
    (incf y))
  self) ;;FILL-RECT


(DEFMETHOD ERASE-RECT ((SELF PICTURE)
                       (X NUMBER) (Y NUMBER) (W NUMBER) (H NUMBER))
  "
DOES:    Fills the specified rectangle with (BACKGROUND SELF).
RETURN:  SELF
"
  (FILL-RECT SELF X Y W H :foreground (BACKGROUND SELF))) ;;ERASE-RECT


(DEFMETHOD FRAME-RECT ((SELF PICTURE)
                       (X NUMBER) (Y NUMBER) (W NUMBER) (H NUMBER)
                       &key (TOP-LEFT "+") (TOP-RIGHT "+") 
                       (BOTTOM-LEFT "+")  (BOTTOM-RIGHT "+")
                       (TOP "-") (BOTTOM "-")  (LEFT "|") (RIGHT "|"))
  "
DOES:    Draws the frame of a rect parallel to the axis
         whose diagonal is [(x,y),(x+w-1,y+h-1)].
RETURN:  SELF
NOTE:    A future implementation won't use DRAW-POINT for performance.
"
  (SETF X            (TRUNCATE X)
        Y            (TRUNCATE Y) 
        W            (1- (truncate W))
        H            (1- (truncate H))
        TOP-LEFT     (TO-CHAR TOP-LEFT)
        TOP          (TO-CHAR TOP)
        TOP-RIGHT    (TO-CHAR TOP-RIGHT)
        LEFT         (TO-CHAR LEFT)
        RIGHT        (TO-CHAR RIGHT)
        BOTTOM-LEFT  (TO-CHAR BOTTOM-LEFT)
        BOTTOM       (TO-CHAR BOTTOM)
        BOTTOM-RIGHT (TO-CHAR BOTTOM-RIGHT))
  (DRAW-LINE  SELF X       Y       W 0 :foreground BOTTOM)
  (DRAW-LINE  SELF X       (+ Y H) W 0 :foreground TOP)
  (DRAW-LINE  SELF X       Y       0 H :foreground LEFT)
  (DRAW-LINE  SELF (+ X W) Y       0 H :foreground RIGHT)
  (DRAW-POINT SELF X       Y           BOTTOM-LEFT)
  (DRAW-POINT SELF (+ X W) Y           BOTTOM-RIGHT)
  (DRAW-POINT SELF X       (+ Y H)     TOP-LEFT)
  (DRAW-POINT SELF (+ X W) (+ Y H)     TOP-RIGHT)
  SELF) ;;FRAME-RECT

  
(DEFUN TEST-PICTURE-OBJECT ()
  "
DOES:    Creates a test picture.
RETURN:  the picture string.
"
  (LET ((P (MAKE-INSTANCE 'PICTURE :width 40 :height 20 :background ".")))
    (FRAME-RECT P 0 0 39 19)

    (DRAW-STRING P 21 10 "East")
    (DRAW-STRING P 19 10 "West"  :direction :W)
    (DRAW-STRING P 20 11 "North" :direction :N)
    (DRAW-STRING P 20  9 "South" :direction :S)
    (DRAW-STRING P 21 11 "North-East" :direction :ENE)
    (DRAW-STRING P 19 11 "North-West" :direction :WNW)
    (DRAW-STRING P 21  9 "South-East" :direction :ESE)
    (DRAW-STRING P 19  9 "South-West" :direction :WSW)

    (ERASE-RECT P 2 10 5 3)
    (FILL-RECT P 30 10 6 4 :foreground ":")
    (FRAME-RECT P 30 10 6 4 :top-left "/" :top-right "\\" 
                :bottom-left "\\" :bottom-right "/" 
                :left "<" :top "^" :bottom "v" :right ">")
    (DRAW-LINE P 0 0 40 20 :foreground "$")
    (DRAW-POINT  P 20 10 "*")
    (print P))) ;;TEST-PICTURE-OBJECT


;; ------------------------------------------------------------------------
;; SPRITE
;; ------------------------------------------------------------------------


(PJB-DEFCLASS SPRITE NIL
  (:ATT NAME        STRING    "sprite"   "Name of this sprite.")
  (:ATT DATA        (ARRAY CHARACTER 3)  "Sprite data.")
  (:ATT SPOT-X      FIXNUM    0          "X coordinate of spot.")
  (:ATT SPOT-Y      FIXNUM    0          "Y coordinate of spot.")
  (:ATT TRANSPARENT CHARACTER (CHARACTER " ") "The transparent character.")
  (:DOC "A sprite is a tri-dimentional (time,y,x) array of characters.")
  ) ;;SPRITE


(DEFMETHOD WIDTH ((SELF SPRITE))
  "
RETURN:  The width of the sprite.
"
  (ARRAY-DIMENSION (DATA SELF) 2)) ;;WIDTH


(DEFMETHOD HEIGHT ((SELF SPRITE))
  "
RETURN:  The height of the sprite.
"
  (ARRAY-DIMENSION (DATA SELF) 1)) ;;HEIGHT


(DEFMETHOD FRAMES ((SELF SPRITE))
  "
RETURN:  The number of frames of the sprite.
"
  (ARRAY-DIMENSION (DATA SELF) 0)) ;;FRAMES


(DEFMETHOD SET-DATA ((SELF SPRITE) DATA)
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
  (COND
    ((STRINGP DATA)
     (SET-DATA SELF
               (MAPCAR (LAMBDA (FRAME) (SPLIT-STRING FRAME +NEW-LINE+))
                       (SPLIT-STRING DATA +FORM-FEED+))))
    ((AND (CONSP DATA) (STRINGP (CAR DATA)))
     (SET-DATA SELF
               (MAPCAR (LAMBDA (FRAME) (SPLIT-STRING FRAME +NEW-LINE+))
                       DATA)))
    ((AND (CONSP DATA) (CONSP (CAR DATA)) (OR (STRINGP (CAAR DATA))
                                              (CONSP (CAAR DATA))))
     (LET ((ADATA
            (MAKE-ARRAY
             (LIST (LENGTH DATA)
                   (APPLY (FUNCTION MAX)
                          (MAPCAR (FUNCTION LENGTH) DATA))
                   (APPLY (FUNCTION MAX)
                          (APPLY (FUNCTION APPEND)
                                 (MAPCAR (LAMBDA (L)
                                           (MAPCAR (FUNCTION LENGTH) L))
                                         DATA))))
             :ELEMENT-TYPE 'CHARACTER))
           (TRANSPARENT (TRANSPARENT SELF)))
       (LOOP
          FOR F FROM 0 BELOW (ARRAY-DIMENSION ADATA 0)
          FOR FRAMES = DATA THEN (CDR FRAMES)
          DO (LOOP
                FOR Y FROM (1- (ARRAY-DIMENSION ADATA 1)) DOWNTO 0
                FOR LINES = (CAR FRAMES) THEN (CDR LINES)
                DO (IF (STRINGP (CAR LINES))
                       (LOOP
                          FOR X FROM 0 BELOW (ARRAY-DIMENSION ADATA 2)
                          FOR CUR-CHAR = (IF (< X (LENGTH (CAR LINES)))
                                             (AREF (CAR LINES) X)
                                             TRANSPARENT)
                          DO (SETF (AREF ADATA F Y X)
                                   (COND
                                     ((NULL CUR-CHAR) TRANSPARENT)
                                     ((CHARACTERP CUR-CHAR) CUR-CHAR)
                                     ((STRINGP CUR-CHAR) (AREF CUR-CHAR 0))
                                     ((NUMBERP CUR-CHAR) (CODE-CHAR CUR-CHAR))
                                     ((SYMBOLP CUR-CHAR)
                                      (AREF (SYMBOL-NAME CUR-CHAR) 0))
                                     (T (ERROR "~S is not a character!" CUR-CHAR))
                                     )))
                       (LOOP FOR X FROM 0 BELOW (ARRAY-DIMENSION ADATA 2)
                          FOR CHARACTERS = (CAR LINES) THEN (CDR CHARACTERS)
                          FOR CUR-CHAR = (CAR CHARACTERS) THEN (CAR CHARACTERS)
                          DO (SETF (AREF ADATA F Y X)
                                   (COND
                                     ((NULL CUR-CHAR) TRANSPARENT)
                                     ((CHARACTERP CUR-CHAR) CUR-CHAR)
                                     ((STRINGP CUR-CHAR) (AREF CUR-CHAR 0))
                                     ((NUMBERP CUR-CHAR) (CODE-CHAR CUR-CHAR))
                                     ((SYMBOLP CUR-CHAR)
                                      (AREF (SYMBOL-NAME CUR-CHAR) 0))
                                     (T (ERROR "~S is not a character!"
                                               CUR-CHAR))
                                     ))))))
       (SETF (DATA SELF) ADATA)))
    ((ARRAYP DATA)
     (SETF (DATA SELF) DATA)))
  SELF) ;;SET-DATA


(DEFMETHOD DRAW-ON-PICTURE ((SELF SPRITE) (PICT PICTURE) (X NUMBER) (Y NUMBER)
                            &OPTIONAL FRAME)
  "
DOES:    Draws the frame FRAME of the sprite on the picture PICT,
         placing the spot of the sprite at coordinates (X,Y).
         Transparent pixels are not drawn.
"
  (UNLESS FRAME (SETQ FRAME 0))
  (SETQ X (TRUNCATE X) Y (TRUNCATE Y) FRAME (TRUNCATE FRAME))
  (SETQ X (- X (SPOT-X SELF)) Y (- Y (SPOT-Y SELF)))
  (LOOP WITH DATA = (DATA SELF)
     WITH TRANSPARENT = (TRANSPARENT SELF)
     FOR J FROM 0 BELOW (HEIGHT SELF)
     FOR YJ = (+ Y J) THEN (+ Y J)
     DO (LOOP FOR I FROM 0 BELOW (WIDTH SELF)
           FOR PIXEL = (AREF DATA FRAME J I) THEN (AREF DATA FRAME J I)
           UNLESS (EQ PIXEL TRANSPARENT)
           DO (DRAW-POINT PICT (+ X I) YJ PIXEL)))
  SELF) ;;DRAW-ON-PICTURE



;;;; picture.lisp                     --                     --          ;;;;
