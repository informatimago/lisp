;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pom.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package computes and ascii-draw the phase of the Moon.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-11-10 <PJB> Created (converted from newmoon.c)
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.MOON"
  (:use "COMMON-LISP")
  (:export "PHASE-OF-THE-MOON" "DRAW-PHASE"
           "DRAW-PHASE-OF-THE-MOON")
  (:documentation "

This package computes and ascii-draw the phase of the Moon.


Copyright Pascal J. Bourguignon 2013 - 2013

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.MOON")


(defun phase-of-the-moon (time)
  "
DO:     Compute the phase of the moon.
RETURN: fraction; day; hour; minute; name.
"
  ;; This is most certainly wrong.  Converted from a function written
  ;; in C that had been massaged to work first in seconds, then in
  ;; minutes using assumedly originally a base time 13:23 10/1/78,
  ;; then 15:19 GMT 2/5/85 but using actually the values coded here.
  (let* ((base         (encode-universal-time 0 59 11 21 3 1985 0))
         (difference   (truncate (- time base) 60))
         (phase-length (+ 44 (* 60 (+ 12 (* 24 29)))))
         (phase        (mod difference phase-length))
         (fraction     (/ phase phase-length))
         (phase-length (truncate phase-length 4))
         (phase        (truncate phase phase-length))
         (difference   (truncate difference phase-length)))
    (multiple-value-bind (hour-diff minute) (truncate difference 60)
      (multiple-value-bind (day hour) (truncate hour-diff 24)
        (values fraction day hour minute (aref #("New Moon "
                                                 "First Quarter "
                                                 "Full Moon "
                                                 "Last Quarter ")
                                               phase))))))

#-(and)  #("New Moon"
           "Waxing Crescent"
           "First Quarter"
           "Waxing Gibbous"
           "Full Moon"
           "Waning Gibbous"
           "Last Quarter"
           "Waning Crescent")



(defvar *default-screen-aspect* 0.5)
(defvar *default-screen-height* 25)
(defvar *default-screen-width*  80)
(defvar *default-fill-pattern*  "*")


(defun draw-phase (phase name &key
                                (height *default-screen-height*)
                                (width *default-screen-width*)
                                (screen-aspect *default-screen-aspect*))
  "
Draws in ASCII art (over HEIGHT lines of WIDTH characters), a circle
with the phase highlighed with characters from the NAME string.
            
SCREEN-ASPECT: the ratio of the height of a character over the width of a
               character.
"
  (let* ((full-moon     0.5) ; the new moon is 0.0
         (fuzz          0.03) ; how far off we must be from an even 0.0
         (1-fuzz        (- 1.0 fuzz))
         (xmin          0) ; x position of leftmost edge of moon (not line)
         (xoffset       (truncate width 2))
         (vdiameter     (min (* width screen-aspect) height))
         (cheight       (/ 2 vdiameter)) ; character height in a circle of radius 1.0
         (xscale        (/ (* cheight screen-aspect))) ; x stretch factor
         (beforep       (< phase full-moon))
         (squisher      (if beforep
                            (cos (* 2 pi phase))
                            (cos (* 2 pi (- phase full-moon))))))
    (flet ((charpos (x xscale xoffset) (max 1 (+ xoffset (round (* x xscale)))))
           (pixel   (i)                (aref name (mod (- i xmin) (length name))))
           (draw    (buffer i p)       (when (< -1 i (length buffer))
                                         (setf (aref buffer i) p))))
      (loop
        :for y = (- 1.0 (/ cheight 2.0)) :then (- y cheight)
        :while (and (< -1.0 y) (< (abs (- y cheight)) y))
        :finally (let ((horizon (sqrt (- 1.0 (* y y)))))
                   (setf xmin (charpos (- horizon) xscale xoffset))))
      (loop
        :for y = (- 1.0 (/ cheight 2))
        :then (- y cheight)
        :while (< -1.0 y)
        :do (let* ((buffer     (make-string width :initial-element #\space))
                   (horizon    (sqrt (- 1.0 (* y y))))
                   (terminator (* squisher horizon))
                   (left       (if beforep terminator (- horizon)))
                   (right      (if beforep  horizon terminator)))
              (let ((i (charpos    horizon  xscale xoffset))) (draw buffer i (pixel i)))
              (let ((i (charpos (- horizon) xscale xoffset))) (draw buffer i (pixel i)))
              (when (< fuzz phase 1-fuzz)
                (loop
                  :for i :from (charpos left xscale xoffset) :to (charpos right xscale xoffset)
                  :do (draw buffer i (pixel i))))
              (write-line buffer))))))


(defun draw-phase-of-the-moon (&key
                                 (name *default-fill-pattern*)
                                 (height *default-screen-height*)
                                 (width *default-screen-width*)
                                 (screen-aspect *default-screen-aspect*))
  (multiple-value-bind (fraction day hour minute phase-name) (phase-of-the-moon (get-universal-time))
    (draw-phase fraction
                (cond
                  ((vectorp name) name)
                  ((eq name :time) (format nil "~2D ~2,'0D:~2,'0D" day hour minute))
                  (t phase-name))
                :height height :width width :screen-aspect screen-aspect)))



(defun pdraw/test ()
  (multiple-value-bind (fraction day hour minute name) (phase-of-the-moon (get-universal-time))
    (format t "~2D ~2,'0D:~2,'0D~%" day hour minute)
    (draw-phase fraction name)
    (draw-phase fraction "*"))
  (loop
    :for fraction :from 0.0 to 1.0 by (/ 1.0 29)
    :do (draw-phase fraction "#") (terpri) (terpri)))


;;;; THE END ;;;;
