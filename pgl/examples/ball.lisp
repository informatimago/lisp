;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ball.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test the pgl library with a bouncing ball.
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

(defpackage "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.BALL"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.PORTABLE-GRAPHICS-LIBRARY.EXAMPLE.BALL")


(defclass ball (compound)
  ((vx :initarg :vx :accessor vx :initform (- (+ 10 (random 20.0d0)) 15))
   (vy :initarg :vy :accessor vy :initform (- (+ 10 (random 30.0d0)) 15))
   (gx :initarg :gx :accessor gx :initform 0                            )
   (gy :initarg :gy :accessor gy :initform (random 10.0d0)              )))

(defun make-ball (diameter)
  (let* ((components (cons (make-instance 'oval :x 0 :y 0 :width diameter :height diameter
                                                :color *red* :fill-color *red* :filled t)
                           (loop :for alpha :from 0 :below 360 :by 30
                                 :collect (make-instance 'arc
                                                         :x 0 :y 0
                                                         :width diameter :height diameter
                                                         :start alpha :sweep 15
                                                         :color *yellow* :fill-color *yellow* :filled t))))
         (ball (make-instance 'ball :x 0 :y 0 :width diameter :height diameter
                                    :components components
                                    :vx (random 30.0d0) :vy 0
                                    :gx 0 :gy (random 10.0d0))))
    (dolist (component components) (send-to-front component))
    ball))

(defun update-position-velocity (x vx gx w)
  (incf x vx)
  (unless (<= 0 x w)
    (setf x (- x vx vx)
          vx (* 0.9 (- vx))))
  (incf vx gx)
  (values x vx gx))

(defmethod update ((b ball) w h)
  (let ((x (x b))
        (y (y b))
        (vx (vx b))
        (vy (vy b))
        (gx (gx b))
        (gy (gy b))
        (s (width b)))
    (multiple-value-setq (x vx gx) (update-position-velocity x vx gx (- w s)))
    (multiple-value-setq (y vy gy) (update-position-velocity y vy gy (- h s)))
    (setf (vx b) vx
          (vy b) vy
          (gx b) gx
          (gy b) gy)
    (set-location b x y)))

(defclass ball-window (window)
  ((ball :initarg :ball :accessor ball)))

(defun make-ball-window ()
  (let* ((w 512)
         (h 342)
         (ball       (make-ball 80))
         (background (make-instance 'compound
                                    :x 0 :y 0 :width w :height h
                                    :components (list (make-instance 'rect
                                                                     :filled t :fill-color *blue* :color *blue*
                                                                     :x 0 :y 0 :width w :height h)
                                                      ball))))
    (make-instance
     'ball-window
     :ball ball
     :title "Beach Ball"
     :color *blue*
     :x 20 :y 40
     :width w :height h
     :components (list background))))

(defmethod tick ((window ball-window))
  (update (ball window) (width window) (height window)))

(defun run ()
  (let ((w  (make-ball-window))
        (dt (make-instance 'timer :duration-ms 100)))
    (start-timer dt)
    (unwind-protect
         (loop
           :for e := (get-next-event (logior +timer-event+ +window-event+))
           :do (case (event-type-keyword e)
                 (:timer-ticked  (when (eql dt (event-timer e))
                                   (tick w)))
                 (:window-closed (when (eql w (event-window e))
                                   (loop-finish)))))
      (stop-timer dt)
      (free dt)
      (close-window w))))

;;; THE END ;;;;
