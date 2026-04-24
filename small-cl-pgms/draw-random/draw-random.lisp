;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               draw-random.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Generate a PNG image:
;;;;    - Canvas of given size (default 512x512);
;;;;    - 6 to 8 random lines delineating areas;
;;;;    - Each area is stippled with dots whose density decreases along
;;;;      a randomly chosen direction proper to the area.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-04-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2026 - 2026
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

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.DRAW-RANDOM"
  (:use "COMMON-LISP")
  (:export "DRAW-RANDOM" "MAIN")
  (:documentation "
Draw a random picture: black lines delineating areas filled with blue
dots of density decreasing along a random direction per area, on a
white background.  The picture is saved as a PNG file.

Usage:

    (com.informatimago.small-cl-pgms.draw-random:draw-random
       :pathname \"/tmp/picture.png\")

Or with custom parameters:

    (draw-random :width 800 :height 600
                 :pathname \"/tmp/picture.png\"
                 :background #(255 255 255)
                 :line-color #(0 0 0)
                 :dot-color  #(0 0 255)
                 :min-lines 6 :max-lines 8
                 :max-density 0.25)

Copyright Pascal J. Bourguignon 2026 - 2026
Licensed under the AGPL3.
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.DRAW-RANDOM")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defaults
;;;

(defparameter *default-width*       512)
(defparameter *default-height*      512)
(defparameter *default-background*  #(255 255 255))
(defparameter *default-line-color*  #(0 0 0))
(defparameter *default-dot-color*   #(0 0 255))
(defparameter *default-min-lines*   6)
(defparameter *default-max-lines*   8)
(defparameter *default-max-density* 0.25
  "Fraction of pixels stippled where the density is maximum.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image primitives (zpng wrappers)
;;;

(defun make-image (width height bg-color)
  "Create a fresh truecolor PNG image filled with BG-COLOR."
  (let* ((img  (make-instance 'zpng:png
                              :color-type :truecolor
                              :width  width
                              :height height))
         (data (zpng:data-array img)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref data y x 0) (aref bg-color 0)
              (aref data y x 1) (aref bg-color 1)
              (aref data y x 2) (aref bg-color 2))))
    img))

(declaim (inline set-pixel))
(defun set-pixel (data width height x y color)
  "Paint pixel (X,Y) of DATA with COLOR if within bounds."
  (when (and (<= 0 x (1- width))
             (<= 0 y (1- height)))
    (setf (aref data y x 0) (aref color 0)
          (aref data y x 1) (aref color 1)
          (aref data y x 2) (aref color 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random lines
;;;

(defun random-point-on-border (width height)
  "Return a random point on the canvas border as (x y)."
  (ecase (random 4)
    (0 (list (random width)      0))
    (1 (list (random width)      (1- height)))
    (2 (list 0                   (random height)))
    (3 (list (1- width)          (random height)))))

(defun random-line (width height)
  "Return a line ((x0 y0) (x1 y1)) with endpoints on the border."
  (let ((p1 (random-point-on-border width height))
        (p2 (random-point-on-border width height)))
    (loop while (equal p1 p2)
          do (setf p2 (random-point-on-border width height)))
    (list p1 p2)))

(defun random-lines (width height n)
  (loop repeat n collect (random-line width height)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Line rasterization (Bresenham) onto a mask
;;;

(defun rasterize-line (mask width height x0 y0 x1 y1)
  "Set MASK[y,x] to 1 along the line from (X0,Y0) to (X1,Y1).
MASK is a 2-D array indexed by (y x) of BIT."
  (let* ((dx  (abs (- x1 x0)))
         (dy  (abs (- y1 y0)))
         (sx  (if (< x0 x1) 1 -1))
         (sy  (if (< y0 y1) 1 -1))
         (err (- dx dy))
         (x   x0)
         (y   y0))
    (loop
      (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
        (setf (aref mask y x) 1))
      (when (and (= x x1) (= y y1)) (return))
      (let ((e2 (* 2 err)))
        (when (> e2 (- dy))
          (decf err dy)
          (incf x sx))
        (when (< e2 dx)
          (incf err dx)
          (incf y sy))))))

(defun rasterize-lines (mask width height lines)
  (dolist (line lines)
    (destructuring-bind ((x0 y0) (x1 y1)) line
      (rasterize-line mask width height x0 y0 x1 y1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Region identification via 4-connected flood fill
;;;
;;; regions[y,x] = 0  means: not yet labelled and not a line pixel
;;; regions[y,x] = -1 means: line pixel
;;; regions[y,x] = k  means: belongs to region k (k >= 1)
;;;

(defun make-region-map (mask width height)
  "Turn MASK into a signed region map: -1 for line pixels, 0 elsewhere."
  (let ((regions (make-array (list height width)
                             :element-type 'fixnum
                             :initial-element 0)))
    (dotimes (y height)
      (dotimes (x width)
        (when (= 1 (aref mask y x))
          (setf (aref regions y x) -1))))
    regions))

(defun flood-fill-region (regions width height start-x start-y id)
  "Flood-fill (4-connectivity) from (START-X, START-Y) assigning ID.
Returns the list of (x . y) pixels in the region."
  (let ((stack (list (cons start-x start-y)))
        (pixels '()))
    (loop while stack do
      (let* ((p (pop stack))
             (x (car p))
             (y (cdr p)))
        (when (and (<= 0 x (1- width))
                   (<= 0 y (1- height))
                   (zerop (aref regions y x)))
          (setf (aref regions y x) id)
          (push p pixels)
          (push (cons (1+ x) y) stack)
          (push (cons (1- x) y) stack)
          (push (cons x (1+ y)) stack)
          (push (cons x (1- y)) stack))))
    pixels))

(defun label-regions (regions width height)
  "Label all non-line connected components.
Return a list of (id . pixels) entries where pixels is a list of (x . y)."
  (let ((next-id 1)
        (entries '()))
    (dotimes (y height)
      (dotimes (x width)
        (when (zerop (aref regions y x))
          (let ((pixels (flood-fill-region regions width height x y next-id)))
            (push (cons next-id pixels) entries)
            (incf next-id)))))
    (nreverse entries)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dot stippling with density decreasing along a random direction
;;;

(defun random-direction ()
  "Return a unit vector (dx dy) at a uniformly random angle."
  (let ((a (* 2 pi (random 1.0d0))))
    (list (cos a) (sin a))))

(defun stipple-region (data width height pixels dot-color max-density)
  "Stipple PIXELS with DOT-COLOR, density decreasing along a random direction.
PIXELS is a list of (x . y) cons cells."
  (when pixels
    (destructuring-bind (dx dy) (random-direction)
      ;; Compute the projection t = dx*x + dy*y extremes over the region.
      (let ((tmin most-positive-double-float)
            (tmax most-negative-double-float))
        (dolist (p pixels)
          (let ((tv (+ (* dx (car p)) (* dy (cdr p)))))
            (when (< tv tmin) (setf tmin tv))
            (when (> tv tmax) (setf tmax tv))))
        (let ((span (- tmax tmin)))
          (when (zerop span) (setf span 1.0d0))
          (dolist (p pixels)
            (let* ((x    (car p))
                   (y    (cdr p))
                   (tv   (+ (* dx x) (* dy y)))
                   (s    (/ (- tv tmin) span))       ; 0..1 along direction
                   (dens (* max-density (- 1.0d0 s)))) ; high at tmin, low at tmax
              (when (< (random 1.0d0) dens)
                (set-pixel data width height x y dot-color)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compose everything
;;;

(defun paint-lines (data width height mask line-color)
  "Paint line-mask pixels in LINE-COLOR on DATA."
  (dotimes (y height)
    (dotimes (x width)
      (when (= 1 (aref mask y x))
        (set-pixel data width height x y line-color)))))

(defun draw-random (&key
                      (width       *default-width*)
                      (height      *default-height*)
                      (background  *default-background*)
                      (line-color  *default-line-color*)
                      (dot-color   *default-dot-color*)
                      (min-lines   *default-min-lines*)
                      (max-lines   *default-max-lines*)
                      (max-density *default-max-density*)
                      (pathname    "draw-random.png"))
  "Generate a random drawing and save it as a PNG file at PATHNAME.
Return the pathname of the written file."
  (let* ((n-lines (+ min-lines (random (1+ (- max-lines min-lines)))))
         (lines   (random-lines width height n-lines))
         (mask    (make-array (list height width)
                              :element-type 'bit
                              :initial-element 0))
         (img     (make-image width height background))
         (data    (zpng:data-array img)))
    (rasterize-lines mask width height lines)
    (let* ((regions (make-region-map mask width height))
           (entries (label-regions regions width height)))
      (dolist (entry entries)
        (stipple-region data width height (cdr entry) dot-color max-density)))
    (paint-lines data width height mask line-color)
    (zpng:write-png img pathname)
    pathname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Script entry point
;;;

(defun main (&optional (pathname "draw-random.png"))
  "Command-line entry point: write a random drawing to PATHNAME."
  (draw-random :pathname pathname)
  (format t "~&Wrote ~A~%" pathname)
  0)

;;;; THE END ;;;;
