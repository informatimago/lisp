;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               histogram.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-11-11 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.HISTOGRAM"
  (:use "COMMON-LISP")
  (:export
   "HISTOGRAM" 
   "HISTOGRAM-BINS-AND-LABELS"
   
   "MAKE-HISTOGRAM" "HISTOGRAM-BINS" "HISTOGRAM-SIZE"
   "HISTOGRAM-COUNT" "HISTOGRAM-MIN-VALUE" "HISTOGRAM-MAX-VALUE"
   "HISTOGRAM-ENTER" "HISTOGRAM-COMPUTE-BIN")
  
  (:documentation "

This package provides functions to deal with histograms.
    

License:

    AGPL3
    
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
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.HISTOGRAM")



(defstruct (histogram
             (:constructor %make-histogram))
  bins
  bin-size
  bin-count
  min-value
  max-value)


(defun make-histogram (bin-count min-value max-value)
  "

     +-------+-------+-------+
     |       |       |       | count=3
     +-------+-------+-------+
     |                       |
    min                     max

"
  (check-type bin-count (integer 1))
  (check-type min-value real)
  (check-type max-value real)
  (assert (< min-value max-value))
  (%make-histogram
   :bins (make-array (+ 2 bin-count) :element-type 'unsigned-byte :initial-element 0)
   :bin-size (/ (- max-value min-value) bin-count)
   :bin-count bin-count
   :min-value min-value
   :max-value max-value))

(defun histogram-bin-labels (histogram)
  (let* ((bins   (histogram-bins histogram))
         (labels (make-array (length bins))))
    (setf (aref labels 0)                  (format nil "x<~A" (histogram-min-value histogram))
          (aref labels (1- (length bins))) (format nil "~A<=x" (histogram-max-value histogram)))
    (loop
      :with size = (histogram-bin-size histogram)
      :for i :from 1 :to (histogram-bin-count histogram)
      :for left := (histogram-min-value histogram) :then right
      :for right = (+ left size)
      :do (setf (aref labels i) (format nil "~A<=x<~A" left right)))
    labels))

(defun histogram-compute-bin (histogram value)
  (cond
    ((< value (histogram-min-value histogram))
     0)
    ((<= (histogram-max-value histogram) value)
     (1- (length (histogram-bins histogram))))
    (t
     (1+ (truncate (- value (histogram-min-value histogram))
                   (histogram-bin-size histogram))))))

(defun histogram-enter (histogram value)
  (check-type value real)
  (incf (aref (histogram-bins histogram) (histogram-compute-bin histogram value))))



(defun histogram (data bin-count &key (key (function identity)) min-value max-value)
  "
If MIN-VALUE or MAX-VALUE is not given, then they're computed from the data.
RETURN: An histogram of BIN-COUNT bins, built from the DATA, mapped by KEY.
"
  (check-type data sequence)
  (assert (or (and (listp data) (cdr data))
              (and (vectorp data) (plusp (length data)))))
  (let* ((data-min-value (or min-value (funcall key (elt data 0))))
         (data-max-value (or max-value (funcall key (elt data 0))))
         (data (map 'vector 
                    (if min-value
                        (if max-value
                            key
                            (lambda (element)
                              (let ((value (funcall key element)))
                                (setf data-max-value (max data-max-value value))
                                value)))
                        (if max-value
                            (lambda (element)
                              (let ((value (funcall key element)))
                                (setf data-min-value (min data-min-value value))
                                value))
                            (lambda (element)
                              (let ((value (funcall key element)))
                                (setf data-min-value (min data-min-value value))
                                (setf data-max-value (max data-max-value value))
                                value))))
                    data))
         (histogram (make-histogram bin-count data-min-value data-max-value)))
    (loop
      :for value :across data
      :do (histogram-enter histogram value))
    histogram))


(defun histogram-bins-and-labels (data bin-count &key (key (function identity)) min-value max-value)
  (let ((histogram (histogram data bin-count :key key :min-value min-value :max-value max-value)))
    (map 'vector 'cons
         (histogram-bins histogram)
         (histogram-bin-labels histogram))))


;; (histogram-bins-and-labels (com.informatimago.common-lisp.cesarum.list:iota 100) 7 :min-value 0 :max-value 7
;;                            :key (lambda (x) (mod x 7)))
;; 
;; #((0 . "x<0") (15 . "0<=x<1") (15 . "1<=x<2") (14 . "2<=x<3") (14 . "3<=x<4") (14 . "4<=x<5") (14 . "5<=x<6") (14 . "6<=x<7") (0 . "7<=x"))

;;;; THE END ;;;;
