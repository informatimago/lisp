;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               douze.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A demographic simulator.
;;;;
;;;;    Assuming an Adam and an Eve 20 years old each,
;;;;    assuming the current US life table,
;;;;    and assuming an "intensive" reproduction rate, with gene selection,
;;;;    simulate the population growth during 80 years
;;;;    and draw the final age pyramid.
;;;;
;;;;USAGE:
;;;;
;;;;    (LOAD (COMPILE-FILE "DOUZE.LISP"))
;;;;    (COM.INFORMATIMAGO.COMMON-LISP.DOUZE:SIMULATE)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-02-25 <PJB> Added this comment.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DOUZE"
  (:documentation "
    A demographic simulator.

    Assuming an Adam and an Eve 20 years old each,
    assuming the current US life table,
    and assuming an \"intensive\" reproduction rate, with gene selection,
    simulate the population growth during 80 years
    and draw the final age pyramid.
    
    Copyright Pascal J. Bourguignon 2003 - 2004
    This package is put in the Public Domain.
    ")
  (:use "COMMON-LISP")
  (:export "SIMULATE")
  );;COM.INFORMATIMAGO.COMMON-LISP.DOUZE
(in-package "COM.INFORMATIMAGO.COMMON-LISP.DOUZE")

(defvar *year* 0)
(defvar *population* '())
(defvar *cemetery* '())



;; http://www.ssa.gov/OACT/STATS/table4c6.html


;;; lynx -source http://www.ssa.gov/OACT/STATS/table4c6.html | html-get-tables | sed -n -e '/^[0-9][0-9]*[|][0-9.,|]*$/p' | awk -F\| '{i=$1;male[i]=$2;female[i]=$5;}END{printf "(defparameter *male-dp* (make-array (quote (%d)) :element-type (quote float) :initial-contents (quote (",1+i;for(k=0;k<=i;k++){printf " %f",male[k];} printf "))))\n";printf "(defparameter *female-dp* (make-array (quote (%d)) :element-type (quote float) :initial-contents (quote (",1+i;for(k=0;k<=i;k++){printf " %f",female[k];} printf "))))\n";}'


(defparameter *male-dp*
  (make-array
   '(120)
   :element-type (quote float)
   :initial-contents
   '(
     0.008115 0.000531 0.000359 0.000298 0.000232 0.000206 0.000192
     0.000180 0.000163 0.000141 0.000125 0.000135 0.000191 0.000308
     0.000467 0.000640 0.000804 0.000954 0.001079 0.001181 0.001285
     0.001383 0.001437 0.001434 0.001391 0.001333 0.001286 0.001259
     0.001267 0.001303 0.001350 0.001400 0.001465 0.001546 0.001642
     0.001754 0.001883 0.002030 0.002196 0.002381 0.002583 0.002802
     0.003045 0.003312 0.003602 0.003928 0.004272 0.004603 0.004908
     0.005210 0.005538 0.005926 0.006386 0.006935 0.007568 0.008292
     0.009083 0.009910 0.010759 0.011663 0.012645 0.013774 0.015117
     0.016717 0.018541 0.020582 0.022740 0.024910 0.027036 0.029205
     0.031630 0.034380 0.037348 0.040548 0.044060 0.048038 0.052535
     0.057502 0.062970 0.069027 0.075760 0.083288 0.091713 0.101108
     0.111468 0.122752 0.134930 0.147987 0.161928 0.176773 0.192542
     0.209250 0.226904 0.245500 0.265023 0.284534 0.303801 0.322578
     0.340612 0.357642 0.375525 0.394301 0.414016 0.434717 0.456452
     0.479275 0.503239 0.528401 0.554821 0.582562 0.611690 0.642274
     0.674388 0.708107 0.743513 0.780688 0.819722 0.860709 0.903744
     0.948931)));;*male-dp*


(defparameter *female-dp*
  (make-array
   '(120)
   :element-type (quote float)
   :initial-contents
   '(
     0.006702 0.000458 0.000299 0.000223 0.000167 0.000155 0.000148
     0.000143 0.000136 0.000128 0.000121 0.000124 0.000144 0.000186
     0.000243 0.000309 0.000369 0.000416 0.000441 0.000451 0.000459
     0.000471 0.000480 0.000486 0.000491 0.000496 0.000505 0.000522
     0.000550 0.000588 0.000632 0.000681 0.000737 0.000800 0.000871
     0.000949 0.001035 0.001131 0.001237 0.001354 0.001484 0.001623
     0.001760 0.001894 0.002029 0.002180 0.002353 0.002541 0.002747
     0.002976 0.003233 0.003523 0.003849 0.004214 0.004621 0.005083
     0.005594 0.006144 0.006731 0.007369 0.008061 0.008837 0.009729
     0.010758 0.011909 0.013209 0.014590 0.015949 0.017242 0.018547
     0.020032 0.021768 0.023697 0.025843 0.028258 0.031071 0.034292
     0.037840 0.041720 0.046042 0.051013 0.056716 0.063090 0.070175
     0.078071 0.086897 0.096754 0.107719 0.119836 0.133124 0.147587
     0.163214 0.179988 0.197882 0.216861 0.236103 0.255356 0.274345
     0.292777 0.310343 0.328964 0.348701 0.369624 0.391801 0.415309
     0.440228 0.466641 0.494640 0.524318 0.555777 0.589124 0.624471
     0.661939 0.701655 0.743513 0.780688 0.819722 0.860709 0.903744
     0.948931)));;*female-dp*



(defclass human ()
  (
   (birthday
    :accessor birthday
    :initarg :birthday
    :initform *year*
    :type     integer
    :documentation "The year of birth.")
   )
  (:documentation "A human.")
  );;HUMAN


(defmethod age ((self human))
  (- *year* (birthday self))
  );;AGE


(defmethod live ((self human))
  (if (should-die self)
    (die self))
  );;LIVE


(defmethod die ((self human))
  (setq *population* (delete self *population*))
  (push self *cemetery*)
  );;DIE


(defclass man (human)
  ()
  (:documentation "A man.")
  );;MAN


(defmethod should-die ((self man))
  (or (<= (length *male-dp*) (age self))
      (< (random 1.0) (aref *male-dp* (age self)) ))
  );;should-die



(defclass woman (human)
  (
   (number-of-children
    :accessor number-of-children
    :initform 0
    :type integer
    :documentation "The number of children brought by this woman.")
   )
  (:documentation "A woman.")
  );;WOMAN


(defmethod should-die ((self woman))
  (or (<= (length *female-dp*) (age self))
      (< (random 1.0) (aref *female-dp* (age self)) ))
  );;should-die


(defmethod live ((self woman))
  (if (and (<= 20 (age self))
           (< (number-of-children self) 12))
    (give-birth self))
  (call-next-method)
  );;LIVE


(defmethod give-birth ((self woman))
  (when (some (lambda (being) (and (typep being 'man)
                                   (<= 20 (age being)))) *population*)
    (push (make-instance (if (oddp (random 2)) 'man 'woman)) *population*))
  );;GIVE-BIRTH



(defun fill-histogram (n-slice slice-width values)
  (let ((histogram (make-array (list n-slice)
                               :initial-element 0
                               :element-type 'integer)))
    (dolist (value values)
      (incf (aref histogram (truncate value slice-width))))
    histogram)
  );;FILL-HISTOGRAM


(defun max-array (array)
  (let ((result (aref array 0)))
    (dotimes (i (length array))
      (setq result (max result (aref array i))))
    result)
  );;MAX-ARRAY


(defun make-bar (alignment width value max-value)
  (let* ((bar-width (truncate (* width value) max-value))
         (rest (- width bar-width))
         (left (truncate rest 2))
         (left (truncate rest 2))
         (result (make-string width)))
    (case alignment
      ((:left)
       (fill result (character "#") :start 0 :end bar-width)
       (fill result (character " ") :start bar-width))
      ((:center)
       (fill result (character " ") :start 0 :end left)
       (fill result (character "#") :start left :end (+ left bar-width))
       (fill result (character " ") :start (+ left bar-width)))
      ((:right)
       (fill result (character " ") :start 0 :end (- width bar-width))
       (fill result (character "#") :start (- width bar-width))))
    result)
  );;MAKE-BAR


(defun print-pyramid (men-ages dead-men women-ages dead-women)
  (let* ((age-slice 5)
         (width 26)
         (max-age (max (apply (function max) men-ages)
                       (apply (function max) women-ages)))
         (n-slice (truncate (+ max-age age-slice -1) age-slice))
         (men     (fill-histogram n-slice age-slice men-ages))
         (women   (fill-histogram n-slice age-slice women-ages))
         (max-count (max (max-array men) (max-array women))))
    (format t "~10A: ~VA ~4D : ~4D ~vA~%"
            "Deceased"  width "" dead-men  dead-women width "")
    (dotimes (j n-slice)
      (let ((i (- n-slice 1 j)))
        (format t "~3D to ~3D: ~VA ~4D : ~4D ~VA~%"
                (* i age-slice) (1- (* (1+ i) age-slice))
                width (make-bar :right  width (aref men   i) max-count)
                (aref men i) (aref women i)
                width (make-bar :left width (aref women i) max-count)))
      ))
  );;PRINT-PYRAMID



;;
;;
;; Table 1: Abbreviated decennial life table for U.S. Males.
;; From: National Center for Health Statistics (1997).
;; -----------------------------------------------------------
;;   x     l(x)  d(x)     q(x)     m(x)   L(x)     T(x)   e(x)
;; -----------------------------------------------------------
;;   0   100000  1039  0.01039  0.01044  99052  7182893   71.8
;;   1    98961    77  0.00078  0.00078  98922  7083841   71.6
;;   2    98883    53  0.00054  0.00054  98857  6984919   70.6
;;   3    98830    41  0.00042  0.00042  98809  6886062   69.7
;;   4    98789    34  0.00035  0.00035  98771  6787252   68.7
;;   5    98754    30  0.00031  0.00031  98739  6688481   67.7
;;   6    98723    27  0.00028  0.00028  98710  6589742   66.7
;;   7    98696    25  0.00026  0.00026  98683  6491032   65.8
;;   8    98670    22  0.00023  0.00023  98659  6392348   64.8
;;   9    98647    19  0.00020  0.00020  98637  6293689   63.8
;;  10    98628    16  0.00017  0.00017  98619  6195051   62.8
;;
;;  20    97855   151  0.00155  0.00155  97779  5211251   53.3
;;
;;  30    96166   197  0.00205  0.00205  96068  4240855   44.1
;;
;;  40    93762   295  0.00315  0.00316  93614  3290379   35.1
;;
;;  50    89867   566  0.00630  0.00632  89584  2370098   26.4
;;  51    89301   615  0.00689  0.00691  88993  2280513   25.5
;;
;;  60    81381  1294  0.01591  0.01604  80733  1508080   18.5
;;
;;  70    64109  2312  0.03607  0.03674  62953   772498   12.0
;;
;;  75    51387  2822  0.05492  0.05649  49976   482656    9.4
;;  76    48565  2886  0.05943  0.06127  47121   432679    8.9
;;
;;  80    36750  3044  0.08283  0.08646  35228   261838    7.1
;;
;;  90     9878  1823  0.18460  0.20408   8966    38380    3.9
;;
;; 100      528   177  0.33505  0.40804    439     1190    2.3
;; -----------------------------------------------------------
;;
;; The columns of the table, from left to right, are:
;;
;; x: age
;;
;; l(x), "the survivorship function": the number of persons alive at age
;; x. For example of the original 100,000 U.S. males in the hypothetical
;; cohort, l(50) = 89,867 (or 89.867%) live to age 50.
;;
;; d(x): number of deaths in the interval (x,x+1) for persons alive at
;; age x. Thus of the l(50)=89,867 persons alive at age 50, d(50) = 566
;; died prior to age 51.
;;
;; q(x): probability of dying at age x. Also known as the (age-specific)
;; risk of death. Note that q(x) = d(x)/l(x), so, for example, q(50) =
;; 566 / 89,867 = 0.00630.
;;
;; m(x): the age-specific mortality rate. Computed as the number of
;; deaths at age x divided by the number of person-years at risk at age
;; x. Note that the mortality rate, m(x), and the probability of death,
;; q(x), are not identical. For a one year interval they will be close in
;; value, but m(x) will always be larger.
;;
;; L(x): total number of person-years lived by the cohort from age x to
;; x+1. This is the sum of the years lived by the l(x+1) persons who
;; survive the interval, and the d(x) persons who die during the
;; interval. The former contribute exactly 1 year each, while the latter
;; contribute, on average, approximately half a year. [At age 0 and at
;; the oldest age, other methods are used; for details see the National
;; Center for Health Statistics (1997) or Schoen (1988). Note: m(x) =
;; d(x)/L(x).]
;;
;; T(x): total number of person-years lived by the cohort from age x
;; until all members of the cohort have died. This is the sum of numbers
;; in the L(x) column from age x to the last row in the table.
;;
;; e(x): the (remaining) life expectancy of persons alive at age x,
;; computed as e(x) = T(x)/l(x). For example, at age 50, the life
;; expectancy is e(50) = T(50)/l(50) = 2,370,099/89,867 = 26.4.



(defun print-stats ()
  (let
      ((men        (remove-if (lambda (being) (typep being 'man))      *population*))
       (women      (remove-if (lambda (being) (not (typep being 'man)))*population*))
       (dead-men   (remove-if (lambda (being) (typep being 'man))      *cemetery*))
       (dead-women (remove-if (lambda (being) (not (typep being 'man)))*cemetery*)))
    (print-pyramid (mapcar (function age) men)   (length dead-men)
                   (mapcar (function age) women) (length dead-women)))
  );;PRINT-STATS


(defun simulate ()
  (setq *random-state* (make-random-state t))
  (setq *population* nil
        *cemetery*   nil
        *year*       0)
  (push (make-instance 'man)   *population*)
  (push (make-instance 'woman) *population*)
  (dotimes (y 80)
    (setq *year* (+ 20 y))
    (let* ((male (count-if (lambda (being) (typep being 'man)) *population*))
           (tpop (length *population*))
           (fema (- tpop male)))
      (format t "Year ~3D : ~5D m + ~5D f = ~5D total (~5D dead)~%"
              *year* male fema tpop (length *cemetery*))
      (mapc (lambda (being) (live being)) (copy-seq *population*))))
  (when (< 2 (length *population*))
    (print-stats))
  );;SIMULATE



;;;; douze.lisp                       -- 2004-03-14 01:38:09 -- pascal   ;;;;
