;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               date.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Handle times and dates.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-23 <PJB> Created.
;;;;BUGS
;;;;
;;;;    The current implementation uses Common Lisp universal-time
;;;;    and therefore inherits all the bugs of the CL universal-time
;;;;    specification.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2007
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


;;; See also: http://emr.cs.uiuc.edu/~reingold/calendars.shtml

;; From: Patrice Lespagnol <patrice.lespagnol@obs-nancay.fr>
;; Subject: Re: convert julian date to universal
;; Newsgroups: comp.lang.lisp
;; Date: Thu, 17 Jan 2008 03:50:07 -0800 (PST)
;; Organization: http://groups.google.com
;; Message-ID: <c2e4c6de-5eb1-4fdc-8644-665e6b68bc8d@v29g2000hsf.googlegroups.com>
;; 
;; > More likely the other way round (year month day).  The code can't
;; > possibly be correct though, since Julian days start at noon UT.
;; > Replacing (ceiling j) by (round j) should cure that.  At least, then
;; > the example given in the wikipedia entry comes out right.
;; Yes, that's right. The function was incorrect in case of a fractional
;; part  < 0.5.
;; 
;; (defun julian-to-gregorian-calendar (j)
;;   (let ((i 0)(l 0)(n 0)(x 0)(d 0))
;;     (setq l (+ (round j) 68569))
;;     (setq n (truncate (* l 4) 146097))
;;     (decf l (truncate (+ (* n 146097) 3) 4))
;;     (setq i (truncate (* (+ 1 l) 4000) 1461001))
;;     (decf l (- (truncate (* i 1461) 4) 31))
;;     (setq x (truncate (* l 80) 2447))
;;     (setq d (- l (truncate (* x 2447) 80)))
;;     (setq l (truncate x 11))
;;     (values (+ (* 100 (- n 49)) i l)
;;             (- (+ x 2) (* l 12))
;;             d)))


;; Note: We use several packages to avoid name clashes between
;;       similar functions for different calendars.
;;       grep -i defpackage date.lisp |awk '{printf ";;%7s%s\n","",$2}'
;;       "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY"
;;       "COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR"
;;       "COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR"
;;       "COM.INFORMATIMAGO.COMMON-LISP.DATE"



(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY"
  (:documentation "Internal date utilities.")
  (:use "COMMON-LISP")
  (:export "DATE" "DATE<" "DATE>" "DATE<=" "DATE>=" "DATE=" "DATE/="
           "DURATION"
           "*DURATION-KEYWORDS*"
           "SECONDE" "MINUTE" "HOUR" "DAY" "WEEK" "MONTH" "YEAR"
           "DURATION+" "DURATION-" "DURATION*" "DURATION*"
           "*SECONDE*" "*MINUTE*" "*HOUR*" "*DAY*" "*WEEK*" "*MONTH*" "*YEAR*"
           "*QUARTER*"
           "DURATION-BETWEEN" "DATE-BEFORE" "DATE-AFTER"
           ;; INTERNAL:
           "AS-LIST-OF-NUMBERS" "DEFINE-DATE-COMPARE-METHODS"
           "HMS60-TO-SECONDES" "HMS60-FROM-SECONDES"
           "COMPARE-LISTS-OF-NUMBERS"
           "CURRENT-TIMEZONE" "DST-IN-YEAR"
           "+JANUARY+" "+FEBRUARY+" "+MARCH+" "+APRIL+"
           "+MAY+" "+JUNE+" "+JULY+" "+AUGUST+"
           "+SEPTEMBER+" "+OCTOBER+" "+NOVEMBER+" "+DECEMBER+"
           "ENGLISH-NAME-OF-JULIAN-WEEKDAY" "ENGLISH-NAME-OF-JULIAN-MONTH"
           "COLLAPSE-JULIAN-DURATION"
           "JULIAN-DAY-NUMBER/GREGORIAN" "JULIAN-DAY-NUMBER/JULIAN"
           "JULIAN-DATE" "WEEKDAY-OF-JULIAN-DATE"))


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY")
  (:export "LEAP-YEAR-P" "DATE-FROM-DAY-NUMBER" "DATE-TO-DAY-NUMBER"))


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY")
  (:export "GREGORIAN-CALENDAR-DATE" "GREGORIAN"
           "SECONDE" "MINUTE" "HOUR" "DAY" "WEEK" "MONTH" "YEAR"
           "WEEKDAY"
           "TIMEZONE"  "TO-TIMEZONE"
           "+JANUARY+" "+FEBRUARY+" "+MARCH+" "+APRIL+"
           "+MAY+" "+JUNE+" "+JULY+" "+AUGUST+"
           "+SEPTEMBER+" "+OCTOBER+" "+NOVEMBER+" "+DECEMBER+"
           "AS-JULIAN-DATE" "LEAP-YEAR-P"
           "AS-UNIVERSAL-TIME"
           "PREVIOUS-DAY" "NEXT-DAY"
           "INCREMENT-DAY" "DECREMENT-DAY"
           "DURATION-BETWEEN" "DATE-AFTER" "DATE-BEFORE"
           ;; Internal:
           "AS-LIST-OF-NUMBERS"
           "DATE-FROM-DAY-NUMBER" "DATE-TO-DAY-NUMBER"))


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.DATE"
  (:nicknames "DATE")
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR"
                "GREGORIAN-CALENDAR-DATE" "GREGORIAN"
                "SECONDE" "MINUTE" "HOUR" "DAY" "WEEK" "MONTH" "YEAR"
                "WEEKDAY"
                "TIMEZONE"  "TO-TIMEZONE"
                "+JANUARY+" "+FEBRUARY+" "+MARCH+" "+APRIL+"
                "+MAY+" "+JUNE+" "+JULY+" "+AUGUST+"
                "+SEPTEMBER+" "+OCTOBER+" "+NOVEMBER+" "+DECEMBER+"
                "LEAP-YEAR-P"
                "AS-UNIVERSAL-TIME" "AS-JULIAN-DATE"
                "PREVIOUS-DAY" "NEXT-DAY"
                "INCREMENT-DAY" "DECREMENT-DAY"
                "DURATION-BETWEEN" "DATE-AFTER" "DATE-BEFORE")
  (:EXPORT "DATE" "DATE<" "DATE>" "DATE<=" "DATE>=" "DATE=" "DATE/="
           "DURATION"
           "*DURATION-KEYWORDS*"
           "SECONDE" "MINUTE" "HOUR" "DAY" "WEEK" "MONTH" "YEAR"
           "DURATION+" "DURATION-" "DURATION*" "DURATION*"
           "*SECONDE*" "*MINUTE*" "*HOUR*" "*DAY*" "*WEEK*" "*MONTH*" "*YEAR*"
           "*QUARTER*"
           "+JANUARY+" "+FEBRUARY+" "+MARCH+" "+APRIL+"
           "+MAY+" "+JUNE+" "+JULY+" "+AUGUST+"
           "+SEPTEMBER+" "+OCTOBER+" "+NOVEMBER+" "+DECEMBER+"
           "GREGORIAN-CALENDAR-DATE" "GREGORIAN"
           "WEEKDAY"
           "TIMEZONE"  "TO-TIMEZONE"
           "AS-UNIVERSAL-TIME" "AS-JULIAN-DATE"
           "PREVIOUS-DAY" "NEXT-DAY"
           "INCREMENT-DAY" "DECREMENT-DAY"
           "DURATION-BETWEEN" "DATE-BEFORE" "DATE-AFTER")
  (:DOCUMENTATION "
    Calendars, dates and times.

    Copyright Pascal Bourguignon 2007 - 2007
    
    This program is free software you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation either version
    2 of the License, or (at your option) any later version.
"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY")



(defun compare-lists-of-numbers (a b)
  "
PRE:    (and (proper-list-p a)
             (proper-list-p b)
             (every (function realp) a)
             (every (function realp) b))
RETURN: The lexicographical order of the two lists of numbers.
"
  (cond
    ((null a)            (if (null b) 0 -1))
    ((null b)            +1)
    ((= (car a) (car b))  (compare-lists-of-numbers (cdr a) (cdr b)))
    ((< (car a) (car b)) -1)
    (t                   +1)))


(defun test/compare-lists-of-numbers ()
  (dolist (test '((()  ()  0)
                  ((1) ()  +1)
                  (()  (1) -1)
                  ((1) (1) 0)
                  ((2) (1) +1)
                  ((1) (2) -1)
                  ((1 1 1)     (1 1 1)   0)
                  ((1 1 1 1)   (1 1 1)   +1)
                  ((1 1 1)     (1 1 1 1) -1)
                  ((2 1 1)     (1 1 1)   +1)
                  ((2 1 1 1)   (1 1 1)   +1)
                  ((2 1 1)     (1 1 1 1) +1)
                  ((0 1 1)     (1 1 1)   -1)
                  ((0 1 1 1)   (1 1 1)   -1)
                  ((0 1 1)     (1 1 1 1) -1)
                  ((1 2 1 1)   (1 1 1)   +1)
                  ((1 2 1 1 1) (1 1 1)   +1)
                  ((1 2 1 1)   (1 1 1 1) +1)
                  ((1 0 1 1)   (1 1 1)   -1)
                  ((1 0 1 1 1) (1 1 1)   -1)
                  ((1 0 1 1)   (1 1 1 1) -1)))
    (assert (= (compare-lists-of-numbers (first test) (second test))
               (third test)))))

(defun hms60-to-secondes (seconde minute hour)
  "
RETURN: The number of seconds corresponding to hour:minute:second in base sixty.
"
  (+ (* (+ (* hour 60) minute) 60) seconde))


(defun hms60-from-secondes (secondes)
  "
RETURN: seconde ; minute ; hour
POST:   (or (not (integerp secondes))
            (= secondes (multiple-value-call (function hms-to-secondes)
                                             (hms60-from-secondes secondes))))
"
  (multiple-value-bind (minutes seconde) (floor secondes 60)
    (multiple-value-bind (hour minute) (floor minutes 60)
      (values seconde minute hour))))


(defun test/hms60 ()
  #- (and)
  (loop
     :for secondes :from 0.0 :below 600.0  :by 13.1
     :do (assert (= secondes
                    (multiple-value-call (function hms60-to-secondes)
                      (hms60-from-secondes secondes)))
                 (secondes)))
  (loop
     :for secondes :from 0 :below 4000
     :do (assert (= secondes
                    (multiple-value-call (function hms60-to-secondes)
                      (hms60-from-secondes secondes)))
                 (secondes))))


(defun current-timezone ()
  "
RETURN:  The current default timezone, expressed as a number of seconds
         from Greenwich; East positive.
WARNING: COMMON-LISP convention is West positive.
"
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time (get-universal-time))
    (- (encode-universal-time se mi ho da mo ye 0)
       (encode-universal-time se mi ho da mo ye))))


(defun dst-in-year (year)
  "
RETURN: A list of (year m d) where DST is shifted in or out in the YEAR/
BUG:    This uses the undetermined  local timezone, we don't know
        what country DST is returned...
"
  (loop
     :for m :from 1 :to 12
     :nconc (loop
               :for d :from 1
               :below (aref
                       (if (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:leap-year-p year)
                           #(31 29 31 30 31 30 31 31 30 31 30 31)
                           #(31 28 31 30 31 30 31 31 30 31 30 31))
                       (1- m))
               :when (/= +day+  (- (encode-universal-time 0 0 0 (1+ d) m year)
                                   (encode-universal-time 0 0 0 d      m year)))
               :collect (list year m d
                              (/ (- (encode-universal-time 0 0 0 (1+ d) m year)
                                    (encode-universal-time 0 0 0 d      m year))
                                 60 60)))))


(defconstant +january+    1)
(defconstant +february+   2)
(defconstant +march+      3)
(defconstant +april+      4)
(defconstant +may+        5)
(defconstant +june+       6)
(defconstant +july+       7)
(defconstant +august+     8)
(defconstant +september+  9)
(defconstant +october+   10)
(defconstant +november+  11)
(defconstant +december+  12)


(defun english-name-of-julian-weekday (weekday)
  "
RETURN: A string containing the name in English of the WEEKDAY.
"
  (aref #("Monday" "Tuesday" "Wednesday"
          "Thirsday" "Friday" "Saturday"
          "Sunday")
        weekday))


(defun english-name-of-julian-month (month)
  "
RETURN: A string containing the name in English of the MONTH
        (Julian, Gregorian, etc, calendars).
"
  (aref #("January" "February" "Mars" "April"
          "May" "June" "July" "August"
          "September" "October" "November")
        (1- month)))


(defun julian-day-number/gregorian (day month year)
  "
RETURN: The Julian Day Number for the given Gregorian Date.
URL:    http://en.wikipedia.org/wiki/Julian_day#Calculation
"
  (let* ((a (floor (- 14 month) 12))
         (y (- (+ year 4800) a))
         (m (+ month (* 12 a) -3)))
    (+ day
       (floor (+ (* 153 m) 2) 5)
       (* 365 y)
       (floor y 4)
       (- (floor y 100))
       (floor y 400)
       -32045)))


(defun julian-day-number/julian (day month year)
  "
RETURN: The Julian Day Number for the given Julian Date.
URL:    http://en.wikipedia.org/wiki/Julian_day#Calculation
"
  (let* ((a (floor (- 14 month) 12))
         (y (- (+ year 4800) a))
         (m (+ month (* 12 a) -3)))
    (+ day
       (floor (+ (* 153 m) 2) 5)
       (* 365 y)
       (floor y 4)
       -32083)))


(defun julian-date (seconde minute hour julian-day-number)
  "
RETURN: The Julian Date for the given time and Julian Day Number.
URL:    http://en.wikipedia.org/wiki/Julian_day#Calculation
"
  (+ julian-day-number (/ (- hour 12) 24) (/ minute 1440) (/ seconde 86400)))


(defun weekday-of-julian-date (julian-date)
  "
RETURN: The Day of the week of the given Julian Day.
        0 = Monday, 1 = Tuesday, ..., 6 = Sunday.
URL:    http://en.wikipedia.org/wiki/Julian_day#Calculation
"
  (mod (truncate (+ julian-date 1/2)) 7))



;;; ---------------------------------------- ;;;
;;; ABSTRACT CALENDAR 
;;; ---------------------------------------- ;;;

(defgeneric as-list-of-numbers (date)
  (:documentation "Return the values of the date from the most significant
number to the last. It can be a list of a single number.
This is meaningful only in the context of the given date class, and used
to compare two dates of the same class."))

(defgeneric units-of-list-of-numbers (date)
  (:documentation "Return a list of duration keywords corresponding
to the numbers returned by as-list-of-numbers.
These keywords may not necessarily be already in *DURATION-KEYWORDS*,
but could be merged there."))


;;; First define the generic functions for date<, date=, etc...
(macrolet ((define-generic-date-compare (name)
             `(defgeneric ,name  (date1 date2)
                (:method (date1 date2)
                  (error (if (and (typep date1 (type-of date2))
                                  (typep date2 (type-of date1)))
                             "~S: Arguments are not comparable dates: ~S vs. ~S"
                             "Both dates must be of the same class ~
                              to be compared with ~S; ~S vs. ~S")
                         ',name date1 date2)))))
  (define-generic-date-compare date<)
  (define-generic-date-compare date>)
  (define-generic-date-compare date<=)
  (define-generic-date-compare date>=)
  (define-generic-date-compare date=)
  (define-generic-date-compare date/=))


;;; Then define the macros for date<, date=, for any class having
;;; an as-list-of-numbers method:
(defmacro define-date-compare-methods (class)
  `(macrolet ((define-date-compare (name op)
               `(defmethod ,name ((a ,',class) (b ,',class))
                  (,op (compare-lists-of-numbers (as-list-of-numbers a)
                                                 (as-list-of-numbers b)) 0))))
    (define-date-compare date<  <)
    (define-date-compare date>  >)
    (define-date-compare date<= <=)
    (define-date-compare date>= >=)
    (define-date-compare date=  =)
    (define-date-compare date/= /=)
    ',class))



(defgeneric duration-between (end start)
  (:documentation "Return the DURATION between END and START.
Both END and START dates must be of the same class."))


;;; ---------------------------------------- ;;;
;;; DURATION
;;; ---------------------------------------- ;;;



(defparameter *duration-keywords*
  '(:year :month :week :day :hour :minute :seconde)
  "A list of keywords allowed in duration expressions,
in the order they should be printed.")


(defclass duration ()
  ((expression :documentation "A p-list" :reader expression))
  (:documentation "
We store durations 'symbolically'. The 'units' may be colinear in a
given calendar, but we don't collapse them because either they may
be not colinear in other calendars, or their ratio may not be
always the same (eg. 1 year is 24 months in the Davian calendar vs
12 months in the Gregorian calendar, or 1 month may be 28, 29, 30
or 31 days).

There is no order for durations.

Moreover, the set of units is not hardwired, the client may add new
units to the *DURATION-KEYWORDS* list.  Only the units listed in
*DURATION-KEYWORDS* are processed by the DURATION arithmetic operators.

This allows us to use these DURATION objects with respect to any
calendar.
"))


(defun duration (&rest expression &key &allow-other-keys)
  "
RETURN:  A new DURATION instance, initialized with the EXPRESSION.
NOTE:    Only the keywords listed in *DURATION-KEYWORDS* are really allowed.
"
  (let ((keywords (loop :for k :in expression :by (function cddr) :collect k)))
    (unless (subsetp keywords *duration-keywords*)
      (error "Unexpected keyword for ~S: ~S"
             'duration (delete-duplicates
                        (set-difference  keywords *duration-keywords*)))))
  (let ((duration (make-instance 'duration)))
    (setf (slot-value duration 'expression)
          (loop
             :for k :in *duration-keywords*
             :for v = (getf expression k)
             :when v :nconc (list k v)))
    duration))


(defmethod print-object ((self duration) stream)
  (if *print-escape*
      (if *print-readably*
          (format stream "#.(~S ~{~S~^ ~})"
                  'duration (slot-value self 'expression))
          (print-unreadable-object (self stream :identity nil :type t)
            (let ((*print-escape* nil))
              (print-object self stream))))
      (format stream "~(~{~*~A ~2:*~A~P~^, ~}~)"
              (slot-value self 'expression)))
  self)


(macrolet ((define-accessor (name &optional no-writer-p)
             `(progn
                (defmethod ,name ((self duration))
                  (getf (slot-value self 'expression)
                        ',(intern (string name) "KEYWORD") 0))
                ,@(unless
                   no-writer-p
                   `((defmethod (setf ,name) (value (self duration))
                       (setf (getf (slot-value self 'expression)
                                   ',(intern (string name) "KEYWORD"))
                             value))))
                ',name)))
  (define-accessor seconde)
  (define-accessor minute)
  (define-accessor hour)
  (define-accessor day)
  (define-accessor week)
  (define-accessor month)
  (define-accessor year))


(defun op-plist (a b n)
  "a+nb"
  (loop
     :with r = '()
     :for k :in *duration-keywords*
     :for u = (getf a k)
     :for v = (getf b k)
     :do (if u
             (if v
                 (let ((s (+ u (* n v))))
                   (unless (zerop s)
                     (push k r) (push s r)))
                 (progn (push k r) (push u r)))
             (when v
               (let ((s (* n v)))
                 (unless (zerop s)
                   (push k r) (push s r)))))
     :finally (return (nreverse r))))


(defmethod duration+ ((a duration) (b duration))
  (apply (function duration)
         (op-plist (slot-value a 'expression) (slot-value b 'expression) 1)))

(defmethod duration- ((a duration) (b duration))
  (apply (function duration)
         (op-plist (slot-value a 'expression) (slot-value b 'expression) -1)))

(defmethod duration* ((a duration) (b real))
  (apply (function duration)
         (op-plist '() (slot-value a 'expression) b)))

(defmethod duration/ ((a duration) (b real))
  (apply (function duration)
         (op-plist '() (slot-value a 'expression) (/ b))))


(defparameter *seconde* (duration :seconde 1))
(defparameter *minute*  (duration :minute  1))
(defparameter *hour*    (duration :hour    1))
(defparameter *day*     (duration :day     1))
(defparameter *week*    (duration :week    1))
(defparameter *month*   (duration :month   1))
(defparameter *quarter* (duration :month   3))
(defparameter *year*    (duration :year    1))


(defmethod collapse-julian-duration ((self duration))
  "
DO:     Collapse weeks as 7 days.
RETURN: secondes; minutes; hours; days; months; years
NOTE:   If you add keywords to *DURATION-KEYWORDS* you may have to
        override this method. 
"
  (values (seconde self) (minute self) (hour self)
          (+ (day self) (* 7 (week self))) (month self) (year self)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR")

(defconstant +days-in-fouryears+    (+ (* 3 365) 366))
(defconstant +days-in-fourcentury+  (* 25 +days-in-fouryears+)
  "Number of days in 400 years.
In the Julian calendar, the four-year cycle repeats indefinitely.")


(defun leap-year-p (year)
  "
RETURN: Whether YEAR is a julian leap year.
NOTE:   We don't implement here the actual leap years (they started with
        a leap year every three years, since they used to count both the
        start and end items...). 
"
  (zerop (mod year 4)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR")


(defconstant +days-in-fouryears+    (+ (* 3 365) 366))
(defconstant +days-in-fourcentury+  (+ (* 97 +days-in-fouryears+)
                                       (*  3 (* 4 365)))
  "Number of days in 400 gregorian years:
96 groups of 4 years including 3 flat years and one leap year
+ 3 groups of four flat years, multiple of 100 not multiple of 400
+ 1 group of 4 years including 3 flat years and one leap year,
multiple of 400.")


(defun leap-year-p (year)
  "
RETURN: Whether YEAR is a gregorian leap year.
"
  (or (zerop (mod year 400))
      (and (not (zerop (mod year 100)))
           (zerop (mod year 4)))))


;; (cons 0 (reverse
;;          (loop
;;             :for f
;;             :on (reverse
;;                  (list
;;                   (* 25 (+ (* 3 365) 366)) ; first century
;;                   (1- (* 25 (+ (* 3 365) 366))) ; second century
;;                   (1- (* 25 (+ (* 3 365) 366))) ; third century
;;                   (1- (* 25 (+ (* 3 365) 366))))) ; fourth century
;;             :collect (reduce '+ f))))             ; fourth century


;; (loop
;;    :for v :in '((31 29 31 30 31 30 31 31 30 31 30 31)
;;                 (31 28 31 30 31 30 31 31 30 31 30 31))
;;    :collect (loop
;;                :for f :in (cons 0 (butlast v))
;;                :for s = f :then (+ s f)
;;                :collect s))



(declaim (inline days-in-month))
(defun days-in-month (year month)
  (aref (if (leap-year-p year)
            #(31 29 31 30 31 30 31 31 30 31 30 31)
            #(31 28 31 30 31 30 31 31 30 31 30 31))
        month))



(declaim (inline days-in-year))
(defun days-in-year (year)
  (if (leap-year-p year)
      #(0 31 60 91 121 152 182 213 244 274 305 335 366)
      #(0 31 59 90 120 151 181 212 243 273 304 334 365)))


;; 400 404         500 504         600 604         700 704
;; +---+---+---....----+---+---....----+---+---....----+---+---...


(defun date-from-day-number (day-number)
  (multiple-value-bind (4centuries rest) (floor day-number
                                                +days-in-fourcentury+)
    (let ((centuries 0))
      (when (<= 36525 rest)
        (multiple-value-setq (centuries rest) (floor (- rest 36525) 36524))
        (incf centuries))
      (multiple-value-bind (4years rest)
          (if (zerop centuries)
              (floor rest +days-in-fouryears+)
              (if (< rest (1- +days-in-fouryears+))
                  (values 0 rest)
                  (multiple-value-bind (4years rest)
                      (floor (- rest +days-in-fouryears+ -1)
                                +days-in-fouryears+)
                    (values (1+ 4years) rest))))
        (let ((years 0))
          (if (and (plusp centuries) (zerop 4years)) ; 500, 600, 700
              (multiple-value-setq (years rest) (floor rest 365))
              (when (<= 366 rest)
                (multiple-value-setq (years rest) (floor (- rest 366) 365))
                (incf years)))
          (let ((year (+ (* 400 4centuries) (* 100 centuries)
                         (* 4 4years) years)))
            (if (zerop rest)
                (values 1 1 year)
                (loop
                   :with c = (days-in-year year)
                   :for month :from 0 
                   :while (<= (aref c month) rest)
                   :finally (return (values
                                     (- rest (aref c (1- month)) -1)
                                     month year))))))))))



(defun date-to-day-number (day month year)
  (multiple-value-bind (yinc month) (floor (1- month) 12)
    (incf year yinc)
    (incf month)
    (multiple-value-bind (4centuries  rest) (floor year 400)
      (multiple-value-bind (centuries rest) (floor rest 100)
        (multiple-value-bind (4years  rest) (floor rest 4)
          (+ (* +days-in-fourcentury+ 4centuries)
             (aref #(0 36525 73049 109573) centuries)
             (if (zerop centuries)
                 (+ (* +days-in-fouryears+ 4years)
                    (aref #(0 366 731 1096) rest))
                 (if (zerop 4years)
                     (+ (* +days-in-fouryears+ 4years)
                        (aref #(0 365 730 1095) rest))
                     (+ (1- +days-in-fouryears+)
                        (* +days-in-fouryears+ (1- 4years))
                        (aref #(0 366 731 1096) rest))))
             (aref (days-in-year year) (1- month))
             (1- day)))))))


(defun check-day-number (&key trace-day  print-all
                         (start 0) (end +days-in-fourcentury+))
  (loop
     :for i :from start :below end
     :do (multiple-value-bind (d m y) (DATE-FROM-DAY-NUMBER i)
           (when (or print-all (and trace-day (<= (abs (- trace-day i)) 10)))
             (format t "~%~6D ~4,'0D-~2,'0D-~2,'0D" i y m d))
           (assert (= i (date-to-day-number d m y))
                   (i y m d)
                   "~%~6D ~4,'0D-~2,'0D-~2,'0D ~6D" i y m d
                   (date-to-day-number d m y))))) 



#-(and)
(CHECK-DAY-NUMBER :start (- +days-in-fourcentury+) :print-all nil)




(defclass gregorian-calendar-date ()
  ((year     :initarg :year     :accessor year     :initform 1582)
   (month    :initarg :month    :accessor month    :initform 10)
   (day      :initarg :day      :accessor day      :initform 15)
   (hour     :initarg :hour     :accessor hour     :initform 12)
   (minute   :initarg :minute   :accessor minute   :initform 0)
   (seconde  :initarg :seconde  :accessor seconde  :initform 0)
   (timezone :initarg :timezone :accessor timezone
             :initform nil
             :documentation "Number of seconds from Greenwich; East positive.")
   ;; Note: It would be more historically correct to define the Gregorian 
   ;;       timezone relative to Rome, but it will be less confusing if all 
   ;;       the calendars of a same planet are relative to the same longitud.
   (weekday  :initform nil
             :documentation "Weekday cache.")))


(defmethod print-object ((self gregorian-calendar-date) stream)
  (flet ((print-it ()
           (with-slots (timezone) self
             (multiple-value-bind (tzh tzm)
                 (and timezone (floor (floor (abs timezone) 60) 60))
               (format stream
                       "~9A ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~
                   ~:[local timezone~;~:[+~;-~]~2,'0D~2,'0D~]"
                       (english-name-of-julian-weekday (weekday self))
                       (year self) (month self) (day self)
                       (hour self) (minute self) (seconde self)
                       timezone (and timezone (minusp timezone)) tzh tzm)))))
    (if *print-escape*
        (print-unreadable-object (self stream :identity nil :type t)
          (print-it))
        (progn (format stream  "Gregorian ") (print-it))))
  self)



(declaim (inline gregorian))
(defun gregorian (&key (year 1582 yearp) (month +october+ monthp) (day 15 dayp)
                  (hour 0 hourp) (minute 0 minutep) (seconde 0 secondep)
                  (timezone nil)
                  (universal-time nil))
  (if universal-time
      (multiple-value-bind (se mi ho da mo ye)
          (apply (function decode-universal-time) universal-time
                 (when timezone (list timezone)))
        (gregorian :year     (if yearp    year    ye)
                   :month    (if monthp   month   mo)
                   :day      (if dayp     day     da)
                   :hour     (if hourp    hour    ho)
                   :minute   (if minutep  minute  mi)
                   :seconde  (if secondep seconde se)
                   :timezone timezone))
      (multiple-value-bind (se mi ho)
          (hms60-from-secondes (hms60-to-secondes seconde minute hour))
        (multiple-value-bind (dinc ho) (floor ho 24)
          ;; Note: since we increment the day first, a null timezone,
          ;; denoting the current timezone, may be a different timezone
          ;; than the original day, for DST.
          (multiple-value-bind (da mo ye)
              (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-FROM-DAY-NUMBER
               (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-TO-DAY-NUMBER
                (+ day dinc) month year))
            (make-instance 'gregorian-calendar-date
              :year ye :month  mo :day da
              :hour ho :minute mi :seconde se
              :timezone timezone))))))



(defmethod weekday ((self gregorian-calendar-date))
  (or (slot-value self 'weekday)
      (setf (slot-value self 'weekday)
            (with-slots (day month year) self
              (mod (- (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-TO-DAY-NUMBER day month year) #.(1+ (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-TO-DAY-NUMBER 15 3 1964))) 7)))))


(defmethod as-julian-date ((self gregorian-calendar-date))
  (with-slots (seconde minute hour day month year) (to-timezone self 0)
    (julian-date seconde minute hour (julian-day-number/gregorian day month year))))



;; (gregorian :year 2007 :month 6 :day 13 :hour 12 :timezone 0)
;; 2454264.
;;  June 13, 2007 (UTC) the JDN is  


(defmethod to-timezone ((self gregorian-calendar-date) timezone)
  (let* ((new-timezone (or timezone (current-timezone)))
         (my-timezone  (or (timezone self) (current-timezone))))
    (with-slots (seconde minute hour day month year) self
          (gregorian :seconde (+ seconde (- new-timezone my-timezone))
                     :minute minute :hour hour
                     :day day :month month :year year
                     :timezone new-timezone))))


(defmethod as-list-of-numbers ((self gregorian-calendar-date))
  (with-slots (year month day hour minute seconde) (to-timezone self 0)
     (list year month day hour minute seconde)))

(defmethod units-of-list-of-numbers  ((self gregorian-calendar-date))
  (list :year :month :day :hour :minute :seconde))


(define-date-compare-methods gregorian-calendar-date)



(defmethod as-universal-time ((self gregorian-calendar-date))
  (with-slots (year month day hour minute seconde timezone) self
    (apply (function encode-universal-time) seconde minute hour day month year
           (when timezone (list (/ timezone -3600))))))




(defmethod next-day      ((self gregorian-calendar-date) &optional (increment 1))
  (with-slots (year month day hour minute seconde timezone) self
    (gregorian :year year :month month :day (+ day increment)
               :hour hour :minute minute :seconde seconde
               :timezone timezone)))


(defmethod increment-day ((self gregorian-calendar-date) &optional (increment 1))
  (with-slots (year month day weekday) self
    (multiple-value-bind (da mo ye)
        (DATE-FROM-DAY-NUMBER
         (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-TO-DAY-NUMBER
          (+ day increment) month year))
      (setf year ye month mo day da weekday nil)
      self)))


(defmethod previous-day  ((self gregorian-calendar-date) &optional (increment 1))
  (next-day self (- increment)))

(defmethod decrement-day ((self gregorian-calendar-date) &optional (increment 1))
  (increment-day self (- increment)))



(defmethod duration-between ((end   gregorian-calendar-date)
                             (start gregorian-calendar-date))
  (with-slots ((eye year) (emo month) (eda day)
               (eho hour) (emi minute) (ese seconde)) (to-timezone end 0)
    (with-slots ((sye year) (smo month) (sda day)
                 (sho hour) (smi minute) (sse seconde)) (to-timezone start 0)
      (let ((ye (- eye sye))
            (mo (- emo smo))
            (da (- eda sda))
            (ho (- eho sho))
            (mi (- emi smi))
            (se (- ese sse)))
        (apply (function duration)
               (append (unless (zerop ye) (list :year    ye))
                       (unless (zerop mo) (list :month   mo))
                       (unless (zerop da) (list :day     da))
                       (unless (zerop ho) (list :hour    ho))
                       (unless (zerop mi) (list :minute  mi))
                       (unless (zerop se) (list :seconde se))))))))



(defmethod date-after ((self gregorian-calendar-date) (duration duration))
  (multiple-value-bind (se mi ho da mo ye) (collapse-julian-duration duration)
    (with-slots (seconde minute hour day month year) self
      (gregorian :seconde  (+ se seconde)
                 :minute   (+ mi minute)
                 :hour     (+ ho hour)
                 :day      (+ da day)
                 :month    (+ mo month)
                 :year     (+ ye year)
                 :timezone (timezone self)))))



(defmethod date-before ((self gregorian-calendar-date) (duration duration))
  (date-after self (duration- duration)))





;; (let ((day  (gregorian :year 2007 :month 3 :day 24 :hour 12)))
;;   (duration-between (date-after day (duration :day 1))  day))
;; #<DURATION 1 day>

;; (let ((day (gregorian :year 2007 :month 3 :day 24 :hour 12)))
;;   (- (as-universal-time (date-after day (duration :day 1)))
;;      (as-universal-time day)))
;; 82800



;; (gregorian :year 1582 :month +october+ :day 15)






;; first monday of the month


;; (defun julian-date-now ()
;;   "
;; RETURN: The julian-DATE of now.
;; "
;;   (error "Not implemented yet.")
;;   (get-universal-time))
;; 
;; 
;; (defun today ()
;;   "
;; RETURN: The DATE of today, that is, the time 00:00:00 in the same day as now.
;; "
;;   (make-instance 'date :date (now) :hour 0 :minute 0 :seconde 0))
;; 
;; 
;; (defmethod monday-before ((date date))
;;   "
;; RETURN:  The DATE of the Monday before date.
;; "
;;   (let ((day (make-instance 'date :date date :hour 0 :minute 0 :seconde 0)))
;;     (date-before day (make-instance 'duration
;;                          :secondes (* +day+ (day-of-week today))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

    On the surface of the same planet, the time flows about the same
    speed everywhere. (There are some places where gravitation
    anomalities make time flow at different speeds, but they're very
    localized).

    On the other hand, on the surfaces of different planets (with
    different gravity, if only that of the Sun), or on different
    orbits, time flows at different speeds, which gives macroscopic
    offsets over time periods of about a year, and which are very
    significant for GPS like applications.

    Proper times flow at different (and possibly varying) speed
    depending on the speed and gravity of the clocks.  Therefore we'd
    need to keep track of several proper times and of the coordinate
    times, and be able to convert from one to the other with
    relativistic formulas.


    For now we just ignore this layer, and will use the CL
    universal-time, which is a kind of proper time.

        +-----------------------------------------------------+
        |  proper time | coordinate time | cl:universal-time  |
        +-----------------------------------------------------+
              TT              TCG
         Terrestrial       Geocentric 
             Time          Coordinate
                              Time

              MT
          Martian Time


    Another layer is that of the calendars.

    Calendars are naming schemes for time intervals, relatively to
    some place and some political entity.  Calendars may be "classes"
    of naming schemes, for example when dealing with time zones.  Note
    that time zones are recent invention (since trains), and before
    them there was a continuum of times all around the planet.  


    NASA defines for Mars a sol (= martian day) as 24 martian hours of
    60 martian minutes of 60 martian secondes.  These martian seconds
    aren't the same length as Earth seconds (first because a martian
    day is 24 hours 39 minutes 35.244 secondes, and then because of
    relativistic time dilation, one second on Mars is different than
    one second on Earth).  The Darian calendar defines the naming
    scheme for martian sols.


    In Gregorian calendar, day Tuesday June 12th 2007 CEST (= at that
    time UTC+2), starts from 3390588000 seconds (cl:universal-time)
    and ends at 3390674400 seconds (cl:universal-time).




    Time intervals, or durations, can be defined physically as the
    difference between two times expressed in the time unit (second).


    Calendar time intervals or durations are defined in terms of
    calendar.  These intervals may correspond to variable physical
    durations, depending on which calendar day they're counted from.
    One Gregorian "month" may be 2419200, 2505600, 2592000, 2678400,
    or even 2566800 or 2700000 (when summer/winter shifts occur)
    seconds long.  Gregorian legal "days" may be 23, 24 or 25 hours
    (summer/winter).  When leap seconds occur, one second may be added
    or subtracted from the day length.



    For these reasons, calendar calculations cannot be done in general
    as calculation on the physical time, but need to be done
    symbolically, in the context of each calendar.

    Some calculations can be done generically for the whole calendar
    class, but when computing times, it is in general needed to
    consider the time zone and the political entity relative to the
    calendar to get precise results.  For example, the number of hours
    between two calendar datetimes may depend on whether a
    summer/winter time transition occurs in between and these
    transition dates depend on the country and time zone.



    Historical calendars


    Days were divided in 12 hours of variable length according to the
    length of the diurn period.

    Calendar day used to started at noon, since it was the stable easily
    measurable time. See JULIAN-DATE too.



    Given a Julian-Date (TT), and a place (or political authority),
    we can determine the calendar used, and the name of the day and time,
    and vice-versa.

        (day-precedes (julian    :year 1582 :month +october+ :day  4)
                      (gregorian :year 1582 :month +october+ :day 15))

        (adoted-calendar
         ("Spain" 
          "Spain Territories"
          "Portugal"
          "Polish-Lithuanian Commonwealth"
          "Italy")
         (gregorian :year 1582 :month +october+ :day 15))


        (adoted-calendar
         ("France")
         (gregorian :year 1582 :month +december+ :day 20))

        (adoted-calendar
         ("Protestant Dutch Provinces of Holand and Zeeland")
         (gregorian :year 1582 :month +december+ :day :unknwon))

        (adopted-calendar
         ("British Empire")
         (gregorian :year 1752 :month ))


    
    

|#



#|

    (defconstant +sun-mass+   1.989e+30)
    (defconstant +earth-mass+ 5.9736e+24)
    (defconstant +mars-mass+  6.419e+23)

    (defconstant +earth-semimajor-axis+  149600000.0)
    (defconstant +mars-semimajor-axis+   227900000.0) 
    (defconstant +earth-radius-mean+       6371000.0)
    (defconstant +earth-equatorial-radius+ 6378100.0)
    (defconstant +mars-equatorial-radius+  3397000.0)

    (defconstant +earth-sidereal-orbit-period+ (* 365.256D0 24 60 60))
    (defconstant +mars-sidereal-orbit-period+  (* 686.98D0  24 60 60))

    (defconstant +G+ 6.672D-11      "Gravitational constant")
    (defconstant +c+ 299792458.0D0  "Speed of light")

    (declaim (inline square))
    (defun square (x) (* x x))

    (defun time-in-gravitational-field (time mass distance)
      ;; http://en.wikipedia.org/wiki/Gravitational_time_dilation#Important_things_to_stress
      ;; (= t0 (* tf (sqrt (- 1 (/ (* 2 G M) (* r (square c)))))))
      ;; 
      ;; * t0 is the proper time between events A and B for a slow-ticking
      ;;   observer within the gravitational field,
      ;; 
      ;; * tf is the proper time between events A and B for a fast-ticking
      ;;   observer distant from the massive object (and therefore outside of
      ;;   the gravitational field),
      ;; 
      ;; * G is the gravitational constant,
      ;; 
      ;; * M is the mass of the object creating the gravitational field,
      ;; 
      ;; * r is the radial coordinate of the observer (which is analogous to
      ;;   the classical distance from the center of the object, but is
      ;;   actually a Schwarzschild coordinate),
      ;; 
      ;; * c is the speed of light, and
      ;; 
      ;; * r0 = 2GM / c² is the called the Schwarzschild Radius of M. If a mass
      ;;   collapses so that its surface lies at less than this radial
      ;;   coordinate (or in other words covers an area of less than 16πG²M²/c⁴),
      ;;   then the object exists within a black hole.
      (* time (sqrt (- 1D0 (/ (* 2D0 +G+ mass) (* distance (square +c+)))))))


    (defun time-in-rotational-field (time distance angular-velocity)
      ;; http://en.wikipedia.org/wiki/Gravitational_time_dilation#Important_things_to_stress
      ;; td = (sqrt (- 1 (/ (* (square r) (square omega)) (square c))))
      ;;
      ;; * r is the distance from the center of the disk (which is the
      ;;   location of the base observer), and
      ;; 
      ;; * ω is the angular velocity of the disk.
      (* time (sqrt (- 1d0 (square (/ (*  distance angular-velocity) +c+))))))


    ;;; Time dilation between Earth and Mars due to the gravitational
    ;;; field of the Sun:
    ;; (let ((m (time-in-gravitational-field 1 +sun-mass+ +mars-semimajor-axis+))
    ;;       (e (time-in-gravitational-field 1 +sun-mass+ +earth-semimajor-axis+)))
    ;;   (/ (- m e) e))
    ;; 3.391119529856771d-6
    ;; (* 365 24 60 60 3.391119529856771d-6) = 106.94234549356312d0
    ;; or 1 minute and 46 secondes of difference each year...

    ;;; Time dilation between Earth and Mars due to the gravitational
    ;;; field on the surface of each planet:
    ;; (let ((m (time-in-gravitational-field 1 +mars-mass+  +mars-equatorial-radius+))
    ;;       (e (time-in-gravitational-field 1 +earth-mass+ +earth-equatorial-radius+)))
    ;;   (/ (- m e) e))
    ;; 5.550024887974419d-10

    ;;; Time dilation between Earth and Mars due to the rotation of the planets
    ;;; around the Sun:
    ;; (let ((m (time-in-rotational-field
    ;;           1 +mars-semimajor-axis+ (/ (* 2D0 pi) +mars-sidereal-orbit-period+)))
    ;;       (e (time-in-rotational-field
    ;;           1 +earth-semimajor-axis+ (/ (* 2D0 pi) +earth-sidereal-orbit-period+))))
    ;;   (/ (- m e) e))
    ;; 1.697589551569186946L-15


    (defconstant +mars-tropical-year+ 668.5921 "sol")
    (loop
         repeat 100
         for year from 0
         for i = 0 then (+ i  0.5921)
         do (format t "~&~4D ~8,6F " year i) 
         do (when (<= 1 i)  (decf i) (princ "bisextile" )))


|#



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Timezone file reading.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Authored by Daniel Lowe <dlowe@sanctuary.org>
;; ;;;
;; ;;; Copyright (c) 2005-2006 Daniel Lowe
;; ;;; 
;; ;;; Permission is hereby granted, free of charge, to any person obtaining
;; ;;; a copy of this software and associated documentation files (the
;; ;;; "Software"), to deal in the Software without restriction, including
;; ;;; without limitation the rights to use, copy, modify, merge, publish,
;; ;;; distribute, sublicense, and/or sell copies of the Software, and to
;; ;;; permit persons to whom the Software is furnished to do so, subject to
;; ;;; the following conditions:
;; ;;; 
;; ;;; The above copyright notice and this permission notice shall be
;; ;;; included in all copies or substantial portions of the Software.
;; ;;; 
;; ;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; ;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; ;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; ;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; ;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; ;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; ;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;; (defstruct timezone
;;   (transitions  nil :type list)
;;   (subzones     nil :type list)
;;   (leap-seconds nil :type list)
;;   (path         nil)
;;   (name         "anonymous" :type string)
;;   (loaded       nil :type boolean))
;; 
;; 
;; (defun read-binary-integer (stream byte-count &optional (signed nil))
;;   "Read BYTE-COUNT bytes from the binary stream STREAM, and
;; return an integer which is its representation in network byte order (MSB).
;; If SIGNED is true, interprets the most significant bit as a sign indicator."
;;   (loop for offset from (* (1- byte-count) 8) downto 0 by 8
;;      with result = 0
;;      do (setf (ldb (byte 8 offset) result) (read-byte stream))
;;      finally (if (and signed (< #x80000000 result))
;;                  (return (- result #x100000000))
;;                  (return result))))
;; 
;; 
;; (defun string-from-unsigned-vector (vector offset)
;;   "Returns a string created from the vector of unsigned bytes VECTOR
;; starting at OFFSET which is terminated by a 0."
;;   (let ((null-pos (or (position 0 vector :start offset) (length vector))))
;;     (with-output-to-string (str)
;;       (loop for idx from offset upto (1- null-pos)
;;          do (princ (code-char (aref vector idx)) str)))))
;; 
;; 
;; (defun realize-timezone (zone &optional reload)
;;   "If timezone has not already been loaded or RELOAD is non-NIL,
;; loads the timezone information from its associated unix file."
;;   (when (or reload (not (timezone-loaded zone)))
;;     (with-open-file (inf (timezone-path zone)
;;                          :direction :input
;;                          :element-type 'unsigned-byte)
;;       ;; read and verify magic number
;;       (let ((magic-buf (make-array 4 :element-type 'unsigned-byte)))
;;         (read-sequence magic-buf inf :start 0 :end 4)
;;         (when (string/= (map 'string #'code-char magic-buf) "TZif" :end1 4)
;;           (error "~a is not a timezone file." (timezone-path zone))))
;;       ;; skip 16 bytes for "future use"
;;       (let ((ignore-buf (make-array 16 :element-type 'unsigned-byte)))
;;         (read-sequence ignore-buf inf :start 0 :end 16))
;;       ;; read header values
;;       (let ((utc-indicator-count (read-binary-integer inf 4))
;;             (wall-indicator-count (read-binary-integer inf 4))
;;             (leap-count (read-binary-integer inf 4))
;;             (transition-count (read-binary-integer inf 4))
;;             (type-count (read-binary-integer inf 4))
;;             (abbrev-length (read-binary-integer inf 4)))
;;         (let ((timezone-transitions
;;                ;; read transition times
;;                (loop for idx from 1 upto transition-count
;;                      collect (read-binary-integer inf 4 t)))
;;               ;; read local time indexes
;;               (local-time-indexes
;;                (loop for idx from 1 upto transition-count
;;                      collect (read-binary-integer inf 1)))
;;               ;; read local time info
;;               (local-time-info
;;                (loop for idx from 1 upto type-count
;;                      collect (list (read-binary-integer inf 4 t)
;;                                    (/= (read-binary-integer inf 1) 0)
;;                                    (read-binary-integer inf 1))))
;;               ;; read leap second info
;;               (leap-second-info
;;                (loop for idx from 1 upto leap-count
;;                      collect (list (read-binary-integer inf 4)
;;                                    (read-binary-integer inf 4))))
;;               (abbreviation-buf (make-array abbrev-length :element-type 'unsigned-byte)))
;;           (read-sequence abbreviation-buf inf :start 0 :end abbrev-length)
;;           (let ((wall-indicators
;;                  ;; read standard/wall indicators
;;                  (loop for idx from 1 upto wall-indicator-count
;;                        collect (read-binary-integer inf 1)))
;;                 ;; read UTC/local indicators
;;                 (local-indicators
;;                  (loop for idx from 1 upto utc-indicator-count
;;                        collect (read-binary-integer inf 1))))
;;             (setf (timezone-transitions zone)
;;                   (nreverse
;;                    (mapcar
;;                     (lambda (info index)
;;                       (list info index))
;;                     timezone-transitions
;;                     local-time-indexes)))
;;             (setf (timezone-subzones zone)
;;                   (mapcar
;;                    (lambda (info wall utc)
;;                      (list (first info)
;;                            (second info)
;;                            (string-from-unsigned-vector abbreviation-buf (third info))
;;                            (/= wall 0)
;;                            (/= utc 0)))
;;                    local-time-info
;;                    wall-indicators
;;                    local-indicators))
;;             (setf (timezone-leap-seconds zone)
;;                   leap-second-info)))))
;;     (setf (timezone-loaded zone) t))
;;   zone)
;; 
;; 
;; (defparameter +utc-zone+ (make-timezone :subzones '((0 nil "UTC" nil nil))
;;                                         :name "UTC"
;;                                         :loaded t)
;;   "The zone for Coordinated Universal Time.")
;; 
;; 
;; (defmacro define-timezone (zone-name zone-file &key (load nil))
;;   "Define zone-name (a symbol or a string) as a new timezone,
;; lazy-loaded from zone-file (a pathname designator relative to the
;; zoneinfo directory on this system.  If load is true, load immediately."
;;   (declare (type (or string symbol) zone-name))
;;   (let ((zone-sym (if (symbolp zone-name) zone-name (intern zone-name))))
;;     `(prog1
;;          (defparameter ,zone-sym
;;            (make-timezone :path ,zone-file
;;                           :name ,(if (symbolp zone-name)
;;                                      (string-downcase (symbol-name zone-name))
;;                                      zone-name)))
;;        ,@(when load
;;                `((realize-timezone ,zone-sym))))))
;; 
;; 
;; (defvar *default-timezone*)
;; (eval-when (:load-toplevel :execute)
;;   (let ((default-timezone-file #p"/etc/localtime"))
;;     (if (probe-file default-timezone-file)
;;         (define-timezone *default-timezone* default-timezone-file :load t)
;;         (defparameter *default-timezone* +utc-zone+))))
;; 
;; 
;; (defparameter *timezone-repository* nil
;;   "A list of (list \"Europe/Budapest\" timezone) entries")
;; (defparameter *timezone-offset->timezone* (make-hash-table))
;; 
;; (eval-when (:load-toplevel :execute)
;;   (defun reread-timezone-repository ()
;;     (let* ((root-directory "/usr/share/zoneinfo/"
;;              #-(and) (merge-pathnames "zoneinfo/" *project-home-directory*))
;;            (cutoff-position (length (princ-to-string root-directory)))
;;            (visitor
;;             (lambda (file)
;;               (let* ((full-name (subseq (princ-to-string file) cutoff-position))
;;                      (name (pathname-name file))
;;                      (timezone (realize-timezone
;;                                 (make-timezone :path file :name name))))
;;                 (push (list full-name timezone) *timezone-repository*)
;;                 ;; TODO this entire *timezone-offset->timezone* is probably
;;                 ;;      useless this way, we can't reverse map a +01:30
;;                 ;;      offset to a timezone struct, or can we?
;;                 (dolist (subzone (timezone-subzones timezone))
;;                   (pushnew timezone (gethash (first subzone)
;;                                              *timezone-offset->timezone*)))))))
;;       (setf *timezone-repository* nil)
;;       (setf *timezone-offset->timezone* (make-hash-table))
;;       (walk-directory root-directory visitor :directories nil
;;                       :test (lambda (file)
;;                               (not (find "Etc" (pathname-directory file)
;;                                          :test #'string=))))
;;       ;; walk the Etc dir last, so they will be the first entries in
;;       ;; the *timezone-offset->timezone* map
;;       (walk-directory (merge-pathnames "Etc/" root-directory) visitor
;;                       :directories nil)
;;       (setf *timezone-repository*
;;             (sort *timezone-repository* #'string< :key #'first)))))
;; 
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;; (defclass julian-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass coptic-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass ethiopian-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass revised-julian-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass chinese-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass hebrew-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass islamic-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass hindu-calendar-day (calendar-day)
;;   ())
;; 
;; (defclass iranian-calendar-day (calendar-day)
;;   ())


;; # Aztec Calendar
;; # Bahá'í calendar
;; # Bengali calendar
;; # Buddhist calendar
;; # Calendar of saints
;; # Chinese calendar
;; # Christian calendar
;; # Discordian calendar
;; # Eastern Orthodox liturgical calendar
;; # Ethiopian calendar
;; # French Republican Calendar
;; # Gregorian calendar
;; # Hebrew calendar
;; # Hindu calendar
;; # Samvat
;; # Iranian calendar
;; # Islamic calendar
;; # Julian calendar
;; # Liturgical year
;; # Maya calendar
;; # Malayalam calendar
;; # Nanak Shahi calendar
;; # Pawukon calendar of Bali
;; # Perpetual calendar
;; # Runic calendar
;; # Wall calendar
;; # Zoroastrian calendar





#-(and)
(loop
   :repeat 365
   :with day = (gregorian :year 2007 :month 1 :day 1 :hour 12) 
   :do (princ day) (terpri) (increment-day day))

#- (and)
(com.informatimago.common-lisp.date.utility:as-list-of-numbers 
 (gregorian :year 2007 :month 9 :day 30))

(list
 (COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY:JULIAN-DAY-NUMBER/julian
  30 9 2007)
 
  (COM.INFORMATIMAGO.COMMON-LISP.DATE.UTILITY:JULIAN-DAY-NUMBER/GREGORIAN
  30 9 2007)

 (COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR:DATE-TO-DAY-NUMBER
  30 9 2007)
)


;;;; THE END ;;;;