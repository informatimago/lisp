;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               roman-calendar.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a Roman Calendar as described in:
;;;;    http://www.webexhibits.org/calendars/calendar-roman.html
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2015
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ROMAN-CALENDAR"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.UTILITY")
  (:export)
  (:documentation "

Defines the Roman calendar.


See also: COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE
          COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DATE.UTILITY
          COM.INFORMATIMAGO.COMMON-LISP.GREGORIAN-CALENDAR
          COM.INFORMATIMAGO.COMMON-LISP.JULIAN-CALENDAR

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2014 - 2014
    
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.ROMAN-CALENDAR")


;; Romulus from -700 
;; 10 months in 304 days plus 61 days year
(defvar *romulus-month-names* '("martius" "aprilis" "maius" "junius" "quintilis"
                                "sextilis" "september" "october" "november" "december"))
;; Numa Pompilius:
;; 355-day year
(defvar *numa-pompilius-month-names* '("january" "february" "mercedinus"
                                       "martius" "aprilis" "maius" "junius" "quintilis"
                                       "sextilis" "september" "october" "november" "december"))

;; mercedinus = 22 or 23 days, _inserted_ after february 23 or 24, every other year.

;; Julius Caesar, from -45 julian Calendar
;; 365 days + bisextile years every 4 years.  extra day after february 23.

;; Kalend 1st day of the month
;; Nones  5th (or 7th) day
;; Ides   15th (or 13th) day.

;; Days after Ides are counted down toward the next month's Kalends.

(defun nundial-letter (nundial)
  (aref "ABCDEFGHI" nundial))

(defun format-kalend (nundial kalend month)
  (format nil "~A~A·~A·F" (nundial-letter nundial) kalend month))

(defun format-special (nundial feriae)
  (format nil "~A~A·N" (nundial-letter nundial) feriae ))

(defun format-regular (nundial day)
  (format nil "~A~A" (nundial-letter nundial) day))

(("F" "dies fasti" "legal action and public voting are permited")
 ("N" "dies nefasti" "no legal action or public voting allowed")
 ("EN" "endotercisus" "days between F or C where morning and afternnons have different designation")
 ("NP" "" "major holidays -- all records have disappeared")
 ("FP" "" "religious holidays -- no definition survives"))

full-moon                               Ides
(+ full-moon 1) .. (- first-quarter 1)  Kalends
first-quarter                           Nones
