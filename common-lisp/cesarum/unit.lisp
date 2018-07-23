;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unit.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Unit converter.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-07-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UNIT"
  (:use "COMMON-LISP")
  (:export "FAHRENHEIT"
           "KELVIN"
           "CELCIUS"
           "RANKINE"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UNIT")

(defun fahrenheit (f)
  "Return f Fahrenheit in Kelvin."
  (* (- f 32.0) 5/9))

(defun kelvin (k)
  "Return k Kelvin in Kelvin."
  k)

(defun celcius (c)
  "Return c Celcius in Kelvin."
  (+ c 273.15))

(defun rankine (r)
  "Return r Rankine in Kelvin."
  (fahrenheit (- r 459.67)))

;;;; THE END ;;;;
