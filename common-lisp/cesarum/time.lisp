;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               time.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Provides a time more precise than GET-UNIVERSAL-TIME.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-12-05 <PJB> Extracted from Activity.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.TIME"
  (:use "COMMON-LISP")
  (:export
   "INITIALIZE-REAL-TIME-OFFSET"
   "GET-REAL-TIME"
   "GET-RUN-TIME"
   "CHRONO-TIME"
   "CHRONO-REAL-TIME"
   "CHRONO-RUN-TIME"
   "FORMAT-TIME"
   "*INTERNAL-TIME-UNIT*")
  (:documentation "

This package provides two clocks more precise than GET-UNIVERSAL-TIME
which has a 1 second resolution, using the
INTERNAL-TIME-UNITS-PER-SECOND.

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.TIME")

(defparameter *internal-time-unit*
  (coerce (/ internal-time-units-per-second) 'double-float)
  "The internal time slice, in seconds, as a DOUBLE-FLOAT.")

(defvar *precise-real-time-offset* 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun initialize-real-time-offset ()
    "This should be called once per POSIX process before GET-REAL-TIME."
    (setf *precise-real-time-offset*
          (loop
            :with now = (get-universal-time)
            :while (= now (get-universal-time))
            :finally (return
                       (- now (* (get-internal-real-time) *internal-time-unit*)))))))

(defparameter *precise-real-time-offset* (initialize-real-time-offset)
    "Contains the number of seconds that must be added to:
      (/ (GET-INTERNAL-REAL-TIME) INTERNAL-TIME-UNITS-PER-SECOND)
to get the current universal-time with the higher internal time precision.")

(defun get-real-time ()
  "
RETURN: The universal-time (in seconds), offset by the
        internal-real-time fraction.
"
  (+ *precise-real-time-offset*
     (* (get-internal-real-time) *internal-time-unit*)))

(defun get-run-time ()
  "
RETURN: The run-time (in seconds).
        Run-time is not synchronized with real-time,
        since the process is scheduled by the kernel.
"
  (* (get-internal-run-time) *internal-time-unit*))



(defun chrono-time* (thunk)
  "
Call the THUNK.
RETURN: the real-time, the run-time, followed by the results of THUNK.
"
  (let* ((start-real   (get-real-time))
         (start-run    (get-run-time))
         (results      (multiple-value-list (funcall thunk)))
         (run-time     (- (get-run-time)  start-run))
         (real-time    (- (get-real-time) start-real)))
    (values-list  (list* real-time run-time results))))

(defmacro chrono-time (&body body)
  "RETURN: the real-time, the run-time, followed by the results of BODY."
  `(chrono-time* (lambda () ,@body)))

(defmacro chrono-real-time (&body body)
  "RETURN: the real-time, followed by the results of BODY."
  `(let ((values (multiple-value-list (chrono-time* (lambda () ,@body)))))
     (values-list (cons (first values) (rest (rest values))))))

(defmacro chrono-run-time (&body body)
  "RETURN: the run-time, followed by the results of BODY."
  `(let ((values (multiple-value-list (chrono-time* (lambda () ,@body)))))
     (values-list (rest values))))


(defun format-time (time)
  (multiple-value-bind (se mi ho) (decode-universal-time time 0)
    (format nil "~2,'0D:~2,'0D:~2,'0D" ho mi (truncate se))))


(defun test/all ()
  (list
   (multiple-value-list (chrono-time (progn (sleep 1) (values 1 2 3))))
   (multiple-value-list (chrono-real-time (progn (sleep 1) (values 1 2 3))))
   (multiple-value-list (chrono-run-time (progn (sleep 1) (values 1 2 3))))))

;;;; THE END ;;;;
