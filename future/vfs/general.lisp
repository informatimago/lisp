
;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               general.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the general file and stream  functions and macro.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-14 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")


(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply (function format) *query-io* format-string args)
    (write-string " (y/n) " *query-io*))
  (loop
     (let ((line (string-left-trim " " (read-line *query-io*))))
       (when (plusp (length line))
         (let ((first-char (char-upcase (char line 0))))
           (when (char-equal first-char #\n) (return nil))
           (when (char-equal first-char #\y) (return t))))
       (write-string "Please answer with 'y' or 'n': " *query-io*))))


(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply (function format) *query-io* format-string args)
    (write-string " (yes/no) " *query-io*))
  (loop
     (clear-input *query-io*)
     (let ((line (string-trim " " (read-line *query-io*))))
       (when (string-equal line "NO")  (return nil))
       (when (string-equal line "YES") (return t)))
     (write-string "Please answer with 'yes' or 'no': " *query-io*)))





;; Macros are taken from clisp sources, and adapted.
 
(defmacro with-open-file ((stream &rest options) &body body)
  (multiple-value-bind (body-rest declarations)  (parse-body :locally body)
    `(let ((,stream (open ,@options)))
       (declare (read-only ,stream) ,@declarations)
       (unwind-protect
            (multiple-value-prog1 (progn ,@body-rest)
              (when ,stream (close ,stream)))
         (when ,stream (close ,stream :abort t))))))


(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (body-rest declarations) (parse-body :locally body)
    `(let ((,var ,stream))
       (declare (read-only ,var) ,@declarations)
       (unwind-protect
            (multiple-value-prog1 (progn ,@body-rest) (close ,var))
         (close ,var :abort t)))))


(defmacro with-input-from-string ((var string  &key (index nil sindex) 
                                       (start '0 sstart) (end 'nil send))
                                  &body body)
  (multiple-value-bind (body-rest declarations) (parse-body :loally body)
    `(let ((,var (make-string-input-stream 
                  ,string
                  ,@(if (or sstart send)
                        `(,start ,@(if send `(,end) '()))
                        '()))))
       (declare (read-only ,var) ,@declarations)
       (unwind-protect
            (progn ,@body-rest)
         ,@(when sindex `((setf ,index (%string-stream-index ,var))))
         (close ,var)))))


(defmacro with-output-to-string ((var &optional (string nil)
                                      &key (element-type ''character))
                                 &body body)
  (multiple-value-bind (body-rest declarations) (parse-body :locally body)
    (if string
        (let ((ignored-var (gensym)))
          `(let ((,var (make-instance 'string-output-stream :string ,string))
                 (,ignored-var ,element-type))
             (declare (read-only ,var) (ignore ,ignored-var) ,@declarations)
             (unwind-protect
                  (progn ,@body-rest)
               (close ,var))))
        `(let ((,var (make-string-output-stream :element-type ,element-type)))
           (declare (read-only ,var) ,@declarations)
           (unwind-protect
                (progn ,@body-rest (get-output-stream-string ,var))
             (close ,var))))))


;;;; THE END ;;;;
