;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               string-output.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file defines the string output operators.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-14 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")

(defclass string-output-stream (string-stream)
  ((string :accessor %string-stream-output-string
           :initarg :string
           :initform (make-array 8
                                 :fill-pointer 0 :adjustable t
                                 :element-type 'character)
           :type     string)))


(defun make-string-output-stream (&key (element-type 'character))
  (make-instance 'string-output-stream
      :open-p t
      :input-p nil
      :output-p t
      :element-type 'character
      :external-format :default
      :string (make-array 8
                          :fill-pointer 0 :adjustable t
                          :element-type element-type)))


(define-stream-methods string-output-stream
    (write-byte
     (vector-push-extend (char-code byte)
                         (%string-stream-output-string stream)
                         (* 2 (length (%string-stream-output-string stream))))
     byte)
  (write-char
   (vector-push-extend character
                       (%string-stream-output-string stream)
                       (* 2 (length (%string-stream-output-string stream))))
   character))


(define-forward get-output-stream-string (string-output-stream)
  (declare (stream-argument   string-output-stream)
           (check-stream-type string-output-stream))
  (:method string-output-stream
    (prog1 (copy-seq (%string-stream-output-string string-output-stream))
      (setf (fill-pointer (%string-stream-output-string string-output-stream)) 0))))


;;;; THE END ;;;;
