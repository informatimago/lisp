;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               two-way-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the two-way stream operators.
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


(defclass two-way-stream (stream)
  ((input-stream  :accessor %two-way-stream-input-stream
                  :initarg :input-stream
                  :initform nil)
   (output-stream :accessor %two-way-stream-output-stream
                  :initarg :output-stream
                  :initform nil)))


(defun make-two-way-stream (input-stream output-stream)
  (unless (input-stream-p input-stream)
    (error (make-condition
            'simple-type-error 
            :datum input-stream
            :expected-type 'stream
            :format-control "Stream is not an input stream")))
  (unless (output-stream-p output-stream)
    (error (make-condition
            'simple-type-error 
            :datum output-stream
            :expected-type 'stream
            :format-control "Stream is not an output stream")))
  (unless (equal (stream-element-type input-stream)
                 (stream-element-type output-stream))
    (error "~S: the element types of the input stream ~S and the output stream ~S are not equal."
           'make-two-way-stream
           (stream-element-type input-stream)
           (stream-element-type output-stream)))
  (unless (equal (stream-external-format input-stream)
                 (stream-external-format output-stream))
    (error "~S: the external-formats of the input stream ~S and the output stream ~S are not equal."
           'make-two-way-stream
           (stream-external-format input-stream)
           (stream-external-format output-stream)))
  (make-instance 'two-way-stream
      :open-p t
      :input-p t
      :output-p t
      :element-type (stream-element-type input-stream)
      :external-format (stream-external-format input-stream)
      :input-stream input-stream
      :output-stream output-stream))


(define-forward two-way-stream-input-stream (two-way-stream)
  (declare (stream-argument two-way-stream)
           (check-stream-type two-way-stream)))


(define-forward two-way-stream-output-stream (two-way-stream)
  (declare (stream-argument two-way-stream)
           (check-stream-type two-way-stream)))


(define-stream-methods two-way-stream
    (read-byte        (read-byte (%two-way-stream-input-stream stream)
                                 eof-error-p eof-value))
  (read-char        (read-char (%two-way-stream-input-stream stream)
                               eof-error-p eof-value))
  (write-byte       (write-byte (%two-way-stream-output-stream stream)))
  (write-char       (write-char (%two-way-stream-output-stream stream)))
  (two-way-stream-input-stream  (%two-way-stream-input-stream  two-way-stream))
  (two-way-stream-output-stream (%two-way-stream-output-stream two-way-stream)))


;;;; THE END ;;;;
