;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               echo-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the echo stream operators.
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")

(defclass echo-stream (stream)
  ((input-stream  :accessor %echo-stream-input-stream
                  :initarg :input-stream
                  :initform nil)
   (output-stream :accessor %echo-stream-output-stream
                  :initarg :output-stream
                  :initform nil)))

(defun make-echo-stream (input-stream output-stream)
  (unless (input-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum input-stream
            :expected-type 'stream
            :format-control "Stream is not an input stream")))
  (unless (output-stream-p stream)
    (error (make-condition
            'simple-type-error 
            :datum output-stream
            :expected-type 'stream
            :format-control "Stream is not an output stream")))
  (make-instance 'echo-stream
    :input-stream input-stream
    :output-stream output-stream))

(define-forward echo-stream-input-stream (echo-stream)
  (declare (stream-argument echo-stream)
           (check-stream-type echo-stream)))

(define-forward echo-stream-output-stream (echo-stream)
  (declare (stream-argument echo-stream)
           (check-stream-type echo-stream)))

(define-stream-methods echo-stream
    (read-byte
     (let ((byte (read-byte (%echo-stream-input-stream stream) nil stream)))
       (if (eq byte stream)
           (eof-error stream eof-error-p eof-value)
           (progn 
             (write-byte byte  (%echo-stream-output-stream stream))
             byte))))
  
  (read-char
   (let ((char (read-char (%echo-stream-input-stream stream) nil stream)))
     (if (eq char stream)
         (eof-error stream eof-error-p eof-value)
         (progn 
           (write-char char (%echo-stream-output-stream stream))
           char))))
  (read-char-no-hang)
  (peek-char)
  (unread-char)
  (read-line)
  (read-sequence)
  (terpri)
  (fresh-line)
  (write-byte  (write-byte byte      (%echo-stream-output-stream stream)))
  (write-char  (write-char character (%echo-stream-output-stream stream)))
  (write-string)
  (write-line)
  (write-sequence)
  (listen)
  (clear-input)
  (clear-output)
  (force-output)
  (finish-output)

  (file-length)
  (file-position)
  (file-string-length)
  (stream-external-format)
  (close))
