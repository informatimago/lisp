;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               string-input.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the string input operators.
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


(defclass string-input-stream (string-stream)
  ((string :accessor %string-stream-input-string
           :initarg :string
           :initform ""
           :type     string)
   (index  :accessor %string-stream-index
           :initarg :index
           :initform 0
           :type (integer 0))
   (start  :accessor %string-stream-start
           :initarg :start
           :initform 0
           :type (integer 0))
   (end    :accessor %string-stream-end
           :initarg :end
           :initform nil
           :type (or null (integer 0)))))


(defun make-string-input-stream (string &optional (start 0) (end nil))
  (make-instance 'string-input-stream
      :open-p t
      :input-p t
      :output-p nil
      :element-type 'character
      :external-format :default
      :string string
      :start start
      :end end))


(defun !string-input-read (stream)
  (if (< (%string-stream-index stream)
         (or (%string-stream-end stream) 
             (length (%string-stream-input-string stream))))
      (aref (%string-stream-input-string stream)
            (prog1 (%string-stream-index stream)
              (incf (%string-stream-index stream))))
      (eof-stream stream eof-error-p eof-value)))


(define-stream-methods string-input-stream
  (read-byte (char-code (!string-input-read stream)))
  (read-char (!string-input-read stream)))


;;;; THE END ;;;;
