;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               synonym-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the synonyms stream operators.
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


(defclass synonym-stream (stream)
  ((symbol  :accessor %synonym-stream-symbol
            :initarg :symbol)))


(defun make-synonym-stream (symbol)
  (check-type symbol symbol)
  (make-instance 'synonym-stream :symbol symbol))


(define-forward synonym-stream-symbol (synonym-stream)
  (declare (stream-argument synonym-stream)
           (check-stream-type synonym-stream)))


(define-stream-methods synonym-stream
    (synonym-stream-symbol (%synonym-stream-symbol synonym-stream))
  (read-byte   (read-byte  (symbol-value (%synonym-stream-symbol stream))
                           eof-error-p eof-value))
  (write-byte  (write-byte (symbol-value (%synonym-stream-symbol stream))))

  (read-char   (read-char  (symbol-value (%synonym-stream-symbol stream))
                           eof-error-p eof-value))
  (write-char  (write-char (symbol-value (%synonym-stream-symbol stream)))))


;;;; THE END ;;;;
