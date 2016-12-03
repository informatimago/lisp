;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               broadcast-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file defines the broadcast stream operators.
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


(defclass broadcast-stream (stream)
  ((streams :accessor %broadcast-stream-streams
            :initarg :streams
            :initform nil)))


(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error (make-condition
              'simple-type-error
              :datum stream
              :expected-type 'stream
              :format-control "Stream is not an output stream"))))
  (make-instance 'broadcast-stream :streams streams))


(define-forward broadcast-stream-streams (broadcast-stream)
  (declare (stream-argument broadcast-stream)
           (check-stream-type broadcast-stream)))


(defmacro do-broadcast ((output-stream broadcast-stream)
                                      &body body)
  `(let ((results '()))
     (dolist (,output-stream (%broadcast-stream-streams ,broadcast-stream)
              (values-list results))
       (setf results (multiple-value-list (progn ,@body))))))



(define-stream-methods broadcast-stream
    (broadcast-stream-streams (%broadcast-stream-streams broadcast-stream))

  (write-byte              (do-broadcast (ostream stream)
                             (write-char byte ostream))
                           byte)
  (write-char              (do-broadcast (ostream stream)
                             (write-char character ostream))
                           character)
  (terpri                  (do-broadcast (ostream stream)
                             (terpri ostream))
                           nil)
  (fresh-line              (do-broadcast (ostream stream)
                             (fresh-line ostream)))
  (write-string            (do-broadcast (ostream stream)
                             (write-string string ostream :start start :end end))
                           string)
  (write-line              (do-broadcast (ostream stream)
                             (write-line string ostream :start start :end end))
                           string)
  (write-sequence          (do-broadcast (ostream stream)
                             (write-sequence sequence ostream :start start :end end))
                           sequence)
  (clear-output            (do-broadcast (ostream stream)
                             (clear-output ostream)))
  (force-output            (do-broadcast (ostream stream)
                             (force-output ostream)))
  (finish-output           (do-broadcast (ostream stream)
                             (finish-output ostream)))
  (file-length             (if (%broadcast-stream-streams stream)
                               (file-length
                                (first (last (%broadcast-stream-streams stream))))
                               0))
  (file-position           (if (%broadcast-stream-streams stream)
                               (file-position
                                (first (last (%broadcast-stream-streams stream))))
                               0))
  (file-string-length      (if (%broadcast-stream-streams stream)
                               (file-string-length
                                (first (last (%broadcast-stream-streams stream))))
                               1))
  (stream-external-format  (if (%broadcast-stream-streams stream)
                               (stream-external-format
                                (car (last (%broadcast-stream-streams stream))))
                               't))
  (close                   (prog1 (%open-stream-p stream)
                             (setf (%open-stream-p stream) nil
                                   (%broadcast-stream-streams stream) nil))))




;;;; THE END ;;;;

