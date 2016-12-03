;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               concatenated-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file defines the concatenated stream operators.
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


(defclass concatenated-stream (stream)
  ((streams :accessor %concatenated-stream-streams
            :initarg :streams
            :initform nil)))


(defun make-concatenated-stream (&rest input-streams)
  (dolist (stream streams)
    (unless (input-stream-p stream)
      (error (make-condition
              'simple-type-error
              :datum stream
              :expected-type 'stream
              :format-control "Stream is not an input stream"))))
  (make-instance 'concatenated-stream :streams input-streams))


(define-forward concatenated-stream-streams (concatenated-stream)
  (declare (stream-argument concatenated-stream)
           (check-stream-type concatenated-stream)))


(defun !concatenated-read-element (read-element
                                   stream eof-error-p eof-value recursive-p)
  (let ((current (first (%concatenated-stream-streams stream))))
    (if (null current)
        (eof-error stream eof-error-p eof-value)
        (let ((element (multiple-value-list
                        (funcall read-element current nil stream recursive-p))))
          (cond
            ((eq (car element) stream)
             (pop (%concatenated-stream-streams stream))
             (!concatenated-read-element
              read-element stream eof-error-p eof-value recursive-p))
            ((second element)
             (pop (%concatenated-stream-streams stream))
             (multiple-value-bind (line missing-newline-p)
                 (!concatenated-read-element
                  read-element stream eof-error-p eof-value recursive-p)
               (values (concatenate 'string (first element) line)
                       missing-newline-p)))
            (t (values-list element)))))))



(define-stream-methods concatenated-stream
    (read-byte         (!concatenated-read-element
                        (lambda (s e v r) (declare (ignore r)) (read-byte s e v))
                        stream eof-error-p eof-value nil))
  (read-char         (!concatenated-read-element
                      (function read-char)
                      stream eof-error-p eof-value recursive-p))
  (read-char-no-hang (!concatenated-read-element
                      (function read-char-no-hang)
                      stream eof-error-p eof-value recursive-p))
  (peek-char         (!concatenated-read-element
                      (lambda (s e v r) (peek-char peek-type s e v r))
                      stream eof-error-p eof-value recursive-p))
  (unread-char
   (let ((current (first (%concatenated-stream-streams stream))))
     (if (null current)
         (push (make-string-input-stream (string character))
               (%concatenated-stream-streams stream))
         (unread-char character current))))
  (read-line         (!concatenated-read-element
                      (lambda (s e v r) (declare (ignore r)) (read-line s e v))
                      stream eof-error-p eof-value recursive-p))
  (read-sequence
   (let ((current (first (%concatenated-stream-streams stream))))
     (if (null current)
         (eof-error stream eof-error-p eof-value)
         (let* ((end      (or end (length sequence)))
                (position (read-stream sequence current start end)))
           (if (< position end)
               (progn
                 (pop (%concatenated-stream-streams stream))
                 (setf current (first (%concatenated-stream-streams stream)))
                 (if (null current)
                     position
                     (read-sequence sequence stream :start position :end end)))
               position)))))
  (listen
   (let ((current (first (%concatenated-stream-streams stream))))
     (warn "LISTEN may return NIL in the middle of a concatenated-stream when we're at the end of one of the substreams")
     (listen current)))
  (clear-input
   (let ((current (first (%concatenated-stream-streams stream))))
     (and current (clear-input current))))
  (stream-external-format ;; or use the attribute?
   (let ((current (first (%concatenated-stream-streams stream))))
     (if current
         (stream-external-format current)
         :default)))
  (close
   (prog1 (%open-stream-p stream)
     (setf (%open-stream-p stream) nil
           (%concatenated-stream-streams stream) nil))))



;;;; THE END ;;;;
