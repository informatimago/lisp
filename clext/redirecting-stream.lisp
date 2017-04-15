;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               redirecting-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This Gray stream redirects to streams determined at I/O time.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2017
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.CLEXT.REDIRECTING-STREAM"
  (:use "COMMON-LISP"
        "TRIVIAL-GRAY-STREAMS")
  (:export "REDIRECTING-CHARACTER-OUTPUT-STREAM"
           "REDIRECTING-CHARACTER-INPUT-STREAM"))
(in-package "COM.INFORMATIMAGO.CLEXT.REDIRECTING-STREAM")

;; ---

(defclass redirecting-character-output-stream (fundamental-character-output-stream)
  ((output-stream-function :initarg :output-stream-function :reader output-stream-function)
   (column :initform 0 :accessor column)))

(defmethod stream-write-char ((stream redirecting-character-output-stream) character)
  (write-char character (funcall (output-stream-function stream)))
  (case character
    ((#\newline #\return #\page) (setf (column stream) 0))
    (otherwise                   (incf (column stream))))
  character)

(defmethod stream-line-column ((stream redirecting-character-output-stream))
  (column stream))

(defmethod stream-start-line-p ((stream redirecting-character-output-stream))
  (zerop (column stream)))

(defmethod stream-write-string ((stream redirecting-character-output-stream) string &optional (start 0) (end nil))
  (let ((end (or end (length string)))
        (last-newline (position-if (lambda (ch) (position ch #(#\newline #\return #\page)))
                               string :from-end t :start start :end end)))
    (write-string string (funcall (output-stream-function stream)) :start start :end end)
    (if last-newline
        (setf (column stream) (- end last-newline 1))
        (incf (column stream) (- end start)))
    string))

(defmethod stream-terpri ((stream redirecting-character-output-stream))
  (terpri (funcall (output-stream-function stream))))

(defmethod stream-fresh-line ((stream redirecting-character-output-stream))
  (fresh-line (funcall (output-stream-function stream))))

(defmethod stream-finish-output ((stream redirecting-character-output-stream))
  (finish-output (funcall (output-stream-function stream))))

(defmethod stream-force-output ((stream redirecting-character-output-stream))
  (force-output (funcall (output-stream-function stream))))

(defmethod stream-clear-output ((stream redirecting-character-output-stream))
  (clear-output (funcall (output-stream-function stream))))

(defmethod stream-advance-to-column ((stream redirecting-character-output-stream) column)
  (let ((spaces (- column (column stream))))
    (when (plusp spaces)
      (write-string (case spaces
                      ((1)  " ")
                      ((2)  "  ")
                      ((3)  "   ")
                      ((4)  "    ")
                      ((5)  "     ")
                      ((6)  "      ")
                      ((7)  "       ")
                      ((8)  "        ")
                      ((9)  "         ")
                      ((10) "          ")
                      ((11) "           ")
                      ((12) "            ")
                      (otherwise (make-string spaces :initial-element #\space)))
                    (funcall (output-stream-function stream))))))

;; ---

(defclass redirecting-character-input-stream (fundamental-character-input-stream)
  ((input-stream-function :initarg :input-stream-function :reader input-stream-function)))

(defmethod stream-read-char ((stream redirecting-character-input-stream))
  (read-char (funcall (input-stream-function stream))))

(defmethod stream-unread-char ((stream redirecting-character-input-stream) character)
  (unread-char (funcall (input-stream-function stream)) character))

(defmethod stream-read-char-no-hang ((stream redirecting-character-input-stream))
  (read-char-no-hang (funcall (input-stream-function stream))))

(defmethod stream-peek-char ((stream redirecting-character-input-stream))
  (peek-char (funcall (input-stream-function stream))))

(defmethod stream-listen ((stream redirecting-character-input-stream))
  (listen (funcall (input-stream-function stream))))

(defmethod stream-read-line ((stream redirecting-character-input-stream))
  (read-line (funcall (input-stream-function stream))))

(defmethod stream-clear-input ((stream redirecting-character-input-stream))
  (clear-input (funcall (input-stream-function stream))))

;;;; THE END ;;;;
